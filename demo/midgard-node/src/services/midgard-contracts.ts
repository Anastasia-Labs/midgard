import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { normalizeOutRef } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applyParamsToScript,
  Constr,
  credentialToAddress,
  Data,
  MintingPolicy,
  mintingPolicyToId,
  Network,
  scriptHashToCredential,
  SpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
  WithdrawalValidator,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { AlwaysSucceedsContract } from "./always-succeeds.js";
import { NodeConfig } from "./config.js";

/**
 * Contract-loading service for Midgard validators.
 *
 * This module can either expose the always-succeeds bundle for test flows or
 * derive the real script set from a blueprint, applying protocol parameters
 * where required.
 */
type BlueprintValidator = {
  title: string;
  compiledCode: string;
};

type Blueprint = {
  validators: BlueprintValidator[];
};

const moduleDir = path.dirname(fileURLToPath(import.meta.url));
const DEFAULT_REAL_BLUEPRINT_CANDIDATES = [
  path.resolve(moduleDir, "../../../../onchain/aiken/plutus.json"),
  path.resolve(moduleDir, "../../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "onchain/aiken/plutus.json"),
] as const;

/**
 * Cached real blueprint loaded from either `MIDGARD_REAL_BLUEPRINT_PATH` or
 * the canonical onchain Aiken build output.
 */
let cachedRealBlueprint:
  | {
      readonly path: string;
      readonly blueprint: Blueprint;
    }
  | undefined;

const parseBlueprint = (raw: string, sourcePath: string): Blueprint => {
  const parsed = JSON.parse(raw) as unknown;
  if (
    typeof parsed !== "object" ||
    parsed === null ||
    !Array.isArray((parsed as { validators?: unknown }).validators)
  ) {
    throw new Error(
      `Blueprint at "${sourcePath}" does not have a validators array`,
    );
  }
  return parsed as Blueprint;
};

const resolveDefaultRealBlueprintPath = (): string => {
  for (const candidate of new Set(DEFAULT_REAL_BLUEPRINT_CANDIDATES)) {
    if (existsSync(candidate)) {
      return candidate;
    }
  }

  throw new Error(
    `Failed to locate canonical real blueprint. Looked in: ${DEFAULT_REAL_BLUEPRINT_CANDIDATES.join(", ")}`,
  );
};

/**
 * Loads the real-contract blueprint, optionally honoring an override path from
 * the environment.
 */
const loadRealBlueprint = (): Effect.Effect<Blueprint, Error> =>
  Effect.try({
    try: () => {
      const configuredPath = process.env.MIDGARD_REAL_BLUEPRINT_PATH?.trim();
      const blueprintPath = configuredPath
        ? configuredPath
        : resolveDefaultRealBlueprintPath();

      if (cachedRealBlueprint?.path === blueprintPath) {
        return cachedRealBlueprint.blueprint;
      }

      const blueprint = parseBlueprint(
        readFileSync(blueprintPath, "utf8"),
        blueprintPath,
      );

      cachedRealBlueprint = {
        path: blueprintPath,
        blueprint,
      };
      return blueprint;
    },
    catch: (cause) =>
      new Error(`Failed to load real blueprint: ${formatUnknownError(cause)}`),
  });

/**
 * Blueprint titles for the real state-queue scripts.
 */
export const REAL_STATE_QUEUE_SCRIPT_TITLES = {
  mint: "state_queue.mint.mint",
  spend: "state_queue.spend.spend",
} as const;

/**
 * Blueprint titles for the real hub-oracle scripts.
 */
export const REAL_HUB_ORACLE_SCRIPT_TITLES = {
  mint: "hub_oracle.mint.mint",
} as const;

/**
 * Blueprint titles for the real registered-operators scripts.
 */
export const REAL_REGISTERED_OPERATORS_SCRIPT_TITLES = {
  mint: "operator_directory/registered_operators.mint.mint",
  spend: "operator_directory/registered_operators.spend.spend",
} as const;

/**
 * Blueprint titles for the real active-operators scripts.
 */
export const REAL_ACTIVE_OPERATORS_SCRIPT_TITLES = {
  mint: "operator_directory/active_operators.mint.mint",
  spend: "operator_directory/active_operators.spend.spend",
} as const;

/**
 * Blueprint titles for the real retired-operators scripts.
 */
export const REAL_RETIRED_OPERATORS_SCRIPT_TITLES = {
  mint: "operator_directory/retired_operators.mint.mint",
  spend: "operator_directory/retired_operators.spend.spend",
} as const;

/**
 * Blueprint titles for the real scheduler scripts.
 */
export const REAL_SCHEDULER_SCRIPT_TITLES = {
  mint: "scheduler.mint.mint",
  spend: "scheduler.spend.spend",
} as const;

/**
 * Blueprint titles for the real deposit scripts.
 */
export const REAL_DEPOSIT_SCRIPT_TITLES = {
  mint: "user_events/deposit.mint.mint",
  spend: "user_events/deposit.spend.spend",
} as const;

/**
 * Blueprint titles for the real tx-order scripts.
 */
export const REAL_TX_ORDER_SCRIPT_TITLES = {
  mint: "user_events/tx_order.mint.mint",
  spend: "user_events/tx_order.spend.spend",
} as const;

/**
 * Blueprint titles for the real withdrawal scripts.
 */
export const REAL_WITHDRAWAL_SCRIPT_TITLES = {
  mint: "user_events/withdrawal.mint.mint",
  spend: "user_events/withdrawal.spend.spend",
} as const;

/**
 * Blueprint titles for the real settlement scripts.
 */
export const REAL_SETTLEMENT_SCRIPT_TITLES = {
  mint: "settlement.mint.mint",
  spend: "settlement.spend.spend",
} as const;

/**
 * Blueprint titles for the real reserve scripts.
 */
export const REAL_RESERVE_SCRIPT_TITLES = {
  spend: "reserve.spend.spend",
  withdraw: "reserve.withdraw.else",
} as const;

/**
 * Blueprint titles for the real payout scripts.
 */
export const REAL_PAYOUT_SCRIPT_TITLES = {
  mint: "payout.mint.mint",
  spend: "payout.spend.spend",
} as const;

export const REAL_FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES = {
  mint: "fraud_proof_catalogue.mint.mint",
  spend: "fraud_proof_catalogue.spend.else",
} as const;

/**
 * One-shot outref used to parameterize the real hub-oracle policy.
 */
export type HubOracleOneShotOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

type ScriptParams = Data[];

type AuthenticatedScriptTitles = {
  readonly mint: string;
  readonly spend: string;
};

/**
 * Normalizes the configured one-shot outref used to parameterize the real
 * hub-oracle policy.
 */
const normalizeHubOracleOneShotOutRef = (
  outRef: HubOracleOneShotOutRef,
): Effect.Effect<HubOracleOneShotOutRef, Error> =>
  Effect.try({
    try: () => normalizeOutRef(outRef),
    catch: (cause) =>
      new Error(`Invalid hub-oracle one-shot outref: ${String(cause)}`),
  });

/**
 * Looks up a compiled script by title inside the resolved blueprint.
 */
const getCompiledScript = (
  blueprint: Blueprint,
  title: string,
): Effect.Effect<string, Error> =>
  Effect.gen(function* () {
    const found = blueprint.validators.find(
      (validator) => validator.title === title,
    );
    if (found === undefined) {
      return yield* Effect.fail(
        new Error(`Validator with title "${title}" not found in blueprint`),
      );
    }
    return found.compiledCode;
  });

const makeMintingPolicy = (mintingScriptCBOR: string): SDK.MintingValidator => {
  const mintingScript: MintingPolicy = {
    type: "PlutusV3",
    script: mintingScriptCBOR,
  };
  return {
    mintingScriptCBOR,
    mintingScript,
    policyId: mintingPolicyToId(mintingScript),
  };
};

const makeSpendingValidator = (
  network: Network,
  spendingScriptCBOR: string,
): SDK.SpendingValidator => {
  const spendingScript: SpendingValidator = {
    type: "PlutusV3",
    script: spendingScriptCBOR,
  };
  return {
    spendingScriptCBOR,
    spendingScript,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
    spendingScriptHash: validatorToScriptHash(spendingScript),
  };
};

const makeWithdrawalValidator = (
  withdrawalScriptCBOR: string,
): SDK.WithdrawalValidator => {
  const withdrawalScript: WithdrawalValidator = {
    type: "PlutusV3",
    script: withdrawalScriptCBOR,
  };
  return {
    withdrawalScriptCBOR,
    withdrawalScript,
    withdrawalScriptHash: validatorToScriptHash(withdrawalScript),
  };
};

const makeAuthenticatedValidator = (
  network: Network,
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): SDK.AuthenticatedValidator => ({
  ...makeSpendingValidator(network, spendingScriptCBOR),
  ...makeMintingPolicy(mintingScriptCBOR),
});

const buildRealAuthenticatedValidator = (
  network: Network,
  titles: AuthenticatedScriptTitles,
  mintParams: ScriptParams,
  spendParams?: (policyId: string) => ScriptParams,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const mintBase = yield* getCompiledScript(blueprint, titles.mint);
    const spendBase = yield* getCompiledScript(blueprint, titles.spend);
    const mintingScriptCBOR = applyParamsToScript(mintBase, mintParams);
    const { policyId } = makeMintingPolicy(mintingScriptCBOR);
    const spendingScriptCBOR =
      spendParams === undefined
        ? spendBase
        : applyParamsToScript(spendBase, spendParams(policyId));
    return makeAuthenticatedValidator(
      network,
      mintingScriptCBOR,
      spendingScriptCBOR,
    );
  });

/**
 * Builds the real hub-oracle minting validator parameterized by the configured
 * one-shot outref.
 */
const buildRealHubOracleValidator = (
  network: Network,
  fallbackSpendingValidator: SDK.SpendingValidator,
  oneShotOutRef: HubOracleOneShotOutRef,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const mintBase = yield* getCompiledScript(
      blueprint,
      REAL_HUB_ORACLE_SCRIPT_TITLES.mint,
    );
    const initOutRef = new Constr(0, [
      oneShotOutRef.txHash,
      BigInt(oneShotOutRef.outputIndex),
    ]);
    const mintingScriptCBOR = applyParamsToScript(mintBase, [
      initOutRef,
      SDK.HUB_ORACLE_ASSET_NAME,
    ]);
    const mintingScript: MintingPolicy = {
      type: "PlutusV3",
      script: mintingScriptCBOR,
    };
    const policyId = mintingPolicyToId(mintingScript);
    return {
      spendingScriptCBOR: fallbackSpendingValidator.spendingScriptCBOR,
      spendingScript: fallbackSpendingValidator.spendingScript,
      // The canonical Aiken tree only ships the one-shot mint policy. The
      // witness UTxO lives at the script credential derived from that policy id.
      spendingScriptHash: policyId,
      spendingScriptAddress: credentialToAddress(
        network,
        scriptHashToCredential(policyId),
      ),
      mintingScriptCBOR,
      mintingScript,
      policyId,
    };
  });

const buildRealFraudProofCatalogueValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
  );

/**
 * Builds the real state-queue authenticated validator.
 */
const buildRealStateQueueValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const activeOperatorsAddress = yield* Effect.mapError(
      Effect.map(
        SDK.addressDataFromBech32(
          contracts.activeOperators.spendingScriptAddress,
        ),
        (addressData) => Data.from(Data.to(addressData, SDK.AddressData)),
      ),
      (cause) =>
        new Error(
          `Failed to encode active-operators address for state_queue mint parameters: ${String(cause)}`,
        ),
    );
    return yield* buildRealAuthenticatedValidator(
      network,
      REAL_STATE_QUEUE_SCRIPT_TITLES,
      [
        contracts.hubOracle.policyId,
        contracts.activeOperators.policyId,
        activeOperatorsAddress,
        contracts.retiredOperators.policyId,
        contracts.scheduler.policyId,
        contracts.fraudProof.policyId,
        contracts.settlement.policyId,
      ],
      (policyId) => [policyId],
    );
  });

/**
 * Builds the real registered-operators authenticated validator.
 */
const buildRealRegisteredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_REGISTERED_OPERATORS_SCRIPT_TITLES,
    [contracts.retiredOperators.policyId, contracts.hubOracle.policyId],
    (policyId) => [policyId],
  );

/**
 * Builds the real active-operators authenticated validator.
 */
const buildRealActiveOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_ACTIVE_OPERATORS_SCRIPT_TITLES,
    [
      contracts.hubOracle.policyId,
      contracts.registeredOperators.policyId,
      contracts.retiredOperators.policyId,
    ],
    (policyId) => [policyId, contracts.hubOracle.policyId],
  );

/**
 * Builds the real retired-operators authenticated validator.
 */
const buildRealRetiredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_RETIRED_OPERATORS_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    (policyId) => [policyId],
  );

/**
 * Builds the real scheduler authenticated validator.
 */
const buildRealSchedulerValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const activeOperatorsAddress = yield* Effect.mapError(
      Effect.map(
        SDK.addressDataFromBech32(
          contracts.activeOperators.spendingScriptAddress,
        ),
        (addressData) => Data.from(Data.to(addressData, SDK.AddressData)),
      ),
      (cause) =>
        new Error(
          `Failed to encode active-operators address for scheduler spend parameters: ${String(cause)}`,
        ),
    );
    return yield* buildRealAuthenticatedValidator(
      network,
      REAL_SCHEDULER_SCRIPT_TITLES,
      [contracts.hubOracle.policyId],
      (policyId) => [
        contracts.registeredOperators.policyId,
        activeOperatorsAddress,
        contracts.activeOperators.policyId,
        policyId,
        contracts.hubOracle.policyId,
      ],
    );
  });

/**
 * Builds the real deposit authenticated validator.
 */
const buildRealDepositValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_DEPOSIT_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    () => [contracts.hubOracle.policyId],
  );

const buildRealTxOrderValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_TX_ORDER_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    () => [contracts.hubOracle.policyId],
  );

const buildRealWithdrawalValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_WITHDRAWAL_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    () => [contracts.hubOracle.policyId],
  );

const buildRealSettlementValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_SETTLEMENT_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    (policyId) => [contracts.hubOracle.policyId, policyId],
  );

const buildRealReserveValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.SpendingValidator & SDK.WithdrawalValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const spendBase = yield* getCompiledScript(
      blueprint,
      REAL_RESERVE_SCRIPT_TITLES.spend,
    );
    const withdrawScriptCBOR = yield* getCompiledScript(
      blueprint,
      REAL_RESERVE_SCRIPT_TITLES.withdraw,
    );

    const spendingScriptCBOR = applyParamsToScript(spendBase, [
      contracts.hubOracle.policyId,
    ]);

    return {
      ...makeSpendingValidator(network, spendingScriptCBOR),
      ...makeWithdrawalValidator(withdrawScriptCBOR),
    };
  });

const buildRealPayoutValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_PAYOUT_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    () => [contracts.hubOracle.policyId],
  );

/**
 * Replaces hub-oracle, deposit, operator-list, scheduler, and state-queue
 * contracts with their real blueprint-derived counterparts.
 */
export const withRealStateQueueAndOperatorContracts = (
  network: Network,
  baseContracts: SDK.MidgardValidators,
  hubOracleOneShotOutRef: HubOracleOneShotOutRef,
): Effect.Effect<SDK.MidgardValidators, Error> =>
  Effect.gen(function* () {
    const normalizedOneShotOutRef = yield* normalizeHubOracleOneShotOutRef(
      hubOracleOneShotOutRef,
    );

    const realHubOracle = yield* buildRealHubOracleValidator(
      network,
      baseContracts.hubOracle,
      normalizedOneShotOutRef,
    );
    const withRealHubOracle: SDK.MidgardValidators = {
      ...baseContracts,
      hubOracle: realHubOracle,
    };

    const realFraudProofCatalogue =
      yield* buildRealFraudProofCatalogueValidator(network, withRealHubOracle);
    const withRealFraudProofCatalogue: SDK.MidgardValidators = {
      ...withRealHubOracle,
      fraudProofCatalogue: realFraudProofCatalogue,
    };

    const realFaultProofs = yield* SDK.buildFaultProofContracts({
      blueprint: SDK.parseFaultProofBlueprint(yield* loadRealBlueprint()),
      network,
      hubOraclePolicyId: withRealFraudProofCatalogue.hubOracle.policyId,
      fraudProofCataloguePolicyId:
        withRealFraudProofCatalogue.fraudProofCatalogue.policyId,
    });
    const withRealFraudProof: SDK.MidgardValidators = {
      ...withRealFraudProofCatalogue,
      fraudProof: realFaultProofs.fraudProof,
      fraudProofs: realFaultProofs.fraudProofs,
    };

    const realRetiredOperators = yield* buildRealRetiredOperatorsValidator(
      network,
      withRealFraudProof,
    );
    const withRealRetiredOperators: SDK.MidgardValidators = {
      ...withRealFraudProof,
      retiredOperators: realRetiredOperators,
    };

    const realRegisteredOperators =
      yield* buildRealRegisteredOperatorsValidator(
        network,
        withRealRetiredOperators,
      );
    const withRealRegisteredOperators: SDK.MidgardValidators = {
      ...withRealRetiredOperators,
      registeredOperators: realRegisteredOperators,
    };

    const realActiveOperators = yield* buildRealActiveOperatorsValidator(
      network,
      withRealRegisteredOperators,
    );
    const withRealOperatorSets: SDK.MidgardValidators = {
      ...withRealRegisteredOperators,
      activeOperators: realActiveOperators,
    };

    const realDeposit = yield* buildRealDepositValidator(
      network,
      withRealOperatorSets,
    );
    const withRealHubOracleAndDeposit: SDK.MidgardValidators = {
      ...withRealOperatorSets,
      deposit: realDeposit,
    };

    const realTxOrder = yield* buildRealTxOrderValidator(
      network,
      withRealHubOracleAndDeposit,
    );
    const withRealHubOracleDepositAndTxOrder: SDK.MidgardValidators = {
      ...withRealHubOracleAndDeposit,
      txOrder: realTxOrder,
    };

    const realWithdrawal = yield* buildRealWithdrawalValidator(
      network,
      withRealHubOracleDepositAndTxOrder,
    );
    const withRealUserEvents: SDK.MidgardValidators = {
      ...withRealHubOracleDepositAndTxOrder,
      withdrawal: realWithdrawal,
    };

    const realScheduler = yield* buildRealSchedulerValidator(
      network,
      withRealUserEvents,
    );
    const withRealScheduler: SDK.MidgardValidators = {
      ...withRealUserEvents,
      scheduler: realScheduler,
    };

    const realSettlement = yield* buildRealSettlementValidator(
      network,
      withRealScheduler,
    );
    const withRealSettlement: SDK.MidgardValidators = {
      ...withRealScheduler,
      settlement: realSettlement,
    };

    const realStateQueue = yield* buildRealStateQueueValidator(
      network,
      withRealSettlement,
    );

    const withRealStateQueue: SDK.MidgardValidators = {
      ...withRealSettlement,
      stateQueue: realStateQueue,
    };

    const realPayout = yield* buildRealPayoutValidator(
      network,
      withRealStateQueue,
    );
    const withRealPayout: SDK.MidgardValidators = {
      ...withRealStateQueue,
      payout: realPayout,
    };

    const realReserve = yield* buildRealReserveValidator(
      network,
      withRealPayout,
    );
    return {
      ...withRealPayout,
      reserve: realReserve,
    };
  });

/**
 * Resolves the production validator bundle from node configuration.
 *
 * The effect fails fast if the one-shot hub-oracle parameters are missing so a
 * node cannot boot into an ambiguous real-contract configuration.
 */
const makeMidgardContracts = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const baseContracts = yield* AlwaysSucceedsContract;
  const oneShotOutRef: HubOracleOneShotOutRef = {
    txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
    outputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
  };
  const resolvedContracts = yield* withRealStateQueueAndOperatorContracts(
    nodeConfig.NETWORK,
    baseContracts,
    oneShotOutRef,
  );
  yield* Effect.logInfo(
    "🔐 Contract source selected: state_queue=real, hub_oracle=real, deposit=real, tx_order=real, withdrawal=real, settlement=real, reserve=real, payout=real, registered_operators=real, active_operators=real, retired_operators=real, scheduler=real, fraud_proofs=real",
  );
  return resolvedContracts;
}).pipe(Effect.orDie);

/**
 * Service providing the validator bundle used by the node.
 */
export class MidgardContracts extends Effect.Service<MidgardContracts>()(
  "MidgardContracts",
  {
    effect: makeMidgardContracts,
    dependencies: [AlwaysSucceedsContract.Default, NodeConfig.layer],
  },
) {}
