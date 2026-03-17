import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  MintingPolicy,
  Network,
  SpendingValidator,
  applyParamsToScript,
  mintingPolicyToId,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract } from "./always-succeeds.js";
import { NodeConfig } from "./config.js";
import * as realScripts from "../../blueprints/real/plutus.json" with { type: "json" };

type BlueprintValidator = {
  title: string;
  compiledCode: string;
};

type Blueprint = {
  validators: BlueprintValidator[];
};

export const REAL_STATE_QUEUE_SCRIPT_TITLES = {
  mint: "state_queue.mint.mint",
  spend: "state_queue.spend.spend",
} as const;

export const REAL_HUB_ORACLE_SCRIPT_TITLES = {
  mint: "hub_oracle.mint.mint",
} as const;

export const REAL_REGISTERED_OPERATORS_SCRIPT_TITLES = {
  mint: "registered_operators.mint.mint",
  spend: "registered_operators.spend.spend",
} as const;

export const REAL_ACTIVE_OPERATORS_SCRIPT_TITLES = {
  mint: "active_operators.mint.mint",
  spend: "active_operators.spend.spend",
} as const;

export const REAL_RETIRED_OPERATORS_SCRIPT_TITLES = {
  mint: "retired_operators.mint.mint",
  spend: "retired_operators.spend.spend",
} as const;

export type HubOracleOneShotOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type OperatorContractParams = {
  readonly requiredBondLovelace: bigint;
  readonly slashingPenaltyLovelace: bigint;
};

const TX_HASH_PATTERN = /^[0-9a-fA-F]{64}$/;

const validateHubOracleOneShotOutRef = (
  outRef: HubOracleOneShotOutRef,
): Effect.Effect<void, Error> =>
  Effect.gen(function* () {
    if (!TX_HASH_PATTERN.test(outRef.txHash)) {
      return yield* Effect.fail(
        new Error(
          `Invalid HUB_ORACLE_ONE_SHOT_TX_HASH: expected 64 hex chars, got "${outRef.txHash}"`,
        ),
      );
    }
    if (!Number.isInteger(outRef.outputIndex) || outRef.outputIndex < 0) {
      return yield* Effect.fail(
        new Error(
          `Invalid HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: expected non-negative integer, got "${outRef.outputIndex}"`,
        ),
      );
    }
  });

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

const buildRealHubOracleValidator = (
  oneShotOutRef: HubOracleOneShotOutRef,
): Effect.Effect<SDK.MintingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = realScripts.default as Blueprint;
    const mintBase = yield* getCompiledScript(
      blueprint,
      REAL_HUB_ORACLE_SCRIPT_TITLES.mint,
    );
    const initOutRef = new Constr(0, [
      oneShotOutRef.txHash.toLowerCase(),
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
    return {
      mintingScriptCBOR,
      mintingScript,
      policyId: mintingPolicyToId(mintingScript),
    };
  });

const buildRealStateQueueValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = realScripts.default as Blueprint;
    const mintBase = yield* getCompiledScript(
      blueprint,
      REAL_STATE_QUEUE_SCRIPT_TITLES.mint,
    );
    const spendBase = yield* getCompiledScript(
      blueprint,
      REAL_STATE_QUEUE_SCRIPT_TITLES.spend,
    );

    const mintingScriptCBOR = applyParamsToScript(mintBase, [
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
      contracts.activeOperators.policyId,
      contracts.retiredOperators.policyId,
      contracts.scheduler.policyId,
      contracts.fraudProof.policyId,
      contracts.settlement.policyId,
    ]);

    const mintingScript: MintingPolicy = {
      type: "PlutusV3",
      script: mintingScriptCBOR,
    };
    const policyId = mintingPolicyToId(mintingScript);
    const spendingScriptCBOR = applyParamsToScript(spendBase, [policyId]);
    const spendingScript: SpendingValidator = {
      type: "PlutusV3",
      script: spendingScriptCBOR,
    };

    return {
      spendingScriptCBOR,
      spendingScript,
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      spendingScriptHash: validatorToScriptHash(spendingScript),
      mintingScriptCBOR,
      mintingScript,
      policyId,
    };
  });

const buildRealRegisteredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  operatorParams: OperatorContractParams,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = realScripts.default as Blueprint;
    const mintBase = yield* getCompiledScript(
      blueprint,
      REAL_REGISTERED_OPERATORS_SCRIPT_TITLES.mint,
    );
    const spendBase = yield* getCompiledScript(
      blueprint,
      REAL_REGISTERED_OPERATORS_SCRIPT_TITLES.spend,
    );

    const mintingScriptCBOR = applyParamsToScript(mintBase, [
      operatorParams.requiredBondLovelace,
      operatorParams.slashingPenaltyLovelace,
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    ]);
    const mintingScript: MintingPolicy = {
      type: "PlutusV3",
      script: mintingScriptCBOR,
    };
    const policyId = mintingPolicyToId(mintingScript);
    const spendingScriptCBOR = applyParamsToScript(spendBase, [policyId]);
    const spendingScript: SpendingValidator = {
      type: "PlutusV3",
      script: spendingScriptCBOR,
    };

    return {
      spendingScriptCBOR,
      spendingScript,
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      spendingScriptHash: validatorToScriptHash(spendingScript),
      mintingScriptCBOR,
      mintingScript,
      policyId,
    };
  });

const buildRealActiveOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  operatorParams: OperatorContractParams,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = realScripts.default as Blueprint;
    const mintBase = yield* getCompiledScript(
      blueprint,
      REAL_ACTIVE_OPERATORS_SCRIPT_TITLES.mint,
    );
    const spendBase = yield* getCompiledScript(
      blueprint,
      REAL_ACTIVE_OPERATORS_SCRIPT_TITLES.spend,
    );

    const mintingScriptCBOR = applyParamsToScript(mintBase, [
      operatorParams.slashingPenaltyLovelace,
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    ]);
    const mintingScript: MintingPolicy = {
      type: "PlutusV3",
      script: mintingScriptCBOR,
    };
    const policyId = mintingPolicyToId(mintingScript);
    const spendingScriptCBOR = applyParamsToScript(spendBase, [
      policyId,
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    ]);
    const spendingScript: SpendingValidator = {
      type: "PlutusV3",
      script: spendingScriptCBOR,
    };

    return {
      spendingScriptCBOR,
      spendingScript,
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      spendingScriptHash: validatorToScriptHash(spendingScript),
      mintingScriptCBOR,
      mintingScript,
      policyId,
    };
  });

const buildRealRetiredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  operatorParams: OperatorContractParams,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = realScripts.default as Blueprint;
    const mintBase = yield* getCompiledScript(
      blueprint,
      REAL_RETIRED_OPERATORS_SCRIPT_TITLES.mint,
    );
    const spendBase = yield* getCompiledScript(
      blueprint,
      REAL_RETIRED_OPERATORS_SCRIPT_TITLES.spend,
    );

    const mintingScriptCBOR = applyParamsToScript(mintBase, [
      operatorParams.slashingPenaltyLovelace,
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    ]);
    const mintingScript: MintingPolicy = {
      type: "PlutusV3",
      script: mintingScriptCBOR,
    };
    const policyId = mintingPolicyToId(mintingScript);
    const spendingScriptCBOR = applyParamsToScript(spendBase, [policyId]);
    const spendingScript: SpendingValidator = {
      type: "PlutusV3",
      script: spendingScriptCBOR,
    };

    return {
      spendingScriptCBOR,
      spendingScript,
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      spendingScriptHash: validatorToScriptHash(spendingScript),
      mintingScriptCBOR,
      mintingScript,
      policyId,
    };
  });

export const withRealStateQueueContracts = (
  network: Network,
  baseContracts: SDK.MidgardValidators,
  hubOracleOneShotOutRef: HubOracleOneShotOutRef,
): Effect.Effect<SDK.MidgardValidators, Error> =>
  Effect.gen(function* () {
    yield* validateHubOracleOneShotOutRef(hubOracleOneShotOutRef);

    const realHubOracle = yield* buildRealHubOracleValidator(
      hubOracleOneShotOutRef,
    );
    const withRealHubOracle: SDK.MidgardValidators = {
      ...baseContracts,
      hubOracle: realHubOracle,
    };

    const realStateQueue = yield* buildRealStateQueueValidator(
      network,
      withRealHubOracle,
    );
    return {
      ...withRealHubOracle,
      stateQueue: realStateQueue,
    };
  });

export const withRealStateQueueAndOperatorContracts = (
  network: Network,
  baseContracts: SDK.MidgardValidators,
  hubOracleOneShotOutRef: HubOracleOneShotOutRef,
  operatorParams: OperatorContractParams = {
    requiredBondLovelace: 5_000_000n,
    slashingPenaltyLovelace: 200_000n,
  },
): Effect.Effect<SDK.MidgardValidators, Error> =>
  Effect.gen(function* () {
    yield* validateHubOracleOneShotOutRef(hubOracleOneShotOutRef);

    const realHubOracle = yield* buildRealHubOracleValidator(
      hubOracleOneShotOutRef,
    );
    const withRealHubOracle: SDK.MidgardValidators = {
      ...baseContracts,
      hubOracle: realHubOracle,
    };

    const realRegisteredOperators = yield* buildRealRegisteredOperatorsValidator(
      network,
      withRealHubOracle,
      operatorParams,
    );
    const withRealRegisteredOperators: SDK.MidgardValidators = {
      ...withRealHubOracle,
      registeredOperators: realRegisteredOperators,
    };

    const realActiveOperators = yield* buildRealActiveOperatorsValidator(
      network,
      withRealRegisteredOperators,
      operatorParams,
    );
    const withRealActiveOperators: SDK.MidgardValidators = {
      ...withRealRegisteredOperators,
      activeOperators: realActiveOperators,
    };

    const realRetiredOperators = yield* buildRealRetiredOperatorsValidator(
      network,
      withRealActiveOperators,
      operatorParams,
    );
    const withRealOperatorSets: SDK.MidgardValidators = {
      ...withRealActiveOperators,
      retiredOperators: realRetiredOperators,
    };

    const realStateQueue = yield* buildRealStateQueueValidator(
      network,
      withRealOperatorSets,
    );
    return {
      ...withRealOperatorSets,
      stateQueue: realStateQueue,
    };
  });

const makeMidgardContracts = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const baseContracts = yield* AlwaysSucceedsContract;
  const oneShotTxHash = nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH.trim();
  if (
    oneShotTxHash.length === 0 ||
    nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX < 0
  ) {
    return yield* Effect.fail(
      new Error(
        "HUB_ORACLE_ONE_SHOT_TX_HASH and HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX must be configured for real state_queue deployment",
      ),
    );
  }
  const oneShotOutRef: HubOracleOneShotOutRef = {
    txHash: oneShotTxHash,
    outputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
  };
  const resolvedContracts = yield* withRealStateQueueAndOperatorContracts(
    nodeConfig.NETWORK,
    baseContracts,
    oneShotOutRef,
    {
      requiredBondLovelace: nodeConfig.OPERATOR_REQUIRED_BOND_LOVELACE,
      slashingPenaltyLovelace: nodeConfig.OPERATOR_SLASHING_PENALTY_LOVELACE,
    },
  );
  yield* Effect.logInfo(
    "🔐 Contract source selected: state_queue=real, hub_oracle=real, registered_operators=real, active_operators=real, retired_operators=real",
  );
  return resolvedContracts;
}).pipe(Effect.orDie);

export class MidgardContracts extends Effect.Service<MidgardContracts>()(
  "MidgardContracts",
  {
    effect: makeMidgardContracts,
    dependencies: [AlwaysSucceedsContract.Default, NodeConfig.layer],
  },
) {}
