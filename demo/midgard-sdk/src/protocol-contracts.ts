import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
/** Production protocol family recipes; environment and manifest loading belong to consumers. */
import { normalizeOutRef, type OutRefLike } from "@al-ft/midgard-core/out-ref";
import { Constr, Data, type Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type DaAvailabilityParameters,
  encodeDaAvailabilityParameters,
} from "./availability-challenge.js";
import type {
  AuthenticatedValidator,
  AvailabilityChallengeValidator,
  MintingValidator,
  SpendingValidator,
  WithdrawalValidator,
} from "./common.js";
import type { MidgardValidators } from "./common.js";
import {
  applyBlueprintParams,
  asAddressDataParam,
  buildAuthenticatedBlueprintValidator,
  type FaultProofBlueprint,
  getUnappliedScript,
  makeMintingPolicy,
  makeSpendingValidator,
  makeWithdrawalValidator,
} from "./fraud-proof/contracts/blueprint.js";
import {
  MPF_CHUNKED_VERIFY_WITHDRAW_TITLE,
  PEXCLUDES_EXCLUSION_WITHDRAW_TITLE,
} from "./fraud-proof/native.js";
import { requireEventHistoryContracts } from "./user-events/history-deployment.js";

export const DA_PARAMS_GOVERNOR_SCRIPT_TITLES = {
  mint: deployedTitle("daParamsGovernorMint"),
  spend: deployedTitle("daParamsGovernorSpend"),
} as const;

export const DA_BOND_POOL_SCRIPT_TITLES = {
  mint: deployedTitle("daBondPoolMint"),
  spend: deployedTitle("daBondPoolSpend"),
} as const;

export const DA_ATTESTATION_SCRIPT_TITLES = {
  mint: deployedTitle("daAttestationMint"),
  spend: deployedTitle("daAttestationSpend"),
} as const;

export const AVAILABILITY_CHALLENGE_SCRIPT_TITLES = {
  mint: deployedTitle("availabilityChallengeMint"),
  spend: deployedTitle("availabilityChallengeSpend"),
  openYield: deployedTitle("availabilityChallengeOpenWithdraw"),
  settleYield: deployedTitle("availabilityChallengeSettleWithdraw"),
  closeYield: deployedTitle("availabilityChallengeCloseWithdraw"),
  timeoutYield: deployedTitle("availabilityChallengeTimeoutWithdraw"),
} as const;

export const REGISTERED_OPERATORS_SCRIPT_TITLES = {
  mint: deployedTitle("registeredOperatorsMint"),
  spend: deployedTitle("registeredOperatorsSpend"),
} as const;

export const ACTIVE_OPERATORS_SCRIPT_TITLES = {
  mint: deployedTitle("activeOperatorsMint"),
  spend: deployedTitle("activeOperatorsSpend"),
} as const;

export const RETIRED_OPERATORS_SCRIPT_TITLES = {
  mint: deployedTitle("retiredOperatorsMint"),
  spend: deployedTitle("retiredOperatorsSpend"),
} as const;

export const SCHEDULER_SCRIPT_TITLES = {
  mint: deployedTitle("schedulerMint"),
  spend: deployedTitle("schedulerSpend"),
} as const;

export const SETTLEMENT_SCRIPT_TITLES = {
  mint: deployedTitle("settlementMint"),
  spend: deployedTitle("settlementSpend"),
} as const;

export const RESERVE_SCRIPT_TITLES = {
  spend: deployedTitle("reserveSpend"),
  withdraw: deployedTitle("reserveWithdraw"),
} as const;

export const PAYOUT_SCRIPT_TITLES = {
  mint: deployedTitle("payoutMint"),
  spend: deployedTitle("payoutSpend"),
} as const;

export const FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES = {
  mint: deployedTitle("fraudProofCatalogueMint"),
  spend: deployedTitle("fraudProofCatalogueSpend"),
} as const;

export const COMPUTATION_THREAD_SCRIPT_TITLES = {
  mint: deployedTitle("computationThreadMint"),
} as const;

export const FRAUD_PROOF_SCRIPT_TITLES = {
  mint: deployedTitle("fraudProofMint"),
  spend: deployedTitle("fraudProofSpend"),
} as const;

const outputReferenceParam = (outRef: OutRefLike): Constr<Data> => {
  const normalized = normalizeOutRef(outRef);
  return new Constr(0, [normalized.txHash, BigInt(normalized.outputIndex)]);
};

export const buildFraudProofCatalogueValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: { readonly hubOracle: { readonly policyId: string } },
): AuthenticatedValidator =>
  buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
  );

export const buildComputationThreadValidator = (
  blueprint: FaultProofBlueprint,
  contracts: {
    readonly fraudProofCatalogue: { readonly policyId: string };
    readonly hubOracle: { readonly policyId: string };
  },
): MintingValidator => {
  const mintValidator = COMPUTATION_THREAD_SCRIPT_TITLES.mint;
  return makeMintingPolicy(
    applyBlueprintParams(blueprint, mintValidator, [
      contracts.fraudProofCatalogue.policyId,
      contracts.hubOracle.policyId,
    ]),
  );
};

export const buildFraudProofValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  computationThread: Pick<MintingValidator, "policyId">,
): AuthenticatedValidator =>
  buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    FRAUD_PROOF_SCRIPT_TITLES,
    [computationThread.policyId],
  );

export const buildFraudProofSharedWithdrawalValidators = (
  blueprint: FaultProofBlueprint,
): Readonly<{
  chunkedVerify: WithdrawalValidator;
  pexcludes: WithdrawalValidator;
}> => {
  const chunkedVerify = MPF_CHUNKED_VERIFY_WITHDRAW_TITLE;
  const pexcludes = PEXCLUDES_EXCLUSION_WITHDRAW_TITLE;
  return {
    chunkedVerify: makeWithdrawalValidator(
      getUnappliedScript(blueprint, chunkedVerify),
    ),
    pexcludes: makeWithdrawalValidator(
      getUnappliedScript(blueprint, pexcludes),
    ),
  };
};

// Build order for the DA contracts, fixed by their parameters:
// DA params governor (init_ref only) -> DA bond pool (hub, DA params policy)
// -> availability challenge (hub, reference-script auth; its timeout yield
// takes the pool policy) -> correction lock (availability policy) -> DA
// attestation (DA params, hub and pool policies). None of them depends on a
// contract later in the chain, so there is no parameter cycle.

export const buildDaParamsGovernorValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  initOutRef: OutRefLike,
  maxCommitteeSize: number,
  maxOwnerCount: number,
): AuthenticatedValidator =>
  buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    DA_PARAMS_GOVERNOR_SCRIPT_TITLES,
    [
      outputReferenceParam(initOutRef),
      BigInt(maxCommitteeSize),
      BigInt(maxOwnerCount),
    ],
    () => [
      outputReferenceParam(initOutRef),
      BigInt(maxCommitteeSize),
      BigInt(maxOwnerCount),
    ],
  );

/**
 * The pooled DA committee bond. Its parameters deliberately exclude the
 * availability-challenge and DA-attestation policies, so the pool's identity
 * survives a redeployment of either.
 */
export const buildDaBondPoolValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  initOutRef: OutRefLike,
  hubOraclePolicyId: string,
  daParamsPolicyId: string,
  parameters: DaAvailabilityParameters,
): AuthenticatedValidator => {
  const poolParameters = [
    outputReferenceParam(initOutRef),
    hubOraclePolicyId,
    daParamsPolicyId,
    Data.from(encodeDaAvailabilityParameters(parameters)),
  ];
  return buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    DA_BOND_POOL_SCRIPT_TITLES,
    poolParameters,
    () => poolParameters,
  );
};

/**
 * Bondless DA attestation. Apply checks the commitment's deployment identity
 * against the hub policy and reads the pool (by its policy) as a reference
 * input; the attestation no longer depends on the availability policy.
 */
export const buildDaAttestationValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: {
    readonly daParamsGovernor: { readonly policyId: string };
    readonly hubOracle: { readonly policyId: string };
    readonly daBondPool: { readonly policyId: string };
  },
  referenceScriptAuthPolicyId: string,
  availabilityParameters: DaAvailabilityParameters,
): AuthenticatedValidator => {
  const attestationParameters = [
    contracts.daParamsGovernor.policyId,
    referenceScriptAuthPolicyId,
    contracts.hubOracle.policyId,
    contracts.daBondPool.policyId,
    Data.from(encodeDaAvailabilityParameters(availabilityParameters)),
  ];
  return buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    DA_ATTESTATION_SCRIPT_TITLES,
    attestationParameters,
    () => attestationParameters,
  );
};

/**
 * The availability-challenge dispatcher and its yields. Only the timeout yield
 * slashes the pool, so only it takes the pool policy; the dispatcher and the
 * open, settle and close yields stay independent of it.
 */
export const buildAvailabilityChallengeValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  hubOraclePolicyId: string,
  referenceScriptAuthPolicyId: string,
  daBondPoolPolicyId: string,
  parameters: DaAvailabilityParameters,
): AvailabilityChallengeValidator => {
  const encodedParameters = Data.from(
    encodeDaAvailabilityParameters(parameters),
  );
  const dispatcherParameters = [
    hubOraclePolicyId,
    referenceScriptAuthPolicyId,
    encodedParameters,
  ];
  const dispatcher = buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    AVAILABILITY_CHALLENGE_SCRIPT_TITLES,
    dispatcherParameters,
    () => dispatcherParameters,
  );
  const buildYield = (
    title: string,
    yieldParameters: readonly Data[],
  ): WithdrawalValidator =>
    makeWithdrawalValidator(
      applyBlueprintParams(blueprint, title, yieldParameters),
    );
  const coordinateParameters = [
    dispatcher.policyId,
    hubOraclePolicyId,
    encodedParameters,
  ];
  return {
    ...dispatcher,
    yields: {
      open: buildYield(
        AVAILABILITY_CHALLENGE_SCRIPT_TITLES.openYield,
        coordinateParameters,
      ),
      settle: buildYield(
        AVAILABILITY_CHALLENGE_SCRIPT_TITLES.settleYield,
        coordinateParameters,
      ),
      close: buildYield(
        AVAILABILITY_CHALLENGE_SCRIPT_TITLES.closeYield,
        coordinateParameters,
      ),
      timeout: buildYield(AVAILABILITY_CHALLENGE_SCRIPT_TITLES.timeoutYield, [
        dispatcher.policyId,
        hubOraclePolicyId,
        daBondPoolPolicyId,
        encodedParameters,
      ]),
    },
  };
};

export const buildRegisteredOperatorsValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: {
    readonly hubOracle: { readonly policyId: string };
    readonly retiredOperators: { readonly policyId: string };
  },
): AuthenticatedValidator =>
  buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    REGISTERED_OPERATORS_SCRIPT_TITLES,
    [contracts.retiredOperators.policyId, contracts.hubOracle.policyId],
    (policyId) => [policyId],
  );

/**
 * The policy of the lost-defense receipts, which both operator directories
 * read to slash for a lost validation-case defense: the mint purpose of the
 * validation-trace award validator.
 */
type LostDefenseReceiptPolicySource = {
  readonly fraudProofContracts: {
    readonly validationTraceDispute: {
      readonly award: { readonly policyId: string };
    };
  };
};

const lostDefenseReceiptPolicyId = (
  contracts: LostDefenseReceiptPolicySource,
): string =>
  contracts.fraudProofContracts.validationTraceDispute.award.policyId;

export const buildActiveOperatorsValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: LostDefenseReceiptPolicySource & {
    readonly hubOracle: { readonly policyId: string };
    readonly registeredOperators: { readonly policyId: string };
    readonly retiredOperators: { readonly policyId: string };
  },
): AuthenticatedValidator =>
  buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    ACTIVE_OPERATORS_SCRIPT_TITLES,
    [
      contracts.hubOracle.policyId,
      contracts.registeredOperators.policyId,
      contracts.retiredOperators.policyId,
      lostDefenseReceiptPolicyId(contracts),
    ],
    (policyId) => [policyId, contracts.hubOracle.policyId],
  );

export const buildRetiredOperatorsValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: LostDefenseReceiptPolicySource & {
    readonly hubOracle: { readonly policyId: string };
  },
): AuthenticatedValidator =>
  buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    RETIRED_OPERATORS_SCRIPT_TITLES,
    [contracts.hubOracle.policyId, lostDefenseReceiptPolicyId(contracts)],
    (policyId) => [policyId],
  );

export const buildSchedulerValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: {
    readonly activeOperators: {
      readonly policyId: string;
      readonly spendingScriptAddress: string;
    };
    readonly hubOracle: { readonly policyId: string };
    readonly registeredOperators: { readonly policyId: string };
  },
): AuthenticatedValidator => {
  const activeOperatorsAddress = Effect.runSync(
    asAddressDataParam(contracts.activeOperators.spendingScriptAddress),
  );
  return buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    SCHEDULER_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    (policyId) => [
      contracts.registeredOperators.policyId,
      activeOperatorsAddress,
      contracts.activeOperators.policyId,
      policyId,
      contracts.hubOracle.policyId,
    ],
  );
};

export const buildSettlementValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: { readonly hubOracle: { readonly policyId: string } },
): AuthenticatedValidator =>
  buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    SETTLEMENT_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    (policyId) => [contracts.hubOracle.policyId, policyId],
  );

export const buildReserveValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: { readonly hubOracle: { readonly policyId: string } },
): SpendingValidator & WithdrawalValidator => {
  const spendValidator = RESERVE_SCRIPT_TITLES.spend;
  const withdrawValidator = RESERVE_SCRIPT_TITLES.withdraw;
  const withdrawScriptCBOR = getUnappliedScript(blueprint, withdrawValidator);

  const spendingScriptCBOR = applyBlueprintParams(blueprint, spendValidator, [
    contracts.hubOracle.policyId,
  ]);

  return {
    ...makeSpendingValidator(network, spendingScriptCBOR),
    ...makeWithdrawalValidator(withdrawScriptCBOR),
  };
};

export const buildPayoutValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: Pick<
    MidgardValidators,
    "hubOracle" | "deposit" | "withdrawal" | "eventHistory"
  >,
): AuthenticatedValidator =>
  buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    PAYOUT_SCRIPT_TITLES,
    [
      contracts.hubOracle.policyId,
      requireEventHistoryContracts(contracts).withdrawal.retirement
        .withdrawalScriptHash,
    ],
    () => [contracts.hubOracle.policyId],
  );
