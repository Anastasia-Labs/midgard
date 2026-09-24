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
  mint: "da_params_governor.da_params_governor.mint",
  spend: "da_params_governor.da_params_governor.spend",
} as const;

export const DA_ATTESTATION_SCRIPT_TITLES = {
  mint: "da_attestation.da_attestation.mint",
  spend: "da_attestation.da_attestation.spend",
} as const;

export const AVAILABILITY_CHALLENGE_SCRIPT_TITLES = {
  mint: "availability_challenge.availability_challenge.mint",
  spend: "availability_challenge.availability_challenge.spend",
  bondYield: "availability_challenge_yields.bond.withdraw",
  openYield: "availability_challenge_yields.open.withdraw",
  settleYield: "availability_challenge_yields.settle.withdraw",
  closeYield: "availability_challenge_yields.close.withdraw",
  timeoutYield: "availability_challenge_yields.timeout.withdraw",
} as const;

export const REGISTERED_OPERATORS_SCRIPT_TITLES = {
  mint: "operator_directory/registered_operators.mint.mint",
  spend: "operator_directory/registered_operators.spend.spend",
} as const;

export const ACTIVE_OPERATORS_SCRIPT_TITLES = {
  mint: "operator_directory/active_operators.mint.mint",
  spend: "operator_directory/active_operators.spend.spend",
} as const;

export const RETIRED_OPERATORS_SCRIPT_TITLES = {
  mint: "operator_directory/retired_operators.mint.mint",
  spend: "operator_directory/retired_operators.spend.spend",
} as const;

export const SCHEDULER_SCRIPT_TITLES = {
  mint: "scheduler.mint.mint",
  spend: "scheduler.spend.spend",
} as const;

export const SETTLEMENT_SCRIPT_TITLES = {
  mint: "settlement.mint.mint",
  spend: "settlement.spend.spend",
} as const;

export const RESERVE_SCRIPT_TITLES = {
  spend: "reserve.spend.spend",
  withdraw: "reserve.withdraw.else",
} as const;

export const PAYOUT_SCRIPT_TITLES = {
  mint: "payout.mint.mint",
  spend: "payout.spend.spend",
} as const;

export const FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES = {
  mint: "fraud_proof_catalogue.mint.mint",
  spend: "fraud_proof_catalogue.spend.else",
} as const;

export const COMPUTATION_THREAD_SCRIPT_TITLES = {
  mint: "computation_thread.mint.mint",
} as const;

export const FRAUD_PROOF_SCRIPT_TITLES = {
  mint: "fraud_proof.mint.mint",
  spend: "fraud_proof.spend.else",
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

export const buildDaAttestationValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: {
    readonly availabilityChallenge: { readonly policyId: string };
    readonly daParamsGovernor: { readonly policyId: string };
  },
  referenceScriptAuthPolicyId: string,
  availabilityParameters: DaAvailabilityParameters,
): AuthenticatedValidator => {
  const encodedParameters = Data.from(
    encodeDaAvailabilityParameters(availabilityParameters),
  );
  return buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    DA_ATTESTATION_SCRIPT_TITLES,
    [
      contracts.daParamsGovernor.policyId,
      referenceScriptAuthPolicyId,
      contracts.availabilityChallenge.policyId,
      encodedParameters,
    ],
    () => [
      contracts.daParamsGovernor.policyId,
      referenceScriptAuthPolicyId,
      contracts.availabilityChallenge.policyId,
      encodedParameters,
    ],
  );
};

export const buildAvailabilityChallengeValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  hubOraclePolicyId: string,
  referenceScriptAuthPolicyId: string,
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
  const buildYield = (arm: string): WithdrawalValidator => {
    const validator = `availability_challenge_yields.${arm}.withdraw`;
    const compiledCode = applyBlueprintParams(blueprint, validator, [
      dispatcher.policyId,
      hubOraclePolicyId,
      encodedParameters,
    ]);
    return makeWithdrawalValidator(compiledCode);
  };
  return {
    ...dispatcher,
    yields: {
      bond: buildYield("bond"),
      open: buildYield("open"),
      settle: buildYield("settle"),
      close: buildYield("close"),
      timeout: buildYield("timeout"),
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

export const buildActiveOperatorsValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: {
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
    ],
    (policyId) => [policyId, contracts.hubOracle.policyId],
  );

export const buildRetiredOperatorsValidator = (
  blueprint: FaultProofBlueprint,
  network: Network,
  contracts: { readonly hubOracle: { readonly policyId: string } },
): AuthenticatedValidator =>
  buildAuthenticatedBlueprintValidator(
    blueprint,
    network,
    RETIRED_OPERATORS_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
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
