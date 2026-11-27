import type { Script } from "@lucid-evolution/lucid";

export const WITHDRAWN_INPUT_CATEGORY_LABEL = "withdrawn-input";

export type WithdrawnInputStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Explicit pre-registration deployment record. */
export type WithdrawnInputContracts = {
  readonly steps: readonly [
    WithdrawnInputStepContract,
    WithdrawnInputStepContract,
    WithdrawnInputStepContract,
  ];
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptHash: string;
    readonly spendingScriptAddress: string;
  };
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
};
