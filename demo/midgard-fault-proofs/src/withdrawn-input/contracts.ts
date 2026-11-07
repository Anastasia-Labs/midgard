import type { Script } from "@lucid-evolution/lucid";

export const WITHDRAWN_INPUT_CATEGORY_LABEL = "withdrawn-input";

export const WITHDRAWN_INPUT_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/withdrawn_input/step_01.main.spend",
  step02: "fraud_proofs/withdrawn_input/step_02.main.spend",
  step03: "fraud_proofs/withdrawn_input/step_03.main.spend",
} as const;

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
