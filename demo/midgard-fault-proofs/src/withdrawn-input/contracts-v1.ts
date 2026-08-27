import type { Script } from "@lucid-evolution/lucid";

export const WITHDRAWN_INPUT_CATEGORY_LABEL = "withdrawn-input";

export const WITHDRAWN_INPUT_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/withdrawn_input/step_01.main.spend",
  step02: "fraud_proofs/withdrawn_input/step_02.main.spend",
  step03: "fraud_proofs/withdrawn_input/step_03.main.spend",
} as const;

export type WithdrawnInputStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Explicit pre-registration deployment record. */
export type WithdrawnInputContractsV1 = {
  readonly steps: readonly [
    WithdrawnInputStepContractV1,
    WithdrawnInputStepContractV1,
    WithdrawnInputStepContractV1,
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
