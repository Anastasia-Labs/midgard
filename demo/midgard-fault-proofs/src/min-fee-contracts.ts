import type { Script } from "@lucid-evolution/lucid";

export const MIN_FEE_CATEGORY_LABEL = "min-fee";

export const MIN_FEE_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/min_fee/step_01.main.spend",
  step02: "fraud_proofs/min_fee/step_02.main.spend",
} as const;

export type MinFeeStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * Explicit pre-registration deployment record. Parameter order is:
 *
 * - step-01: step-02 hash, computation-thread policy, hub oracle;
 * - step-02: fraud-proof policy, fraud-proof address, computation-thread
 *   policy, field-preimage certificate policy.
 */
export type MinFeeContracts = {
  readonly steps: readonly [MinFeeStepContract, MinFeeStepContract];
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
};
