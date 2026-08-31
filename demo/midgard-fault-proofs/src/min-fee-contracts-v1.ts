import type { Script } from "@lucid-evolution/lucid";

import type { FaultProofClaimRegistryContractV1 } from "./claim-registry-transaction-v1.js";

export const MIN_FEE_CATEGORY_LABEL = "min-fee";

export const MIN_FEE_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/min_fee/step_01.main.spend",
  step02: "fraud_proofs/min_fee/step_02.main.spend",
} as const;

export type MinFeeStepContractV1 = {
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
export type MinFeeContractsV1 = {
  readonly steps: readonly [MinFeeStepContractV1, MinFeeStepContractV1];
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
  /**
   * The applied `claim_registry.spend` validator. Every arm of
   * `computation_thread.mint` requires the claim-registry input in the same
   * transaction, so each submitter resolves its mutation from here.
   */
  readonly claimRegistry: FaultProofClaimRegistryContractV1;
  readonly stateQueuePolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
};
