import type { Script } from "@lucid-evolution/lucid";

import type { FaultProofClaimRegistryContractV1 } from "../claim-registry-transaction-v1.js";

export const DOUBLE_WITHDRAW_CATEGORY_LABEL = "double-withdraw";

export const DOUBLE_WITHDRAW_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/double_withdraw/step_01.main.spend",
  step02: "fraud_proofs/double_withdraw/step_02.main.spend",
} as const;

export type DoubleWithdrawStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Explicit pre-registration deployment record; the category id is separate. */
export type DoubleWithdrawContractsV1 = {
  readonly steps: readonly [
    DoubleWithdrawStepContractV1,
    DoubleWithdrawStepContractV1,
  ];
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
};
