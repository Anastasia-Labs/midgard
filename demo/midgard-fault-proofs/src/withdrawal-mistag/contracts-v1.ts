import type { Script } from "@lucid-evolution/lucid";

import type { FaultProofClaimRegistryContractV1 } from "../claim-registry-transaction-v1.js";

export const WITHDRAWAL_MISTAG_CATEGORY_LABEL = "withdrawal-mistag";

export const WITHDRAWAL_MISTAG_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/withdrawal_mistag/step_01.main.spend",
  step02: "fraud_proofs/withdrawal_mistag/step_02.main.spend",
  step03: "fraud_proofs/withdrawal_mistag/step_03.main.spend",
  step04: "fraud_proofs/withdrawal_mistag/step_04.main.spend",
  step05: "fraud_proofs/withdrawal_mistag/step_05.main.spend",
} as const;

export type WithdrawalMistagStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Applied backwards with the exact parameter order pinned in the plan. */
export type WithdrawalMistagContractsV1 = {
  readonly steps: readonly [
    WithdrawalMistagStepContractV1,
    WithdrawalMistagStepContractV1,
    WithdrawalMistagStepContractV1,
    WithdrawalMistagStepContractV1,
    WithdrawalMistagStepContractV1,
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
  /**
   * The applied `claim_registry.spend` validator. Every arm of
   * `computation_thread.mint` requires the claim-registry input in the same
   * transaction, so each submitter resolves its mutation from here.
   */
  readonly claimRegistry: FaultProofClaimRegistryContractV1;
  readonly stateQueuePolicyId: string;
};

export type WithdrawalMistagCatalogueCategoryV1 = {
  readonly categoryId: "00000014";
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};
