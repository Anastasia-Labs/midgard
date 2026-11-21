import type { Script } from "@lucid-evolution/lucid";

export const WITHDRAWAL_MISTAG_CATEGORY_LABEL = "withdrawal-mistag";

export type WithdrawalMistagStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Applied backwards with the exact parameter order pinned in the plan. */
export type WithdrawalMistagContracts = {
  readonly steps: readonly [
    WithdrawalMistagStepContract,
    WithdrawalMistagStepContract,
    WithdrawalMistagStepContract,
    WithdrawalMistagStepContract,
    WithdrawalMistagStepContract,
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
};

export type WithdrawalMistagCatalogueCategory = {
  readonly categoryId: "00000014";
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};
