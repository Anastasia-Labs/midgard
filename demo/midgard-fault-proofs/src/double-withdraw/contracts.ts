import type { Script } from "@lucid-evolution/lucid";

export const DOUBLE_WITHDRAW_CATEGORY_LABEL = "double-withdraw";

export type DoubleWithdrawStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Explicit pre-registration deployment record; the category id is separate. */
export type DoubleWithdrawContracts = {
  readonly steps: readonly [
    DoubleWithdrawStepContract,
    DoubleWithdrawStepContract,
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
  readonly stateQueuePolicyId: string;
};
