import { type Script } from "@lucid-evolution/lucid";

export const L2_TX_MISTAG_CATEGORY_LABEL = "l2-tx-mistag";

export type L2TxMistagStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type L2TxMistagContracts = {
  readonly steps: readonly [L2TxMistagStepContract, L2TxMistagStepContract];
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
