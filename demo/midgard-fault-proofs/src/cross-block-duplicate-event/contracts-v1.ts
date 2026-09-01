import type { Script } from "@lucid-evolution/lucid";

export const CROSS_BLOCK_DUPLICATE_EVENT_CATEGORY_LABEL =
  "cross-block-duplicate-event";

export const CROSS_BLOCK_DUPLICATE_EVENT_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/cross_block_duplicate_event/step_01.main.spend",
  step02: "fraud_proofs/cross_block_duplicate_event/step_02.main.spend",
} as const;

export type CrossBlockDuplicateEventStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Explicit pre-registration deployment. Production catalogue resolution is deferred. */
export type CrossBlockDuplicateEventContractsV1 = {
  readonly steps: readonly [
    CrossBlockDuplicateEventStepContractV1,
    CrossBlockDuplicateEventStepContractV1,
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
