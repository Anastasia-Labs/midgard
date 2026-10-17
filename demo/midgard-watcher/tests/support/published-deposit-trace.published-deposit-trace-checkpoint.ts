import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import type { publishWorkflowDeploymentOnChain } from "midgard-node/tests/helpers/published-workflow-deployment";

import { type PublishedWatcherBlock } from "./published-block-actor.js";
import { type PublishedDepositHistory } from "./published-deposit-history.js";

export type Published = Awaited<
  ReturnType<typeof publishWorkflowDeploymentOnChain>
>;

export type PublishedDepositTraceCheckpoint = {
  predecessor: Awaited<ReturnType<typeof depositEventsRetainedBlock>>;
  current: Awaited<ReturnType<typeof depositEventsRetainedBlock>>;
  depositEvent: UTxO;
  depositHistory: PublishedDepositHistory;
  depositMetadata: Pick<
    Effect.Effect.Success<
      ReturnType<typeof SDK.buildUnsignedDepositTxWithMetadataProgram>
    >["metadata"],
    "depositAssetName" | "depositAuthUnit" | "inclusionTime"
  >;
  commits: string[];
  attestationsComplete: boolean;
  priorDeposits?: readonly {
    event: UTxO;
    history: PublishedDepositHistory;
    metadata: PublishedDepositTraceCheckpoint["depositMetadata"];
  }[];
};

/**
 * The honest successor's durable progress. The block is persisted before its
 * header transaction is signed, the signed bytes before submission, and the
 * hash after inclusion, so a stopped harness resumes from whatever landed.
 */
export type PublishedSuccessorCheckpoint = {
  block: PublishedWatcherBlock;
  signedCommit?: { txHash: string; signedCbor: string };
  commitTxHash?: string;
};

/** The successor header's validity window; block production is Poisson. */
export const SUCCESSOR_HEADER_INTERVAL_MS = 179_999;

/**
 * Longest interval the empty predecessor may close after; it is shortened to
 * end before the staged deposit becomes eligible. Its commit's validity range
 * ends at this interval (Q60), so a longer interval survives more block gaps.
 */
export const EMPTY_PREDECESSOR_INTERVAL_MS = 79_999;

/** The deposit block closes this long after its event's inclusion second. */
export const DEPOSIT_BLOCK_INTERVAL_MS = 59_999;
