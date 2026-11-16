import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import { Effect, Metric } from "effect";

import { DatabaseError } from "../../database/utils/common.js";
import { Database } from "../../services/index.js";

export const MERGE_CONFIRMATION_PROVIDER_RETRIES = 12;

export const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merged blocks",
  bigint: true,
  incremental: true,
});

export const mergeFailureCounter = Metric.counter("merge_failure_count", {
  description: "A counter for tracking merge failures",
  bigint: true,
  incremental: true,
});

export const mergeLandingPendingSkipCounter = Metric.counter(
  "merge_landing_pending_skip_count",
  {
    description:
      "A counter for scheduled merge ticks skipped while this node's previous merge awaits landing",
    bigint: true,
    incremental: true,
  },
);

export const mergeMissingBlockTxsCounter = Metric.counter(
  "merge_missing_block_txs_count",
  {
    description: "A counter for merge attempts blocked by missing BlocksDB txs",
    bigint: true,
    incremental: true,
  },
);

export const mergeBlockTxDecodeFailureCounter = Metric.counter(
  "merge_block_tx_decode_failure_count",
  {
    description:
      "A counter for merge attempts blocked by malformed block transactions",
    bigint: true,
    incremental: true,
  },
);

export const mergeDurationTimer = Metric.timer(
  "merge_duration",
  "Duration of one merge attempt in milliseconds",
);

export type MergeErrorCode =
  | "E_MERGE_LAYOUT_DERIVATION_FAILED"
  | "E_MERGE_REDEEMER_INDEX_MISMATCH"
  | "E_MERGE_MISSING_BLOCK_TXS"
  | "E_MERGE_BLOCK_TX_DECODE_FAILED"
  | "E_MERGE_UPLC_EVAL_FAILED";

type MissingBlockTxsDiagnosis = {
  readonly reason: "IMMUTABLE_DB_TX_LOOKUP_INCOMPLETE";
  readonly txHashesFound: number;
  readonly txsResolved: number;
};

export const diagnoseMissingBlockTxs = (
  txHashesFound: number,
  txsResolved: number,
): MissingBlockTxsDiagnosis | undefined => {
  if (txsResolved !== txHashesFound) {
    return {
      reason: "IMMUTABLE_DB_TX_LOOKUP_INCOMPLETE",
      txHashesFound,
      txsResolved,
    };
  }
  return undefined;
};

export type MergeOptions = {
  readonly bypassQueueLengthGuard?: boolean;
  readonly referenceScriptsAddress?: string;
  readonly leaseToken?: string;
  readonly headerHash?: string;
  readonly submitSlotSnapshot?: () => Effect.Effect<
    SubmitSlotSnapshot,
    unknown
  >;
  /**
   * Checked immediately before the merge transaction is signed and submitted:
   * the caller re-proves its state-queue lease, so a lease lost after the
   * merge started refuses the submission.
   */
  readonly assertSubmitAuthority?: () => Effect.Effect<
    void,
    DatabaseError,
    Database
  >;
  /**
   * Called once the merge transaction is submitted, before its L1
   * confirmation wait. A caller that bounds the attempt with a hold timeout
   * uses it to tell a merge already submitted when the timeout interrupted
   * it from one that never left.
   */
  readonly onSubmitted?: (submitted: SubmittedMergeTx) => Effect.Effect<void>;
};

/** A merge transaction this attempt submitted. */
export type SubmittedMergeTx = {
  readonly headerHash: string;
  readonly txHash: string;
};
