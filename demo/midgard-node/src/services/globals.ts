import { TxHash } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";

import { SerializedStateQueueUTxO } from "@/workers/utils/commit-block-header.js";

export type CommitPipelinePhase =
  | "idle"
  | "scheduler_alignment"
  | "mutation_worker";

/**
 * Process-wide mutable references shared between long-running fibers.
 *
 * These refs hold operational state that does not belong in durable storage but
 * still needs coordination across workers, readiness checks, and recovery
 * paths.
 */
export class Globals extends Effect.Service<Globals>()("Globals", {
  effect: Effect.gen(function* () {
    const now = Date.now();

    // In-memory state queue length.
    const BLOCKS_IN_QUEUE = yield* Ref.make<number>(0);

    // Latest moment the in-memory state queue length was synchronized with
    // on-chain state.
    const LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH = yield* Ref.make<number>(0);

    // Needed for development to prevent other actions triggering while spending
    // all UTxOs at state queue.
    const RESET_IN_PROGRESS = yield* Ref.make<boolean>(false);

    // Prevents overlapping commitment workers (periodic + manual trigger).
    const COMMIT_WORKER_ACTIVE = yield* Ref.make<boolean>(false);

    // Serializes pre-worker scheduler alignment with actual mutation workers.
    // COMMIT_WORKER_ACTIVE intentionally remains true only for the worker phase.
    const COMMIT_PIPELINE_PHASE = yield* Ref.make<CommitPipelinePhase>("idle");

    // The state queue UTxO confirmed by the confirmation worker, unused for
    // block commitment.
    const AVAILABLE_CONFIRMED_BLOCK = yield* Ref.make<
      "" | SerializedStateQueueUTxO
    >("");

    // The specific confirmed state_queue block whose roots must be used for
    // local finalization recovery when a submission succeeded on-chain but the
    // node crashed or failed during local persistence.
    const AVAILABLE_LOCAL_FINALIZATION_BLOCK = yield* Ref.make<
      "" | SerializedStateQueueUTxO
    >("");

    // Accumulator for the number of processed mempool transactions (only used
    // in metrics)
    const PROCESSED_UNSUBMITTED_TXS_COUNT = yield* Ref.make<number>(0);

    // Accumulator for the total size of L2 transactions submitted in a state
    // queue block.
    const PROCESSED_UNSUBMITTED_TXS_SIZE = yield* Ref.make<number>(0);

    const UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH = yield* Ref.make<"" | TxHash>(
      "",
    );
    const UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS = yield* Ref.make<number>(0);

    const LATEST_DEPOSIT_FETCH_TIME = yield* Ref.make<number>(0);

    // The end time of the latest block the node has locally accepted as the
    // pre-state boundary for the next block (confirmed on startup, then
    // advanced optimistically on successful submissions).
    const LATEST_LOCAL_BLOCK_END_TIME_MS = yield* Ref.make<number>(0);

    // Monotonic version used to invalidate the tx-queue processor's cached
    // UTxO state whenever another fiber mutates mempool_ledger directly.
    const MEMPOOL_LEDGER_VERSION = yield* Ref.make<number>(0);

    // Coordinates event-driven tx queue wakeups without allowing overlapping
    // validation processors.
    const TX_QUEUE_PROCESSOR_ACTIVE = yield* Ref.make<boolean>(false);
    const TX_QUEUE_WAKE_REQUESTED = yield* Ref.make<boolean>(false);

    // Indicates that on-chain block submission succeeded but local persistence
    // failed and must be retried against the confirmed block.
    const LOCAL_FINALIZATION_PENDING = yield* Ref.make<boolean>(false);

    // Worker liveness signals used by readiness checks.
    const HEARTBEAT_BLOCK_COMMITMENT = yield* Ref.make<number>(now);
    const HEARTBEAT_BLOCK_CONFIRMATION = yield* Ref.make<number>(now);
    const HEARTBEAT_MERGE = yield* Ref.make<number>(now);
    const HEARTBEAT_DEPOSIT_FETCH = yield* Ref.make<number>(now);
    const HEARTBEAT_WITHDRAWAL_FETCH = yield* Ref.make<number>(now);
    const HEARTBEAT_TX_QUEUE_PROCESSOR = yield* Ref.make<number>(now);

    return {
      BLOCKS_IN_QUEUE,
      LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
      RESET_IN_PROGRESS,
      COMMIT_WORKER_ACTIVE,
      COMMIT_PIPELINE_PHASE,
      AVAILABLE_CONFIRMED_BLOCK,
      AVAILABLE_LOCAL_FINALIZATION_BLOCK,
      PROCESSED_UNSUBMITTED_TXS_COUNT,
      PROCESSED_UNSUBMITTED_TXS_SIZE,
      UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
      UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
      LATEST_DEPOSIT_FETCH_TIME,
      LATEST_LOCAL_BLOCK_END_TIME_MS,
      MEMPOOL_LEDGER_VERSION,
      TX_QUEUE_PROCESSOR_ACTIVE,
      TX_QUEUE_WAKE_REQUESTED,
      LOCAL_FINALIZATION_PENDING,
      HEARTBEAT_BLOCK_COMMITMENT,
      HEARTBEAT_BLOCK_CONFIRMATION,
      HEARTBEAT_MERGE,
      HEARTBEAT_DEPOSIT_FETCH,
      HEARTBEAT_WITHDRAWAL_FETCH,
      HEARTBEAT_TX_QUEUE_PROCESSOR,
    };
  }),
}) {}
