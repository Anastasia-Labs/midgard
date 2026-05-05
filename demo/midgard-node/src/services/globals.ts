import { TxHash } from "@lucid-evolution/lucid";
import { SerializedStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { Effect, Ref } from "effect";

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
    const BLOCKS_IN_QUEUE: Ref.Ref<number> = yield* Ref.make(0);

    // Latest moment the in-memory state queue length was synchronized with
    // on-chain state.
    const LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH: Ref.Ref<number> =
      yield* Ref.make(0);

    // Needed for development to prevent other actions triggering while spending
    // all UTxOs at state queue.
    const RESET_IN_PROGRESS: Ref.Ref<boolean> = yield* Ref.make(false);

    // Prevents overlapping commitment workers (periodic + manual trigger).
    const COMMIT_WORKER_ACTIVE: Ref.Ref<boolean> = yield* Ref.make(false);

    // The state queue UTxO confirmed by the confirmation worker, unused for
    // block commitment. Using `as` since it seems to be the only way to satisfy
    // the compiler (TODO?).
    const AVAILABLE_CONFIRMED_BLOCK: Ref.Ref<"" | SerializedStateQueueUTxO> =
      yield* Ref.make("" as "" | SerializedStateQueueUTxO);

    // The specific confirmed state_queue block whose roots must be used for
    // local finalization recovery when a submission succeeded on-chain but the
    // node crashed or failed during local persistence.
    const AVAILABLE_LOCAL_FINALIZATION_BLOCK: Ref.Ref<
      "" | SerializedStateQueueUTxO
    > = yield* Ref.make("" as "" | SerializedStateQueueUTxO);

    // Accumulator for the number of processed mempool transactions (only used
    // in metrics)
    const PROCESSED_UNSUBMITTED_TXS_COUNT: Ref.Ref<number> = yield* Ref.make(0);

    // Accumulator for the total size of L2 transactions submitted in a state
    // queue block.
    const PROCESSED_UNSUBMITTED_TXS_SIZE: Ref.Ref<number> = yield* Ref.make(0);

    // TODO?: We might be able to avoid `as` here.
    const UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH: Ref.Ref<"" | TxHash> =
      yield* Ref.make("" as "" | TxHash);
    const UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS: Ref.Ref<number> =
      yield* Ref.make(0);

    const LATEST_DEPOSIT_FETCH_TIME: Ref.Ref<number> = yield* Ref.make(0);

    // The end time of the latest block the node has locally accepted as the
    // pre-state boundary for the next block (confirmed on startup, then
    // advanced optimistically on successful submissions).
    const LATEST_LOCAL_BLOCK_END_TIME_MS: Ref.Ref<number> =
      yield* Ref.make(0);

    // Monotonic version used to invalidate the tx-queue processor's cached
    // UTxO state whenever another fiber mutates mempool_ledger directly.
    const MEMPOOL_LEDGER_VERSION: Ref.Ref<number> = yield* Ref.make(0);

    // Indicates that on-chain block submission succeeded but local persistence
    // failed and must be retried against the confirmed block.
    const LOCAL_FINALIZATION_PENDING: Ref.Ref<boolean> = yield* Ref.make(false);

    // Worker liveness signals used by readiness checks.
    const HEARTBEAT_BLOCK_COMMITMENT: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_BLOCK_CONFIRMATION: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_MERGE: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_DEPOSIT_FETCH: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_WITHDRAWAL_FETCH: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_TX_QUEUE_PROCESSOR: Ref.Ref<number> = yield* Ref.make(now);

    return {
      BLOCKS_IN_QUEUE,
      LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
      RESET_IN_PROGRESS,
      COMMIT_WORKER_ACTIVE,
      AVAILABLE_CONFIRMED_BLOCK,
      AVAILABLE_LOCAL_FINALIZATION_BLOCK,
      PROCESSED_UNSUBMITTED_TXS_COUNT,
      PROCESSED_UNSUBMITTED_TXS_SIZE,
      UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
      UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
      LATEST_DEPOSIT_FETCH_TIME,
      LATEST_LOCAL_BLOCK_END_TIME_MS,
      MEMPOOL_LEDGER_VERSION,
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
