import { TxHash } from "@lucid-evolution/lucid";
import { SerializedStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { Effect, Ref } from "effect";

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

    // The state queue UTxO confirmed by the confirmation worker, unused for
    // block commitment. Using `as` since it seems to be the only way to satisfy
    // the compiler (TODO?).
    const AVAILABLE_CONFIRMED_BLOCK: Ref.Ref<"" | SerializedStateQueueUTxO> =
      yield* Ref.make("" as "" | SerializedStateQueueUTxO);

    // Accumulator for the number of processed mempool transactions (only used
    // in metrics)
    const PROCESSED_UNSUBMITTED_TXS_COUNT: Ref.Ref<number> = yield* Ref.make(0);

    // Accumulator for the total size of L2 transactions submitted in a state
    // queue block.
    const PROCESSED_UNSUBMITTED_TXS_SIZE: Ref.Ref<number> = yield* Ref.make(0);

    // TODO?: We might be able to avoid `as` here.
    const UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH: Ref.Ref<"" | TxHash> =
      yield* Ref.make("" as "" | TxHash);

    const LATEST_DEPOSIT_FETCH_TIME: Ref.Ref<number> = yield* Ref.make(0);

    // Indicates that on-chain block submission succeeded but local persistence
    // failed and must be retried against the confirmed block.
    const LOCAL_FINALIZATION_PENDING: Ref.Ref<boolean> = yield* Ref.make(false);

    // Worker liveness signals used by readiness checks.
    const HEARTBEAT_BLOCK_COMMITMENT: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_BLOCK_CONFIRMATION: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_MERGE: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_DEPOSIT_FETCH: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_TX_QUEUE_PROCESSOR: Ref.Ref<number> = yield* Ref.make(now);

    return {
      BLOCKS_IN_QUEUE,
      LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
      RESET_IN_PROGRESS,
      AVAILABLE_CONFIRMED_BLOCK,
      PROCESSED_UNSUBMITTED_TXS_COUNT,
      PROCESSED_UNSUBMITTED_TXS_SIZE,
      UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
      LATEST_DEPOSIT_FETCH_TIME,
      LOCAL_FINALIZATION_PENDING,
      HEARTBEAT_BLOCK_COMMITMENT,
      HEARTBEAT_BLOCK_CONFIRMATION,
      HEARTBEAT_MERGE,
      HEARTBEAT_DEPOSIT_FETCH,
      HEARTBEAT_TX_QUEUE_PROCESSOR,
    };
  }),
}) {}
