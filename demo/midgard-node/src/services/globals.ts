import { TxHash } from "@lucid-evolution/lucid";
import { SerializedStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { Effect, Queue, Ref } from "effect";

export class Globals extends Effect.Service<Globals>()("Globals", {
  effect: Effect.gen(function* () {
    // In-memory state queue length.
    const BLOCKS_IN_QUEUE: Ref.Ref<number> = yield* Ref.make(0);

    // Latest moment the in-memory state queue length was synchronized with
    // on-chain state.
    const LATEST_SYNC_OF_STATE_QUEUE_LENGTH: Ref.Ref<number> =
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
    const UNCONFIRMED_SUBMITTED_BLOCK_HASH: Ref.Ref<"" | TxHash> = yield* Ref.make(
      "" as "" | TxHash,
    );

    const UNCONFIRMED_SUBMITTED_BLOCK_TIME: Ref.Ref<bigint> = yield* Ref.make(BigInt(0));

    // A global queue with calculated deposit blocks.
    const DEPOSIT_ROOTS_QUEUE: Queue.Queue<string> = yield* Queue.unbounded<string>();

    return {
      BLOCKS_IN_QUEUE,
      LATEST_SYNC_OF_STATE_QUEUE_LENGTH,
      RESET_IN_PROGRESS,
      AVAILABLE_CONFIRMED_BLOCK,
      PROCESSED_UNSUBMITTED_TXS_COUNT,
      PROCESSED_UNSUBMITTED_TXS_SIZE,
      UNCONFIRMED_SUBMITTED_BLOCK_HASH,
      UNCONFIRMED_SUBMITTED_BLOCK_TIME,
      DEPOSIT_ROOTS_QUEUE,
    };
  }),
}) {}
