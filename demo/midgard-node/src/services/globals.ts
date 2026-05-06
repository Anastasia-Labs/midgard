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

    // Prevents overlapping commitment workers (periodic + manual trigger).
    const COMMIT_WORKER_ACTIVE: Ref.Ref<boolean> = yield* Ref.make(false);

    // The state queue UTxO confirmed by the confirmation worker, used to gate
    // block commitment so a new block is only built once the previous one is
    // confirmed on L1. Empty string means "no confirmation available yet".
    const AVAILABLE_CONFIRMED_BLOCK: Ref.Ref<"" | SerializedStateQueueUTxO> =
      yield* Ref.make("" as "" | SerializedStateQueueUTxO);

    // The hash of an L1 commit tx that has been submitted but not yet
    // observed as confirmed on-chain. Empty when there is no in-flight
    // submission to wait for.
    const UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH: Ref.Ref<"" | TxHash> =
      yield* Ref.make("" as "" | TxHash);
    const UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS: Ref.Ref<number> =
      yield* Ref.make(0);

    const LATEST_USER_EVENTS_FETCH_TIME: Ref.Ref<number> = yield* Ref.make(
      Date.now(),
    );

    // Monotonic version used to invalidate the tx-queue processor's cached
    // UTxO state whenever another fiber mutates mempool_ledger directly.
    const MEMPOOL_LEDGER_VERSION: Ref.Ref<number> = yield* Ref.make(0);

    // Worker liveness signals used by readiness checks.
    const HEARTBEAT_BLOCK_COMMITMENT: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_BLOCK_CONFIRMATION: Ref.Ref<number> = yield* Ref.make(now);
    const HEARTBEAT_MERGE: Ref.Ref<number> = yield* Ref.make(now);

    // Set while merge tx has been submitted on-chain but local DB
    // finalization (clearing spent UTxOs / inserting produced UTxOs / clearing
    // the merged block from BlocksTxsDB) is still in flight or has failed and
    // is awaiting recovery. Gates the next merge attempt.
    const LOCAL_FINALIZATION_PENDING: Ref.Ref<boolean> = yield* Ref.make(false);

    return {
      BLOCKS_IN_QUEUE,
      LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
      RESET_IN_PROGRESS,
      COMMIT_WORKER_ACTIVE,
      AVAILABLE_CONFIRMED_BLOCK,
      UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
      UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
      LATEST_USER_EVENTS_FETCH_TIME,
      MEMPOOL_LEDGER_VERSION,
      HEARTBEAT_BLOCK_COMMITMENT,
      HEARTBEAT_BLOCK_CONFIRMATION,
      HEARTBEAT_MERGE,
      LOCAL_FINALIZATION_PENDING,
    };
  }),
}) {}
