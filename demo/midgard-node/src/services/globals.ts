import { Effect, Ref } from "effect";

export class Globals extends Effect.Service<Globals>()("Globals", {
  effect: Effect.gen(function* () {
    // In-memory state queue length.
    const BLOCKS_IN_QUEUE: Ref.Ref<number> = yield* Ref.make(0);

    // Latest moment the in-memory state queue length was synchronized with
    // on-chain state.
    const LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH: Ref.Ref<number> =
      yield* Ref.make(0);

    // Needed for development to prevent other actions triggering while spending
    // all UTxOs at state queue.
    const RESET_IN_PROGRESS: Ref.Ref<boolean> = yield* Ref.make(false);

    const LATEST_USER_EVENTS_FETCH_TIME: Ref.Ref<number> = yield* Ref.make(
      Date.now(),
    );

    return {
      BLOCKS_IN_QUEUE,
      LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
      RESET_IN_PROGRESS,
      LATEST_USER_EVENTS_FETCH_TIME,
    };
  }),
}) {}
