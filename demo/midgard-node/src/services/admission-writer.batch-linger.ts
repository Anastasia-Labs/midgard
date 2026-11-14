import { Deferred, Duration, Effect, Exit } from "effect";

/**
 * The admission writer's batch linger: a lane's collector holding a
 * part-full batch waits up to `lingerMs` for more rows before it writes.
 * Each batch costs the event loop about the same whatever its size, so a
 * steady stream is cheaper written in few large batches than in many small
 * ones. A wait ends early when the batch fills (`wakeIfFull`), a `flush`
 * caller arrives or a stop begins (`wakeAll`); none starts on a full batch
 * or while `mayWait` says no. Every method but `wait` is synchronous,
 * so the writer calls them inside its own synchronous sections.
 */
export const makeBatchLinger = ({
  laneCount,
  lingerMs,
  isFull,
  mayWait,
}: {
  readonly laneCount: number;
  /** 0 never waits. */
  readonly lingerMs: number;
  /** Whether `lane`'s input queue holds a full batch. */
  readonly isFull: (lane: number) => boolean;
  /** Whether any collector may start a wait now. */
  readonly mayWait: () => boolean;
}) => {
  // The wake-up of the collector waiting on each lane, if one is.
  const waiting: (Deferred.Deferred<void> | undefined)[] = Array.from(
    { length: laneCount },
    () => undefined,
  );

  const wake = (lane: number): void => {
    const done = waiting[lane];
    if (done === undefined) return;
    waiting[lane] = undefined;
    Deferred.unsafeDone(done, Exit.void);
  };

  const wait = (lane: number): Effect.Effect<void> =>
    lingerMs === 0
      ? Effect.void
      : Effect.gen(function* () {
          const done = yield* Deferred.make<void>();
          const waits = yield* Effect.sync(() => {
            if (!mayWait() || isFull(lane)) return false;
            waiting[lane] = done;
            return true;
          });
          if (!waits) return;
          yield* Deferred.await(done).pipe(
            Effect.timeoutOption(Duration.millis(lingerMs)),
          );
          yield* Effect.sync(() => {
            if (waiting[lane] === done) waiting[lane] = undefined;
          });
        });

  return {
    wait,
    wakeIfFull: (lane: number): void => {
      if (isFull(lane)) wake(lane);
    },
    wakeAll: (): void => {
      for (let lane = 0; lane < laneCount; lane += 1) wake(lane);
    },
  };
};
