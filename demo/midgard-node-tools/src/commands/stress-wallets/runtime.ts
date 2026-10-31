import { Effect } from "effect";

export const runWithSharedFanoutContext = <A, R>(
  run: (
    execute: <B, E>(effect: Effect.Effect<B, E, R>) => Promise<B>,
  ) => Promise<A>,
): Effect.Effect<A, never, R> =>
  Effect.gen(function* () {
    const context = yield* Effect.context<R>();
    return yield* Effect.promise(() =>
      run((effect) => Effect.runPromise(Effect.provide(effect, context))),
    );
  });

export const acceptedTxStatuses = new Set([
  "accepted",
  "committed",
  "confirmed",
  "awaiting_local_recovery",
]);

export const rejectedTxStatuses = new Set(["rejected", "failed"]);
export const consolidationPendingTxStatuses = new Set([
  "not_found",
  "queued",
  "submitted",
  "validating",
  "accepted",
  "pending_commit",
  "awaiting_local_recovery",
]);

export const nextFanoutPollDelayMs = ({
  attempt,
  initialMs,
  maxMs,
}: {
  readonly attempt: number;
  readonly initialMs: number;
  readonly maxMs: number;
}): number => Math.min(maxMs, initialMs * 2 ** attempt);

export const runBounded = async <T>(
  items: readonly T[],
  concurrency: number,
  task: (item: T) => Promise<void>,
): Promise<void> => {
  let next = 0;
  let failed = false;
  let firstError: unknown;
  const workers = Array.from(
    {
      length: Math.min(concurrency, items.length),
    },
    async () => {
      while (!failed && next < items.length) {
        const item = items[next]!;
        next += 1;
        try {
          await task(item);
        } catch (error) {
          if (!failed) {
            failed = true;
            firstError = error;
          }
        }
      }
    },
  );
  await Promise.all(workers);
  if (failed) {
    throw firstError;
  }
};
