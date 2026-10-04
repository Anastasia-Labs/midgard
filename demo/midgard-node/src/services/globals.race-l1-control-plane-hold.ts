import { Effect, Fiber } from "effect";

/** Join protected work before the caller can release its service scopes.
 * Interrupting raceFirst alone can return while its masked loser still runs. */
export const raceL1ControlPlaneHold = <A, E, R, E2, R2>(
  work: Effect.Effect<A, E, R>,
  deadline: Effect.Effect<never, E2, R2>,
): Effect.Effect<A, E | E2, R | R2> =>
  Effect.uninterruptibleMask((restore) =>
    Effect.gen(function* () {
      const worker = yield* Effect.fork(Effect.interruptible(work));
      return yield* restore(
        Effect.raceFirst(Fiber.join(worker), deadline),
      ).pipe(Effect.ensuring(Fiber.interrupt(worker)));
    }),
  );
