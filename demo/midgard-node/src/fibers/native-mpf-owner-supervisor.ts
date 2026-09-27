import { Effect, Ref, Schedule } from "effect";

import { Globals } from "../services/globals.js";

/**
 * Fails, and so stops the node, once the live Architecture G native owner
 * reports a terminal failure. The owner refuses every operation from then on;
 * exiting lets the process supervisor restart the node from its durable marker
 * instead of leaving it up but unable to commit, merge or recover. The owner is
 * re-read each tick because recovery flows replace it.
 */
export const nativeMpfOwnerSupervisorFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<number, Error, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const failure = (yield* Ref.get(
      globals.NATIVE_MPF_OWNER,
    ))?.terminalFailure();
    if (failure !== undefined) return yield* Effect.fail(failure);
  }).pipe(Effect.repeat(schedule));
