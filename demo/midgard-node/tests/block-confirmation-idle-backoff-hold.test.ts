import { Effect, Option, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  CONFIRMATION_IDLE_BACKOFF_KEY,
  recordConfirmationTickIdleness,
  skipIdleConfirmationTick,
} from "../src/fibers/block-confirmation.idle-backoff.js";
import { Globals } from "../src/services/index.js";
import {
  clearLivenessIncident,
  HaltSource,
  raiseLivenessIncident,
} from "../src/services/liveness-halt.js";
import { SIGNED_INTENT_UNDECIDED } from "../src/services/signed-intent-undecided.js";

/**
 * Only a confirmation refresh re-derives `signed_intent_undecided`, so a
 * provably idle node must not back off its refreshes while that hold is
 * raised: the worker runs at the configured cadence until a tick decides.
 * Every other hold leaves the idle backoff as it is.
 */

const confirmedTip = { utxo: "confirmed" } as never;

const run = <A>(effect: Effect.Effect<A, never, Globals>) =>
  Effect.runPromise(effect.pipe(Effect.provide(Globals.Default)));

/** An idle node that has recorded one idle tick, so the next is skipped. */
const idleNode = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, confirmedTip);
  yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, true);
  yield* recordConfirmationTickIdleness(globals, Option.none(), 60_000);
  return globals;
});

const backingOff = (globals: Globals) =>
  Effect.map(Ref.get(globals.IDLE_BACKOFF), (backoff) =>
    backoff.has(CONFIRMATION_IDLE_BACKOFF_KEY),
  );

describe("confirmation idle backoff under a signed-intent hold", () => {
  it("refreshes every tick while the hold is raised, and backs off again once it clears", async () => {
    const outcome = await run(
      Effect.gen(function* () {
        const globals = yield* idleNode;
        const none = Option.none();
        const idleSkip = yield* skipIdleConfirmationTick(globals, none);
        yield* raiseLivenessIncident(
          globals,
          HaltSource.blockConfirmationSignedIntent,
          SIGNED_INTENT_UNDECIDED,
          "replaced block holds its base's slot",
        );
        const heldSkip = yield* skipIdleConfirmationTick(globals, none);
        yield* recordConfirmationTickIdleness(globals, none, 60_000);
        const heldBackoff = yield* backingOff(globals);
        const heldSkipAfterTick = yield* skipIdleConfirmationTick(
          globals,
          none,
        );
        yield* clearLivenessIncident(
          globals,
          HaltSource.blockConfirmationSignedIntent,
        );
        yield* recordConfirmationTickIdleness(globals, none, 60_000);
        const clearedSkip = yield* skipIdleConfirmationTick(globals, none);
        return {
          idleSkip,
          heldSkip,
          heldBackoff,
          heldSkipAfterTick,
          clearedSkip,
        };
      }),
    );
    expect(outcome).toEqual({
      idleSkip: true,
      heldSkip: false,
      heldBackoff: false,
      heldSkipAfterTick: false,
      clearedSkip: true,
    });
  });

  it("still skips an idle tick while another source's hold is raised", async () => {
    const skipped = await run(
      Effect.gen(function* () {
        const globals = yield* idleNode;
        yield* raiseLivenessIncident(
          globals,
          HaltSource.stateQueueCorrectionRewind,
          "state_queue_correction_rewind_conflict",
          "removal disagrees with L1",
        );
        return yield* skipIdleConfirmationTick(globals, Option.none());
      }),
    );
    expect(skipped).toBe(true);
  });
});
