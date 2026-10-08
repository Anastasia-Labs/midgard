import { Effect, Option, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  recordConfirmationTickIdleness,
  skipIdleConfirmationTick,
} from "../src/fibers/block-confirmation.idle-backoff.js";
import { Globals } from "../src/services/index.js";
import {
  HaltSource,
  raiseLivenessIncident,
} from "../src/services/liveness-halt.js";

/**
 * A raised liveness hold leaves the confirmation idle backoff as it is: a
 * provably idle node keeps skipping its refreshes.
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

describe("confirmation idle backoff under a liveness hold", () => {
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
