/**
 * Startup's wait for the follower-change driver's first published view
 * (`follower_view_apply`): its first recompute runs the startup preparation,
 * starts the native MPF owner and recomputes the node's derived state from
 * the follower's view, and producers start only once the gate is open.
 *
 * - The wait never fails and never exits: what the driver is held on (a
 *   failed startup preparation, a failed rebase it retries, orphan recovery)
 *   and every raised liveness reason are named on `/readyz` during startup
 *   (`startup.setStage`) once per change, with `/healthz` live.
 * - It reads only process state (the gate's local side, the follower's
 *   holds and the liveness reasons), once per `pollInterval`.
 */
import { Duration, Effect, Ref } from "effect";

import { followerWriteGateReasons } from "../services/follower-write-gate.local.js";
import { Globals } from "../services/globals.js";
import { currentLivenessReasons } from "../services/globals.liveness-reasons.js";
import { l1FollowerReadiness } from "../services/l1-follower.readiness.js";

export const FOLLOWER_VIEW_STARTUP_POLL = Duration.seconds(1);

/** What startup still waits on, or none once the driver published a view. */
export const followerViewStartupReasons = Effect.gen(function* () {
  const globals = yield* Globals;
  const gate = followerWriteGateReasons(
    yield* Ref.get(globals.FOLLOWER_WRITE_GATE),
  );
  if (gate.length === 0) return gate;
  const reasons = [...gate];
  const follower = l1FollowerReadiness(yield* Ref.get(globals.L1_FOLLOWER));
  for (const reason of [
    ...follower.reasons,
    ...(yield* currentLivenessReasons(globals)),
  ])
    if (!reasons.includes(reason)) reasons.push(reason);
  return reasons;
});

/**
 * Waits until the driver has published a view, reporting each change of
 * what it waits on through `report` (startup's `/readyz` stage).
 */
export const awaitFollowerViewOnStartup = (
  report: (reasons: readonly string[]) => Effect.Effect<void>,
  pollInterval: Duration.DurationInput = FOLLOWER_VIEW_STARTUP_POLL,
): Effect.Effect<void, never, Globals> =>
  Effect.gen(function* () {
    let last = "";
    for (;;) {
      const reasons = yield* followerViewStartupReasons;
      const key = reasons.join(",");
      if (key !== last) {
        last = key;
        yield* report(reasons);
        if (reasons.length > 0)
          yield* Effect.logWarning(
            `Startup waits for the follower-change driver: ${key}`,
          );
      }
      if (reasons.length === 0) return;
      yield* Effect.sleep(pollInterval);
    }
  });
