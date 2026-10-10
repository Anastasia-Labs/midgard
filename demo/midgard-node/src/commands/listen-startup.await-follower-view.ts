/**
 * Startup's wait for the follower-change driver's first published view
 * (`follower_view_apply`): its first recompute runs the startup preparation,
 * starts the native MPF owner and recomputes the node's derived state from
 * the follower's view, and producers start only once the gate is open.
 *
 * - What the driver is held on (the startup preparation, a rebase, orphan
 *   recovery) and every raised liveness reason are named on `/readyz`
 *   during startup (`startup.setStage`) once per change, with `/healthz`
 *   live.
 * - Waiting on the follower, the chain or a peer has no deadline.
 * - A failure hold the driver does not retry (`isRetriedHold` false: the
 *   failure is not transient) fails startup at once with
 *   `StartupStepFailedError` (step `follower_view_apply`, the hold's reason,
 *   its detail as the cause). A failure hold it retries (transient) that is
 *   still held after `failureBudget` (`STARTUP_DATABASE_BUDGET`: what the
 *   preparation and recompute wait on is the node database) fails startup
 *   the same way, marked exhausted. The failure holds are
 *   `STARTUP_FAILURE_HOLDS`.
 * - It reads only process state (the gate's local side, the follower's
 *   holds and the liveness reasons), once per `pollInterval`.
 */
import { Clock, Duration, Effect, Ref } from "effect";

import {
  type DriverHold,
  EVENTS_INGESTION_FAILED,
  isRetriedHold,
} from "../l1-events/driver.js";
import { LANDED_BLOCK_REBASE_FAILED } from "../landed-blocks/holds.js";
import { followerWriteGateReasons } from "../services/follower-write-gate.local.js";
import { Globals } from "../services/globals.js";
import { currentLivenessReasons } from "../services/globals.liveness-reasons.js";
import { l1FollowerReadiness } from "../services/l1-follower.readiness.js";
import {
  DRIVER_RECOMPUTE_FAILED,
  STARTUP_PREPARATION_FAILED,
} from "../services/l1-follower.recompute.js";
import {
  MPF_CLOSURE_MISSING,
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
} from "../services/liveness-halt.js";
import {
  STARTUP_DATABASE_BUDGET,
  startupStepFailed,
  type StartupStepFailedError,
} from "../services/startup-waiting.js";

export const FOLLOWER_VIEW_STARTUP_POLL = Duration.seconds(1);

/** The startup step the wait reports under. */
const STEP_KEY = "follower_view_apply";

/** The driver holds that name a failure (not a wait) while the gate is pending. */
export const STARTUP_FAILURE_HOLDS: ReadonlySet<string> = new Set([
  STARTUP_PREPARATION_FAILED,
  DRIVER_RECOMPUTE_FAILED,
  EVENTS_INGESTION_FAILED,
  LANDED_BLOCK_REBASE_FAILED,
  MPF_CLOSURE_MISSING,
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
]);

type FollowerViewWait = Readonly<{
  reasons: readonly string[];
  /** The failure holds among the follower's holds while the gate is pending. */
  failures: readonly DriverHold[];
}>;

const followerViewWait = Effect.gen(function* () {
  const globals = yield* Globals;
  const gate = followerWriteGateReasons(
    yield* Ref.get(globals.FOLLOWER_WRITE_GATE),
  );
  if (gate.length === 0)
    return { reasons: gate, failures: [] } satisfies FollowerViewWait;
  const reasons = [...gate];
  const state = yield* Ref.get(globals.L1_FOLLOWER);
  const follower = l1FollowerReadiness(state);
  for (const reason of [
    ...follower.reasons,
    ...(yield* currentLivenessReasons(globals)),
  ])
    if (!reasons.includes(reason)) reasons.push(reason);
  const failures =
    state.kind === "running"
      ? state.holds().filter((hold) => STARTUP_FAILURE_HOLDS.has(hold.reason))
      : [];
  return { reasons, failures } satisfies FollowerViewWait;
});

/** What startup still waits on, or none once the driver published a view. */
export const followerViewStartupReasons = Effect.map(
  followerViewWait,
  (wait) => wait.reasons,
);

/**
 * Waits until the driver has published a view, reporting each change of
 * what it waits on through `report` (startup's `/readyz` stage). Fails on a
 * failure hold that is not retried, or one retried past `failureBudget`.
 */
export const awaitFollowerViewOnStartup = (
  report: (reasons: readonly string[]) => Effect.Effect<void>,
  pollInterval: Duration.DurationInput = FOLLOWER_VIEW_STARTUP_POLL,
  failureBudget: Duration.DurationInput = STARTUP_DATABASE_BUDGET,
): Effect.Effect<void, StartupStepFailedError, Globals> =>
  Effect.gen(function* () {
    const budgetMs = Duration.toMillis(Duration.decode(failureBudget));
    let last = "";
    let polls = 0;
    let failingSince: number | undefined;
    for (;;) {
      const { reasons, failures } = yield* followerViewWait;
      const terminal = failures.find((hold) => !isRetriedHold(hold));
      if (terminal !== undefined)
        return yield* Effect.fail(
          startupStepFailed({
            step: STEP_KEY,
            reason: terminal.reason,
            cause: terminal.detail,
          }),
        );
      if (failures.length > 0) {
        const now = yield* Clock.currentTimeMillis;
        polls += 1;
        failingSince ??= now;
        if (now - failingSince >= budgetMs)
          return yield* Effect.fail(
            startupStepFailed({
              step: STEP_KEY,
              reason: failures[0].reason,
              cause: failures[0].detail,
              exhausted: true,
              attempts: polls,
              waitedMs: now - failingSince,
            }),
          );
      } else {
        polls = 0;
        failingSince = undefined;
      }
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
