/**
 * Startup's wait for the landed state queue (N2). Startup recovery seeds the
 * node's commit base and journals from P1, so it waits until the follower is
 * caught up with the node's tip and P1 at its view is readable and healthy.
 *
 * - Waiting on the follower and the chain (catching up, the queue not yet
 *   initialized or healthy, the view moving) has no deadline: each reason
 *   it is waiting on is named on `/readyz` during startup
 *   (`startup.setStage`) and logged once per change, and `/healthz` stays
 *   live.
 * - A database read that fails transiently (`isConnectionClassError`) is
 *   waited out under `state_queue_unavailable` for at most
 *   `STARTUP_DATABASE_BUDGET`. Any other read failure or defect, or one that
 *   outlives the budget, fails startup with `StartupStepFailedError`
 *   (step `l1_follower_catch_up`, reason `state_queue_unavailable`).
 * - It reads only the follower's in-memory status and the node database,
 *   never L1. It polls those once per `pollInterval`.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import { Clock, Duration, Effect, Ref } from "effect";

import { STATE_QUEUE_UNHEALTHY } from "../l1-state-queue/index.js";
import { isConnectionClassError } from "../provider-retry.js";
import { Globals } from "../services/globals.js";
import {
  caughtUpFollower,
  l1FollowerReadiness,
} from "../services/l1-follower.readiness.js";
import { readLandedStateQueue } from "../services/landed-state-queue.js";
import { MidgardContracts } from "../services/midgard-contracts.js";
import {
  STARTUP_DATABASE_BUDGET,
  startupStepFailed,
  type StartupStepFailedError,
} from "../services/startup-waiting.js";

/** The follower is caught up, but P1 cannot be read at its view; the detail says why. */
export const STATE_QUEUE_UNAVAILABLE = "state_queue_unavailable";

export const LANDED_STATE_QUEUE_STARTUP_POLL = Duration.seconds(1);

/** The startup step the wait reports under. */
const STEP_KEY = "l1_follower_catch_up";

/** A database read that failed for a reason waiting does not fix. */
export class StateQueueReadFailed extends Error {
  readonly _tag = "StateQueueReadFailed";
  constructor(override readonly cause: unknown) {
    super(`the landed state queue read failed: ${formatUnknownError(cause)}`);
  }
}

type StartupRead = Readonly<{
  reasons: readonly string[];
  /** The database read failed transiently (on the startup's budget). */
  databaseDown: boolean;
}>;

const startupRead: Effect.Effect<
  StartupRead,
  StateQueueReadFailed,
  Globals | MidgardContracts | SqlClient.SqlClient
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const contracts = yield* MidgardContracts;
  const state = yield* Ref.get(globals.L1_FOLLOWER);
  if (caughtUpFollower(state) === undefined) {
    const { reasons } = l1FollowerReadiness(state);
    return {
      reasons: reasons.length > 0 ? reasons : ["l1_follower_catching_up"],
      databaseDown: false,
    };
  }
  return yield* readLandedStateQueue(contracts.stateQueue).pipe(
    Effect.map(
      (read): StartupRead => ({
        reasons:
          read.kind !== "ok"
            ? [STATE_QUEUE_UNAVAILABLE]
            : read.queue.healthy
              ? []
              : [STATE_QUEUE_UNHEALTHY],
        databaseDown: false,
      }),
    ),
    Effect.catchAll((error) =>
      isConnectionClassError(error)
        ? Effect.succeed({
            reasons: [STATE_QUEUE_UNAVAILABLE],
            databaseDown: true,
          })
        : Effect.fail(new StateQueueReadFailed(error)),
    ),
  );
});

/**
 * What startup is still waiting on, or none when P1 is ready. A transient
 * database failure is `state_queue_unavailable`; any other fails.
 */
export const landedStateQueueStartupReasons: Effect.Effect<
  readonly string[],
  StateQueueReadFailed,
  Globals | MidgardContracts | SqlClient.SqlClient
> = Effect.map(startupRead, (read) => read.reasons);

/**
 * Waits until P1 is ready, reporting each change of what it waits on through
 * `report` (startup's `/readyz` stage) and the log. A transient database
 * failure is waited out for at most `databaseBudget`.
 */
export const awaitLandedStateQueueOnStartup = (
  report: (reasons: readonly string[]) => Effect.Effect<void>,
  pollInterval: Duration.DurationInput = LANDED_STATE_QUEUE_STARTUP_POLL,
  databaseBudget: Duration.DurationInput = STARTUP_DATABASE_BUDGET,
): Effect.Effect<
  void,
  StartupStepFailedError,
  Globals | MidgardContracts | SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const budgetMs = Duration.toMillis(Duration.decode(databaseBudget));
    let last = "";
    let attempts = 0;
    let failingSince: number | undefined;
    for (;;) {
      const failed = (cause: unknown, exhausted: boolean, waitedMs?: number) =>
        startupStepFailed({
          step: STEP_KEY,
          reason: STATE_QUEUE_UNAVAILABLE,
          cause,
          exhausted,
          attempts: Math.max(attempts, 1),
          ...(waitedMs === undefined ? {} : { waitedMs }),
        });
      const { reasons, databaseDown } = yield* startupRead.pipe(
        Effect.catchAll((error) => Effect.fail(failed(error.cause, false))),
        Effect.catchAllDefect((defect) =>
          Effect.logError(
            `Startup state-queue wait: ${formatUnknownError(defect)}`,
          ).pipe(Effect.zipRight(Effect.fail(failed(defect, false)))),
        ),
      );
      // Only the database read's transient failure is on the budget.
      if (databaseDown) {
        const now = yield* Clock.currentTimeMillis;
        attempts += 1;
        failingSince ??= now;
        if (now - failingSince >= budgetMs)
          return yield* Effect.fail(
            failed(
              "the node database stayed unreachable",
              true,
              now - failingSince,
            ),
          );
      } else {
        attempts = 0;
        failingSince = undefined;
      }
      const key = reasons.join(",");
      if (key !== last) {
        last = key;
        yield* report(reasons);
        if (reasons.length > 0)
          yield* Effect.logWarning(
            `Startup waits for the landed state queue: ${key}`,
          );
      }
      if (reasons.length === 0) return;
      yield* Effect.sleep(pollInterval);
    }
  });
