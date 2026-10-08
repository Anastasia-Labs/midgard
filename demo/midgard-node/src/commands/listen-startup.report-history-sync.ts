/**
 * Startup's `history_sync` stage names what holds the history owner back:
 * every raised liveness reason (a failed landed-block rebase the owner
 * retries, for one) is reported on `/readyz` during startup
 * (`startup.setStage`) once per change, with `/healthz` live. It never fails
 * and never ends; the caller interrupts it once the owner is ready.
 */
import { Duration, Effect } from "effect";

import { Globals } from "../services/globals.js";
import { currentLivenessReasons } from "../services/globals.liveness-reasons.js";

export const HISTORY_SYNC_REPORT_POLL = Duration.seconds(1);

export const reportHistorySyncReasons = (
  report: (reasons: readonly string[]) => Effect.Effect<void>,
  pollInterval: Duration.DurationInput = HISTORY_SYNC_REPORT_POLL,
): Effect.Effect<never, never, Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    let last = "";
    for (;;) {
      const reasons = yield* currentLivenessReasons(globals);
      const key = reasons.join(",");
      if (key !== last) {
        last = key;
        yield* report(reasons);
      }
      yield* Effect.sleep(pollInterval);
    }
  });
