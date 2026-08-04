import { SqlClient } from "@effect/sql/SqlClient";
import { Effect, Metric, Schedule } from "effect";

import { MempoolDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";

/**
 * Background mempool metrics collection.
 *
 * The node uses this to expose the current mempool depth as a simple gauge for
 * dashboards and alerts.
 */
const mempoolTxGauge = Metric.gauge("mempool_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the mempool",
  bigint: true,
});

const mempoolOldestTxAgeGauge = Metric.gauge("mempool_oldest_tx_age_ms", {
  description: "Age of the oldest transaction currently in the mempool",
});

/**
 * Reads the current mempool transaction count and publishes it as a metric.
 */
const monitorMempoolAction: Effect.Effect<void, DatabaseError, SqlClient> =
  Effect.gen(function* () {
    const [numTx, oldestPage] = yield* Effect.all(
      [MempoolDB.retrieveTxCount, MempoolDB.retrievePage({ limit: 1 })],
      { concurrency: "unbounded" },
    );
    yield* mempoolTxGauge(Effect.succeed(numTx));
    const oldest = oldestPage.entries[0];
    yield* mempoolOldestTxAgeGauge(
      Effect.succeed(
        oldest === undefined
          ? 0
          : Math.max(0, Date.now() - oldest.time_stamp_tz.getTime()),
      ),
    );
  });

/**
 * Fiber wrapper that repeats mempool sampling on the provided schedule.
 */
export const monitorMempoolFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟢 Mempool monitor fiber started.");
    yield* Effect.repeat(monitorMempoolAction, schedule);
  }).pipe(Effect.catchAllCause(Effect.logWarning));
