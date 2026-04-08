import { Effect, Metric, pipe, Schedule } from "effect";
import { MempoolDB } from "@/database/index.js";
import { SqlClient } from "@effect/sql/SqlClient";
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

/**
 * Reads the current mempool transaction count and publishes it as a metric.
 */
const monitorMempoolAction: Effect.Effect<void, DatabaseError, SqlClient> =
  Effect.gen(function* () {
    const numTx = yield* MempoolDB.retrieveTxCount;
    yield* mempoolTxGauge(Effect.succeed(BigInt(numTx)));
  });

/**
 * Fiber wrapper that repeats mempool sampling on the provided schedule.
 */
export const monitorMempoolFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, SqlClient> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟢 Mempool monitor fiber started.");
      yield* Effect.repeat(monitorMempoolAction, schedule);
    }),
    Effect.catchAllCause(Effect.logWarning),
  );
