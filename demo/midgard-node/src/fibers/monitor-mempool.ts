import { Effect, Metric, pipe, Schedule } from "effect";
import { MempoolDB } from "@/database/index.js";
import { SqlClient } from "@effect/sql/SqlClient";

const mempoolTxGauge = Metric.gauge("mempool_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the mempool",
  bigint: true,
});

const monitorMempoolAction = Effect.gen(function* () {
  const numTx = yield* MempoolDB.retrieveTxCount;
  yield* mempoolTxGauge(Effect.succeed(BigInt(numTx)));
});

export const monitorMempoolFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, SqlClient> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟢 Mempool monitor fork started.");
      yield* Effect.repeat(monitorMempoolAction, schedule);
    }),
    Effect.catchAllCause(Effect.logWarning),
  );
