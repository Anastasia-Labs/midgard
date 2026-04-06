import { fromHex } from "@lucid-evolution/lucid";
import { Chunk, Effect, Metric, pipe, Queue, Schedule } from "effect";
import { MempoolDB } from "@/database/index.js";
import { ProcessedTx, breakDownTx } from "@/utils.js";
import { SqlClient } from "@effect/sql/SqlClient";

const txQueueSizeGauge = Metric.gauge("tx_queue_size", {
  description: "A tracker for the size of the tx queue before processing",
  bigint: true,
});

const processSingleTx = (
  tx: string,
): Effect.Effect<ProcessedTx | null, never, SqlClient> =>
  breakDownTx(fromHex(tx)).pipe(
    Effect.flatMap((processed) =>
      MempoolDB.insert(processed).pipe(Effect.as(processed)),
    ),
    Effect.catchAllCause((cause) =>
      Effect.logWarning(`🔶 Failed to process tx, skipping: ${cause}`).pipe(
        Effect.as(null),
      ),
    ),
  );

const txQueueProcessorAction = (
  txQueue: Queue.Dequeue<string>,
  withMonitoring?: boolean,
): Effect.Effect<void, never, SqlClient> =>
  Effect.gen(function* () {
    const queueSize = yield* txQueue.size;

    if (withMonitoring) {
      yield* txQueueSizeGauge(Effect.succeed(BigInt(queueSize)));
    }

    const txStringsChunk: Chunk.Chunk<string> = yield* Queue.takeAll(txQueue);
    const txStrings = Chunk.toReadonlyArray(txStringsChunk);

    // Process each tx individually so one bad tx does not drop the whole batch.
    yield* Effect.forEach(txStrings, processSingleTx);
  });

export const txQueueProcessorFiber = (
  schedule: Schedule.Schedule<number>,
  txQueue: Queue.Dequeue<string>,
  withMonitoring?: boolean,
): Effect.Effect<void, never, SqlClient> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🔶 Tx queue processor fiber started.");
      // Catch errors per-iteration so a single bad tx (e.g. duplicate submission
      // from wallet retry) does not kill the whole processor fiber.
      yield* Effect.repeat(
        txQueueProcessorAction(txQueue, withMonitoring).pipe(
          Effect.catchAllCause(Effect.logWarning),
        ),
        schedule,
      );
    }),
    Effect.catchAllCause(Effect.logWarning),
  );
