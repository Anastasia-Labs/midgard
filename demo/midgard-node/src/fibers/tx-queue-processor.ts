import { fromHex } from "@lucid-evolution/lucid";
import { Chunk, Effect, Metric, pipe, Queue, Schedule } from "effect";
import { MempoolDB } from "@/database/index.js";
import { ProcessedTx, breakDownTx } from "@/utils.js";
import { SqlClient } from "@effect/sql/SqlClient";

const txQueueSizeGauge = Metric.gauge("tx_queue_size", {
  description: "A tracker for the size of the tx queue before processing",
  bigint: true,
});

const txQueueProcessorAction = (txQueue: Queue.Dequeue<string>) =>
  Effect.gen(function* () {
    const queueSize = yield* txQueue.size;
    yield* txQueueSizeGauge(Effect.succeed(BigInt(queueSize)));

    const txStringsChunk: Chunk.Chunk<string> = yield* Queue.takeAll(txQueue);
    const txStrings = Chunk.toReadonlyArray(txStringsChunk);
    const brokeDownTxs: ProcessedTx[] = yield* Effect.forEach(txStrings, (tx) =>
      Effect.gen(function* () {
        return yield* breakDownTx(fromHex(tx));
      }),
    );
    yield* MempoolDB.insertMultiple(brokeDownTxs);
  });

export const txQueueProcessorFiber = (
  schedule: Schedule.Schedule<number>,
  txQueue: Queue.Dequeue<string>,
): Effect.Effect<void, never, SqlClient> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🔶 Tx queue processor fork started.");
      yield* Effect.repeat(txQueueProcessorAction(txQueue), schedule);
    }),
    Effect.catchAllCause(Effect.logWarning),
  );
