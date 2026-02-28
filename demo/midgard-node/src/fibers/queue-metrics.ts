import { Globals } from "@/services/index.js";
import { Effect, Metric, Ref } from "effect";

const blocksInQueueGauge = Metric.gauge("blocks_in_queue", {
  description: "Current in-memory count of unmerged blocks in state queue",
  bigint: true,
});

const processedUnsubmittedTxsCountGauge = Metric.gauge(
  "processed_unsubmitted_txs_count",
  {
    description:
      "Processed L2 tx count waiting to be committed in a future L1 block",
    bigint: true,
  },
);

const processedUnsubmittedTxsSizeGauge = Metric.gauge(
  "processed_unsubmitted_txs_size_bytes",
  {
    description:
      "Total size of processed L2 tx payloads waiting for future commitment",
    bigint: true,
  },
);

const unconfirmedSubmittedBlockGauge = Metric.gauge(
  "unconfirmed_submitted_block_pending",
  {
    description: "1 when an L1 block submission is pending confirmation",
    bigint: true,
  },
);

const unconfirmedSubmittedBlockAgeGauge = Metric.gauge(
  "unconfirmed_submitted_block_age_ms",
  {
    description:
      "Age of the currently pending unconfirmed block submission in milliseconds",
  },
);

export const emitQueueStateMetrics = Effect.gen(function* () {
  const globals = yield* Globals;
  const blocksInQueue = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const processedCount = yield* Ref.get(globals.PROCESSED_UNSUBMITTED_TXS_COUNT);
  const processedSize = yield* Ref.get(globals.PROCESSED_UNSUBMITTED_TXS_SIZE);
  const unconfirmedHash = yield* Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH);
  const unconfirmedSince = yield* Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS);

  const hasUnconfirmed = unconfirmedHash !== "";
  const unconfirmedAgeMs =
    hasUnconfirmed && unconfirmedSince > 0 ? Date.now() - unconfirmedSince : 0;

  yield* blocksInQueueGauge(Effect.succeed(BigInt(Math.max(0, blocksInQueue))));
  yield* processedUnsubmittedTxsCountGauge(
    Effect.succeed(BigInt(Math.max(0, processedCount))),
  );
  yield* processedUnsubmittedTxsSizeGauge(
    Effect.succeed(BigInt(Math.max(0, processedSize))),
  );
  yield* unconfirmedSubmittedBlockGauge(Effect.succeed(hasUnconfirmed ? 1n : 0n));
  yield* unconfirmedSubmittedBlockAgeGauge(
    Effect.succeed(Math.max(0, unconfirmedAgeMs)),
  );
});
