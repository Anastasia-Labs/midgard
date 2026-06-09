import { Globals } from "@/services/index.js";
import { Effect, Metric, Ref } from "effect";

/**
 * Background metrics emission for queue-related in-memory node state.
 */
const blocksInQueueGauge = Metric.gauge("blocks_in_queue", {
  description: "Current in-memory count of unmerged blocks in state queue",
  bigint: true,
});

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
  const unconfirmedHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const unconfirmedSince = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );

  const hasUnconfirmed = unconfirmedHash !== "";
  const unconfirmedAgeMs =
    hasUnconfirmed && unconfirmedSince > 0 ? Date.now() - unconfirmedSince : 0;

  yield* blocksInQueueGauge(Effect.succeed(BigInt(Math.max(0, blocksInQueue))));
  yield* unconfirmedSubmittedBlockGauge(
    Effect.succeed(hasUnconfirmed ? 1n : 0n),
  );
  yield* unconfirmedSubmittedBlockAgeGauge(
    Effect.succeed(Math.max(0, unconfirmedAgeMs)),
  );
});
