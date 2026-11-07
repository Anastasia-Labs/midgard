import { Duration, Effect, Metric } from "effect";

import {
  admissionWriteBatchDurationTimer,
  admissionWriteBatchRowsHistogram,
  admissionWriteBytesUsedGauge,
  admissionWriteCapacityUsedGauge,
  type AdmissionWriteItem,
  admissionWriteQueueDepthGauge,
  admissionWriteQueueMaxDepthGauge,
  type AdmissionWriterShardStats,
  type AdmissionWriterStats,
  admissionWriteStageDepthGauge,
  COMMIT_DURATION_UPPER_BOUNDS_MS,
} from "./admission-writer.validate-options.js";

type Phase = AdmissionWriteItem["phase"];

const STAGE_OF: Record<
  Phase,
  "input" | "prepared" | "persisting" | "completion"
> = {
  registered: "input",
  collected: "prepared",
  inflight: "persisting",
  completing: "completion",
};

export type WriterStatsInput = Omit<
  AdmissionWriterStats,
  "queueDepths" | "queueDepth" | "maxQueueDepth" | "lanes" | "shards"
>;

/**
 * Per-lane stage depths and batch telemetry of the admission writer. Every
 * method but `recordBatch` and `report` is synchronous, so the writer calls
 * them inside its own synchronous sections.
 */
export const makeWriterTelemetry = (shardCount: number) => {
  let maxQueueDepth = 0;
  const stageDepths = Array.from({ length: shardCount }, () => ({
    input: 0,
    prepared: 0,
    persisting: 0,
    completion: 0,
    maxInput: 0,
    maxPrepared: 0,
    maxPersisting: 0,
    maxCompletion: 0,
  }));
  const shardTelemetry = Array.from({ length: shardCount }, () => ({
    batches: 0,
    rows: 0,
    commitDurationMs: 0,
    maxCommitDurationMs: 0,
    maxRowsPerBatch: 0,
    maxQueueDepth: 0,
    batchSizeCounts: new Map<number, number>(),
    commitDurationUpperBoundCounts: new Map<number, number>(),
  }));

  const updateMaxDepths = (): void => {
    let aggregate = 0;
    for (const [lane, stages] of stageDepths.entries()) {
      stages.maxInput = Math.max(stages.maxInput, stages.input);
      stages.maxPrepared = Math.max(stages.maxPrepared, stages.prepared);
      stages.maxPersisting = Math.max(stages.maxPersisting, stages.persisting);
      stages.maxCompletion = Math.max(stages.maxCompletion, stages.completion);
      const laneDepth = stages.input + stages.prepared;
      aggregate += laneDepth;
      const telemetry = shardTelemetry[lane]!;
      telemetry.maxQueueDepth = Math.max(telemetry.maxQueueDepth, laneDepth);
    }
    maxQueueDepth = Math.max(maxQueueDepth, aggregate);
  };

  const queueDepths = () =>
    stageDepths.map((stages) => stages.input + stages.prepared);

  return {
    enter: (item: AdmissionWriteItem): void => {
      item.phase = "registered";
      stageDepths[item.lane]!.input += 1;
      updateMaxDepths();
    },
    move: (items: readonly AdmissionWriteItem[], phase: Phase): void => {
      for (const item of items) {
        if (item.completed) continue;
        stageDepths[item.lane]![STAGE_OF[item.phase]] -= 1;
        item.phase = phase;
        stageDepths[item.lane]![STAGE_OF[phase]] += 1;
      }
      updateMaxDepths();
    },
    leave: (item: AdmissionWriteItem): void => {
      stageDepths[item.lane]![STAGE_OF[item.phase]] -= 1;
    },
    recordBatch: (lane: number, rows: number, durationMs: number) =>
      Effect.gen(function* () {
        yield* Effect.sync(() => {
          const telemetry = shardTelemetry[lane]!;
          telemetry.batches += 1;
          telemetry.rows += rows;
          telemetry.commitDurationMs += durationMs;
          telemetry.maxCommitDurationMs = Math.max(
            telemetry.maxCommitDurationMs,
            durationMs,
          );
          telemetry.maxRowsPerBatch = Math.max(telemetry.maxRowsPerBatch, rows);
          telemetry.batchSizeCounts.set(
            rows,
            (telemetry.batchSizeCounts.get(rows) ?? 0) + 1,
          );
          const upperBound =
            COMMIT_DURATION_UPPER_BOUNDS_MS.find(
              (candidate) => durationMs <= candidate,
            ) ?? Number.POSITIVE_INFINITY;
          telemetry.commitDurationUpperBoundCounts.set(
            upperBound,
            (telemetry.commitDurationUpperBoundCounts.get(upperBound) ?? 0) + 1,
          );
        });
        yield* admissionWriteBatchDurationTimer(
          Effect.succeed(Duration.millis(durationMs)),
        );
        yield* admissionWriteBatchRowsHistogram(Effect.succeed(rows));
      }),
    report: (capacityUsed: number, bytesUsed: number) =>
      Effect.gen(function* () {
        const depths = yield* Effect.sync(queueDepths);
        yield* admissionWriteQueueDepthGauge(
          Effect.succeed(depths.reduce((sum, value) => sum + value, 0)),
        );
        yield* admissionWriteQueueMaxDepthGauge(Effect.succeed(maxQueueDepth));
        yield* admissionWriteCapacityUsedGauge(Effect.succeed(capacityUsed));
        yield* admissionWriteBytesUsedGauge(Effect.succeed(bytesUsed));
        for (const [lane, stages] of stageDepths.entries()) {
          for (const stage of [
            "input",
            "prepared",
            "persisting",
            "completion",
          ] as const) {
            yield* Metric.tagged(
              Metric.tagged(
                admissionWriteStageDepthGauge,
                "lane",
                lane.toString(),
              ),
              "stage",
              stage,
            )(Effect.succeed(stages[stage]));
          }
        }
      }),
    stats: (input: WriterStatsInput): AdmissionWriterStats => {
      const depths = queueDepths();
      const lanes: AdmissionWriterShardStats[] = shardTelemetry.map(
        (telemetry, shard) => ({
          shard,
          batches: telemetry.batches,
          rows: telemetry.rows,
          averageRowsPerBatch:
            telemetry.batches === 0 ? null : telemetry.rows / telemetry.batches,
          maxRowsPerBatch: telemetry.maxRowsPerBatch,
          commitDurationMs: {
            average:
              telemetry.batches === 0
                ? null
                : telemetry.commitDurationMs / telemetry.batches,
            max: telemetry.maxCommitDurationMs,
          },
          batchSizeCounts: Object.fromEntries(
            [...telemetry.batchSizeCounts.entries()]
              .sort(([left], [right]) => left - right)
              .map(([size, count]) => [size.toString(), count]),
          ),
          commitDurationUpperBoundCounts: Object.fromEntries(
            [...telemetry.commitDurationUpperBoundCounts.entries()]
              .sort(([left], [right]) => left - right)
              .map(([upperBound, count]) => [
                Number.isFinite(upperBound)
                  ? upperBound.toString()
                  : "infinity",
                count,
              ]),
          ),
          queueDepth: depths[shard]!,
          maxQueueDepth: telemetry.maxQueueDepth,
          stages: { ...stageDepths[shard]! },
        }),
      );
      return {
        ...input,
        queueDepths: depths,
        queueDepth: depths.reduce((sum, value) => sum + value, 0),
        maxQueueDepth,
        lanes,
        shards: lanes,
      };
    },
  };
};
