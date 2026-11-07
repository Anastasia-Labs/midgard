import {
  Context,
  Data,
  Deferred,
  Effect,
  Exit,
  Metric,
  MetricBoundaries,
} from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { DatabaseError } from "../database/utils/common.js";

export const ADMISSION_WRITE_SHARD_COUNT = 2;

export const ADMISSION_WRITE_BATCH_MAX_ROWS = 256;

export const ADMISSION_WRITE_QUEUE_CAPACITY = 20_000;

/** Payload bytes the writer may hold across all its stages (64 MiB). */
export const ADMISSION_WRITE_QUEUE_BYTES = 64 * 1024 * 1024;

export class AdmissionWriterShutdownError extends Data.TaggedError(
  "AdmissionWriterShutdownError",
)<{
  readonly message: string;
  readonly mayHaveCommitted: boolean;
}> {}

/**
 * The writer already holds its row or byte bound. It refuses at once instead
 * of making the caller wait for a slot; nothing was written.
 */
export class AdmissionWriterFullError extends Data.TaggedError(
  "AdmissionWriterFullError",
)<{
  readonly message: string;
  readonly unit: "rows" | "bytes";
  readonly used: number;
  readonly capacity: number;
}> {}

export type AdmissionWriteError =
  | DatabaseError
  | TxAdmissionsDB.TxAdmissionConflictError
  | AdmissionWriterShutdownError
  | AdmissionWriterFullError;

export type AdmissionWriteItem = {
  readonly request: TxAdmissionsDB.ReservedAdmissionRequest;
  readonly lane: number;
  readonly deferred: Deferred.Deferred<
    TxAdmissionsDB.AdmitResult,
    AdmissionWriteError
  >;
  phase: "registered" | "collected" | "inflight" | "completing";
  completed: boolean;
};

export type AdmissionCompletion = {
  readonly items: readonly AdmissionWriteItem[];
  readonly exit: Exit.Exit<
    readonly TxAdmissionsDB.ReservedAdmissionOutcome[],
    DatabaseError
  >;
};

export type AdmissionWriterStats = {
  readonly accepting: boolean;
  readonly pending: number;
  readonly capacity: number;
  readonly capacityUsed: number;
  readonly capacityBytes: number;
  readonly bytesUsed: number;
  readonly queueDepths: readonly number[];
  readonly queueDepth: number;
  readonly maxQueueDepth: number;
  readonly lanes: readonly AdmissionWriterShardStats[];
  readonly shards: readonly AdmissionWriterShardStats[];
};

export type AdmissionWriterShardStats = {
  readonly shard: number;
  readonly batches: number;
  readonly rows: number;
  readonly averageRowsPerBatch: number | null;
  readonly maxRowsPerBatch: number;
  readonly commitDurationMs: {
    readonly average: number | null;
    readonly max: number;
  };
  readonly batchSizeCounts: Readonly<Record<string, number>>;
  readonly commitDurationUpperBoundCounts: Readonly<Record<string, number>>;
  readonly queueDepth: number;
  readonly maxQueueDepth: number;
  readonly stages: {
    readonly input: number;
    readonly prepared: number;
    readonly persisting: number;
    readonly completion: number;
    readonly maxInput: number;
    readonly maxPrepared: number;
    readonly maxPersisting: number;
    readonly maxCompletion: number;
  };
};

export type AdmissionWriterService = {
  readonly admitReserved: (
    request: TxAdmissionsDB.ReservedAdmissionRequest,
  ) => Effect.Effect<TxAdmissionsDB.AdmitResult, AdmissionWriteError>;
  readonly stats: Effect.Effect<AdmissionWriterStats>;
};

export class AdmissionWriter extends Context.Tag("AdmissionWriter")<
  AdmissionWriter,
  AdmissionWriterService
>() {}

export type AdmissionWriterOptions = {
  readonly shardCount: number;
  readonly batchMaxRows: number;
  readonly queueCapacity: number;
  readonly queueBytes: number;
};

export type AdmissionWriterTestHooks = {
  readonly beforeCollect?: (lane: number) => Effect.Effect<void>;
  readonly beforePersist?: (lane: number) => Effect.Effect<void>;
  readonly beforeComplete?: (lane: number) => Effect.Effect<void>;
};

export type AdmissionBatchPersistence<R> = (
  requests: readonly TxAdmissionsDB.ReservedAdmissionRequest[],
) => Effect.Effect<
  readonly TxAdmissionsDB.ReservedAdmissionOutcome[],
  DatabaseError,
  R
>;

export const DEFAULT_OPTIONS: AdmissionWriterOptions = {
  shardCount: ADMISSION_WRITE_SHARD_COUNT,
  batchMaxRows: ADMISSION_WRITE_BATCH_MAX_ROWS,
  queueCapacity: ADMISSION_WRITE_QUEUE_CAPACITY,
  queueBytes: ADMISSION_WRITE_QUEUE_BYTES,
};

/**
 * Deterministic FNV-1a routing over the canonical tx id. A tx id and every
 * conflicting byte variant carrying that id always reach one FIFO consumer.
 * Distinct cryptographic tx ids may collide safely; they simply share a shard.
 */
export const admissionWriterShardForTxId = (
  txId: Uint8Array,
  shardCount: number = ADMISSION_WRITE_SHARD_COUNT,
): number => {
  if (!Number.isSafeInteger(shardCount) || shardCount <= 0) {
    throw new Error("admission writer shardCount must be a positive integer");
  }
  let hash = 0x811c9dc5;
  for (const byte of txId) {
    hash = Math.imul(hash ^ byte, 0x01000193);
  }
  return (hash >>> 0) % shardCount;
};

export const admissionWriteBatchDurationTimer = Metric.timer(
  "admission_write_batch_duration",
  "Duration of one durable admission microbatch statement and commit",
);

export const admissionWriteBatchRowsHistogram = Metric.histogram(
  "admission_write_batch_rows",
  MetricBoundaries.fromIterable([1, 8, 16, 32, 64, 128, 256]),
  "Number of reserved HTTP requests resolved by one admission microbatch",
);

export const admissionWriteQueueDepthGauge = Metric.gauge(
  "admission_write_queue_depth",
  { description: "Reserved admission requests waiting for a durable batch" },
);

export const admissionWriteQueueMaxDepthGauge = Metric.gauge(
  "admission_write_queue_max_depth",
  { description: "Maximum durable admission writer queue depth observed" },
);

export const admissionWriteStageDepthGauge = Metric.gauge(
  "admission_write_stage_depth",
  { description: "Admission writer rows held by a pipeline stage and lane" },
);

export const admissionWriteCapacityUsedGauge = Metric.gauge(
  "admission_write_capacity_used",
  { description: "Admission writer permits held across all pipeline stages" },
);

export const admissionWriteBytesUsedGauge = Metric.gauge(
  "admission_write_bytes_used",
  { description: "Payload bytes held across all admission writer stages" },
);

export const validateOptions = (options: AdmissionWriterOptions): void => {
  for (const [name, value] of Object.entries({
    shardCount: options.shardCount,
    batchMaxRows: options.batchMaxRows,
    queueCapacity: options.queueCapacity,
    queueBytes: options.queueBytes,
  })) {
    if (!Number.isSafeInteger(value) || value <= 0) {
      throw new Error(`admission writer ${name} must be a positive integer`);
    }
  }
  if (options.queueCapacity < options.shardCount) {
    throw new Error(
      "admission writer queueCapacity must be at least shardCount",
    );
  }
};

export const COMMIT_DURATION_UPPER_BOUNDS_MS = [
  1, 2, 4, 8, 16, 32, 64, 128, 256,
];
