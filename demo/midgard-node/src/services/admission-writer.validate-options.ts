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

export const ADMISSION_WRITE_BATCH_TARGET_ROWS = 128;

export const ADMISSION_WRITE_BATCH_DEADLINE_MS = 100;

export const ADMISSION_WRITE_QUEUE_CAPACITY = 20_000;

export class AdmissionWriterShutdownError extends Data.TaggedError(
  "AdmissionWriterShutdownError",
)<{
  readonly message: string;
  readonly mayHaveCommitted: boolean;
}> {}

export type AdmissionWriteError =
  | DatabaseError
  | TxAdmissionsDB.TxAdmissionConflictError
  | TxAdmissionsDB.TxAdmissionBacklogFullError
  | AdmissionWriterShutdownError;

export type AdmissionWriteItem = {
  readonly request: TxAdmissionsDB.ReservedAdmissionRequest;
  readonly lane: number;
  readonly deferred: Deferred.Deferred<
    TxAdmissionsDB.AdmitResult,
    AdmissionWriteError
  >;
  phase:
    | "waiting_capacity"
    | "registered"
    | "collected"
    | "inflight"
    | "completing";
  capacityHeld: boolean;
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
  readonly waitingCapacity: number;
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
  readonly batchTargetRows: number;
  readonly batchDeadlineMs: number;
  readonly queueCapacity: number;
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
  batchTargetRows: ADMISSION_WRITE_BATCH_TARGET_ROWS,
  batchDeadlineMs: ADMISSION_WRITE_BATCH_DEADLINE_MS,
  queueCapacity: ADMISSION_WRITE_QUEUE_CAPACITY,
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

export const admissionWriteCapacityWaitersGauge = Metric.gauge(
  "admission_write_capacity_waiters",
  { description: "Admission requests waiting for a global writer permit" },
);

export const validateOptions = (options: AdmissionWriterOptions): void => {
  for (const [name, value] of Object.entries({
    shardCount: options.shardCount,
    batchMaxRows: options.batchMaxRows,
    batchTargetRows: options.batchTargetRows,
    queueCapacity: options.queueCapacity,
  })) {
    if (!Number.isSafeInteger(value) || value <= 0) {
      throw new Error(`admission writer ${name} must be a positive integer`);
    }
  }
  if (
    !Number.isSafeInteger(options.batchDeadlineMs) ||
    options.batchDeadlineMs < 0
  ) {
    throw new Error(
      "admission writer batchDeadlineMs must be a non-negative integer",
    );
  }
  if (options.batchTargetRows > options.batchMaxRows) {
    throw new Error(
      "admission writer batchTargetRows must not exceed batchMaxRows",
    );
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
