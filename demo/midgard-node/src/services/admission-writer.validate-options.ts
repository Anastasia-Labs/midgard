import {
  Cause,
  Context,
  Data,
  Effect,
  type Either,
  Metric,
  MetricBoundaries,
} from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { DatabaseError } from "../database/utils/common.js";
import type { AdmissionInputSpentError } from "./admission-input-claims.js";

export const ADMISSION_WRITE_SHARD_COUNT = 2;

export const ADMISSION_WRITE_BATCH_MAX_ROWS = 256;

export const ADMISSION_WRITE_QUEUE_CAPACITY = 20_000;

/** Payload bytes the writer may hold across all its stages (64 MiB). */
export const ADMISSION_WRITE_QUEUE_BYTES = 64 * 1024 * 1024;

/** A pending tx older than this means the writer cannot flush: the node
 * turns not-ready and admission answers 503 until writes succeed again. */
export const ADMISSION_WRITE_STALL_MS = 30_000;

/** How long a graceful stop waits for the pending set to flush. */
export const ADMISSION_WRITE_SHUTDOWN_FLUSH_MS = 10_000;

/** Backoff between attempts of a batch that failed transiently. */
export const ADMISSION_WRITE_RETRY_BASE_MS = 50;
export const ADMISSION_WRITE_RETRY_MAX_MS = 5_000;

/** One batch attempt longer than this is cancelled and retried. */
export const ADMISSION_WRITE_ATTEMPT_TIMEOUT_MS = 30_000;

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

/**
 * The oldest pending tx has waited longer than the stall limit: Postgres is
 * not taking the writes. Admission refuses until the pending set flushes.
 */
export class AdmissionWriterStalledError extends Data.TaggedError(
  "AdmissionWriterStalledError",
)<{
  readonly message: string;
  readonly oldestPendingAgeMs: number;
  readonly stallMs: number;
}> {}

/** Why the writer refused to take a tx; it holds nothing for it. */
export type AdmissionWriteRefusal =
  | TxAdmissionsDB.TxAdmissionConflictError
  | AdmissionWriterFullError
  | AdmissionWriterStalledError
  | AdmissionWriterShutdownError
  | AdmissionInputSpentError;

/** A checked tx handed to the writer. */
export type AdmissionWriteRequest = {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly txFullHash: Buffer;
  readonly submitSource: "native" | "peer";
  /** Settles the caller's backlog reservation once the writer knows whether
   * it wrote a new row. The writer owns it only after answering `new`. */
  readonly settle: (written: boolean) => Effect.Effect<void>;
  /** Run after a batch holding this tx commits (the tx-queue wake-up). */
  readonly wakeOnCommit: Effect.Effect<void>;
  /** Takes the tx's input claims (design D6). The writer calls it in its
   * synchronous admit section, after the duplicate, stall and bound checks
   * and in one step with the registration, so no second spender can slip
   * in between; a refusal leaves nothing registered. */
  readonly claimInputs?: () => AdmissionInputSpentError | undefined;
};

/** What the writer answers for a tx it took or already knows. */
export type AdmissionAck = {
  /** `new`: the tx entered the pending set (202). `duplicate`: the same
   * bytes are pending or were recently written (200). */
  readonly kind: "new" | "duplicate";
  readonly txIdHex: string;
  /** `queued` while pending; for a recent write, the admission status when
   * the node wrote it (read `/tx-status` for the current one). */
  readonly status: TxAdmissionsDB.Status;
  readonly firstSeenAt: Date;
  readonly lastSeenAt: Date;
};

/** A known tx id: the duplicate answer, or a conflict when the bytes differ. */
export type AdmissionLookup =
  | { readonly _tag: "Unknown" }
  | { readonly _tag: "Duplicate"; readonly ack: AdmissionAck }
  | {
      readonly _tag: "Conflict";
      readonly error: TxAdmissionsDB.TxAdmissionConflictError;
    };

export type AdmissionWriterHealth = {
  readonly accepting: boolean;
  readonly pending: number;
  readonly oldestPendingAgeMs: number;
  readonly stallMs: number;
  /** The oldest pending tx is older than `stallMs`. */
  readonly stalled: boolean;
};

export type AdmissionWriteItem = {
  readonly request: AdmissionWriteRequest;
  readonly txIdHex: string;
  /** Registration order, for `flush`. */
  readonly seq: number;
  readonly lane: number;
  readonly registeredAtMs: number;
  lastSeenAtMs: number;
  requestCount: number;
  phase: "registered" | "collected" | "inflight" | "completing";
  completed: boolean;
};

/** What became of one pending tx's write. */
export type AdmissionItemResult =
  | {
      readonly _tag: "Written";
      readonly outcome: TxAdmissionsDB.ReservedAdmissionOutcome;
    }
  | { readonly _tag: "Poisoned"; readonly cause: Cause.Cause<unknown> };

export type AdmissionCompletion = {
  readonly items: readonly AdmissionWriteItem[];
  readonly results: readonly AdmissionItemResult[];
};

export type AdmissionWriterStats = {
  readonly accepting: boolean;
  readonly pending: number;
  /** Txs dropped after a non-transient write failure of their own row. */
  readonly poisoned: number;
  readonly recentTxIds: number;
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

/**
 * The in-memory pending set in front of Postgres (design D9). `admit` answers
 * from memory and never waits for the database: the tx is written by a
 * background group commit. See `commands/http/checked-transaction-admission.ts`
 * for the admission path built on it.
 */
export type AdmissionWriterService = {
  /** Where the tx id stands in memory: pending, recently written, or
   * unknown. No database read. */
  readonly lookup: (txId: Buffer, txFullHash: Buffer) => AdmissionLookup;
  /** Takes the tx into the pending set, or refuses it. On `new` the writer
   * owns `request.settle`; on a duplicate or a refusal the caller does. */
  readonly admit: (
    request: AdmissionWriteRequest,
  ) => Effect.Effect<AdmissionAck, AdmissionWriteRefusal>;
  /** `admit` for each request, in order, in one synchronous registration:
   * a request repeating an earlier one's tx id answers as its duplicate (or
   * conflict). Each answer is its own; one refusal stops no other. A plain
   * call, so a caller outside Effect admits without a fiber. */
  readonly admitBatch: (
    requests: readonly AdmissionWriteRequest[],
  ) => readonly Either.Either<AdmissionAck, AdmissionWriteRefusal>[];
  /** Whether `txId` is waiting for its write; its first-seen time if so. */
  readonly pendingSince: (txId: Buffer) => Date | undefined;
  readonly health: Effect.Effect<AdmissionWriterHealth>;
  /** Waits until every tx pending when called has left the pending set. */
  readonly flush: Effect.Effect<void>;
  readonly stats: Effect.Effect<AdmissionWriterStats>;
};

export class AdmissionWriter extends Context.Tag("AdmissionWriter")<
  AdmissionWriter,
  AdmissionWriterService
>() {}

export type AdmissionWriterOptions = {
  readonly shardCount: number;
  readonly batchMaxRows: number;
  /** Row bound of the pending set (the unpersisted part). */
  readonly queueCapacity: number;
  /** Payload byte bound of the pending set. */
  readonly queueBytes: number;
  readonly stallMs?: number;
  readonly shutdownFlushMs?: number;
  readonly retryBaseMs?: number;
  readonly retryMaxMs?: number;
  readonly attemptTimeoutMs?: number;
  readonly recentTxIdCapacity?: number;
  /**
   * How long a recently written id answers duplicates from memory; 0 or
   * absent keeps it until evicted. The node sets it to
   * `ADMISSION_TERMINAL_RETENTION_MS`: past that age the row may have been
   * pruned, and a resubmission must reach the database to be admitted again.
   */
  readonly recentTxIdMaxAgeMs?: number;
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

export const admissionWritePoisonCounter = Metric.counter(
  "admission_write_poison_total",
  {
    description:
      "Acknowledged txs dropped because their own row failed a non-transient write",
    incremental: true,
  },
);

export const admissionWriteRetryCounter = Metric.counter(
  "admission_write_retry_total",
  {
    description: "Admission batch attempts retried after a transient failure",
    incremental: true,
  },
);

export const admissionWriteLostCounter = Metric.counter(
  "admission_write_lost_total",
  {
    description:
      "Acknowledged txs still unwritten when a graceful stop gave up waiting",
    incremental: true,
  },
);

export const admissionWriteStoredVariantCounter = Metric.counter(
  "admission_write_stored_variant_total",
  {
    description:
      "Acknowledged txs whose id was already stored with other witnesses; the stored version stays",
    incremental: true,
  },
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
  for (const [name, value] of Object.entries({
    stallMs: options.stallMs,
    shutdownFlushMs: options.shutdownFlushMs,
    retryBaseMs: options.retryBaseMs,
    retryMaxMs: options.retryMaxMs,
    attemptTimeoutMs: options.attemptTimeoutMs,
    recentTxIdCapacity: options.recentTxIdCapacity,
  })) {
    if (value !== undefined && (!Number.isSafeInteger(value) || value <= 0)) {
      throw new Error(`admission writer ${name} must be a positive integer`);
    }
  }
  if (
    options.recentTxIdMaxAgeMs !== undefined &&
    (!Number.isSafeInteger(options.recentTxIdMaxAgeMs) ||
      options.recentTxIdMaxAgeMs < 0)
  ) {
    throw new Error(
      "admission writer recentTxIdMaxAgeMs must be a non-negative integer",
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
