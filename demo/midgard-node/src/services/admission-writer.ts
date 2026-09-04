import { SqlClient } from "@effect/sql";
import {
  Cause,
  Chunk,
  Clock,
  Context,
  Data,
  Deferred,
  Duration,
  Effect,
  Exit,
  Layer,
  Metric,
  MetricBoundaries,
  Option,
  Queue,
  Scope,
} from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { DatabaseError } from "../database/utils/common.js";
import { AdmissionSql } from "./database.js";

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

type AdmissionWriteItem = {
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

type AdmissionCompletion = {
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

type AdmissionBatchPersistence<R> = (
  requests: readonly TxAdmissionsDB.ReservedAdmissionRequest[],
) => Effect.Effect<
  readonly TxAdmissionsDB.ReservedAdmissionOutcome[],
  DatabaseError,
  R
>;

const DEFAULT_OPTIONS: AdmissionWriterOptions = {
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

const validateOptions = (options: AdmissionWriterOptions): void => {
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

const COMMIT_DURATION_UPPER_BOUNDS_MS = [1, 2, 4, 8, 16, 32, 64, 128, 256];

/**
 * Testable constructor. Production supplies the durable PostgreSQL batch
 * statement below; focused lifecycle tests can inject a controlled persister.
 */
export const makeAdmissionWriterWithOptions = <R>(
  persistBatch: AdmissionBatchPersistence<R>,
  options: AdmissionWriterOptions = DEFAULT_OPTIONS,
  testHooks: AdmissionWriterTestHooks = {},
): Effect.Effect<AdmissionWriterService, never, R | Scope.Scope> =>
  Effect.gen(function* () {
    validateOptions(options);
    const inputQueues = yield* Effect.forEach(
      Array.from({ length: options.shardCount }),
      () => Queue.unbounded<AdmissionWriteItem>(),
    );
    const preparedQueues = yield* Effect.forEach(
      Array.from({ length: options.shardCount }),
      () => Queue.unbounded<readonly AdmissionWriteItem[]>(),
    );
    const completionQueues = yield* Effect.forEach(
      Array.from({ length: options.shardCount }),
      () => Queue.unbounded<AdmissionCompletion>(),
    );
    const capacity = yield* Effect.makeSemaphore(options.queueCapacity);
    let capacityUsed = 0;
    let waitingCapacity = 0;
    let maxQueueDepth = 0;
    const stageDepths = Array.from({ length: options.shardCount }, () => ({
      input: 0,
      prepared: 0,
      persisting: 0,
      completion: 0,
      maxInput: 0,
      maxPrepared: 0,
      maxPersisting: 0,
      maxCompletion: 0,
    }));
    const shardTelemetry = Array.from({ length: options.shardCount }, () => ({
      batches: 0,
      rows: 0,
      commitDurationMs: 0,
      maxCommitDurationMs: 0,
      maxRowsPerBatch: 0,
      maxQueueDepth: 0,
      batchSizeCounts: new Map<number, number>(),
      commitDurationUpperBoundCounts: new Map<number, number>(),
    }));

    // Effect fibers execute synchronous sections atomically on one JS event
    // loop. Keeping this lifecycle registry mutable avoids a 20k-entry Map
    // copy on every admission while still giving shutdown one exact snapshot.
    let accepting = true;
    const pending = new Set<AdmissionWriteItem>();

    const updateMaxDepths = (): void => {
      let aggregateQueueDepth = 0;
      for (const [lane, stages] of stageDepths.entries()) {
        stages.maxInput = Math.max(stages.maxInput, stages.input);
        stages.maxPrepared = Math.max(stages.maxPrepared, stages.prepared);
        stages.maxPersisting = Math.max(
          stages.maxPersisting,
          stages.persisting,
        );
        stages.maxCompletion = Math.max(
          stages.maxCompletion,
          stages.completion,
        );
        const laneQueueDepth = stages.input + stages.prepared;
        aggregateQueueDepth += laneQueueDepth;
        const telemetry = shardTelemetry[lane]!;
        telemetry.maxQueueDepth = Math.max(
          telemetry.maxQueueDepth,
          laneQueueDepth,
        );
      }
      maxQueueDepth = Math.max(maxQueueDepth, aggregateQueueDepth);
    };

    const decrementPhase = (item: AdmissionWriteItem): void => {
      const stages = stageDepths[item.lane]!;
      switch (item.phase) {
        case "waiting_capacity":
          waitingCapacity -= 1;
          break;
        case "registered":
          stages.input -= 1;
          break;
        case "collected":
          stages.prepared -= 1;
          break;
        case "inflight":
          stages.persisting -= 1;
          break;
        case "completing":
          stages.completion -= 1;
          break;
      }
    };

    const incrementPhase = (
      item: AdmissionWriteItem,
      phase: AdmissionWriteItem["phase"],
    ): void => {
      item.phase = phase;
      const stages = stageDepths[item.lane]!;
      switch (phase) {
        case "waiting_capacity":
          waitingCapacity += 1;
          break;
        case "registered":
          stages.input += 1;
          break;
        case "collected":
          stages.prepared += 1;
          break;
        case "inflight":
          stages.persisting += 1;
          break;
        case "completing":
          stages.completion += 1;
          break;
      }
    };

    const transitionItems = (
      items: readonly AdmissionWriteItem[],
      phase: AdmissionWriteItem["phase"],
    ) =>
      Effect.sync(() => {
        for (const item of items) {
          if (item.completed) continue;
          decrementPhase(item);
          incrementPhase(item, phase);
        }
        updateMaxDepths();
      });

    const stageSnapshot = () => ({
      capacityUsed,
      waitingCapacity,
      lanes: stageDepths.map((stages) => ({ ...stages })),
      queueDepths: stageDepths.map((stages) => stages.input + stages.prepared),
      maxQueueDepth,
    });

    const reportDepth = Effect.gen(function* () {
      const snapshot = yield* Effect.sync(stageSnapshot);
      const depth = snapshot.queueDepths.reduce((sum, value) => sum + value, 0);
      yield* admissionWriteQueueDepthGauge(Effect.succeed(depth));
      yield* admissionWriteQueueMaxDepthGauge(
        Effect.succeed(snapshot.maxQueueDepth),
      );
      yield* admissionWriteCapacityUsedGauge(
        Effect.succeed(snapshot.capacityUsed),
      );
      yield* admissionWriteCapacityWaitersGauge(
        Effect.succeed(snapshot.waitingCapacity),
      );
      for (const [lane, stages] of snapshot.lanes.entries()) {
        for (const [stage, value] of Object.entries({
          input: stages.input,
          prepared: stages.prepared,
          persisting: stages.persisting,
          completion: stages.completion,
        })) {
          yield* Metric.tagged(
            Metric.tagged(
              admissionWriteStageDepthGauge,
              "lane",
              lane.toString(),
            ),
            "stage",
            stage,
          )(Effect.succeed(value));
        }
      }
    });

    const completeItem = (
      item: AdmissionWriteItem,
      effect: Effect.Effect<boolean>,
    ): Effect.Effect<void> =>
      Effect.uninterruptible(
        Effect.gen(function* () {
          const claimed = yield* Effect.sync(() => {
            if (item.completed) return { complete: false, release: false };
            decrementPhase(item);
            item.completed = true;
            pending.delete(item);
            const release = item.capacityHeld;
            if (release) {
              item.capacityHeld = false;
              capacityUsed -= 1;
            }
            updateMaxDepths();
            return { complete: true, release };
          });
          if (!claimed.complete) return;
          yield* effect.pipe(
            Effect.ensuring(
              claimed.release
                ? capacity.release(1).pipe(Effect.asVoid)
                : Effect.void,
            ),
            Effect.asVoid,
          );
        }),
      );

    const completeBatch = (
      items: readonly AdmissionWriteItem[],
      exit: Exit.Exit<
        readonly TxAdmissionsDB.ReservedAdmissionOutcome[],
        DatabaseError
      >,
    ): Effect.Effect<void> =>
      Effect.gen(function* () {
        if (Exit.isSuccess(exit) && exit.value.length !== items.length) {
          const cause = Cause.die(
            new Error(
              `Admission microbatch outcome cardinality mismatch: expected ${items.length.toString()}, received ${exit.value.length.toString()}`,
            ),
          );
          for (const item of items) {
            yield* completeItem(item, Deferred.failCause(item.deferred, cause));
          }
          return;
        }
        for (const [index, item] of items.entries()) {
          if (Exit.isFailure(exit)) {
            yield* completeItem(
              item,
              Deferred.failCause(item.deferred, exit.cause),
            );
            continue;
          }
          const outcome = exit.value[index]!;
          yield* completeItem(
            item,
            outcome._tag === "Success"
              ? Deferred.succeed(item.deferred, outcome.result)
              : Deferred.fail(item.deferred, outcome.error),
          );
        }
      });

    const collectBatch = (queue: Queue.Queue<AdmissionWriteItem>) =>
      Effect.gen(function* () {
        const first = yield* Queue.take(queue);
        const deadlineAt =
          (yield* Clock.currentTimeMillis) + options.batchDeadlineMs;
        const items: AdmissionWriteItem[] = [first];
        while (
          items.length < options.batchTargetRows &&
          items.length < options.batchMaxRows
        ) {
          const available = yield* Queue.takeUpTo(
            queue,
            options.batchMaxRows - items.length,
          );
          if (!Chunk.isEmpty(available)) {
            items.push(...Chunk.toReadonlyArray(available));
            if (items.length >= options.batchTargetRows) break;
            continue;
          }
          const remainingMs = deadlineAt - (yield* Clock.currentTimeMillis);
          if (remainingMs <= 0) break;
          const next = yield* Queue.take(queue).pipe(
            Effect.timeoutOption(Duration.millis(remainingMs)),
          );
          if (Option.isNone(next)) break;
          items.push(next.value);
        }
        return items;
      });

    const runCollector = (lane: number) =>
      Effect.gen(function* () {
        if (testHooks.beforeCollect !== undefined) {
          yield* testHooks.beforeCollect(lane);
        }
        const items = yield* collectBatch(inputQueues[lane]!);
        yield* transitionItems(items, "collected");
        yield* Queue.offer(preparedQueues[lane]!, items);
        yield* reportDepth;
      });

    const runPersister = (lane: number) =>
      Effect.gen(function* () {
        if (testHooks.beforePersist !== undefined) {
          yield* testHooks.beforePersist(lane);
        }
        const items = yield* Queue.take(preparedQueues[lane]!);
        yield* transitionItems(items, "inflight");
        yield* reportDepth;
        const startedAt = Date.now();
        const exit = yield* Effect.exit(
          persistBatch(items.map((item) => item.request)),
        );
        const durationMs = Date.now() - startedAt;
        yield* Effect.sync(() => {
          const telemetry = shardTelemetry[lane]!;
          telemetry.batches += 1;
          telemetry.rows += items.length;
          telemetry.commitDurationMs += durationMs;
          telemetry.maxCommitDurationMs = Math.max(
            telemetry.maxCommitDurationMs,
            durationMs,
          );
          telemetry.maxRowsPerBatch = Math.max(
            telemetry.maxRowsPerBatch,
            items.length,
          );
          telemetry.batchSizeCounts.set(
            items.length,
            (telemetry.batchSizeCounts.get(items.length) ?? 0) + 1,
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
        yield* admissionWriteBatchRowsHistogram(Effect.succeed(items.length));
        yield* transitionItems(items, "completing");
        yield* Queue.offer(completionQueues[lane]!, { items, exit });
        yield* reportDepth;
      });

    const runCompletion = (lane: number) =>
      Effect.gen(function* () {
        if (testHooks.beforeComplete !== undefined) {
          yield* testHooks.beforeComplete(lane);
        }
        const completion = yield* Queue.take(completionQueues[lane]!);
        yield* completeBatch(completion.items, completion.exit);
        yield* reportDepth;
      });

    const service = {
      admitReserved: (request) =>
        Effect.gen(function* () {
          const deferred = yield* Deferred.make<
            TxAdmissionsDB.AdmitResult,
            AdmissionWriteError
          >();
          const item: AdmissionWriteItem = {
            request,
            lane: admissionWriterShardForTxId(request.txId, options.shardCount),
            deferred,
            phase: "waiting_capacity",
            capacityHeld: false,
            completed: false,
          };
          const registered = yield* Effect.sync(() => {
            if (!accepting) return false;
            pending.add(item);
            incrementPhase(item, "waiting_capacity");
            updateMaxDepths();
            return true;
          });
          if (!registered) {
            return yield* Effect.fail(
              new AdmissionWriterShutdownError({
                message: "Admission writer is shutting down",
                mayHaveCommitted: false,
              }),
            );
          }
          const enqueue = Effect.uninterruptibleMask((restore) =>
            Effect.gen(function* () {
              yield* restore(capacity.take(1));
              const shouldEnqueue = yield* Effect.sync(() => {
                if (!accepting || item.completed) return false;
                decrementPhase(item);
                item.capacityHeld = true;
                capacityUsed += 1;
                incrementPhase(item, "registered");
                updateMaxDepths();
                return true;
              });
              if (!shouldEnqueue) {
                yield* capacity.release(1);
                return;
              }
              yield* Queue.offer(inputQueues[item.lane]!, item);
            }),
          );
          yield* Effect.raceFirst(
            enqueue,
            Deferred.await(deferred).pipe(Effect.asVoid),
          ).pipe(
            Effect.onInterrupt(() =>
              Effect.sync(() => {
                if (item.phase !== "waiting_capacity" || item.completed) return;
                decrementPhase(item);
                item.completed = true;
                pending.delete(item);
                updateMaxDepths();
              }),
            ),
          );
          return yield* Deferred.await(deferred).pipe(
            Effect.onInterrupt(() =>
              Effect.sync(() => {
                if (item.phase !== "waiting_capacity" || item.completed) return;
                decrementPhase(item);
                item.completed = true;
                pending.delete(item);
                updateMaxDepths();
              }),
            ),
          );
        }),
      stats: Effect.sync(() => {
        const snapshot = stageSnapshot();
        const laneStats = shardTelemetry.map((telemetry, shard) => ({
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
          queueDepth: snapshot.queueDepths[shard]!,
          maxQueueDepth: telemetry.maxQueueDepth,
          stages: { ...snapshot.lanes[shard]! },
        }));
        return {
          accepting,
          pending: pending.size,
          capacity: options.queueCapacity,
          capacityUsed: snapshot.capacityUsed,
          waitingCapacity: snapshot.waitingCapacity,
          queueDepths: snapshot.queueDepths,
          queueDepth: snapshot.queueDepths.reduce(
            (sum, value) => sum + value,
            0,
          ),
          maxQueueDepth: snapshot.maxQueueDepth,
          lanes: laneStats,
          shards: laneStats,
        };
      }),
    } satisfies AdmissionWriterService;

    yield* Effect.forEach(
      inputQueues,
      (_, lane) =>
        Effect.all(
          [
            Effect.forkScoped(
              Effect.logInfo(
                `🧺 Durable admission writer lane ${lane.toString()} collector started.`,
              ).pipe(Effect.zipRight(Effect.forever(runCollector(lane)))),
            ),
            Effect.forkScoped(Effect.forever(runPersister(lane))),
            Effect.forkScoped(Effect.forever(runCompletion(lane))),
          ],
          { concurrency: "unbounded", discard: true },
        ),
      { concurrency: "unbounded", discard: true },
    );
    // Registered after the consumer fibers so this LIFO finalizer runs first.
    yield* Effect.addFinalizer(() =>
      Effect.gen(function* () {
        const outstanding = yield* Effect.sync(() => {
          accepting = false;
          return [...pending];
        });
        for (const item of outstanding) {
          yield* completeItem(
            item,
            Deferred.fail(
              item.deferred,
              new AdmissionWriterShutdownError({
                message: "Admission writer shut down before durable completion",
                mayHaveCommitted:
                  item.phase === "inflight" || item.phase === "completing",
              }),
            ),
          );
        }
        yield* Effect.forEach(inputQueues, Queue.shutdown, {
          concurrency: "unbounded",
          discard: true,
        });
        yield* Effect.forEach(preparedQueues, Queue.shutdown, {
          concurrency: "unbounded",
          discard: true,
        });
        yield* Effect.forEach(completionQueues, Queue.shutdown, {
          concurrency: "unbounded",
          discard: true,
        });
        yield* reportDepth;
      }),
    );
    return service;
  });

export const makeAdmissionWriter = Effect.gen(function* () {
  const admissionSql = yield* AdmissionSql;
  return yield* makeAdmissionWriterWithOptions((requests) =>
    TxAdmissionsDB.admitReservedBatch(requests).pipe(
      Effect.provideService(SqlClient.SqlClient, admissionSql),
    ),
  );
});

export const AdmissionWriterLive = Layer.scoped(
  AdmissionWriter,
  makeAdmissionWriter,
);
