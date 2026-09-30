import {
  Cause,
  Chunk,
  Clock,
  Deferred,
  Duration,
  Effect,
  Exit,
  Metric,
  Option,
  Queue,
  Scope,
} from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  type AdmissionBatchPersistence,
  type AdmissionCompletion,
  admissionWriteBatchDurationTimer,
  admissionWriteBatchRowsHistogram,
  admissionWriteCapacityUsedGauge,
  admissionWriteCapacityWaitersGauge,
  type AdmissionWriteError,
  type AdmissionWriteItem,
  admissionWriteQueueDepthGauge,
  admissionWriteQueueMaxDepthGauge,
  type AdmissionWriterOptions,
  type AdmissionWriterService,
  admissionWriterShardForTxId,
  AdmissionWriterShutdownError,
  type AdmissionWriterTestHooks,
  admissionWriteStageDepthGauge,
  COMMIT_DURATION_UPPER_BOUNDS_MS,
  DEFAULT_OPTIONS,
  validateOptions,
} from "./admission-writer.validate-options.js";

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
