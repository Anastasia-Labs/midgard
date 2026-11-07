import {
  Chunk,
  Deferred,
  Duration,
  Effect,
  Exit,
  Metric,
  Queue,
  Scope,
} from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { persistWithRetry } from "./admission-writer.persist-with-retry.js";
import { makeRecentTxIds } from "./admission-writer.recent-tx-ids.js";
import { makeWriterTelemetry } from "./admission-writer.telemetry.js";
import {
  ADMISSION_WRITE_ATTEMPT_TIMEOUT_MS,
  ADMISSION_WRITE_RETRY_BASE_MS,
  ADMISSION_WRITE_RETRY_MAX_MS,
  ADMISSION_WRITE_SHUTDOWN_FLUSH_MS,
  ADMISSION_WRITE_STALL_MS,
  type AdmissionAck,
  type AdmissionBatchPersistence,
  type AdmissionCompletion,
  type AdmissionItemResult,
  type AdmissionLookup,
  type AdmissionWriteItem,
  admissionWriteLostCounter,
  type AdmissionWriteRefusal,
  AdmissionWriterFullError,
  type AdmissionWriterOptions,
  type AdmissionWriterService,
  admissionWriterShardForTxId,
  AdmissionWriterShutdownError,
  AdmissionWriterStalledError,
  type AdmissionWriterTestHooks,
  admissionWriteStoredVariantCounter,
  DEFAULT_OPTIONS,
  validateOptions,
} from "./admission-writer.validate-options.js";

/**
 * The admission writer (design D9): an in-memory pending set in front of a
 * group-committing Postgres writer. `admit` registers a tx and answers at
 * once; one collector, persister and completion fiber per shard write it.
 * A tx leaves the pending set when its row commits, when its own row cannot
 * be written (poisoned), or when a graceful stop gives up on it.
 *
 * `persistBatch` is the durable statement; focused lifecycle tests inject a
 * controlled one.
 */
export const makeAdmissionWriterWithOptions = <R>(
  persistBatch: AdmissionBatchPersistence<R>,
  options: AdmissionWriterOptions = DEFAULT_OPTIONS,
  testHooks: AdmissionWriterTestHooks = {},
): Effect.Effect<AdmissionWriterService, never, R | Scope.Scope> =>
  Effect.gen(function* () {
    validateOptions(options);
    const stallMs = options.stallMs ?? ADMISSION_WRITE_STALL_MS;
    const shutdownFlushMs =
      options.shutdownFlushMs ?? ADMISSION_WRITE_SHUTDOWN_FLUSH_MS;
    const lanes = Array.from({ length: options.shardCount }, (_, lane) => lane);
    const inputQueues = yield* Effect.forEach(lanes, () =>
      Queue.unbounded<AdmissionWriteItem>(),
    );
    const preparedQueues = yield* Effect.forEach(lanes, () =>
      Queue.unbounded<readonly AdmissionWriteItem[]>(),
    );
    const completionQueues = yield* Effect.forEach(lanes, () =>
      Queue.unbounded<AdmissionCompletion>(),
    );
    // Group commit: a persister offers one token each time it is free, and
    // its collector waits for that token before taking what is queued. The
    // batch therefore holds everything that arrived during the previous
    // write, with no timer.
    const persisterFree = yield* Effect.forEach(lanes, () =>
      Queue.bounded<void>(1),
    );
    const telemetry = makeWriterTelemetry(options.shardCount);
    const recent = makeRecentTxIds(options.recentTxIdCapacity);
    // The cached first-seen time is never later than the row's terminal
    // mark, so a younger entry's row cannot have been pruned yet.
    const recentMaxAgeMs = options.recentTxIdMaxAgeMs ?? 0;

    // Effect fibers run synchronous sections atomically on one event loop, so
    // these plain mutable fields are the writer's exact state. The Map keeps
    // registration order: its first value is the oldest pending tx.
    let accepting = true;
    let nextSeq = 0;
    let capacityUsed = 0;
    let bytesUsed = 0;
    let poisoned = 0;
    const pending = new Map<string, AdmissionWriteItem>();
    // `flush` callers, each waiting until no tx registered before its call
    // is pending. Released synchronously as txs leave, with no clock.
    let flushWaiters: {
      readonly target: number;
      readonly done: Deferred.Deferred<void>;
    }[] = [];

    const settledBefore = (target: number): boolean => {
      const oldest = pending.values().next();
      return oldest.done === true || oldest.value.seq >= target;
    };

    const releaseFlushWaiters = (): void => {
      if (flushWaiters.length === 0) return;
      const waiting: typeof flushWaiters = [];
      for (const waiter of flushWaiters) {
        if (settledBefore(waiter.target)) {
          Deferred.unsafeDone(waiter.done, Exit.void);
        } else {
          waiting.push(waiter);
        }
      }
      flushWaiters = waiting;
    };

    const oldestPendingAgeMs = (nowMs: number): number => {
      const oldest = pending.values().next();
      return oldest.done === true
        ? 0
        : Math.max(0, nowMs - oldest.value.registeredAtMs);
    };

    const lookupAt = (
      txId: Buffer,
      txFullHash: Buffer,
      nowMs: number,
    ): AdmissionLookup => {
      const txIdHex = txId.toString("hex");
      const conflict = (): AdmissionLookup => ({
        _tag: "Conflict",
        error: new TxAdmissionsDB.TxAdmissionConflictError({
          txIdHex,
          message: `Refusing to admit transaction ${txIdHex}: tx_id already exists with different normalized bytes`,
        }),
      });
      const item = pending.get(txIdHex);
      if (item !== undefined) {
        if (!item.request.txFullHash.equals(txFullHash)) return conflict();
        item.requestCount += 1;
        item.lastSeenAtMs = nowMs;
        return {
          _tag: "Duplicate",
          ack: {
            kind: "duplicate",
            txIdHex,
            status: "queued",
            firstSeenAt: new Date(item.registeredAtMs),
            lastSeenAt: new Date(nowMs),
          },
        };
      }
      const written = recent.get(txId);
      if (
        written === undefined ||
        (recentMaxAgeMs > 0 &&
          nowMs - written.firstSeenAt.getTime() >= recentMaxAgeMs)
      ) {
        return { _tag: "Unknown" };
      }
      if (!written.txFullHash.equals(txFullHash)) return conflict();
      return {
        _tag: "Duplicate",
        ack: {
          kind: "duplicate",
          txIdHex,
          status: written.status,
          firstSeenAt: written.firstSeenAt,
          lastSeenAt: new Date(nowMs),
        },
      };
    };

    const takeOut = (item: AdmissionWriteItem): boolean => {
      if (item.completed) return false;
      telemetry.leave(item);
      item.completed = true;
      pending.delete(item.txIdHex);
      capacityUsed -= 1;
      bytesUsed -= item.request.txCanonicalCbor.length;
      releaseFlushWaiters();
      return true;
    };

    const completeItem = (
      item: AdmissionWriteItem,
      result: AdmissionItemResult,
    ): Effect.Effect<boolean> =>
      Effect.uninterruptible(
        Effect.gen(function* () {
          if (!(yield* Effect.sync(() => takeOut(item)))) return false;
          if (result._tag === "Poisoned") {
            yield* Effect.sync(() => {
              poisoned += 1;
            });
            yield* item.request.settle(false);
            return false;
          }
          const outcome = result.outcome;
          if (outcome._tag === "Conflict") {
            // The id is stored with other witnesses; that version stays.
            yield* Metric.increment(admissionWriteStoredVariantCounter);
            yield* item.request.settle(false);
            return true;
          }
          const entry = outcome.result.entry;
          // Only rows stored as native: the tx-queue processor deletes a
          // peer row whose inputs it has not seen yet, and the sender's next
          // offer must then reach the database again.
          if (entry.submit_source === "native") {
            yield* Effect.sync(() =>
              recent.remember(item.request.txId, {
                txFullHash: item.request.txFullHash,
                status: entry.status,
                // The first-seen time this node acknowledged, so a later
                // duplicate answers the same one the 202 carried.
                firstSeenAt:
                  outcome.result.kind === "new"
                    ? new Date(item.registeredAtMs)
                    : entry.first_seen_at,
              }),
            );
          }
          yield* item.request.settle(outcome.result.kind === "new");
          return true;
        }),
      );

    const completeBatch = ({ items, results }: AdmissionCompletion) =>
      Effect.gen(function* () {
        let wake: Effect.Effect<void> | undefined;
        for (const [index, item] of items.entries()) {
          const written = yield* completeItem(item, results[index]!);
          if (written && wake === undefined) wake = item.request.wakeOnCommit;
        }
        // One wake-up per committed batch: every request carries the same
        // tx-queue wake-up, which drains the whole queue.
        if (wake !== undefined) yield* wake;
      });

    const write = persistWithRetry(persistBatch, {
      retryBaseMs: options.retryBaseMs ?? ADMISSION_WRITE_RETRY_BASE_MS,
      retryMaxMs: options.retryMaxMs ?? ADMISSION_WRITE_RETRY_MAX_MS,
      attemptTimeoutMs:
        options.attemptTimeoutMs ?? ADMISSION_WRITE_ATTEMPT_TIMEOUT_MS,
      onWritten: (items, durationMs) =>
        telemetry.recordBatch(items[0]!.lane, items.length, durationMs),
    });

    const runCollector = (lane: number) =>
      Effect.gen(function* () {
        yield* Queue.take(persisterFree[lane]!);
        if (testHooks.beforeCollect !== undefined) {
          yield* testHooks.beforeCollect(lane);
        }
        const first = yield* Queue.take(inputQueues[lane]!);
        const rest = yield* Queue.takeUpTo(
          inputQueues[lane]!,
          options.batchMaxRows - 1,
        );
        const items = [first, ...Chunk.toReadonlyArray(rest)];
        yield* Effect.sync(() => telemetry.move(items, "collected"));
        yield* Queue.offer(preparedQueues[lane]!, items);
        yield* telemetry.report(capacityUsed, bytesUsed);
      });

    const runPersister = (lane: number) =>
      Effect.gen(function* () {
        if (testHooks.beforePersist !== undefined) {
          yield* testHooks.beforePersist(lane);
        }
        yield* Queue.offer(persisterFree[lane]!, undefined);
        const items = yield* Queue.take(preparedQueues[lane]!);
        yield* Effect.sync(() => telemetry.move(items, "inflight"));
        yield* telemetry.report(capacityUsed, bytesUsed);
        const results = yield* write(items);
        yield* Effect.sync(() => telemetry.move(items, "completing"));
        yield* Queue.offer(completionQueues[lane]!, { items, results });
        yield* telemetry.report(capacityUsed, bytesUsed);
      });

    const runCompletion = (lane: number) =>
      Effect.gen(function* () {
        if (testHooks.beforeComplete !== undefined) {
          yield* testHooks.beforeComplete(lane);
        }
        yield* completeBatch(yield* Queue.take(completionQueues[lane]!));
        yield* telemetry.report(capacityUsed, bytesUsed);
      });

    const flush = Effect.gen(function* () {
      const done = yield* Deferred.make<void>();
      const settled = yield* Effect.sync(() => {
        if (settledBefore(nextSeq)) return true;
        flushWaiters.push({ target: nextSeq, done });
        return false;
      });
      if (!settled) yield* Deferred.await(done);
    });

    const service: AdmissionWriterService = {
      lookup: (txId, txFullHash) => lookupAt(txId, txFullHash, Date.now()),
      admit: (request) =>
        Effect.gen(function* () {
          // One synchronous section: the duplicate check, the bounds and the
          // registration cannot interleave with another caller's.
          const decision = yield* Effect.sync(
            ():
              | {
                  readonly ack: AdmissionAck;
                  readonly item?: AdmissionWriteItem;
                }
              | { readonly refusal: AdmissionWriteRefusal } => {
              const nowMs = Date.now();
              if (!accepting) {
                return {
                  refusal: new AdmissionWriterShutdownError({
                    message: "Admission writer is shutting down",
                    mayHaveCommitted: false,
                  }),
                };
              }
              const known = lookupAt(request.txId, request.txFullHash, nowMs);
              if (known._tag === "Conflict") return { refusal: known.error };
              if (known._tag === "Duplicate") return { ack: known.ack };
              const oldestAgeMs = oldestPendingAgeMs(nowMs);
              if (oldestAgeMs > stallMs) {
                return {
                  refusal: new AdmissionWriterStalledError({
                    message:
                      "Admission writes are stalled; retry once the node is ready",
                    oldestPendingAgeMs: oldestAgeMs,
                    stallMs,
                  }),
                };
              }
              const bytes = request.txCanonicalCbor.length;
              if (capacityUsed >= options.queueCapacity) {
                return {
                  refusal: new AdmissionWriterFullError({
                    message: "Admission writer row bound is full",
                    unit: "rows",
                    used: capacityUsed,
                    capacity: options.queueCapacity,
                  }),
                };
              }
              if (bytesUsed + bytes > options.queueBytes) {
                return {
                  refusal: new AdmissionWriterFullError({
                    message: "Admission writer byte bound is full",
                    unit: "bytes",
                    used: bytesUsed,
                    capacity: options.queueBytes,
                  }),
                };
              }
              const spent = request.claimInputs?.();
              if (spent !== undefined) return { refusal: spent };
              const item: AdmissionWriteItem = {
                request,
                txIdHex: request.txId.toString("hex"),
                seq: nextSeq,
                lane: admissionWriterShardForTxId(
                  request.txId,
                  options.shardCount,
                ),
                registeredAtMs: nowMs,
                lastSeenAtMs: nowMs,
                requestCount: 1,
                phase: "registered",
                completed: false,
              };
              nextSeq += 1;
              pending.set(item.txIdHex, item);
              capacityUsed += 1;
              bytesUsed += bytes;
              telemetry.enter(item);
              return {
                item,
                ack: {
                  kind: "new",
                  txIdHex: item.txIdHex,
                  status: "queued",
                  firstSeenAt: new Date(nowMs),
                  lastSeenAt: new Date(nowMs),
                },
              };
            },
          );
          if ("refusal" in decision)
            return yield* Effect.fail(decision.refusal);
          if (decision.item !== undefined) {
            yield* Queue.offer(inputQueues[decision.item.lane]!, decision.item);
          }
          return decision.ack;
        }),
      pendingSince: (txId) => {
        const item = pending.get(txId.toString("hex"));
        return item === undefined ? undefined : new Date(item.registeredAtMs);
      },
      health: Effect.sync(() => {
        const ageMs = oldestPendingAgeMs(Date.now());
        return {
          accepting,
          pending: pending.size,
          oldestPendingAgeMs: ageMs,
          stallMs,
          stalled: ageMs > stallMs,
        };
      }),
      flush,
      stats: Effect.sync(() =>
        telemetry.stats({
          accepting,
          pending: pending.size,
          poisoned,
          recentTxIds: recent.size(),
          capacity: options.queueCapacity,
          capacityUsed,
          capacityBytes: options.queueBytes,
          bytesUsed,
        }),
      ),
    };

    yield* Effect.forEach(
      lanes,
      (lane) =>
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
    // Registered after the lane fibers, so this LIFO finalizer runs while
    // they are still writing: stop taking txs, give the pending set a bounded
    // time to flush, then name every tx still unwritten.
    yield* Effect.addFinalizer(() =>
      Effect.gen(function* () {
        yield* Effect.sync(() => {
          accepting = false;
        });
        // Finalizers run uninterruptibly: without `interruptible` the
        // timeout could not stop the wait, and a stop with Postgres stalled
        // would hang for good.
        yield* flush.pipe(
          Effect.interruptible,
          Effect.timeout(Duration.millis(shutdownFlushMs)),
          Effect.ignore,
        );
        const unwritten = yield* Effect.sync(() => [...pending.values()]);
        if (unwritten.length > 0) {
          yield* Metric.incrementBy(
            admissionWriteLostCounter,
            unwritten.length,
          );
          yield* Effect.logError(
            `Admission writer stopped with ${unwritten.length.toString()} acknowledged txs unwritten.`,
          ).pipe(
            Effect.annotateLogs({
              unwrittenTxIds: unwritten.map((item) => item.txIdHex),
            }),
          );
        }
        for (const item of unwritten) {
          const mayHaveCommitted =
            item.phase === "inflight" || item.phase === "completing";
          if (yield* Effect.sync(() => takeOut(item))) {
            // A row that may have committed keeps its backlog slot until the
            // next live count.
            yield* item.request.settle(mayHaveCommitted);
          }
        }
        const shutdown = { concurrency: "unbounded", discard: true } as const;
        yield* Effect.forEach(inputQueues, Queue.shutdown, shutdown);
        yield* Effect.forEach(persisterFree, Queue.shutdown, shutdown);
        yield* Effect.forEach(preparedQueues, Queue.shutdown, shutdown);
        yield* Effect.forEach(completionQueues, Queue.shutdown, shutdown);
        yield* telemetry.report(capacityUsed, bytesUsed);
      }),
    );
    return service;
  });
