import { SqlClient } from "@effect/sql";
import {
  Chunk,
  Clock,
  Duration,
  Effect,
  Layer,
  Metric,
  Queue,
  Ref,
} from "effect";

import * as AddressHistoryDB from "../database/addressHistory.js";
import * as MempoolTxDeltasDB from "../database/mempoolTxDeltas.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import { NodeConfig } from "./config.js";
import { BatchSql } from "./database.js";
import {
  chunkItem,
  itemRowCount,
  mempoolPersistAddressHistoryDurationTimer,
  mempoolPersistDeltasDurationTimer,
  recordWriteBehindTransactionTelemetry,
  sliceItem,
  WriteBehind,
  type WriteBehindDepths,
  writeBehindFlushDurationTimer,
  writeBehindInlineFallbackCounter,
  type WriteBehindItem,
  writeBehindQueueDepthGauge,
  type WriteBehindService,
} from "./write-behind.summarize-write-behind-telemetry.js";
import {
  persistWriteBehindInlineOverflowWithRetry,
  takeWriteBehindProjectionBatch,
} from "./write-behind.take-write-behind-projection-batch.js";

export const makeWriteBehind = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const batchSql = yield* BatchSql;
  // Queue capacity is enforced in rows through reservedRows. The queue itself
  // stores row batches so a 4k accepted batch is O(chunks), not O(transactions).
  const queue = yield* Queue.unbounded<WriteBehindItem>();
  const wakeup = yield* Queue.dropping<void>(1);
  const pending = yield* Ref.make<readonly WriteBehindItem[]>([]);
  const queuedRows = yield* Ref.make(0);
  const reservedRows = yield* Ref.make(0);
  const reservationLock = yield* Effect.makeSemaphore(1);
  const consumerLock = yield* Effect.makeSemaphore(1);
  const writeLock = yield* Effect.makeSemaphore(1);

  const depths: Effect.Effect<WriteBehindDepths> = Effect.gen(function* () {
    const queueDepth = yield* Ref.get(queuedRows);
    const pendingDepth = (yield* Ref.get(pending)).reduce(
      (sum, item) => sum + itemRowCount(item),
      0,
    );
    return {
      queueDepth,
      pendingDepth,
      totalDepth: yield* Ref.get(reservedRows),
    };
  });

  const reportDepth = Effect.gen(function* () {
    const current = yield* depths;
    yield* writeBehindQueueDepthGauge(Effect.succeed(current.totalDepth));
  });

  const persistItems = (
    items: readonly WriteBehindItem[],
  ): Effect.Effect<void, DatabaseError> =>
    writeLock
      .withPermits(1)(
        Effect.gen(function* () {
          const transactionStartedAt = performance.now();
          const rowCount = yield* batchSql.withTransaction(
            Effect.gen(function* () {
              const deltas = items.flatMap((item) =>
                item.kind === "tx_deltas" ? item.deltas : [],
              );
              const addressEntries = items.flatMap((item) =>
                item.kind === "address_history" ? item.entries : [],
              );
              const currentRowCount = deltas.length + addressEntries.length;
              if (currentRowCount === 0) {
                return 0;
              }

              // These are reconstructable derived projections: tx deltas have a
              // decode fallback and address history is idempotent. Avoid making
              // their deferred flush wait for its own WAL sync; the authoritative
              // accepted admission/mempool/ledger transaction remains synchronous.
              yield* batchSql`SET LOCAL synchronous_commit = off`;
              const flushStartedAt = Date.now();
              if (deltas.length > 0) {
                const startedAt = Date.now();
                yield* MempoolTxDeltasDB.upsertMany(deltas);
                yield* mempoolPersistDeltasDurationTimer(
                  Effect.succeed(Duration.millis(Date.now() - startedAt)),
                );
              }
              if (addressEntries.length > 0) {
                const startedAt = Date.now();
                yield* AddressHistoryDB.insertEntries([...addressEntries]);
                yield* mempoolPersistAddressHistoryDurationTimer(
                  Effect.succeed(Duration.millis(Date.now() - startedAt)),
                );
              }
              yield* writeBehindFlushDurationTimer(
                Effect.succeed(Duration.millis(Date.now() - flushStartedAt)),
              );
              return currentRowCount;
            }).pipe(Effect.provideService(SqlClient.SqlClient, batchSql)),
          );
          if (rowCount === 0) {
            return;
          }
          yield* recordWriteBehindTransactionTelemetry(
            rowCount,
            performance.now() - transactionStartedAt,
          );
        }),
      )
      .pipe(
        sqlErrorToDatabaseError(
          "write_behind",
          "Failed to persist the write-behind batch atomically",
        ),
      );

  const enqueueItem = (
    item: WriteBehindItem,
  ): Effect.Effect<void, DatabaseError> =>
    Effect.gen(function* () {
      const requestedRows = itemRowCount(item);
      if (requestedRows === 0) {
        return;
      }
      const reservedCount = yield* reservationLock.withPermits(1)(
        Ref.modify(reservedRows, (current) => {
          const count = Math.min(
            requestedRows,
            Math.max(0, nodeConfig.WRITE_BEHIND_QUEUE_CAPACITY - current),
          );
          return [count, current + count] as const;
        }),
      );
      if (reservedCount > 0) {
        yield* Ref.update(queuedRows, (current) => current + reservedCount);
        const reservedItem = sliceItem(item, 0, reservedCount);
        for (const chunk of chunkItem(
          reservedItem,
          nodeConfig.WRITE_BEHIND_MAX_BATCH,
        )) {
          Queue.unsafeOffer(queue, chunk);
        }
        Queue.unsafeOffer(wakeup, undefined);
      }
      if (reservedCount < requestedRows) {
        yield* Metric.increment(writeBehindInlineFallbackCounter);
        const overflow = sliceItem(item, reservedCount);
        for (const chunk of chunkItem(
          overflow,
          nodeConfig.WRITE_BEHIND_MAX_BATCH,
        )) {
          yield* persistWriteBehindInlineOverflowWithRetry(
            persistItems([chunk]),
            nodeConfig.WRITE_BEHIND_FLUSH_INTERVAL_MS,
          );
        }
      }
      yield* reportDepth;
    });

  const takeFirstPendingBatch = Ref.modify(pending, (items) => {
    const { batch, remaining } = takeWriteBehindProjectionBatch(
      items,
      nodeConfig.WRITE_BEHIND_MAX_BATCH,
    );
    return [batch, remaining] as const;
  });

  const releaseReservedRows = (count: number): Effect.Effect<void> =>
    reservationLock.withPermits(1)(
      Ref.update(reservedRows, (current) => Math.max(0, current - count)),
    );

  const prependPending = (
    items: readonly WriteBehindItem[],
  ): Effect.Effect<void> =>
    Ref.update(pending, (current) => [...items, ...current]);

  const appendPending = (
    items: readonly WriteBehindItem[],
  ): Effect.Effect<void> =>
    Ref.update(pending, (current) => [...current, ...items]);

  const moveQueuedRowsToPending = (
    items: readonly WriteBehindItem[],
  ): Effect.Effect<void> =>
    Effect.gen(function* () {
      const rowCount = items.reduce((sum, item) => sum + itemRowCount(item), 0);
      if (rowCount === 0) return;
      yield* Ref.update(queuedRows, (current) =>
        Math.max(0, current - rowCount),
      );
      yield* appendPending(items);
    });

  const retainFailedBatch = (
    batch: readonly WriteBehindItem[],
  ): Effect.Effect<void> => prependPending(batch);

  const completePersistedBatch = (
    batch: readonly WriteBehindItem[],
  ): Effect.Effect<void> =>
    releaseReservedRows(
      batch.reduce((sum, item) => sum + itemRowCount(item), 0),
    );

  const queuedItems = (): Effect.Effect<readonly WriteBehindItem[]> =>
    Queue.takeAll(queue).pipe(Effect.map(Chunk.toReadonlyArray));

  const appendQueuedItemsToPending = Effect.gen(function* () {
    const queued = yield* queuedItems();
    yield* moveQueuedRowsToPending(queued);
  });

  const flushOnePendingBatch: Effect.Effect<void, DatabaseError> = Effect.gen(
    function* () {
      const batch = yield* takeFirstPendingBatch;
      if (batch.length === 0) {
        return;
      }
      const result = yield* Effect.either(persistItems(batch));
      if (result._tag === "Left") {
        yield* retainFailedBatch(batch);
        return yield* Effect.fail(result.left);
      }
      yield* completePersistedBatch(batch);
      yield* reportDepth;
    },
  );

  const flushNow: Effect.Effect<void, DatabaseError> = consumerLock.withPermits(
    1,
  )(
    Effect.gen(function* () {
      yield* appendQueuedItemsToPending;
      while ((yield* Ref.get(pending)).length > 0) {
        yield* flushOnePendingBatch;
        yield* appendQueuedItemsToPending;
      }
      yield* reportDepth;
    }),
  );

  const service: WriteBehindService = {
    enqueueTxDeltas: (deltas) => enqueueItem({ kind: "tx_deltas", deltas }),
    enqueueAddressHistory: (entries) =>
      enqueueItem({ kind: "address_history", entries }),
    flushNow,
    depths,
    run: Effect.gen(function* () {
      let nonEmptySinceMs = 0;
      while (true) {
        let currentDepth = yield* depths;
        if (currentDepth.totalDepth === 0) {
          nonEmptySinceMs = 0;
          yield* Queue.take(wakeup);
          currentDepth = yield* depths;
          if (currentDepth.totalDepth === 0) {
            continue;
          }
          nonEmptySinceMs = yield* Clock.currentTimeMillis;
        } else if (nonEmptySinceMs === 0) {
          nonEmptySinceMs = yield* Clock.currentTimeMillis;
        }

        const elapsedMs = (yield* Clock.currentTimeMillis) - nonEmptySinceMs;
        if (
          currentDepth.totalDepth >= nodeConfig.WRITE_BEHIND_MAX_BATCH ||
          elapsedMs >= nodeConfig.WRITE_BEHIND_FLUSH_INTERVAL_MS
        ) {
          yield* consumerLock
            .withPermits(1)(
              Effect.gen(function* () {
                yield* appendQueuedItemsToPending;
                yield* flushOnePendingBatch;
              }),
            )
            .pipe(
              Effect.catchAll((error) =>
                Effect.logWarning(
                  `Write-behind flush failed; retained rows will be retried: ${String(error)}`,
                ).pipe(
                  Effect.zipRight(
                    Effect.sleep(
                      Duration.millis(
                        nodeConfig.WRITE_BEHIND_FLUSH_INTERVAL_MS,
                      ),
                    ),
                  ),
                ),
              ),
            );
          nonEmptySinceMs =
            (yield* depths).totalDepth > 0 ? yield* Clock.currentTimeMillis : 0;
          continue;
        }

        yield* Effect.sleep(
          Duration.millis(
            Math.min(10, nodeConfig.WRITE_BEHIND_FLUSH_INTERVAL_MS - elapsedMs),
          ),
        );
      }
    }),
  };

  yield* Effect.addFinalizer(() =>
    service.flushNow.pipe(
      Effect.catchAllCause((cause) =>
        Effect.logError(
          `Write-behind shutdown flush failed with rows retained in memory: ${String(cause)}`,
        ),
      ),
    ),
  );
  return service;
});

export const WriteBehindLive = Layer.scoped(WriteBehind, makeWriteBehind);
