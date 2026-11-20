import { SqlClient } from "@effect/sql";
import {
  Chunk,
  Clock,
  Context,
  Duration,
  Effect,
  Layer,
  Metric,
  Queue,
  Ref,
} from "effect";

import * as AddressHistoryDB from "@/database/addressHistory.js";
import * as MempoolTxDeltasDB from "@/database/mempoolTxDeltas.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { NodeConfig } from "@/services/config.js";
import { BatchSql } from "@/services/database.js";

export type WriteBehindItem =
  | {
      readonly kind: "tx_deltas";
      readonly deltas: readonly MempoolTxDeltasDB.TxDelta[];
    }
  | {
      readonly kind: "address_history";
      readonly entries: readonly AddressHistoryDB.Entry[];
    };

export type WriteBehindDepths = {
  readonly queueDepth: number;
  readonly pendingDepth: number;
  readonly totalDepth: number;
};

export type WriteBehindService = {
  readonly enqueueTxDeltas: (
    deltas: readonly MempoolTxDeltasDB.TxDelta[],
  ) => Effect.Effect<void, DatabaseError>;
  readonly enqueueAddressHistory: (
    entries: readonly AddressHistoryDB.Entry[],
  ) => Effect.Effect<void, DatabaseError>;
  readonly flushNow: Effect.Effect<void, DatabaseError>;
  readonly depths: Effect.Effect<WriteBehindDepths>;
  readonly run: Effect.Effect<void>;
};

export class WriteBehind extends Context.Tag("WriteBehind")<
  WriteBehind,
  WriteBehindService
>() {}

const writeBehindQueueDepthGauge = Metric.gauge("write_behind_queue_depth", {
  description: "Queued and pending write-behind rows",
});

export const writeBehindFlushDurationTimer = Metric.timer(
  "write_behind_flush_duration",
  "Duration of write-behind database flushes",
);

export const writeBehindTransactionDurationTimer = Metric.timer(
  "write_behind_transaction_duration",
  "Total duration of successful write-behind transactions including commit",
);

export const writeBehindFlushCounter = Metric.counter(
  "write_behind_flush_total",
  {
    description: "Successful write-behind database flushes",
    bigint: true,
    incremental: true,
  },
);

export const writeBehindFlushRowsCounter = Metric.counter(
  "write_behind_flush_rows_total",
  {
    description: "Rows persisted by the write-behind writer",
    bigint: true,
    incremental: true,
  },
);

export const writeBehindInlineFallbackCounter = Metric.counter(
  "write_behind_inline_fallback_total",
  {
    description:
      "Write-behind enqueue attempts that activated synchronous overflow persistence",
    bigint: true,
    incremental: true,
  },
);

const mempoolPersistDeltasDurationTimer = Metric.timer(
  "mempool_persist_deltas_duration",
  "Duration of deferred mempool tx delta upserts",
);

const mempoolPersistAddressHistoryDurationTimer = Metric.timer(
  "mempool_persist_address_history_duration",
  "Duration of deferred address-history persistence",
);

type DurationMetricSnapshot = {
  readonly count: number;
  readonly sum: number;
};

type BigIntCounterSnapshot = {
  readonly count: bigint;
};

export type WriteBehindTelemetrySnapshot = {
  readonly flushDuration: DurationMetricSnapshot;
  readonly transactionDuration: DurationMetricSnapshot;
  readonly txDeltaPreparationDuration: DurationMetricSnapshot;
  readonly deltaSqlDuration: DurationMetricSnapshot;
  readonly addressSqlDuration: DurationMetricSnapshot;
  readonly flushes: BigIntCounterSnapshot;
  readonly rows: BigIntCounterSnapshot;
  readonly inlineFallbacks: BigIntCounterSnapshot;
};

export type WriteBehindTelemetryReport = {
  readonly writeBehindFlushMs: number;
  readonly writeBehindFlushCount: number;
  readonly writeBehindFlushRows: number;
  readonly writeBehindTxDeltaPreparationCborMs: number;
  readonly writeBehindDeltaSqlMs: number;
  readonly writeBehindAddressSqlMs: number;
  readonly writeBehindTransactionMs: number;
  readonly writeBehindTransactionOverheadMs: number;
  readonly writeBehindInlineFallbackCount: number;
};

export const readWriteBehindTelemetry: Effect.Effect<WriteBehindTelemetrySnapshot> =
  Effect.gen(function* () {
    const [
      flushDuration,
      transactionDuration,
      txDeltaPreparationDuration,
      deltaSqlDuration,
      addressSqlDuration,
      flushes,
      rows,
      inlineFallbacks,
    ] = yield* Effect.all([
      Metric.value(writeBehindFlushDurationTimer),
      Metric.value(writeBehindTransactionDurationTimer),
      Metric.value(MempoolTxDeltasDB.mempoolTxDeltasPreparationDurationTimer),
      Metric.value(MempoolTxDeltasDB.mempoolTxDeltasSqlDurationTimer),
      Metric.value(AddressHistoryDB.addressHistoryInsertSqlDurationTimer),
      Metric.value(writeBehindFlushCounter),
      Metric.value(writeBehindFlushRowsCounter),
      Metric.value(writeBehindInlineFallbackCounter),
    ]);
    return {
      flushDuration,
      transactionDuration,
      txDeltaPreparationDuration,
      deltaSqlDuration,
      addressSqlDuration,
      flushes,
      rows,
      inlineFallbacks,
    };
  });

const durationMetricDelta = (
  before: DurationMetricSnapshot,
  after: DurationMetricSnapshot,
): number => Math.max(0, after.sum - before.sum);

const counterMetricDelta = (
  before: BigIntCounterSnapshot,
  after: BigIntCounterSnapshot,
): number => Math.max(0, Number(after.count - before.count));

export const summarizeWriteBehindTelemetry = (
  before: WriteBehindTelemetrySnapshot,
  after: WriteBehindTelemetrySnapshot,
): WriteBehindTelemetryReport => {
  const writeBehindTxDeltaPreparationCborMs = durationMetricDelta(
    before.txDeltaPreparationDuration,
    after.txDeltaPreparationDuration,
  );
  const writeBehindDeltaSqlMs = durationMetricDelta(
    before.deltaSqlDuration,
    after.deltaSqlDuration,
  );
  const writeBehindAddressSqlMs = durationMetricDelta(
    before.addressSqlDuration,
    after.addressSqlDuration,
  );
  const writeBehindTransactionMs = durationMetricDelta(
    before.transactionDuration,
    after.transactionDuration,
  );
  return {
    writeBehindFlushMs: durationMetricDelta(
      before.flushDuration,
      after.flushDuration,
    ),
    writeBehindFlushCount: counterMetricDelta(before.flushes, after.flushes),
    writeBehindFlushRows: counterMetricDelta(before.rows, after.rows),
    writeBehindTxDeltaPreparationCborMs,
    writeBehindDeltaSqlMs,
    writeBehindAddressSqlMs,
    writeBehindTransactionMs,
    writeBehindTransactionOverheadMs: Math.max(
      0,
      writeBehindTransactionMs -
        writeBehindTxDeltaPreparationCborMs -
        writeBehindDeltaSqlMs -
        writeBehindAddressSqlMs,
    ),
    writeBehindInlineFallbackCount: counterMetricDelta(
      before.inlineFallbacks,
      after.inlineFallbacks,
    ),
  };
};

export const recordWriteBehindTransactionTelemetry = (
  rowCount: number,
  durationMs: number,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    yield* writeBehindTransactionDurationTimer(
      Effect.succeed(Duration.millis(durationMs)),
    );
    yield* Metric.increment(writeBehindFlushCounter);
    yield* Metric.incrementBy(writeBehindFlushRowsCounter, BigInt(rowCount));
  });

const itemRowCount = (item: WriteBehindItem): number =>
  item.kind === "tx_deltas" ? item.deltas.length : item.entries.length;

const sliceItem = (
  item: WriteBehindItem,
  start: number,
  end?: number,
): WriteBehindItem =>
  item.kind === "tx_deltas"
    ? { kind: "tx_deltas", deltas: item.deltas.slice(start, end) }
    : { kind: "address_history", entries: item.entries.slice(start, end) };

const chunkItem = (
  item: WriteBehindItem,
  maxRows: number,
): readonly WriteBehindItem[] => {
  const chunks: WriteBehindItem[] = [];
  for (let offset = 0; offset < itemRowCount(item); offset += maxRows) {
    chunks.push(sliceItem(item, offset, offset + maxRows));
  }
  return chunks;
};

export const takeWriteBehindRowBatch = (
  items: readonly WriteBehindItem[],
  maxRows: number,
): {
  readonly batch: readonly WriteBehindItem[];
  readonly remaining: readonly WriteBehindItem[];
} => {
  const batch: WriteBehindItem[] = [];
  let remainingRows = Math.max(1, maxRows);
  for (let index = 0; index < items.length; index += 1) {
    const item = items[index]!;
    const rows = itemRowCount(item);
    if (rows <= remainingRows) {
      batch.push(item);
      remainingRows -= rows;
      if (remainingRows === 0) {
        return { batch, remaining: items.slice(index + 1) };
      }
      continue;
    }
    batch.push(sliceItem(item, 0, remainingRows));
    return {
      batch,
      remaining: [sliceItem(item, remainingRows), ...items.slice(index + 1)],
    };
  }
  return { batch, remaining: [] };
};

/** Selects up to the row cap for each target table in one transaction. */
export const takeWriteBehindProjectionBatch = (
  items: readonly WriteBehindItem[],
  maxRowsPerProjection: number,
): {
  readonly batch: readonly WriteBehindItem[];
  readonly remaining: readonly WriteBehindItem[];
} => {
  let deltaCapacity = Math.max(1, maxRowsPerProjection);
  let addressCapacity = Math.max(1, maxRowsPerProjection);
  const batch: WriteBehindItem[] = [];
  const remaining: WriteBehindItem[] = [];
  for (const item of items) {
    const capacity =
      item.kind === "tx_deltas" ? deltaCapacity : addressCapacity;
    if (capacity === 0) {
      remaining.push(item);
      continue;
    }
    const rows = itemRowCount(item);
    const selectedRows = Math.min(rows, capacity);
    batch.push(sliceItem(item, 0, selectedRows));
    if (selectedRows < rows) {
      remaining.push(sliceItem(item, selectedRows));
    }
    if (item.kind === "tx_deltas") {
      deltaCapacity -= selectedRows;
    } else {
      addressCapacity -= selectedRows;
    }
  }
  return { batch, remaining };
};

/**
 * Keeps queue-overflow rows owned by the producer until their inline write
 * succeeds. The iterative retry is deliberate: returning the persistence
 * error would strand derived rows after the authoritative accept transaction
 * has already committed, while recursive retries could grow the JS stack.
 */
export const persistWriteBehindInlineOverflowWithRetry = <E, R>(
  persist: Effect.Effect<void, E, R>,
  retryDelayMs: number,
): Effect.Effect<void, never, R> =>
  Effect.gen(function* () {
    const delayMs = Math.max(1, Math.floor(retryDelayMs));
    let attempt = 0;
    while (true) {
      const result = yield* Effect.either(persist);
      if (result._tag === "Right") return;
      attempt += 1;
      if (attempt === 1 || attempt % 10 === 0) {
        yield* Effect.logWarning(
          `Write-behind inline overflow persistence failed; retaining producer backpressure and retrying (attempt=${attempt.toString()}): ${String(result.left)}`,
        );
      }
      yield* Effect.sleep(Duration.millis(delayMs));
    }
  });

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

export const writeBehindFiber: Effect.Effect<void, never, WriteBehind> =
  Effect.gen(function* () {
    const service = yield* WriteBehind;
    yield* Effect.logInfo("📝 Write-behind writer fiber started.");
    yield* service.run;
  });
