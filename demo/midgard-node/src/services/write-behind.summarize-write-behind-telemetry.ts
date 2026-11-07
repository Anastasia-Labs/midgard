import { Context, Duration, Effect, Metric } from "effect";

import * as AddressHistoryDB from "../database/addressHistory.js";
import * as MempoolTxDeltasDB from "../database/mempoolTxDeltas.js";
import { DatabaseError } from "../database/utils/common.js";

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

export const writeBehindQueueDepthGauge = Metric.gauge(
  "write_behind_queue_depth",
  {
    description: "Queued and pending write-behind rows",
  },
);

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

export const mempoolPersistDeltasDurationTimer = Metric.timer(
  "mempool_persist_deltas_duration",
  "Duration of deferred mempool tx delta upserts",
);

export const mempoolPersistAddressHistoryDurationTimer = Metric.timer(
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

export const itemRowCount = (item: WriteBehindItem): number =>
  item.kind === "tx_deltas" ? item.deltas.length : item.entries.length;

export const sliceItem = (
  item: WriteBehindItem,
  start: number,
  end?: number,
): WriteBehindItem =>
  item.kind === "tx_deltas"
    ? { kind: "tx_deltas", deltas: item.deltas.slice(start, end) }
    : { kind: "address_history", entries: item.entries.slice(start, end) };

export const chunkItem = (
  item: WriteBehindItem,
  maxRows: number,
): readonly WriteBehindItem[] => {
  const chunks: WriteBehindItem[] = [];
  for (let offset = 0; offset < itemRowCount(item); offset += maxRows) {
    chunks.push(sliceItem(item, offset, offset + maxRows));
  }
  return chunks;
};
