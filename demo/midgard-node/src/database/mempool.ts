import { SqlClient } from "@effect/sql";
import { Duration, Effect, Metric } from "effect";

import type * as AddressHistoryDB from "@/database/addressHistory.js";
import {
  clearTable,
  DatabaseError,
  logDatabaseError,
  retrieveNumberOfEntries,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import * as Tx from "@/database/utils/tx.js";
import { Database } from "@/services/database.js";
import { WriteBehind } from "@/services/write-behind.js";
import { ProcessedTx } from "@/utils.js";

import * as DepositsDB from "./deposits.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import * as MempoolTxDeltasDB from "./mempoolTxDeltas.js";

export const tableName = "mempool";

const mempoolPersistTxRowsDurationTimer = Metric.timer(
  "mempool_persist_tx_rows_duration",
  "Duration of accepted transaction row inserts into mempool",
);

const mempoolPersistProducedDurationTimer = Metric.timer(
  "mempool_persist_produced_duration",
  "Duration of accepted produced UTxO inserts into mempool_ledger",
);

const mempoolPersistSpentDurationTimer = Metric.timer(
  "mempool_persist_spent_duration",
  "Duration of accepted spent-input deletion from mempool_ledger",
);

const mempoolRetrievePageDurationTimer = Metric.timer(
  "mempool_retrieve_page_duration",
  "Duration of oldest-first mempool page retrieval",
);

const mempoolRetrievePageRowsGauge = Metric.gauge(
  "mempool_retrieve_page_rows",
  {
    description: "Rows returned by the latest mempool page retrieval",
    bigint: true,
  },
);

export const toTxDelta = (
  processedTx: ProcessedTx,
): MempoolTxDeltasDB.TxDelta => ({
  txId: processedTx.txId,
  spent: processedTx.spent.map((outRef) => Buffer.from(outRef)),
  produced: processedTx.produced.map((entry) => ({
    [Ledger.Columns.OUTREF]: Buffer.from(entry[Ledger.Columns.OUTREF]),
    [Ledger.Columns.OUTPUT]: Buffer.from(entry[Ledger.Columns.OUTPUT]),
  })),
});

export const toAddressHistoryEntries = (
  processedTxs: readonly ProcessedTx[],
): readonly AddressHistoryDB.Entry[] => {
  const unique = new Map<string, AddressHistoryDB.Entry>();
  for (const processedTx of processedTxs) {
    for (const entry of processedTx.produced) {
      const address = entry[Ledger.Columns.ADDRESS];
      unique.set(`${processedTx.txId.toString("hex")}:${address}`, {
        [Ledger.Columns.TX_ID]: processedTx.txId,
        [Ledger.Columns.ADDRESS]: address,
      });
    }
  }
  return [...unique.values()];
};

export const compactLedgerEffects = (
  processedTxs: readonly ProcessedTx[],
): {
  readonly produced: readonly Ledger.Entry[];
  readonly spent: readonly Buffer[];
} => {
  const produced = processedTxs.flatMap((tx) => tx.produced);
  const producedOutRefs = new Set(
    produced.map((entry) => entry[Ledger.Columns.OUTREF].toString("hex")),
  );
  const spentByOutRef = new Map<string, Buffer>();
  for (const tx of processedTxs) {
    for (const spent of tx.spent) {
      const outRefHex = spent.toString("hex");
      if (!producedOutRefs.has(outRefHex)) {
        spentByOutRef.set(outRefHex, spent);
      }
    }
  }
  const spentOutRefs = new Set(
    processedTxs.flatMap((tx) =>
      tx.spent.map((spent) => spent.toString("hex")),
    ),
  );
  return {
    produced: produced.filter(
      (entry) =>
        !spentOutRefs.has(entry[Ledger.Columns.OUTREF].toString("hex")),
    ),
    spent: [...spentByOutRef.values()],
  };
};

export const enqueueAcceptedWriteBehind = (
  processedTxs: readonly ProcessedTx[],
): Effect.Effect<void, DatabaseError, WriteBehind> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const writeBehind = yield* WriteBehind;
    yield* writeBehind.enqueueTxDeltas(processedTxs.map(toTxDelta));
    yield* writeBehind.enqueueAddressHistory(
      toAddressHistoryEntries(processedTxs),
    );
  });

export const insertMultipleCore = (
  processedTxs: readonly ProcessedTx[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const txEntries = processedTxs.map((v) => ({
      tx_id: v.txId,
      tx: v.txCbor,
    }));
    const txRowsStartedAt = Date.now();
    yield* Tx.insertEntries(tableName, txEntries);
    yield* mempoolPersistTxRowsDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - txRowsStartedAt)),
    );

    yield* applyLedgerEffectsCore(processedTxs);
  });

export const applyLedgerEffectsCore = (
  processedTxs: readonly ProcessedTx[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }

    // Phase B may accept dependency chains in one batch. Persist only the net
    // ledger transition: intermediate outputs do not need to be inserted and
    // immediately deleted inside the same transaction.
    const { produced, spent } = compactLedgerEffects(processedTxs);

    const producedStartedAt = Date.now();
    yield* MempoolLedgerDB.insert(produced);
    yield* mempoolPersistProducedDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - producedStartedAt)),
    );

    const spentStartedAt = Date.now();
    const consumedDepositEventIds = yield* MempoolLedgerDB.clearUTxOs(spent);
    yield* DepositsDB.markConsumedByEventIds(consumedDepositEventIds);
    yield* mempoolPersistSpentDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - spentStartedAt)),
    );
  });

export const insertMultiple = (
  processedTxs: readonly ProcessedTx[],
): Effect.Effect<void, DatabaseError, Database | WriteBehind> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(insertMultipleCore(processedTxs));
    yield* enqueueAcceptedWriteBehind(processedTxs);
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.tapError((e) => logDatabaseError(tableName, "insert", e)),
    sqlErrorToDatabaseError(tableName, "Failed to insert mempool transactions"),
  );

export const insert = (
  processedTx: ProcessedTx,
): Effect.Effect<void, DatabaseError, Database | WriteBehind> =>
  insertMultiple([processedTx]);

/**
 * Retrieves mempool transaction CBOR by transaction hash.
 */
export const retrieveTxCborByHash = (txHash: Buffer) =>
  Tx.retrieveValue(tableName, txHash);

/**
 * Retrieves mempool transaction CBOR blobs for a batch of hashes.
 */
export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => Tx.retrieveValues(tableName, txHashes);

export type MempoolCursor = {
  readonly timeStampTz: Date;
  readonly txId: Buffer;
};

export type MempoolPage = {
  readonly entries: readonly Tx.EntryWithTimeStamp[];
  readonly nextCursor: MempoolCursor | null;
};

export const retrievePage = ({
  after,
  limit,
  upTo,
}: {
  readonly after?: MempoolCursor;
  readonly limit: number;
  readonly upTo?: Date;
}): Effect.Effect<MempoolPage, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve page`);
    const startedAt = Date.now();
    const sql = yield* SqlClient.SqlClient;
    const pageLimit = Math.max(1, Math.floor(limit));
    const afterTime = after?.timeStampTz ?? null;
    const afterTxId = after?.txId ?? null;
    const upperTime = upTo ?? null;
    const rows = yield* sql<Tx.EntryWithTimeStamp>`
      SELECT
        ${sql(Tx.Columns.TX_ID)},
        ${sql(Tx.Columns.TX)},
        ${sql(Tx.Columns.TIMESTAMPTZ)}
      FROM ${sql(tableName)}
      WHERE ((${afterTime}::timestamptz IS NULL)
        OR (${sql(Tx.Columns.TIMESTAMPTZ)}, ${sql(Tx.Columns.TX_ID)}) >
           (${afterTime}::timestamptz, ${afterTxId}::bytea))
        AND (${upperTime}::timestamptz IS NULL
        OR ${sql(Tx.Columns.TIMESTAMPTZ)} <= ${upperTime}::timestamptz)
      ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} ASC, ${sql(Tx.Columns.TX_ID)} ASC
      LIMIT ${pageLimit}`;
    const entries: readonly Tx.EntryWithTimeStamp[] = rows;
    yield* mempoolRetrievePageDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - startedAt)),
    );
    yield* mempoolRetrievePageRowsGauge(Effect.succeed(BigInt(entries.length)));
    const last = entries.at(-1);
    return {
      entries,
      nextCursor:
        entries.length === pageLimit && last !== undefined
          ? {
              timeStampTz: last[Tx.Columns.TIMESTAMPTZ],
              txId: last[Tx.Columns.TX_ID],
            }
          : null,
    };
  }).pipe(
    Effect.withLogSpan(`retrievePage ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      logDatabaseError(tableName, "retrievePage", e),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve mempool page"),
  );

export const retrieveTxCount: Effect.Effect<bigint, DatabaseError, Database> =
  retrieveNumberOfEntries(tableName);

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Tx.delMultiple(tableName, txHashes);
    yield* MempoolTxDeltasDB.clearTxs(txHashes);
  });

export const clear: Effect.Effect<void, DatabaseError, Database> = Effect.gen(
  function* () {
    yield* clearTable(tableName);
    yield* MempoolTxDeltasDB.clear;
  },
);
