import { Database } from "@/services/database.js";
import * as Tx from "@/database/utils/tx.js";
import {
  clearTable,
  sqlErrorToDatabaseError,
  DatabaseError,
  retrieveNumberOfEntries,
} from "@/database/utils/common.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import * as MempoolTxDeltasDB from "./mempoolTxDeltas.js";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import * as AddressHistoryDB from "@/database/addressHistory.js";
import { ProcessedTx } from "@/utils.js";
import * as Ledger from "@/database/utils/ledger.js";

export const tableName = "mempool";

const toTxDelta = (
  processedTx: ProcessedTx,
): MempoolTxDeltasDB.TxDelta => ({
  txId: processedTx.txId,
  spent: processedTx.spent.map((outRef) => Buffer.from(outRef)),
  produced: processedTx.produced.map((entry) => ({
    [Ledger.Columns.OUTREF]: Buffer.from(entry[Ledger.Columns.OUTREF]),
    [Ledger.Columns.OUTPUT]: Buffer.from(entry[Ledger.Columns.OUTPUT]),
  })),
});

export const insert = (
  processedTx: ProcessedTx,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const { txId, txCbor, spent, produced } = processedTx;
    // Insert the tx itself in `MempoolDB`.
    yield* Tx.insertEntry(tableName, {
      tx_id: txId,
      tx: txCbor,
    });
    // Insert produced UTxOs in `MempoolLedgerDB`.
    yield* MempoolLedgerDB.insert(produced);
    yield* MempoolTxDeltasDB.upsertMany([toTxDelta(processedTx)]);
    // Remove spent inputs from MempoolLedgerDB.
    yield* MempoolLedgerDB.clearUTxOs(spent);
    // Add handled addresses to the lookup table
    yield* AddressHistoryDB.insert(spent, produced);
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.tapError((e) =>
      Effect.logError(`${tableName} db: insert: ${JSON.stringify(e)}`),
    ),
  );

export const insertMultiple = (
  processedTxs: ProcessedTx[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const txEntries = processedTxs.map((v) => ({
      tx_id: v.txId,
      tx: v.txCbor,
    }));
    // Insert the tx itself in `MempoolDB`.
    yield* Tx.insertEntries(tableName, txEntries);

    const initAcc: { allProduced: Ledger.Entry[]; allSpent: Buffer[] } = {
      allProduced: [],
      allSpent: [],
    };
    const { allProduced, allSpent } = processedTxs.reduce((acc, v) => {
      acc.allProduced.push(...v.produced);
      acc.allSpent.push(...v.spent);
      return acc;
    }, initAcc);

    // Insert produced UTxOs in `MempoolLedgerDB`.
    yield* MempoolLedgerDB.insert(allProduced);
    yield* MempoolTxDeltasDB.upsertMany(processedTxs.map(toTxDelta));
    // Remove spent inputs from MempoolLedgerDB.
    yield* MempoolLedgerDB.clearUTxOs(allSpent);
    // Update AddressHistoryDB
    yield* AddressHistoryDB.insert(allSpent, allProduced);
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.tapError((e) => Effect.logError(`${tableName} db: insert: ${e}`)),
  );

export const retrieveTxCborByHash = (txHash: Buffer) =>
  Tx.retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  Tx.retrieveValues(tableName, txHashes);

export const retrieve: Effect.Effect<
  readonly Tx.EntryWithTimeStamp[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<Tx.EntryWithTimeStamp>`SELECT ${sql(
    Tx.Columns.TX_ID,
  )}, ${sql(Tx.Columns.TX)}, ${sql(Tx.Columns.TIMESTAMPTZ)} FROM ${sql(tableName)} ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} DESC LIMIT 100000`;
}).pipe(
  Effect.withLogSpan(`retrieve ${tableName}`),
  Effect.tapErrorTag("SqlError", (e) =>
    Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
  ),
  sqlErrorToDatabaseError(tableName, "Failed to retrieve given transactions"),
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

export const clear: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    yield* clearTable(tableName);
    yield* MempoolTxDeltasDB.clear;
  });
