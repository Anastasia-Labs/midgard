import { Database } from "@/services/database.js";
import * as Tx from "@/database/utils/tx.js";
import {
  clearTable,
  sqlErrorToDatabaseError,
  DatabaseError,
  retrieveNumberOfEntries,
} from "@/database/utils/common.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import { ProcessedTx } from "@/utils.js";

export const tableName = "mempool";

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
    // Remove spent inputs from MempoolLedgerDB.
    yield* MempoolLedgerDB.clearUTxOs(spent);
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

    const allProduced = processedTxs.flatMap((v) => v.produced);
    const allSpent = processedTxs.flatMap((v) => v.spent);

    // Insert produced UTxOs in `MempoolLedgerDB`.
    yield* MempoolLedgerDB.insert(allProduced);
    // Remove spent inputs from MempoolLedgerDB.
    yield* MempoolLedgerDB.clearUTxOs(allSpent);
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
  )}, ${sql(Tx.Columns.TX)} FROM ${sql(tableName)} ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} DESC LIMIT 100000`; // Add ordering by time
}).pipe(
  Effect.withLogSpan(`retrieve ${tableName}`),
  Effect.tapErrorTag("SqlError", (e) =>
    Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
  ),
  sqlErrorToDatabaseError(tableName, "Failed to retrieve given transactions"),
);

export const retrieveTxCount: Effect.Effect<number, DatabaseError, Database> =
  retrieveNumberOfEntries(tableName);

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.delMultiple(tableName, txHashes);

export const clear = clearTable(tableName);
