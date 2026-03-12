import { Database } from "@/services/database.js";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import * as SDK from "@al-ft/midgard-sdk";
import {
  clearTable,
  sqlErrorToDatabaseError,
  DatabaseError,
  retrieveNumberOfEntries,
} from "@/database/utils/common.js";
import { ProcessedTx } from "@/utils.js";
import { AddressHistoryDB, MempoolLedgerDB, Tx } from "./index.js";

export const tableName = "mempool";

/**
 * Along with insertions to MempoolDB, applies transactions to MempoolLedgerDB,
 * updating it. Also adds corresponding entries to AddressHistoryDB.
 */
export const insertMultiple = (
  processedTxs: ProcessedTx[],
): Effect.Effect<
  void,
  SDK.CmlDeserializationError | SDK.DataCoercionError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }

    const {
      allTxEntries,
      addressHistoryEntries,
      collectiveProduced,
      collectiveSpent,
    } = yield* AddressHistoryDB.aggregateProcessedTxs(
      MempoolLedgerDB.tableName,
      processedTxs,
      AddressHistoryDB.Status.SLATED,
    );

    // TODO: Batching might be needed.
    yield* Effect.all(
      [
        // Insert the transactions themselves in `MempoolDB`.
        Tx.insertEntries(tableName, allTxEntries),
        // Insert transactions corresponding entries to `AddressHistoryDB`.
        AddressHistoryDB.insertEntries(addressHistoryEntries),
        // Insertion to `MempoolLedgerDB` followed by removal of spent outrefs in
        // sequence.
        Effect.all([
          MempoolLedgerDB.insert(collectiveProduced),
          MempoolLedgerDB.clearUTxOs(collectiveSpent),
        ]),
      ],
      { concurrency: "unbounded" },
    );
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

export const retrieveTimeBoundEntries = (
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly Tx.Entry[], DatabaseError, Database> =>
  Tx.retrieveTimeBoundEntries(tableName, startTime, endTime);

export const retrieveTxCount: Effect.Effect<bigint, DatabaseError, Database> =
  retrieveNumberOfEntries(tableName);

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.delMultiple(tableName, txHashes);

export const clear = clearTable(tableName);
