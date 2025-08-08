import { Database } from "@/services/database.js";
import * as TxUtils from "@/database/utils/tx.js";
import * as LedgerUtils from "@/database/utils/ledger.js";
import * as Common from "@/database/utils/common.js"

import * as MempoolLedgerDB from "./mempoolLedger.js";
import { Effect } from "effect";
import { fromHex } from "@lucid-evolution/lucid";
import { SqlClient } from "@effect/sql";
import { breakDownTx } from "../utils.js";

export const tableName = "mempool";

export const insert = (
  txString: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const txCborBytes = fromHex(txString);
    const { txId, txCbor, spent, produced } = yield* breakDownTx(txCborBytes);
    // Insert the tx itself in `MempoolDB`.
    yield* TxUtils.insertEntry(tableName, {
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

export const retrieveTxCborByHash = (txHash: Buffer) =>
  TxUtils.retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  TxUtils.retrieveValues(tableName, txHashes);

export const retrieve = (): Effect.Effect<readonly TxUtils.Entry[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<TxUtils.Entry>`SELECT ${sql(
      TxUtils.Columns.TX_ID,
    )}, ${sql(TxUtils.Columns.TX)} FROM ${sql(tableName)} LIMIT 100000`;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
    ),
    Common.mapSqlError,
  );

export const retrieveTxCount = () => TxUtils.retrieveNumberOfEntries(tableName);

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, Error, Database> => TxUtils.delMultiple(tableName, txHashes);

export const clear = () => Common.clearTable(tableName);
