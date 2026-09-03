import { Effect } from "effect";

import { Database } from "../services/database.js";
import { clearTable, DatabaseError } from "./utils/common.js";
import * as Tx from "./utils/tx.js";

export const tableName = "processed_mempool";

export const insertTx = (
  tx: Tx.Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.insertEntry(tableName, tx);

export const insertTxs = (
  txs: Tx.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.insertEntries(tableName, txs);

export const retrieve = Tx.retrieveAllEntries(tableName);

/**
 * Retrieves processed-mempool transaction CBOR by transaction hash.
 */
export const retrieveTxCborByHash = (txHash: Buffer) =>
  Tx.retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => Tx.retrieveValues(tableName, txHashes);

export const clearTxs = (txHashes: Buffer[]) =>
  Tx.delMultiple(tableName, txHashes);

export const clear = clearTable(tableName);
