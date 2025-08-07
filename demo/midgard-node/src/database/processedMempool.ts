import { Effect } from "effect";
import {
  KVPair,
  clearTable,
  insertKeyValue,
  insertKeyValues,
  retrieveKeyValues,
  retrieveValue,
  retrieveValues,
} from "./utils.js";
import { Database } from "@/services/database.js";

export const tableName = "processed_mempool";

export const insert = (tx: KVPair): Effect.Effect<void, Error, Database> =>
  insertKeyValue(tableName, tx);

export const insertTxs = (
  txs: KVPair[],
): Effect.Effect<void, Error, Database> => insertKeyValues(tableName, txs);

export const retrieve = () => retrieveKeyValues(tableName);

export const retrieveTxCborByHash = (txHash: Buffer) =>
  retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => retrieveValues(tableName, txHashes);

export const clear = () => clearTable(tableName);
