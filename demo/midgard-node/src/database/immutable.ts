import { Effect } from "effect";
import {
  clearTable,
  insertKeyValue,
  insertKeyValues,
  retrieveKeyValues,
  retrieveValue,
  retrieveValues,
} from "./utils.js";
import { Database } from "@/services/database.js";

export const tableName = "immutable";

export const insert = (
  txHash: Uint8Array,
  txCbor: Uint8Array,
): Effect.Effect<void, Error, Database> =>
  insertKeyValue(tableName, txHash, txCbor);

export const insertTxs = (
  txs: { key: Uint8Array; value: Uint8Array }[],
): Effect.Effect<void, Error, Database> => insertKeyValues(tableName, txs);

export const retrieve = () => retrieveKeyValues(tableName);

export const retrieveTxCborByHash = (txHash: Uint8Array) =>
  retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (txHashes: Uint8Array[]) =>
  retrieveValues(tableName, txHashes);

export const clear = () => clearTable(tableName);
