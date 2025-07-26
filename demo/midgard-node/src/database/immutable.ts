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
  txHash: Buffer,
  txCbor: Buffer,
): Effect.Effect<void, Error, Database> =>
  insertKeyValue(tableName, txHash, txCbor);

export const insertTxs = (
  txs: { key: Buffer; value: Buffer }[],
): Effect.Effect<void, Error, Database> => insertKeyValues(tableName, txs);

export const retrieve = () => retrieveKeyValues(tableName);

export const retrieveTxCborByHash = (txHash: Buffer) =>
  retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => retrieveValues(tableName, txHashes);

export const clear = () => clearTable(tableName);
