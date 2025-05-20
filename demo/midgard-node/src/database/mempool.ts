import {
  clearTable,
  insertKeyValue,
  delMultiple,
  retrieveValue,
  retrieveValues,
  retrieveKeyValues,
} from "./utils.js";

export const tableName = "mempool";

export const insert = (txHash: Uint8Array, txCbor: Uint8Array) =>
  insertKeyValue(tableName, txHash, txCbor);

export const retrieveTxCborByHash = (txHash: Uint8Array) =>
  retrieveValue(tableName, txHash);

export const retrieveTxCborsByHashes = (txHashes: Uint8Array[]) =>
  retrieveValues(tableName, txHashes);

export const retrieve = () => retrieveKeyValues(tableName);

export const clearTxs = (txHashes: Uint8Array[]) =>
  delMultiple(tableName, txHashes);

export const clear = () => clearTable(tableName);
