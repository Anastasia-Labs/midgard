import {
  clearTable,
  insertKeyValues,
  retrieveKeyValues,
  delMultiple,
} from "./utils.js";

export const tableName = "latest_ledger";

export const insert = (utxosCBOR: { key: Uint8Array; value: Uint8Array }[]) =>
  insertKeyValues(tableName, utxosCBOR);

export const retrieve = () => retrieveKeyValues(tableName);

export const clearUTxOs = (refs: Uint8Array[]) => delMultiple(tableName, refs);

export const clear = () => clearTable(tableName);
