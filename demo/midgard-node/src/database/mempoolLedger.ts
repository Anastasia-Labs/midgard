import { Effect } from "effect";
import {
  clearTable,
  insertKeyValues,
  retrieveKeyValues,
  delMultiple,
} from "./utils.js";
import { Database } from "@/services/database.js";

export const tableName = "mempool_ledger";

export const insert = (utxosCBOR: { key: Uint8Array; value: Uint8Array }[]) =>
  insertKeyValues(tableName, utxosCBOR);

export const retrieve = (): Effect.Effect<
  { key: Uint8Array; value: Uint8Array }[],
  Error,
  Database
> => retrieveKeyValues(tableName);

export const clearUTxOs = (refs: Uint8Array[]) => delMultiple(tableName, refs);

export const clear = () => clearTable(tableName);
