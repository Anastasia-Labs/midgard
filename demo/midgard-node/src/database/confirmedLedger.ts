import * as Ledger from "@/database/utils/ledger.js";
import { clearTable } from "@/database/utils/common.js";

export const tableName = "confirmed_ledger";

/**
 * Inserts multiple confirmed-ledger entries.
 */
export const insertMultiple = (entries: Ledger.Entry[]) =>
  Ledger.insertEntries(tableName, entries);

export const retrieve = Ledger.retrieveAllEntries(tableName);

/**
 * Deletes confirmed-ledger entries for the supplied UTxO references.
 */
export const clearUTxOs = (refs: Buffer[]) =>
  Ledger.delEntries(tableName, refs);

export const clear = clearTable(tableName);
