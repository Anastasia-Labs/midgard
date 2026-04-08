import * as Ledger from "@/database/utils/ledger.js";
import { clearTable } from "@/database/utils/common.js";

export const tableName = "latest_ledger";

/**
 * Inserts multiple latest-ledger entries.
 */
export const insertMultiple = (entries: Ledger.Entry[]) =>
  Ledger.insertEntries(tableName, entries);

export const retrieve = Ledger.retrieveAllEntries(tableName);

/**
 * Deletes latest-ledger entries for the supplied UTxO references.
 */
export const clearUTxOs = (refs: Buffer[]) =>
  Ledger.delEntries(tableName, refs);

export const clear = clearTable(tableName);
