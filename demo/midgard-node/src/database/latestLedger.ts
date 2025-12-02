import * as Ledger from "@/database/utils/ledger.js";
import { clearTable } from "@/database/utils/common.js";

export const tableName = "latest_ledger";

export const insertMultiple = (entries: Ledger.Entry[]) =>
  Ledger.insertEntries(tableName, entries);

export const retrieve = Ledger.retrieveAllEntries(tableName);

export const clearUTxOs = (refs: Buffer[]) =>
  Ledger.delEntries(tableName, refs);

export const clear = clearTable(tableName);
