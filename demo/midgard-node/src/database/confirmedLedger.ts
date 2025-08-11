import * as LedgerUtils from "@/database/utils/ledger.js";
import { clearTable } from "@/database/utils/common.js"

export const tableName = "confirmed_ledger";

export const insertMultiple = (entries: LedgerUtils.Entry[]) =>
  LedgerUtils.insertEntries(tableName, entries);

export const retrieve = () => LedgerUtils.retrieveEntries(tableName);

export const clearUTxOs = (refs: Buffer[]) => LedgerUtils.delEntries(tableName, refs);

export const clear = () => clearTable(tableName);
