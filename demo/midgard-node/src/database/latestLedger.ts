import * as LedgerUtils from "@/database/utils/ledger.js";
import * as Common from "@/database/utils/common.js"

export const tableName = "latest_ledger";

export const insertMultiple = (entries: LedgerUtils.Entry[]) =>
  LedgerUtils.insertEntries(tableName, entries);

export const retrieve = () => LedgerUtils.retrieveEntries(tableName);

export const clearUTxOs = (refs: Buffer[]) => LedgerUtils.delEntries(tableName, refs);

export const clear = () => Common.clearTable(tableName);
