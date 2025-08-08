import { Effect } from "effect";
import * as LedgerUtils from "@/database/utils/ledger.js";
import * as Common from "@/database/utils/common.js"

import { Database } from "@/services/database.js";

export const tableName = "mempool_ledger";

export const insert = (entries: LedgerUtils.Entry[]) =>
  LedgerUtils.insertEntries(tableName, entries);

export const retrieve = (): Effect.Effect<
  readonly LedgerUtils.Entry[],
  Error,
  Database
> => LedgerUtils.retrieveEntries(tableName);

export const retrieveByAddress = (
  address: string,
): Effect.Effect<readonly LedgerUtils.Entry[], Error, Database> =>
  LedgerUtils.retrieveEntriesWithAddress(tableName, address);

export const clearUTxOs = (refs: Buffer[]) => LedgerUtils.delEntries(tableName, refs);

export const clear = () => Common.clearTable(tableName);
