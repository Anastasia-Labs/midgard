import { Effect } from "effect";
import * as Ledger from "@/database/utils/ledger.js";
import { clearTable } from "@/database/utils/common.js";

import { Database } from "@/services/database.js";

export const tableName = "mempool_ledger";

export const insert = (entries: Ledger.Entry[]) =>
  Ledger.insertEntries(tableName, entries);

export const retrieve = (): Effect.Effect<
  readonly Ledger.Entry[],
  Error,
  Database
> => Ledger.retrieveEntries(tableName);

export const retrieveByAddress = (
  address: string,
): Effect.Effect<readonly Ledger.Entry[], Error, Database> =>
  Ledger.retrieveEntriesWithAddress(tableName, address);

export const clearUTxOs = (refs: Buffer[]) =>
  Ledger.delEntries(tableName, refs);

export const clear = () => clearTable(tableName);
