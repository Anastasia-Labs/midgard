import { Effect } from "effect";
import * as Ledger from "@/database/utils/ledger.js";
import {
  clearTable,
  DatabaseError,
  NotFoundError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

export const tableName = "mempool_ledger";

export const insert = (entries: Ledger.Entry[]) =>
  Ledger.upsertEntries(tableName, entries);

export const retrieve: Effect.Effect<
  readonly Ledger.Entry[],
  DatabaseError,
  Database
> = Ledger.retrieveAllEntries(tableName);

export const retrieveByAddress = (
  address: string,
): Effect.Effect<readonly Ledger.Entry[], DatabaseError, Database> =>
  Ledger.retrieveEntriesWithAddress(tableName, address);

export const retrieveByOutRefs = (
  outRefs: Buffer[] | readonly Buffer[],
): Effect.Effect<readonly Ledger.Entry[], DatabaseError, Database> =>
  Ledger.retrieveByOutRefs(tableName, outRefs);

retrieve;

export const retrieveByOutRef = (
  spentOutRef: Buffer,
): Effect.Effect<Ledger.Entry, DatabaseError | NotFoundError, Database> =>
  Ledger.retrieveByOutRef(tableName, spentOutRef);

export const clearUTxOs = (refs: Buffer[]) =>
  Ledger.delEntries(tableName, refs);

export const clear = clearTable(tableName);
