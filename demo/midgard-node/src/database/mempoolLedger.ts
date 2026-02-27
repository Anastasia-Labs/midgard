import { Effect } from "effect";
import * as Ledger from "@/database/utils/ledger.js";
import {
  clearTable,
  DatabaseError,
  NotFoundError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";

export const tableName = "mempool_ledger";

export const insert = (entries: Ledger.Entry[]) =>
  Ledger.insertEntries(tableName, entries);

export const retrieve: Effect.Effect<
  readonly Ledger.Entry[],
  DatabaseError,
  Database
> = Ledger.retrieveAllEntries(tableName);

export const retrieveByAddress = (
  address: string,
): Effect.Effect<readonly Ledger.Entry[], DatabaseError, Database> =>
  Ledger.retrieveEntriesWithAddress(tableName, address);

export const retrieveEntry = (
  spentOutref: Buffer,
): Effect.Effect<Ledger.Entry, DatabaseError | NotFoundError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const result = yield* sql<Ledger.EntryWithTimeStamp>`SELECT * FROM ${sql(
      tableName,
    )} WHERE ${sql(Ledger.Columns.OUTREF)} = ${spentOutref} LIMIT 1`;
    if (result.length <= 0) {
      return yield* new NotFoundError({
        message: `No mempool ledger entry found for outref ${spentOutref.toString("hex")}`,
        cause: "",
        table: tableName,
      });
    }
    return result[0];
  }).pipe(
    Effect.withLogSpan(`retrieveEntry ${tableName}`),
    Effect.mapError((error): DatabaseError | NotFoundError =>
      error._tag === "SqlError"
        ? new DatabaseError({
            message: "Failed to retrieve mempool ledger entry by outref",
            table: tableName,
            cause: error,
          })
        : error,
    ),
  );

export const clearUTxOs = (refs: Buffer[]) =>
  Ledger.delEntries(tableName, refs);

export const clear = clearTable(tableName);
