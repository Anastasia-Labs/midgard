import { Effect } from "effect";
import { Database } from "@/services/database.js";
import { Data } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import * as SDK from "@al-ft/midgard-sdk";

export const retrieveNumberOfEntries = (
  tableName: string,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to get number of entries`);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      count: number;
    }>`SELECT COUNT(*) FROM ${sql(tableName)}`;
    return rows[0].count ?? 0;
  }).pipe(
    Effect.withLogSpan(`retrieveNumberOfEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveNumberOfEntries: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve row count"),
  );

export const clearTable = (
  tableName: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to clear table`);
    const sql = yield* SqlClient.SqlClient;

    yield* sql`TRUNCATE TABLE ${sql(tableName)} CASCADE`;

    yield* Effect.logInfo(`${tableName} db: Successfully cleared table`);
  }).pipe(
    Effect.withLogSpan(`clear ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: clearing error: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed at truncating table"),
  );

export class DatabaseError extends Data.TaggedError("DatabaseError")<
  SDK.Utils.GenericErrorFields & { readonly table: string }
> {}

type SqlErrorToDatabaseError = <A, R>(
  error: Effect.Effect<A, SqlError.SqlError, R>,
) => Effect.Effect<A, DatabaseError, R>;

export const sqlErrorToDatabaseError = (
  tableName: string,
  message: string,
): SqlErrorToDatabaseError =>
  Effect.mapError(
    (error: SqlError.SqlError) =>
      new DatabaseError({
        message,
        table: tableName,
        cause: error,
      }),
  );
