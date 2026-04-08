import { Data, Effect } from "effect";
import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import * as SDK from "@al-ft/midgard-sdk";

/**
 * Shared building blocks for the Midgard node's SQL table adapters.
 *
 * The rest of the database utility modules stay deliberately thin and reuse
 * these helpers so they surface a consistent tagged error type and logging
 * shape no matter which table fails underneath.
 */
export const retrieveNumberOfEntries = (
  tableName: string,
): Effect.Effect<bigint, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to get number of entries`);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      // sql treats COUNT(*) as a `string`, regardless of any type annotations.
      count: string;
    }>`SELECT COUNT(*) FROM ${sql(tableName)}`;
    return BigInt(rows[0].count) ?? 0;
  }).pipe(
    Effect.withLogSpan(`retrieveNumberOfEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveNumberOfEntries: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve row count"),
  );

/**
 * Removes every row from a table while preserving the table definition and
 * letting PostgreSQL cascade to dependent relations.
 */
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
      Effect.logError(`${tableName} db: truncate error: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed at truncating table"),
  );

/**
 * Midgard-specific wrapper around lower-level SQL failures.
 */
export class DatabaseError extends Data.TaggedError("DatabaseError")<
  SDK.GenericErrorFields & { readonly table: string }
> {}

type SqlErrorToDatabaseError = <A, R>(
  error: Effect.Effect<A, SqlError.SqlError | DatabaseError, R>,
) => Effect.Effect<A, DatabaseError, R>;

/**
 * Promotes raw `SqlError`s into `DatabaseError`s annotated with the logical
 * table name, while leaving already-normalized database errors untouched.
 */
export const sqlErrorToDatabaseError = (
  tableName: string,
  message: string,
): SqlErrorToDatabaseError =>
  Effect.mapError((error: SqlError.SqlError | DatabaseError) =>
    error._tag === "SqlError"
      ? new DatabaseError({
          message,
          table: tableName,
          cause: error,
        })
      : error,
  );
