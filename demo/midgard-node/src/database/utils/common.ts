import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient, SqlError } from "@effect/sql";
import { Data, Effect } from "effect";

import { Database } from "@/services/database.js";

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
    return BigInt(rows[0].count);
  }).pipe(
    Effect.withLogSpan(`retrieveNumberOfEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      logDatabaseError(tableName, "retrieveNumberOfEntries", e),
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
      logDatabaseError(tableName, "truncate error", e),
    ),
    sqlErrorToDatabaseError(tableName, "Failed at truncating table"),
  );

/**
 * Midgard-specific wrapper around lower-level SQL failures.
 */
export class DatabaseError extends Data.TaggedError("DatabaseError")<
  SDK.GenericErrorFields & { readonly table: string }
> {}

const collectDatabaseErrorCodes = (error: unknown): readonly string[] => {
  const codes: string[] = [];
  let current = error;
  while (typeof current === "object" && current !== null) {
    if ("code" in current) {
      const code = (current as { readonly code?: unknown }).code;
      if (typeof code === "string" || typeof code === "number") {
        const normalized = String(code).slice(0, 128);
        if (normalized.length > 0 && !codes.includes(normalized)) {
          codes.push(normalized);
        }
      }
    }
    current = (current as { readonly cause?: unknown }).cause;
  }
  return codes;
};

export const formatDatabaseError = (error: unknown): string => {
  const message = formatUnknownError(error, { includeCause: true });
  const codes = collectDatabaseErrorCodes(error);
  return codes.length === 0 ? message : `${message}; codes=${codes.join(",")}`;
};

export const logDatabaseError = (
  tableName: string,
  operation: string,
  error: unknown,
): Effect.Effect<void> =>
  Effect.logError(
    `${tableName} db: ${operation}: ${formatDatabaseError(error)}`,
  );

type SqlErrorToDatabaseError = <A, E, R>(
  error: Effect.Effect<A, E, R>,
) => Effect.Effect<A, Exclude<E, SqlError.SqlError> | DatabaseError, R>;

/**
 * Promotes raw `SqlError`s into `DatabaseError`s annotated with the logical
 * table name, while leaving already-normalized database errors untouched.
 */
export const sqlErrorToDatabaseError =
  (tableName: string, message: string): SqlErrorToDatabaseError =>
  <A, E, R>(effect: Effect.Effect<A, E, R>) =>
    Effect.mapError(effect, (error: E) =>
      error instanceof SqlError.SqlError
        ? new DatabaseError({
            message,
            table: tableName,
            cause: error,
          })
        : (error as Exclude<E, SqlError.SqlError>),
    );
