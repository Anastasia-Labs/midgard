import { Effect } from "effect";
import { Database } from "@/services/database.js";
import { Data } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import * as SDK from "@al-ft/midgard-sdk";

export const retrieveNumberOfEntries = (
  tableName: string,
): Effect.Effect<number, DBSelectError, Database> =>
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
    sqlErrorToDBSelectError(tableName),
  );

export const clearTable = (
  tableName: string,
): Effect.Effect<void, DBTruncateError, Database> =>
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
    sqlErrorToDBTruncateError(tableName),
  );

type DBErrorFields = SDK.Utils.GenericErrorFields & {
  readonly table: string;
};

export class DBSelectError extends Data.TaggedError(
  "DBSelectError",
)<DBErrorFields> {}
export class DBInsertError extends Data.TaggedError(
  "DBInsertError",
)<DBErrorFields> {}
export class DBUpdateError extends Data.TaggedError(
  "DBUpdateError",
)<DBErrorFields> {}
export class DBDeleteError extends Data.TaggedError(
  "DBDeleteError",
)<DBErrorFields> {}
export class DBTruncateError extends Data.TaggedError(
  "DBTruncateError",
)<DBErrorFields> {}
export class DBCreateError extends Data.TaggedError(
  "DBCreateError",
)<DBErrorFields> {}
export class DBOtherError extends Data.TaggedError(
  "DBOtherError",
)<SDK.Utils.GenericErrorFields> {}

type DBErrorConstructor<T> = new (args: DBErrorFields) => T;

type SqlErrorMapper<E> = <A, R>(
  self: Effect.Effect<A, SqlError.SqlError, R>,
) => Effect.Effect<A, E, R>;

/**
 * A helper for creating mapper functions which convert generic `SqlError`s to
 * any of our custom DB error types.
 */
const makeSqlErrorMapper = <E>(
  ErrorClass: DBErrorConstructor<E>,
  action: string,
): ((tableName: string) => SqlErrorMapper<E>) => {
  return (tableName: string) =>
    Effect.mapError(
      (error: SqlError.SqlError) =>
        new ErrorClass({
          message: `Failed to ${action}`,
          table: tableName,
          cause: error,
        }),
    );
};

export const sqlErrorToDBSelectError: (
  tableName: string,
) => SqlErrorMapper<DBSelectError> = makeSqlErrorMapper(
  DBSelectError,
  "select from table",
);

export const sqlErrorToDBInsertError: (
  tableName: string,
) => SqlErrorMapper<DBInsertError> = makeSqlErrorMapper(
  DBInsertError,
  "insert into table",
);

export const sqlErrorToDBUpdateError: (
  tableName: string,
) => SqlErrorMapper<DBUpdateError> = makeSqlErrorMapper(
  DBUpdateError,
  "update table",
);

export const sqlErrorToDBDeleteError: (
  tableName: string,
) => SqlErrorMapper<DBDeleteError> = makeSqlErrorMapper(
  DBDeleteError,
  "delete from table",
);

export const sqlErrorToDBTruncateError: (
  tableName: string,
) => SqlErrorMapper<DBTruncateError> = makeSqlErrorMapper(
  DBTruncateError,
  "truncate table",
);

export const sqlErrorToDBCreateError: (
  tableName: string,
) => SqlErrorMapper<DBCreateError> = makeSqlErrorMapper(
  DBCreateError,
  "create table",
);
