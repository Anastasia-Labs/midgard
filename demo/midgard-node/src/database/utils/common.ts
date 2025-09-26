import { Effect } from "effect";
import { Database } from "@/services/database.js";
import { Data } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import { GenericErrorFields } from "@/utils.js";

export const retrieveNumberOfEntries = (
  tableName: string,
): Effect.Effect<bigint, DBSelectError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to get number of entries`);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      // sql threats COUNT(*) as a string, even if with number type in that field
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
      Effect.logError(`${tableName} db: truncate error: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDBTruncateError(tableName),
  );

type DBErrorFields = GenericErrorFields & {
  readonly table?: string;
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
)<DBErrorFields> {}

function makeSqlErrorMapper<
  DbE extends new (fields: DBErrorFields) => any,
  Es = never,
>(ErrorClass: DbE, action: string, tableName: string) {
  return <A, R = SqlClient.SqlClient>(
    eff: Effect.Effect<A, SqlError.SqlError | Es, R>,
  ): Effect.Effect<
    A,
    Exclude<SqlError.SqlError | Es, SqlError.SqlError> | InstanceType<DbE>,
    R
  > =>
    Effect.catchTag(eff, "SqlError", (error) =>
      Effect.fail(
        new ErrorClass({
          message: `Failed to ${action} table ${tableName}`,
          table: tableName,
          cause: error,
        }),
      ),
    );
}

export const sqlErrorToDBSelectError =
  <Es = never>(tableName: string) =>
  <A, R = SqlClient.SqlClient>(
    self: Effect.Effect<A, SqlError.SqlError | Es, R>,
  ) =>
    makeSqlErrorMapper<typeof DBSelectError, Es>(
      DBSelectError,
      "select from",
      tableName,
    )(self);

export const sqlErrorToDBInsertError =
  <Es = never>(tableName: string) =>
  <A, R = SqlClient.SqlClient>(
    self: Effect.Effect<A, SqlError.SqlError | Es, R>,
  ) =>
    makeSqlErrorMapper<typeof DBInsertError, Es>(
      DBInsertError,
      "insert into",
      tableName,
    )(self);

export const sqlErrorToDBUpdateError =
  <Es = never>(tableName: string) =>
  <A, R = SqlClient.SqlClient>(
    self: Effect.Effect<A, SqlError.SqlError | Es, R>,
  ) =>
    makeSqlErrorMapper<typeof DBUpdateError, Es>(
      DBUpdateError,
      "update",
      tableName,
    )(self);

export const sqlErrorToDBDeleteError =
  <Es = never>(tableName: string) =>
  <A, R = SqlClient.SqlClient>(
    self: Effect.Effect<A, SqlError.SqlError | Es, R>,
  ) =>
    makeSqlErrorMapper<typeof DBDeleteError, Es>(
      DBDeleteError,
      "delete from",
      tableName,
    )(self);

export const sqlErrorToDBTruncateError =
  <Es = never>(tableName: string) =>
  <A, R = SqlClient.SqlClient>(
    self: Effect.Effect<A, SqlError.SqlError | Es, R>,
  ) =>
    makeSqlErrorMapper<typeof DBTruncateError, Es>(
      DBTruncateError,
      "truncate",
      tableName,
    )(self);

export const sqlErrorToDBCreateError =
  <Es = never>(tableName: string) =>
  <A, R = SqlClient.SqlClient>(
    self: Effect.Effect<A, SqlError.SqlError | Es, R>,
  ) =>
    makeSqlErrorMapper<typeof DBCreateError, Es>(
      DBCreateError,
      "create",
      tableName,
    )(self);

export const sqlErrorToDBOtherError =
  <Es = never>(tableName: string) =>
  <A, R = SqlClient.SqlClient>(
    self: Effect.Effect<A, SqlError.SqlError | Es, R>,
  ) =>
    makeSqlErrorMapper<typeof DBOtherError, Es>(
      DBOtherError,
      "other operation",
      tableName,
    )(self);
