import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";

export const mkKeyValueCreateQuery = (
  tableName: string,
  keyColumnName: string,
  valueColumnName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(keyColumnName)} TEXT NOT NULL,
      ${sql(valueColumnName)} TEXT NOT NULL,
      PRIMARY KEY (${sql(keyColumnName)})
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const delMultiple = (
  tableName: string,
  keyColumnName: string,
  keys: string[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(
      `${tableName} db: attempt to delete multiply entries`,
    );
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql.in(keyColumnName, keys)}`;
  }).pipe(Effect.withLogSpan(`delMutiple table ${tableName}`), mapSqlError);

export const clearTable = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
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
    mapSqlError,
  );

export const mapSqlError = Effect.mapError(
  (sqlError: SqlError.SqlError) =>
    new Error(`SQL Error (${sqlError._tag}): ${JSON.stringify(sqlError)}`),
);
