import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect, Option } from "effect";

export const mkKeyValueCreateQuery = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      key BYTEA NOT NULL,
      value BYTEA NOT NULL,
      PRIMARY KEY (key)
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const delMultiple = (
  tableName: string,
  keys: Uint8Array[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(
      `${tableName} db: attempt to delete multiply entries`,
    );
    yield* sql`DELETE FROM ${sql(tableName)} WHERE key IN ${sql.in(keys)}`;
  }).pipe(Effect.withLogSpan(`delMutiple table ${tableName}`), mapSqlError);

export const retrieveValue = (
  tableName: string,
  key: Uint8Array,
): Effect.Effect<Option.Option<Uint8Array>, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve value`);

    const result = yield* sql<Uint8Array[]>`SELECT value FROM ${sql(
      tableName,
    )} WHERE key = ${Buffer.from(key)} LIMIT 1 `;
    return Option.fromNullable(result[0]?.[0]);
  }).pipe(
    Effect.withLogSpan(`retrieve value ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving value error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveValues = (
  tableName: string,
  keys: Uint8Array[],
): Effect.Effect<Uint8Array[], Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve values`);

    const result = yield* sql`SELECT value FROM ${sql(
      tableName,
    )} WHERE ${sql.in("key", keys.map(Buffer.from))}`;
    return result.map((row: any) => Uint8Array.from(row.value));
  }).pipe(
    Effect.withLogSpan(`retrieve values ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving values error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

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

export const insertKeyValue = (
  tableName: string,
  key: Uint8Array,
  value: Uint8Array,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert keyValue`);
    const sql = yield* SqlClient.SqlClient;
    const valueBuffer: Buffer = Buffer.from(value);
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert({
      key: Buffer.from(key),
      value: valueBuffer,
    })} ON CONFLICT (key) DO UPDATE SET value = ${valueBuffer}`;
  }).pipe(
    Effect.withLogSpan(`insert keyValue ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insert keyValue: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const insertKeyValues = (
  tableName: string,
  values: { key: Uint8Array; value: Uint8Array }[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert keyValues`);
    const sql = yield* SqlClient.SqlClient;
    const pairs = values.map((kv) => ({
      key: Buffer.from(kv.key),
      value: Buffer.from(kv.value),
    }));
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(pairs)}`;
  }).pipe(
    Effect.withLogSpan(`insert keyValues ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insert keyValues: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveKeyValues = (
  tableName: string,
): Effect.Effect<
  {
    key: Uint8Array;
    value: Uint8Array;
  }[],
  Error,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql`SELECT * FROM ${sql(tableName)}`;
    return rows.map((row: unknown) => {
      const { key, value } = row as {
        key: Buffer;
        value: Buffer;
      };
      return {
        key: new Uint8Array(key.buffer, key.byteOffset, key.byteLength),
        value: new Uint8Array(value.buffer, value.byteOffset, value.byteLength),
      };
    });
  }).pipe(
    Effect.withLogSpan(`insert keyValues ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: insert keyValues: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const mapSqlError = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, Exclude<E, SqlError.SqlError> | Error, R> =>
  effect.pipe(
    Effect.catchAll(
      (e): Effect.Effect<A, Exclude<E, SqlError.SqlError> | Error, R> => {
        if (e instanceof SqlError.SqlError) {
          return Effect.fail(
            new Error(`SQL Error (${e._tag}): ${JSON.stringify(e)}`),
          );
        } else return Effect.fail(e as Exclude<E, SqlError.SqlError>);
      },
    ),
  );
