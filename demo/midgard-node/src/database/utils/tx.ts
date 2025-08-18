import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";
import { mapSqlError } from "@/database/utils/common.js";

export enum Columns {
  TX_ID = "tx_id",
  TX = "tx",
  TIMESTAMPTZ = "time_stamp_tz",
}

export type EntryNoTimeStamp = {
  [Columns.TX_ID]: Buffer;
  [Columns.TX]: Buffer;
};

export type EntryWithTimeStamp = EntryNoTimeStamp & {
  [Columns.TIMESTAMPTZ]: Date;
};

export type Entry = EntryNoTimeStamp | EntryWithTimeStamp;

export const createTable = (
  tableName: string,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.TX_ID)} BYTEA NOT NULL,
      ${sql(Columns.TX)} BYTEA NOT NULL,
      ${sql(Columns.TIMESTAMPTZ)} TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
      PRIMARY KEY (${sql(Columns.TX_ID)})
    );`;
  }).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const delMultiple = (
  tableName: string,
  tx_id: Buffer[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(
      `${tableName} db: attempt to delete multiply entries`,
    );
    const result = yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.TX_ID,
    )} IN ${sql.in(tx_id)} RETURNING ${sql(Columns.TX_ID)}`;
    yield* Effect.logDebug(`${tableName} db: deleted ${result.length} rows`);
  }).pipe(Effect.withLogSpan(`delMutiple table ${tableName}`), mapSqlError);

export const retrieveValue = (
  tableName: string,
  tx_id: Buffer,
): Effect.Effect<Buffer, Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve value`);

    const result = yield* sql<Buffer>`SELECT ${sql(Columns.TX)} FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.TX_ID)} = ${tx_id}`;

    if (result.length <= 0) {
      yield* Effect.fail(
        new SqlError.SqlError({ cause: `No value found for tx_id ${tx_id}` }),
      );
    }

    return result[0];
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
  tx_ids: Buffer[] | readonly Buffer[],
): Effect.Effect<readonly Buffer[], Error, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve values`);

    const result = yield* sql<Buffer>`SELECT ${sql(Columns.TX)} FROM ${sql(
      tableName,
    )} WHERE ${sql.in(Columns.TX_ID, tx_ids)}`;

    return result;
  }).pipe(
    Effect.withLogSpan(`retrieve values ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving values error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const insertEntry = (
  tableName: string,
  txPair: Entry,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insertTX`);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(
      txPair,
    )} ON CONFLICT (${sql(Columns.TX_ID)}) DO UPDATE SET ${sql(Columns.TX)} = ${txPair.tx}`;
  }).pipe(
    Effect.withLogSpan(`insertTX ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertTX: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const insertEntries = (
  tableName: string,
  pairs: Entry[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insertTXs`);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(pairs)}`;
  }).pipe(
    Effect.withLogSpan(`insertTXs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertTXs: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const retrieveEntries = (
  tableName: string,
): Effect.Effect<readonly EntryWithTimeStamp[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithTimeStamp>`SELECT * FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
    ),
    mapSqlError,
  );

export const retrieveNumberOfEntries = (
  tableName: string,
): Effect.Effect<number, Error, Database> =>
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
    mapSqlError,
  );
