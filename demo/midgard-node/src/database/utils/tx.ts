import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";
import {
  DBSelectError,
  sqlErrorToDBCreateError,
  sqlErrorToDBSelectError,
  sqlErrorToDBInsertError,
  sqlErrorToDBDeleteError,
  DBDeleteError,
  DBCreateError,
  DBInsertError,
} from "@/database/utils/common.js";

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
): Effect.Effect<void, DBCreateError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.TX_ID)} BYTEA NOT NULL,
      ${sql(Columns.TX)} BYTEA NOT NULL,
      ${sql(Columns.TIMESTAMPTZ)} TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
      PRIMARY KEY (${sql(Columns.TX_ID)})
    );`;
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDBCreateError<never>(tableName),
  );

export const delMultiple = (
  tableName: string,
  tx_ids: Buffer[],
): Effect.Effect<void, DBDeleteError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(
      `${tableName} db: attempt to delete multiply entries`,
    );
    const result = yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.TX_ID,
    )} IN ${sql.in(tx_ids)} RETURNING ${sql(Columns.TX_ID)}`;
    yield* Effect.logDebug(`${tableName} db: deleted ${result.length} rows`);
  }).pipe(
    Effect.withLogSpan(`delMutiple table ${tableName}`),
    sqlErrorToDBDeleteError<never>(tableName),
  );

export const retrieveValue = (
  tableName: string,
  tx_id: Buffer,
): Effect.Effect<Buffer, DBSelectError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve value`);

    const result = yield* sql<{
      [Columns.TX]: Buffer;
    }>`SELECT ${sql(Columns.TX)} FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.TX_ID)} = ${tx_id}`;

    // We probably don't need this. SqlError should cover this already.
    // TODO
    // if (result.length <= 0) {
    //   yield*
    //     new DBSelectError({
    //       message: `No value found for tx_id ${tx_id.toString("hex")}`,
    //       table: tableName,
    //     }) ;
    // }

    if (result.length === 0)
      yield* new SqlError.SqlError({
        cause: `No value found for tx_id ${tx_id.toString("hex")}`,
        // table: tableName,
      });

    return result[0][Columns.TX];
  }).pipe(
    Effect.withLogSpan(`retrieve value ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving value error: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDBSelectError<never>(tableName),
  );

export const retrieveValues = (
  tableName: string,
  tx_ids: Buffer[] | readonly Buffer[],
): Effect.Effect<readonly Buffer[], DBSelectError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve values`);

    const rows = yield* sql<{
      [Columns.TX]: Buffer;
    }>`SELECT ${sql(Columns.TX)} FROM ${sql(
      tableName,
    )} WHERE ${sql.in(Columns.TX_ID, tx_ids)}`;

    return rows.map((r) => r[Columns.TX]);
  }).pipe(
    Effect.withLogSpan(`retrieve values ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving values error: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDBSelectError<never>(tableName),
  );

export const insertEntry = (
  tableName: string,
  txPair: Entry,
): Effect.Effect<void, DBInsertError, Database> =>
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
    sqlErrorToDBInsertError<never>(tableName),
  );

export const insertEntries = (
  tableName: string,
  pairs: Entry[],
): Effect.Effect<void, DBInsertError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insertTXs`);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(pairs)}`;
  }).pipe(
    Effect.withLogSpan(`insertTXs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertTXs: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDBInsertError<never>(tableName),
  );

export const retrieveAllEntries = (
  tableName: string,
): Effect.Effect<readonly EntryWithTimeStamp[], DBSelectError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt to retrieve all tx entries`,
    );
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithTimeStamp>`SELECT * FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDBSelectError<never>(tableName),
  );
