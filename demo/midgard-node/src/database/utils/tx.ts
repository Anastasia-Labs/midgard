import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
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
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.TX_ID)} BYTEA NOT NULL,
          ${sql(Columns.TX)} BYTEA NOT NULL,
          ${sql(Columns.TIMESTAMPTZ)} TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
          PRIMARY KEY (${sql(Columns.TX_ID)})
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.TIMESTAMPTZ}`,
        )} ON ${sql(tableName)} (${sql(Columns.TIMESTAMPTZ)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const delMultiple = (
  tableName: string,
  tx_ids: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
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
    Effect.withLogSpan(`delMultiple table ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to remove the given transactions",
    ),
  );

export const retrieveValue = (
  tableName: string,
  tx_id: Buffer,
): Effect.Effect<Buffer, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve value`);

    const result = yield* sql<
      Pick<Entry, Columns.TX>
    >`SELECT ${sql(Columns.TX)} FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.TX_ID)} = ${tx_id}`;

    if (result.length === 0)
      yield* new SqlError.SqlError({
        cause: `No value found for tx_id ${tx_id.toString("hex")}`,
      });

    return result[0][Columns.TX];
  }).pipe(
    Effect.withLogSpan(`retrieve value ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving value error: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve the given transaction",
    ),
  );

export const retrieveValues = (
  tableName: string,
  tx_ids: Buffer[] | readonly Buffer[],
): Effect.Effect<readonly Buffer[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve values`);

    const rows = yield* sql<
      Pick<Entry, Columns.TX>
    >`SELECT ${sql(Columns.TX)} FROM ${sql(
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
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve the given transactions",
    ),
  );

export const insertEntry = (
  tableName: string,
  txPair: Entry,
): Effect.Effect<void, DatabaseError, Database> =>
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
    sqlErrorToDatabaseError(
      tableName,
      "Failed to insert the given transaction",
    ),
  );

export const insertEntries = (
  tableName: string,
  pairs: Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insertTXs`);
    const sql = yield* SqlClient.SqlClient;
    if (pairs.length <= 0) {
      yield* Effect.logDebug("No pairs provided, skipping insertion.");
      return;
    }
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(
      pairs,
    )} ON CONFLICT (${sql(Columns.TX_ID)}) DO NOTHING`;
  }).pipe(
    Effect.withLogSpan(`insertTXs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertTXs: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to insert the given transactions",
    ),
  );

export const retrieveAllEntries = (
  tableName: string,
): Effect.Effect<readonly EntryWithTimeStamp[], DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt to retrieve all tx entries`,
    );
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithTimeStamp>`SELECT * FROM ${sql(tableName)} ORDER BY ${Columns.TIMESTAMPTZ} DESC; `;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: retrieve: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve the whole table"),
  );

export const pruneOlderThan = (
  tableName: string,
  cutoff: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql`DELETE FROM ${sql(tableName)}
      WHERE ${sql(Columns.TIMESTAMPTZ)} < ${cutoff}
      RETURNING ${sql(Columns.TX_ID)}`;
    return deleted.length;
  }).pipe(
    Effect.withLogSpan(`pruneOlderThan ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: pruneOlderThan: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to prune old transactions"),
  );
