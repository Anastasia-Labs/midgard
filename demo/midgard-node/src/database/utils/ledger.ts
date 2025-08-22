import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";
import { Address } from "@lucid-evolution/lucid";
import {
  CreateTableError,
  DeleteError,
  InsertError,
  mapCreateTableError,
  mapDeleteError,
  mapInsertError,
  mapSelectError,
  SelectError,
} from "@/database/utils/common.js";

export enum Columns {
  TX_ID = "tx_id",
  OUTREF = "outref",
  OUTPUT = "output",
  ADDRESS = "address",
  TIMESTAMPTZ = "time_stamp_tz",
}

export type EntryNoTimeStamp = {
  [Columns.TX_ID]: Buffer; // for linking the tables
  [Columns.OUTREF]: Buffer; // for root calc and updating the ledger
  [Columns.OUTPUT]: Buffer; // for root calc and updating the ledger
  [Columns.ADDRESS]: Address; // for provider
};

export type EntryWithTimeStamp = EntryNoTimeStamp & {
  [Columns.TIMESTAMPTZ]: Date; // for provider
};

export type Entry = EntryNoTimeStamp | EntryWithTimeStamp;

export type MinimalEntry = {
  [Columns.OUTREF]: Buffer;
  [Columns.OUTPUT]: Buffer;
};

export const createTable = (
  tableName: string,
): Effect.Effect<void, CreateTableError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
        ${sql(Columns.TX_ID)} BYTEA NOT NULL,
        ${sql(Columns.OUTREF)} BYTEA NOT NULL,
        ${sql(Columns.OUTPUT)} BYTEA NOT NULL,
        ${sql(Columns.ADDRESS)} TEXT NOT NULL,
        ${sql(Columns.TIMESTAMPTZ)} TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
        PRIMARY KEY (${sql(Columns.OUTREF)})
      );`;
        yield* sql`CREATE INDEX ${sql(
          `idx_${tableName}_${Columns.ADDRESS}`,
        )} ON ${sql(tableName)} (${sql(Columns.ADDRESS)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    mapCreateTableError(tableName),
  );

export const insertEntry = (
  tableName: string,
  entry: Entry,
): Effect.Effect<void, InsertError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert Ledger UTxO`);
    const sql = yield* SqlClient.SqlClient;
    // No need to handle conflicts.
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entry)}`;
  }).pipe(
    Effect.withLogSpan(`insertEntry ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntry: ${JSON.stringify(e)}`),
    ),
    mapInsertError(tableName),
  );

export const insertEntries = (
  tableName: string,
  entries: Entry[],
): Effect.Effect<void, InsertError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert Ledger UTxOs`);
    const sql = yield* SqlClient.SqlClient;

    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}`;
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntries: ${JSON.stringify(e)}`),
    ),
    mapInsertError(tableName),
  );

export const retrieveEntries = (
  tableName: string,
): Effect.Effect<readonly EntryWithTimeStamp[], SelectError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieveEntries`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithTimeStamp>`SELECT * FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (double) =>
      Effect.logError(
        `${tableName} db: retrieveEntries: ${JSON.stringify(double)}`,
      ),
    ),
    mapSelectError(tableName),
  );

export const retrieveEntriesWithAddress = (
  tableName: string,
  address: Address,
): Effect.Effect<readonly EntryWithTimeStamp[], SelectError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve Ledger UTxOs`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithTimeStamp>`SELECT * FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.ADDRESS)} = ${address}`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntriesWithAddress ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveEntriesWithAddress: ${JSON.stringify(e)}`,
      ),
    ),
    mapSelectError(tableName),
  );

export const delEntries = (
  tableName: string,
  outrefs: Buffer[],
): Effect.Effect<void, DeleteError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.OUTREF,
    )} IN ${sql.in(outrefs)}`;
  }).pipe(mapDeleteError(tableName));
