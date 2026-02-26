import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { Address } from "@lucid-evolution/lucid";
import {
  sqlErrorToDatabaseError,
  DatabaseError,
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
): Effect.Effect<void, DatabaseError, Database> =>
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
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.ADDRESS}`,
        )} ON ${sql(tableName)} (${sql(Columns.ADDRESS)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const insertEntry = (
  tableName: string,
  entry: Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert Ledger UTxO`);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entry)}
      ON CONFLICT (${sql(Columns.OUTREF)}) DO NOTHING`;
  }).pipe(
    Effect.withLogSpan(`insertEntry ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntry: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert the given UTxO"),
  );

export const insertEntries = (
  tableName: string,
  entries: Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert Ledger UTxOs`);
    const sql = yield* SqlClient.SqlClient;
    if (entries.length <= 0) {
      yield* Effect.logDebug("No entries provided, skipping insertion.");
      return;
    }
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}
      ON CONFLICT (${sql(Columns.OUTREF)}) DO NOTHING`;
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert given UTxOs"),
  );

export const retrieveAllEntries = (
  tableName: string,
): Effect.Effect<readonly EntryWithTimeStamp[], DatabaseError, Database> =>
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
    sqlErrorToDatabaseError(tableName, "Failed to retrieve the whole ledger"),
  );

export const retrieveEntriesWithAddress = (
  tableName: string,
  address: Address,
): Effect.Effect<readonly EntryWithTimeStamp[], DatabaseError, Database> =>
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
    sqlErrorToDatabaseError(
      tableName,
      `Failed to retrieve UTxOs of address: ${address}`,
    ),
  );

export const delEntries = (
  tableName: string,
  outrefs: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.OUTREF,
    )} IN ${sql.in(outrefs)}`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to delete given UTxOs"));
