import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import {
  sqlErrorToDatabaseError,
  DatabaseError,
} from "@/database/utils/common.js";

export const tableName = "deposits_utxos";

export enum Columns {
  ID = "event_id",
  INFO = "event_info",
  INCLUSION_TIME = "inclusion_time",
}

export type Entry = {
  [Columns.ID]: Buffer;
  [Columns.INFO]: Buffer;
  [Columns.INCLUSION_TIME]: Date;
};

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
        ${sql(Columns.ID)} BYTEA NOT NULL,
        ${sql(Columns.INFO)} BYTEA NOT NULL,
        ${sql(Columns.INCLUSION_TIME)} TIMESTAMPTZ NOT NULL,
        PRIMARY KEY (${sql(Columns.ID)})
      );`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const insertEntry = (
  entry: Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert Deposit UTxO`);
    const sql = yield* SqlClient.SqlClient;
    // No need to handle conflicts.
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entry)}`;
  }).pipe(
    Effect.withLogSpan(`insertEntry ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntry: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert the given UTxO"),
  );

export const insertEntries = (
  entries: Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert Deposit UTxOs`);
    const sql = yield* SqlClient.SqlClient;
    if (entries.length <= 0) {
      yield* Effect.logDebug("No entries provided, skipping insertion.");
      return;
    }
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}`;
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert given UTxOs"),
  );

export const retrieveTimeBoundEntries = (
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt to retrieveTimeBoundEntries`,
    );
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(
      tableName,
    )} WHERE ${startTime} < ${sql(Columns.INCLUSION_TIME)} AND ${sql(Columns.INCLUSION_TIME)} <= ${endTime}`;
  }).pipe(
    Effect.withLogSpan(`retrieveTimeBoundEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (double) =>
      Effect.logError(
        `${tableName} db: retrieveTimeBoundEntries: ${JSON.stringify(double)}`,
      ),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve all deposit UTxOs"),
  );

export const retrieveAllEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieveEntries`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (double) =>
      Effect.logError(
        `${tableName} db: retrieveEntries: ${JSON.stringify(double)}`,
      ),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve all deposit UTxOs"),
  );

export const delEntries = (
  ids: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.ID,
    )} IN ${sql.in(ids)}`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to delete given UTxOs"));
