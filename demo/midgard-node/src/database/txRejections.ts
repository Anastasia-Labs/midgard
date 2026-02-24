import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";

export const tableName = "tx_rejections";

export enum Columns {
  TX_ID = "tx_id",
  REJECT_CODE = "reject_code",
  REJECT_DETAIL = "reject_detail",
  CREATED_AT = "created_at",
}

export type Entry = {
  [Columns.TX_ID]: Buffer;
  [Columns.REJECT_CODE]: string;
  [Columns.REJECT_DETAIL]: string | null;
  [Columns.CREATED_AT]: Date;
};

export type EntryNoTimestamp = Omit<Entry, Columns.CREATED_AT>;

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.TX_ID)} BYTEA NOT NULL,
          ${sql(Columns.REJECT_CODE)} TEXT NOT NULL,
          ${sql(Columns.REJECT_DETAIL)} TEXT,
          ${sql(Columns.CREATED_AT)} TIMESTAMPTZ NOT NULL DEFAULT NOW()
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.TX_ID}`,
        )} ON ${sql(tableName)} (${sql(Columns.TX_ID)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.CREATED_AT}`,
        )} ON ${sql(tableName)} (${sql(Columns.CREATED_AT)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create tx_rejections table"),
  );

export const insert = (
  entry: EntryNoTimestamp,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entry)}`;
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to insert tx rejection"),
  );

export const insertMany = (
  entries: readonly EntryNoTimestamp[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}`;
  }).pipe(
    Effect.withLogSpan(`insert many ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to insert tx rejections"),
  );

export const retrieveByTxId = (
  txId: Buffer,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT ${sql(Columns.TX_ID)}, ${sql(
      Columns.REJECT_CODE,
    )}, ${sql(Columns.REJECT_DETAIL)}, ${sql(Columns.CREATED_AT)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.TX_ID)} = ${txId}
      ORDER BY ${sql(Columns.CREATED_AT)} DESC`;
  }).pipe(
    Effect.withLogSpan(`retrieve by txid ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve tx rejection"),
  );

export const pruneOlderThan = (
  cutoff: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql`DELETE FROM ${sql(tableName)}
      WHERE ${sql(Columns.CREATED_AT)} < ${cutoff}
      RETURNING ${sql(Columns.TX_ID)}`;
    return deleted.length;
  }).pipe(
    Effect.withLogSpan(`pruneOlderThan ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to prune tx rejections"),
  );

export const clear = clearTable(tableName);
