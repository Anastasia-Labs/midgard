import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";

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

/** The rejected transactions a rejection follows from (migration 0019). */
export const causesTableName = "tx_rejection_causes";

export type Cause = Readonly<{ txId: Buffer; causeTxId: Buffer }>;

/** Records the causes of rejections inserted in the same transaction. */
export const insertCauses = (
  causes: readonly Cause[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (causes.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    for (let start = 0; start < causes.length; start += 1_000)
      yield* sql`INSERT INTO ${sql(causesTableName)} ${sql.insert(
        causes.slice(start, start + 1_000).map(({ txId, causeTxId }) => ({
          tx_id: txId,
          cause_tx_id: causeTxId,
        })),
      )}`;
  }).pipe(
    Effect.withLogSpan(`insert ${causesTableName}`),
    sqlErrorToDatabaseError(
      causesTableName,
      "Failed to insert tx rejection causes",
    ),
  );

/**
 * Deletes the rejections of `txIds`, then every rejection whose recorded
 * causes are all deleted ones, transitively; a rejection with no recorded
 * cause is deleted only if it is one of `txIds`. Runs in the caller's
 * transaction. Returns the deleted ids.
 */
export const deleteWithTracedRejections = (
  txIds: readonly Buffer[],
): Effect.Effect<readonly Buffer[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const deleted = new Map(txIds.map((id) => [id.toString("hex"), id]));
    for (let frontier = [...txIds]; frontier.length > 0; ) {
      const all = [...deleted.values()];
      const traced = yield* sql<{ tx_id: Buffer }>`
        SELECT c.tx_id FROM ${sql(causesTableName)} c
        WHERE c.tx_id IN (SELECT tx_id FROM ${sql(causesTableName)}
            WHERE cause_tx_id IN ${sql.in(frontier)})
          AND c.tx_id NOT IN ${sql.in(all)}
        GROUP BY c.tx_id
        HAVING bool_and(c.cause_tx_id IN ${sql.in(all)})`;
      frontier = traced.map(({ tx_id }) => tx_id);
      for (const id of frontier) deleted.set(id.toString("hex"), id);
    }
    const ids = [...deleted.values()];
    const removed: Buffer[] = [];
    for (let start = 0; start < ids.length; start += 1_000)
      removed.push(
        ...(yield* sql<{ tx_id: Buffer }>`DELETE FROM ${sql(tableName)}
          WHERE ${sql(Columns.TX_ID)} IN ${sql.in(ids.slice(start, start + 1_000))}
          RETURNING ${sql(Columns.TX_ID)}`).map(({ tx_id }) => tx_id),
      );
    return removed;
  }).pipe(
    Effect.withLogSpan(`deleteWithTracedRejections ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to delete tx rejections"),
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
