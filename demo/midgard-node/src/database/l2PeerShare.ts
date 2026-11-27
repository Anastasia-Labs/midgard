/**
 * Reads behind sharing pending L2 transactions with the other operators.
 * Pending means accepted and not yet marked included by a landed block.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { INCLUDED_BY, PENDING_TABLES } from "./mempoolInclusions.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as Tx from "./utils/tx.js";

const ADMISSIONS = "tx_admissions";

export type PendingTx = {
  readonly arrivalSeq: bigint;
  readonly txId: Buffer;
  readonly tx: Buffer;
};

/**
 * Pending mempool txs this node admitted from its own users, after admission
 * sequence `afterSeq`, in admission order. A child always follows its parent,
 * because a native child admitted first would have been rejected.
 */
export const nativePendingAfter = (
  afterSeq: bigint,
  limit: number,
): Effect.Effect<readonly PendingTx[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ seq: string; tx_id: Buffer; tx: Buffer }>`
      SELECT admission.arrival_seq AS seq, mempool.${sql(Tx.Columns.TX_ID)},
        mempool.${sql(Tx.Columns.TX)}
      FROM ${sql(ADMISSIONS)} AS admission
      JOIN mempool ON mempool.${sql(Tx.Columns.TX_ID)} = admission.tx_id
      WHERE admission.submit_source = 'native'
        AND admission.arrival_seq > ${afterSeq.toString()}::bigint
        AND mempool.${sql(INCLUDED_BY)} IS NULL
      ORDER BY admission.arrival_seq ASC
      LIMIT ${Math.max(1, Math.floor(limit))}`;
    return rows.map((row) => ({
      arrivalSeq: BigInt(row.seq),
      txId: row.tx_id,
      tx: row.tx,
    }));
  }).pipe(sqlErrorToDatabaseError(ADMISSIONS, "Failed to read native txs"));

/**
 * Whether either pending table holds a pending row. It reads only those
 * tables, which are pruned once a block's fold is final, never the admission
 * history: an idle node's L1 advance costs this one bounded read.
 */
export const hasPending: Effect.Effect<boolean, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [first, second] = PENDING_TABLES;
    const [row] = yield* sql<{ pending: boolean }>`
      SELECT EXISTS (
          SELECT 1 FROM ${sql(first)} WHERE ${sql(INCLUDED_BY)} IS NULL
        ) OR EXISTS (
          SELECT 1 FROM ${sql(second)} WHERE ${sql(INCLUDED_BY)} IS NULL
        ) AS pending`;
    return row?.pending === true;
  }).pipe(sqlErrorToDatabaseError("mempool", "Failed to read pending"));

/**
 * An admission sequence at or below which every row is terminal or pending,
 * so a catch-up that offers every pending id covers it: one below
 * the oldest queued or validating row, else the newest pending row, else
 * `null`. It reads the partial indexes of open admissions and the pending
 * rows only, never the whole history.
 */
export const catchUpMark: Effect.Effect<
  bigint | null,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [row] = yield* sql<{ seq: string | null }>`
    SELECT COALESCE(
      LEAST(
        (SELECT MIN(arrival_seq) FROM ${sql(ADMISSIONS)} WHERE status = 'queued'),
        (SELECT MIN(arrival_seq) FROM ${sql(ADMISSIONS)}
          WHERE status = 'validating')
      ) - 1,
      (SELECT MAX(admission.arrival_seq)
        FROM mempool JOIN ${sql(ADMISSIONS)} AS admission
          ON admission.tx_id = mempool.${sql(Tx.Columns.TX_ID)}
        WHERE mempool.${sql(INCLUDED_BY)} IS NULL)
    ) AS seq`;
  return row?.seq == null ? null : BigInt(row.seq);
}).pipe(sqlErrorToDatabaseError(ADMISSIONS, "Failed to read the mark"));

/** Every pending tx id of either pending table, oldest first. */
export const pendingIds: Effect.Effect<
  readonly Buffer[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [first, second] = PENDING_TABLES;
  const rows = yield* sql<{ tx_id: Buffer }>`
    SELECT pending.tx_id FROM (
      SELECT ${sql(Tx.Columns.TX_ID)}, ${sql(Tx.Columns.TIMESTAMPTZ)}
      FROM ${sql(first)} WHERE ${sql(INCLUDED_BY)} IS NULL
      UNION ALL
      SELECT ${sql(Tx.Columns.TX_ID)}, ${sql(Tx.Columns.TIMESTAMPTZ)}
      FROM ${sql(second)} WHERE ${sql(INCLUDED_BY)} IS NULL
    ) AS pending
    LEFT JOIN ${sql(ADMISSIONS)} AS admission
      ON admission.tx_id = pending.tx_id
    ORDER BY pending.${sql(Tx.Columns.TIMESTAMPTZ)} ASC,
      admission.arrival_seq ASC NULLS LAST, pending.tx_id ASC`;
  return rows.map((row) => row.tx_id);
}).pipe(sqlErrorToDatabaseError("mempool", "Failed to read pending ids"));

/** The bytes of the still-pending txs among `txIds`, by hex id. */
export const pendingBytes = (
  txIds: readonly Buffer[],
): Effect.Effect<ReadonlyMap<string, Buffer>, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) return new Map();
    const sql = yield* SqlClient.SqlClient;
    const [first, second] = PENDING_TABLES;
    const rows = yield* sql<{ tx_id: Buffer; tx: Buffer }>`
      SELECT ${sql(Tx.Columns.TX_ID)}, ${sql(Tx.Columns.TX)} FROM ${sql(first)}
      WHERE ${sql.in(Tx.Columns.TX_ID, [...txIds])} AND ${sql(INCLUDED_BY)} IS NULL
      UNION ALL
      SELECT ${sql(Tx.Columns.TX_ID)}, ${sql(Tx.Columns.TX)} FROM ${sql(second)}
      WHERE ${sql.in(Tx.Columns.TX_ID, [...txIds])} AND ${sql(INCLUDED_BY)} IS NULL`;
    return new Map(rows.map((row) => [row.tx_id.toString("hex"), row.tx]));
  }).pipe(sqlErrorToDatabaseError("mempool", "Failed to read pending bytes"));

/** The ids among `txIds` this node has no admission row for, in order. */
export const missingAdmissionIds = (
  txIds: readonly Buffer[],
): Effect.Effect<readonly Buffer[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ tx_id: Buffer }>`
      SELECT tx_id FROM ${sql(ADMISSIONS)}
      WHERE ${sql.in("tx_id", [...txIds])}`;
    const known = new Set(rows.map((row) => row.tx_id.toString("hex")));
    return txIds.filter((id) => !known.has(id.toString("hex")));
  }).pipe(sqlErrorToDatabaseError(ADMISSIONS, "Failed to read admissions"));
