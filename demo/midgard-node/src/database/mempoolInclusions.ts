/**
 * Inclusion marks on the pending tables (`mempool`, `processed_mempool`;
 * migration 0013, plan §7.3, N3). A row a block includes keeps its bytes and
 * its delta, marked by that block's header hash (`included_by`), until the
 * block folds into `confirmed_ledger`; the rollback that takes the block off
 * the landed chain clears the mark, and the row is pending again.
 *
 * - Marked: this node's local finalization of its own block, and landed-block
 *   processing of any block (the processing insert, a reland, the rebase).
 * - Cleared: the rollback of a processed block, and the reopening of an own
 *   block's journal.
 * - Deleted (rows and deltas): the fold of the block (a foreign fold, or the
 *   local merge finalization of an own block).
 *
 * A pending row is an unmarked one. Every reader that means "pending" reads
 * through the helpers here (or filters `included_by IS NULL` itself).
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import * as MempoolTxDeltasDB from "./mempoolTxDeltas.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as Tx from "./utils/tx.js";

export const INCLUDED_BY = "included_by";

/** The pending tables, which carry the mark. */
export const PENDING_TABLES = ["mempool", "processed_mempool"] as const;

export type PendingTable = (typeof PENDING_TABLES)[number];

const bytea = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);

const headerBuffer = (headerHash: string | Buffer) =>
  typeof headerHash === "string" ? Buffer.from(headerHash, "hex") : headerHash;

/** Every pending (unmarked) row of `tableName`, oldest first. */
export const retrievePendingEntries = (
  tableName: PendingTable,
): Effect.Effect<readonly Tx.EntryWithTimeStamp[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Tx.EntryWithTimeStamp>`
      SELECT ${sql(Tx.Columns.TX_ID)}, ${sql(Tx.Columns.TX)},
        ${sql(Tx.Columns.TIMESTAMPTZ)}
      FROM ${sql(tableName)}
      WHERE ${sql(INCLUDED_BY)} IS NULL
      ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} ASC, ${sql(Tx.Columns.TX_ID)} ASC`;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to retrieve the pending rows"),
  );

/** The bytes of the pending rows of `tableName` among `txIds`. */
export const retrievePendingValues = (
  tableName: PendingTable,
  txIds: readonly Buffer[],
): Effect.Effect<readonly Buffer[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Pick<Tx.EntryNoTimeStamp, Tx.Columns.TX>>`
      SELECT ${sql(Tx.Columns.TX)} FROM ${sql(tableName)}
      WHERE ${sql.in(Tx.Columns.TX_ID, [...txIds])}
        AND ${sql(INCLUDED_BY)} IS NULL`;
    return rows.map((row) => row[Tx.Columns.TX]);
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve the given pending transactions",
    ),
  );

/** The bytes of the pending row `txId` of `tableName`; fails if there is none. */
export const retrievePendingValue = (tableName: PendingTable, txId: Buffer) =>
  retrievePendingValues(tableName, [txId]).pipe(
    Effect.flatMap((values) =>
      values.length === 0
        ? Effect.fail(
            new DatabaseError({
              table: tableName,
              message: "Failed to retrieve the given transaction",
              cause: `No pending value found for tx_id ${txId.toString("hex")}`,
            }),
          )
        : Effect.succeed(values[0]!),
    ),
  );

/** The number of pending rows of `tableName`. */
export const countPending = (
  tableName: PendingTable,
): Effect.Effect<bigint, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ count: string }>`
      SELECT COUNT(*) AS count FROM ${sql(tableName)}
      WHERE ${sql(INCLUDED_BY)} IS NULL`;
    return BigInt(rows[0]!.count);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to count the pending rows"),
  );

/** Marks the rows `txIds` (in either table) as included by `headerHash`. */
export const markIncluded = (
  headerHash: string | Buffer,
  txIds: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const header = headerBuffer(headerHash);
    for (const table of PENDING_TABLES)
      yield* sql`UPDATE ${sql(table)} SET ${sql(INCLUDED_BY)} = ${header}
        WHERE ${sql(Tx.Columns.TX_ID)} = ANY(${pg.array(bytea(txIds))}::bytea[])`;
  }).pipe(
    sqlErrorToDatabaseError(
      "mempool",
      "Failed to mark the transactions a block includes",
    ),
  );

/** Clears the marks the blocks `headerHashes` set: their rows are pending again. */
export const clearMarks = (
  headerHashes: readonly (string | Buffer)[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (headerHashes.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const headers = pg.array(bytea(headerHashes.map(headerBuffer)));
    for (const table of PENDING_TABLES)
      yield* sql`UPDATE ${sql(table)} SET ${sql(INCLUDED_BY)} = NULL
        WHERE ${sql(INCLUDED_BY)} = ANY(${headers}::bytea[])`;
  }).pipe(
    sqlErrorToDatabaseError(
      "mempool",
      "Failed to clear the marks of blocks that left the landed chain",
    ),
  );

/** Deletes the rows `headerHash` marked, and their deltas: the block folded. */
export const deleteIncluded = (
  headerHash: string | Buffer,
): Effect.Effect<readonly Buffer[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const header = headerBuffer(headerHash);
    const deleted: Buffer[] = [];
    for (const table of PENDING_TABLES) {
      const rows = yield* sql<{ tx_id: Buffer }>`
        DELETE FROM ${sql(table)} WHERE ${sql(INCLUDED_BY)} = ${header}
        RETURNING ${sql(Tx.Columns.TX_ID)}`;
      deleted.push(...rows.map((row) => row.tx_id));
    }
    yield* MempoolTxDeltasDB.clearTxs(deleted);
    return deleted;
  }).pipe(
    sqlErrorToDatabaseError(
      "mempool",
      "Failed to delete the transactions a folded block included",
    ),
  );

/** Every header hash (hex) that marks a row of either pending table. */
export const markingHeaders: Effect.Effect<
  readonly string[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ included_by: Buffer }>`
    SELECT DISTINCT included_by FROM mempool WHERE included_by IS NOT NULL
    UNION
    SELECT DISTINCT included_by FROM processed_mempool
    WHERE included_by IS NOT NULL`;
  return rows.map((row) => Buffer.from(row.included_by).toString("hex"));
}).pipe(
  sqlErrorToDatabaseError("mempool", "Failed to read the inclusion marks"),
);

/** The ids of every marked row of either pending table. */
export const markedTxIds: Effect.Effect<
  readonly Buffer[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ tx_id: Buffer }>`
    SELECT tx_id FROM mempool WHERE included_by IS NOT NULL
    UNION
    SELECT tx_id FROM processed_mempool WHERE included_by IS NOT NULL`;
  return rows.map((row) => Buffer.from(row.tx_id));
}).pipe(
  sqlErrorToDatabaseError("mempool", "Failed to read the marked transactions"),
);
