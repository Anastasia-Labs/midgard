import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import * as MempoolInclusionsDB from "./mempoolInclusions.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";
import * as Tx from "./utils/tx.js";

export const tableName = "processed_mempool";

export const insertTx = (
  tx: Tx.Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.insertEntry(tableName, tx);

export const insertTxs = (
  txs: Tx.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Tx.insertEntries(tableName, txs);

/**
 * Moves the mempool rows `txIds` to this table in one statement, each with
 * the bytes, admission time and inclusion mark the mempool row holds when it
 * moves; the deltas stay. A row this table already holds keeps its own
 * columns, and takes the moved mark when it has none.
 */
export const moveFromMempool = (
  txIds: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql`WITH moved AS (
        DELETE FROM mempool
        WHERE tx_id = ANY(${pg.array(txIds.map((id) => `\\x${id.toString("hex")}`))}::bytea[])
        RETURNING tx_id, tx, time_stamp_tz, included_by)
      INSERT INTO processed_mempool (tx_id, tx, time_stamp_tz, included_by)
      SELECT tx_id, tx, time_stamp_tz, included_by FROM moved
      ON CONFLICT (tx_id) DO UPDATE SET included_by =
        COALESCE(processed_mempool.included_by, EXCLUDED.included_by)`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to move the given transactions from the mempool",
    ),
  );

/** Every pending (unmarked) processed-mempool row, oldest first. */
export const retrieve = MempoolInclusionsDB.retrievePendingEntries(tableName);

/**
 * Retrieves pending processed-mempool transaction CBOR by transaction hash.
 */
export const retrieveTxCborByHash = (txHash: Buffer) =>
  MempoolInclusionsDB.retrievePendingValue(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Buffer[] | readonly Buffer[],
) => MempoolInclusionsDB.retrievePendingValues(tableName, txHashes);

export const clearTxs = (txHashes: Buffer[]) =>
  Tx.delMultiple(tableName, txHashes);

export const clear = clearTable(tableName);
