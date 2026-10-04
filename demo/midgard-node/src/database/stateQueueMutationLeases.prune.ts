import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import {
  Columns as JournalColumns,
  tableName as JournalTable,
} from "./pendingBlockFinalizations.columns.js";
import {
  Columns,
  SCOPE,
  Status,
  tableName,
} from "./stateQueueMutationLeases.columns.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** The newest rows `inspect` can show; a prune never removes them. */
export const INSPECTABLE_LEASE_ROWS = 100;

/**
 * Deletes ended leases (released or failed) that ended more than
 * `olderThanMs` ago, `batchLimit` rows per statement until a batch comes up
 * short or `maxBatches` ran. Never removed: an active lease, any of the
 * newest INSPECTABLE_LEASE_ROWS rows, and any lease a retained journal names
 * by `state_queue_lease_token` (recovery and replacement paths settle a
 * journal's lease by that token). No foreign key references a lease row.
 * Returns the number of rows removed.
 */
export const pruneSettledLeases = ({
  olderThanMs,
  batchLimit = 1_000,
  maxBatches = 100,
}: {
  readonly olderThanMs: number;
  readonly batchLimit?: number;
  readonly maxBatches?: number;
}): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const windowMs = Math.max(0, Math.floor(olderThanMs));
    const limit = Math.max(1, Math.floor(batchLimit));
    let removed = 0;
    for (let batch = 0; batch < Math.max(1, maxBatches); batch++) {
      const rows = yield* sql<{ token: string }>`DELETE FROM ${sql(tableName)}
        WHERE ${sql(Columns.TOKEN)} IN (
          SELECT ${sql(Columns.TOKEN)} FROM ${sql(tableName)}
          WHERE ${sql(Columns.SCOPE)} = ${SCOPE}
            AND ${sql(Columns.STATUS)} <> ${Status.Active}
            AND ${sql(Columns.RELEASED_AT)} <
              NOW() - (${windowMs} * INTERVAL '1 millisecond')
            AND ${sql(Columns.TOKEN)} NOT IN (
              SELECT ${sql(Columns.TOKEN)} FROM ${sql(tableName)}
              WHERE ${sql(Columns.SCOPE)} = ${SCOPE}
              ORDER BY ${sql(Columns.ACQUIRED_AT)} DESC
              LIMIT ${INSPECTABLE_LEASE_ROWS})
            AND NOT EXISTS (
              SELECT 1 FROM ${sql(JournalTable)} AS journal
              WHERE journal.${sql(JournalColumns.STATE_QUEUE_LEASE_TOKEN)} =
                ${sql(tableName)}.${sql(Columns.TOKEN)})
          ORDER BY ${sql(Columns.RELEASED_AT)} ASC
          LIMIT ${limit})
        RETURNING ${sql(Columns.TOKEN)}`;
      removed += rows.length;
      if (rows.length < limit) break;
    }
    return removed;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to prune ended state-queue mutation leases",
    ),
  );
