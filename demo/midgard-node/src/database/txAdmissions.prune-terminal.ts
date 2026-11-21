import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { PENDING_TABLES } from "./mempoolInclusions.js";
import {
  Columns,
  tableName,
} from "./txAdmissions.verify-claimed-payload-rows.js";
import * as TxRejectionsDB from "./txRejections.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** Where a terminal-row sweep stopped: the last row it examined. The time
 * is Postgres' own text form, which keeps the microseconds a `Date` would
 * drop: one terminal mark stamps every row of its batch with the same time,
 * so a truncated cursor would examine that batch again and again. */
export type TerminalAdmissionCursor = Readonly<{
  terminalAt: string;
  txId: Buffer;
}>;

export type TerminalAdmissionPruneBatch = Readonly<{
  /** Terminal rows past the window that the batch looked at. */
  examined: number;
  deleted: number;
  /** The last row examined; `null` when the batch examined none. */
  last: TerminalAdmissionCursor | null;
}>;

/**
 * Examines up to `limit` terminal admission rows older than `olderThanMs`,
 * oldest first after `after`, and deletes the ones nothing still needs. A
 * row is kept while
 *
 * - a pending table holds its tx: the mempool order, the replay order and
 *   the peer catch-up read its `arrival_seq`;
 * - `tx_rejections` holds its tx: re-admitting it could reject it again
 *   with another code, which the exactly-once rejection insert refuses. The
 *   row goes once housekeeping prunes the rejection.
 *
 * Queued and validating rows have no `terminal_at` and are never examined.
 * Rows another transaction holds are skipped, so the sweep never waits on a
 * lock; a later pass examines them again. The payload row, if one is left,
 * goes with the admission (ON DELETE CASCADE).
 */
export const pruneTerminalBatch = ({
  olderThanMs,
  after,
  limit,
}: {
  readonly olderThanMs: number;
  readonly after: TerminalAdmissionCursor | null;
  readonly limit: number;
}): Effect.Effect<TerminalAdmissionPruneBatch, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [mempool, processedMempool] = PENDING_TABLES;
    const afterCursor =
      after === null
        ? sql`TRUE`
        : sql`(${sql(Columns.TERMINAL_AT)}, ${sql(Columns.TX_ID)}) >
            (${after.terminalAt}::timestamptz, ${after.txId}::bytea)`;
    const rows = yield* sql<{
      readonly examined: string | number | bigint;
      readonly deleted: string | number | bigint;
      readonly last_terminal_at: string | null;
      readonly last_tx_id: Buffer | null;
    }>`WITH candidates AS (
        SELECT ${sql(Columns.TX_ID)}, ${sql(Columns.TERMINAL_AT)}
        FROM ${sql(tableName)}
        WHERE ${sql(Columns.STATUS)} IN ('accepted', 'rejected')
          AND ${sql(Columns.TERMINAL_AT)} <
            NOW() - (${Math.max(0, olderThanMs)} * INTERVAL '1 millisecond')
          AND ${afterCursor}
        ORDER BY ${sql(Columns.TERMINAL_AT)} ASC, ${sql(Columns.TX_ID)} ASC
        LIMIT ${Math.max(1, Math.floor(limit))}
        FOR UPDATE SKIP LOCKED
      ), deleted AS (
        DELETE FROM ${sql(tableName)} AS admission
        USING candidates
        WHERE admission.${sql(Columns.TX_ID)} = candidates.${sql(Columns.TX_ID)}
          AND NOT EXISTS (
            SELECT 1 FROM ${sql(mempool)} pending
            WHERE pending.tx_id = admission.${sql(Columns.TX_ID)}
          )
          AND NOT EXISTS (
            SELECT 1 FROM ${sql(processedMempool)} pending
            WHERE pending.tx_id = admission.${sql(Columns.TX_ID)}
          )
          AND NOT EXISTS (
            SELECT 1 FROM ${sql(TxRejectionsDB.tableName)} rejection
            WHERE rejection.${sql(TxRejectionsDB.Columns.TX_ID)} =
              admission.${sql(Columns.TX_ID)}
          )
        RETURNING 1
      ), last_candidate AS (
        SELECT ${sql(Columns.TX_ID)}, ${sql(Columns.TERMINAL_AT)}
        FROM candidates
        ORDER BY ${sql(Columns.TERMINAL_AT)} DESC, ${sql(Columns.TX_ID)} DESC
        LIMIT 1
      )
      SELECT
        (SELECT COUNT(*) FROM candidates)::bigint AS examined,
        (SELECT COUNT(*) FROM deleted)::bigint AS deleted,
        (SELECT ${sql(Columns.TERMINAL_AT)}::text FROM last_candidate)
          AS last_terminal_at,
        (SELECT ${sql(Columns.TX_ID)} FROM last_candidate) AS last_tx_id`;
    const row = rows[0];
    return {
      examined: Number(row?.examined ?? 0),
      deleted: Number(row?.deleted ?? 0),
      last:
        row?.last_terminal_at == null || row.last_tx_id == null
          ? null
          : { terminalAt: row.last_terminal_at, txId: row.last_tx_id },
    };
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to prune terminal admissions"),
  );
