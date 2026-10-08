/**
 * Acceptance-receipt members a landed block settled (plan §7.3, N3), kept
 * on the receipt (`event_history_l2_ledger_receipt_settlements`, migration
 * 0012): one row per unreversed receipt member a landed block includes,
 * naming that block. Every write that makes a landed row processed records
 * them in its own transaction: the processing insert, a reland, and the
 * rebase that applies the row; the local merge finalization of an own block
 * records them too, so a block that folds without a rebase between is
 * recorded. The batch closure of every later rebuild reads a recorded
 * member as settled by the base. A rollback that takes the block off the
 * landed chain rewinds its rows in the transaction that records the
 * rollback (and a block that relands records them again), so the record
 * holds only the landed base, folded blocks included.
 *
 * The same writes mark the block's rows in the pending tables
 * (`mempoolInclusions.ts`); the rollback clears the marks.
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import * as MempoolInclusionsDB from "../database/mempoolInclusions.js";
import { sqlErrorToDatabaseError } from "../database/utils/common.js";
import {
  deleteRows,
  insertRow,
  type LandedBlockRow,
  setState,
} from "./store.js";

export const settlementsTableName =
  "event_history_l2_ledger_receipt_settlements";

const bytea = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);

/** Records every unreversed receipt member a landed block in `rows` includes. */
export const recordSettlements = (
  rows: readonly Pick<LandedBlockRow, "headerHash" | "txIds">[],
) =>
  Effect.gen(function* () {
    const pairs = rows.flatMap((row) =>
      row.txIds.map(
        (txId) => [Buffer.from(row.headerHash, "hex"), txId] as const,
      ),
    );
    if (pairs.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql`INSERT INTO event_history_l2_ledger_receipt_settlements
        (receipt_sequence, tx_id, settled_by)
      SELECT r.sequence, included.tx_id, included.settled_by
      FROM unnest(
          ${pg.array(bytea(pairs.map(([, txId]) => txId)))}::bytea[],
          ${pg.array(bytea(pairs.map(([header]) => header)))}::bytea[]
        ) AS included(tx_id, settled_by)
      JOIN event_history_l2_ledger_receipts r
        ON r.reversed_at_revision IS NULL
       AND included.tx_id = ANY(r.tx_ids)
      ON CONFLICT (receipt_sequence, tx_id) DO NOTHING`;
  }).pipe(
    sqlErrorToDatabaseError(
      settlementsTableName,
      "Failed to record the receipt members landed blocks settled",
    ),
  );

/** Drops the settlements the blocks `headerHashes` recorded. */
export const rewindSettlements = (headerHashes: readonly string[]) =>
  Effect.gen(function* () {
    if (headerHashes.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql`DELETE FROM event_history_l2_ledger_receipt_settlements
      WHERE settled_by = ANY(${pg.array(
        bytea(headerHashes.map((hash) => Buffer.from(hash, "hex"))),
      )}::bytea[])`;
  }).pipe(
    sqlErrorToDatabaseError(
      settlementsTableName,
      "Failed to rewind the receipt members rolled-back blocks settled",
    ),
  );

/** Marks the pending-table rows each block in `rows` includes. */
export const markRows = (
  rows: readonly Pick<LandedBlockRow, "headerHash" | "txIds">[],
) =>
  Effect.forEach(
    rows,
    (row) => MempoolInclusionsDB.markIncluded(row.headerHash, row.txIds),
    { discard: true },
  );

/**
 * Processes a landed block: inserts its row, marks the pending-table rows
 * it includes, and records the receipt members it settles.
 */
export const processRow = (row: LandedBlockRow) =>
  Effect.gen(function* () {
    yield* insertRow(row);
    yield* markRows([row]);
    yield* recordSettlements([row]);
  });

/**
 * A rollback took the processed rows `left` off the landed chain: an
 * applied row (foreign, or own) stays as `removed` until the rebase reverts
 * it (and disposes of an own row's journal), every other row goes, the settlements any of them recorded are rewound, and the
 * marks they set are cleared, so the rows they included are pending again.
 * The removed rows `relands` landed again before a rebase reverted them:
 * they are processed again, mark their rows and settle their receipt
 * members again.
 */
export const rollBackRows = (
  left: readonly LandedBlockRow[],
  relands: readonly LandedBlockRow[],
) =>
  Effect.gen(function* () {
    yield* setState(
      left.filter((row) => row.applied).map((row) => row.headerHash),
      "removed",
    );
    yield* deleteRows(
      left.filter((row) => !row.applied).map((row) => row.headerHash),
    );
    yield* rewindSettlements(left.map((row) => row.headerHash));
    yield* MempoolInclusionsDB.clearMarks(left.map((row) => row.headerHash));
    yield* setState(
      relands.map((row) => row.headerHash),
      "processed",
    );
    yield* markRows(relands);
    yield* recordSettlements(relands);
  });
