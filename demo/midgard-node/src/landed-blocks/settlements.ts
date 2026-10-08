/**
 * Acceptance-receipt members a landed block settled (plan §7.3, N3), kept
 * on the receipt (`event_history_l2_ledger_receipt_settlements`, migration
 * 0010): one row per unreversed receipt member a landed block includes,
 * naming that block. The working-ledger rebuild that applies the block
 * writes them in its transaction; the batch closure of that and every later
 * rebuild reads a recorded member as settled by the base. A rollback that
 * takes the block off the landed chain rewinds its rows in the transaction
 * that records the rollback (and a block that relands records them again),
 * so the record holds only the landed base.
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import { sqlErrorToDatabaseError } from "../database/utils/common.js";
import { deleteRows, type LandedBlockRow, setState } from "./store.js";

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

/**
 * A rollback took the processed rows `left` off the landed chain: an
 * applied foreign row stays as `removed` until the rebase reverts it, every
 * other row goes, and the settlements any of them recorded are rewound.
 * The removed rows `relands` landed again before a rebase reverted them:
 * they are processed again and settle their receipt members again.
 */
export const rollBackRows = (
  left: readonly LandedBlockRow[],
  relands: readonly LandedBlockRow[],
) =>
  Effect.gen(function* () {
    yield* setState(
      left
        .filter((row) => row.kind === "foreign" && row.applied)
        .map((row) => row.headerHash),
      "removed",
    );
    yield* deleteRows(
      left
        .filter((row) => row.kind === "own" || !row.applied)
        .map((row) => row.headerHash),
    );
    yield* rewindSettlements(left.map((row) => row.headerHash));
    yield* setState(
      relands.map((row) => row.headerHash),
      "processed",
    );
    yield* recordSettlements(relands);
  });
