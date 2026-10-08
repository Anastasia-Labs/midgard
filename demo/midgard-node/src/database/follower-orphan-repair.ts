/**
 * The orphan repair of the follower-change driver's recompute (plan §7.3,
 * N1, N3): event rows whose follower admission left the chain (orphans,
 * `l1-admission-identity.ts`) are removed when nothing published holds
 * them, and the working-ledger recompute that runs after it rejects every
 * pending transaction that spent one ("direct") and their dependents.
 *
 * An orphan is held, and stays until its holder is resolved, while
 *
 * - it is assigned to a header (`projected_header_hash`) or finalized: its
 *   block's disposition (the landed-block rebase) or a landed correction
 *   settles it;
 * - a block journal that is not abandoned names it: that journal's
 *   disposition comes first (an abandoned journal's memberships are its
 *   archived record);
 * - it is a forced orphan: one is counted only while an unfinished block
 *   journal holds it, and that journal's disposition clears it.
 *
 * Runs in the caller's gated transaction.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  orphanedAdmission,
  orphanedForcedAdmission,
  sameAdmission,
} from "./l1-admission-identity.js";
import { sqlErrorToDatabaseError } from "./utils/common.js";

const table = "follower_orphan_repair";

/** SQL condition: the orphan aliased `o` is held (see the module doc). */
const held = (sql: SqlClient.SqlClient) =>
  sql`(o.projected_header_hash IS NOT NULL OR o.status::text = 'finalized'
    OR EXISTS (SELECT 1 FROM pending_block_finalization_deposits m
      JOIN pending_block_finalizations j ON j.header_hash = m.header_hash
      WHERE ${sameAdmission(sql, "m", "o")} AND j.status <> 'abandoned')
    OR EXISTS (SELECT 1 FROM pending_block_finalization_withdrawals m
      JOIN pending_block_finalizations j ON j.header_hash = m.header_hash
      WHERE ${sameAdmission(sql, "m", "o")} AND j.status <> 'abandoned'))`;

/**
 * Deletes every unheld orphaned deposit (with the working-ledger row it
 * projected) and withdrawal, and re-runs withdrawal classification when
 * any was deleted. Returns how many rows it deleted.
 */
export const deleteUnheldOrphans = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const deposits = yield* sql<{ event_id: Buffer }>`SELECT o.event_id
    FROM deposits_utxos o
    WHERE ${orphanedAdmission(sql, "o", "deposit")} AND NOT ${held(sql)}
    FOR UPDATE OF o`;
  if (deposits.length > 0) {
    const ids = deposits.map((row) => row.event_id);
    // The working-ledger row references its deposit (ON DELETE RESTRICT).
    yield* sql`DELETE FROM mempool_ledger WHERE ${sql.in("source_event_id", ids)}`;
    yield* sql`DELETE FROM deposits_utxos WHERE ${sql.in("event_id", ids)}`;
  }
  const withdrawals = yield* sql<{
    event_id: Buffer;
  }>`DELETE FROM withdrawal_utxos o
    WHERE ${orphanedAdmission(sql, "o", "withdrawal")} AND NOT ${held(sql)}
    RETURNING o.event_id`;
  const deleted = deposits.length + withdrawals.length;
  if (deleted > 0)
    // Classification runs again against the recomputed ledger; submitted
    // raw withdrawal bodies and assignments to published headers stay.
    yield* sql`UPDATE withdrawal_utxos SET settlement_event_info = NULL,
      validity = NULL, validity_detail = '{}'::jsonb, status = 'awaiting',
      classification_revision = classification_revision + 1, updated_at = NOW()
      WHERE projected_header_hash IS NULL AND status <> 'finalized'
        AND (status <> 'awaiting' OR validity IS NOT NULL OR settlement_event_info IS NOT NULL)`;
  return deleted;
}).pipe(sqlErrorToDatabaseError(table, "Failed to delete orphaned event rows"));

/** The orphans still held (see the module doc), forced ones included. */
export const countHeldOrphans = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ count: string }>`SELECT
    (SELECT count(*) FROM deposits_utxos o
      WHERE ${orphanedAdmission(sql, "o", "deposit")} AND ${held(sql)})
    + (SELECT count(*) FROM withdrawal_utxos o
      WHERE ${orphanedAdmission(sql, "o", "withdrawal")} AND ${held(sql)})
    + (SELECT count(*) FROM forced_transaction_utxos f
      WHERE ${orphanedForcedAdmission(sql, "f")}) AS count`;
  return Number(rows[0]?.count ?? 0);
}).pipe(sqlErrorToDatabaseError(table, "Failed to count held orphans"));
