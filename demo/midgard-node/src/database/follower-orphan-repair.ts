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
 * A held orphan is of one of three kinds, by its header:
 *
 * - own-landed: a deposit or withdrawal assigned to a processed landed
 *   block this operator committed. A landed own block whose event left the
 *   chain holds commits and its own merge until it leaves the landed queue
 *   (`poisoned-own-headers.ts`); the recompute publishes its view.
 * - foreign-landed: the same for a block another operator committed. It is
 *   held until the header leaves the landed queue, and the recompute keeps
 *   the gate pending meanwhile (`l1_events_orphan_recovery`). The header
 *   includes an event no longer on L1, so a fabricated-deposit or
 *   fabricated-withdrawal fault proof applies to it, and an operator whose
 *   block descends from it is culpable too: the node must not build on it.
 *   The hold waits on another actor (a fault proof or an L1 rollback that
 *   removes the header); it is not a retry.
 * - journal: every other held orphan (a block journal not landed, a header
 *   the rebase has yet to release, a forced orphan an unfinished journal
 *   holds). Transient: the own-journal disposition or the rebase clears it,
 *   and the recompute keeps the gate pending meanwhile.
 *
 * A finalized orphan is the kind of its header.
 *
 * Runs in the caller's gated transaction.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  type AdmissionKind,
  onLandedBlock,
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

const ORPHAN_TABLES: ReadonlyArray<
  Readonly<{ table: string; kind: AdmissionKind }>
> = [
  { table: "deposits_utxos", kind: "deposit" },
  { table: "withdrawal_utxos", kind: "withdrawal" },
];

/** The sum of the `count(*)` subqueries `counts`. */
const sumOf = (
  sql: SqlClient.SqlClient,
  counts: ReadonlyArray<ReturnType<typeof onLandedBlock>>,
) =>
  Effect.map(
    sql<{ count: string }>`SELECT ${sql.join(" + ", false)(counts)} AS count`,
    (rows) => Number(rows[0]?.count ?? 0),
  );

/** The held deposit and withdrawal orphans `where` selects (alias `o`), counted. */
const countHeld = (
  where: (sql: SqlClient.SqlClient) => ReturnType<typeof onLandedBlock>,
  forced: boolean,
  message: string,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const counts = [
      ...ORPHAN_TABLES.map(
        ({ table, kind }) => sql`(SELECT count(*) FROM ${sql(table)} o
          WHERE ${orphanedAdmission(sql, "o", kind)} AND ${held(sql)}
            AND ${where(sql)})`,
      ),
      ...(forced
        ? [
            sql`(SELECT count(*) FROM forced_transaction_utxos f
              WHERE ${orphanedForcedAdmission(sql, "f")})`,
          ]
        : []),
    ];
    return yield* sumOf(sql, counts);
  }).pipe(sqlErrorToDatabaseError(table, message));

/**
 * Own-landed orphans: held deposits and withdrawals assigned to a processed
 * own landed block.
 */
export const countOwnLandedOrphans = countHeld(
  (sql) => onLandedBlock(sql, "o", "own"),
  false,
  "Failed to count own-landed orphans",
);

/**
 * Foreign-landed orphans: held deposits and withdrawals assigned to a
 * processed foreign landed block; held until the header leaves the landed
 * queue.
 */
export const countForeignLandedOrphans = countHeld(
  (sql) => onLandedBlock(sql, "o", "foreign"),
  false,
  "Failed to count foreign-landed orphans",
);

/**
 * The detail of the orphan-recovery hold (`l1_events_orphan_recovery`) for
 * `held` orphans awaiting recovery: the foreign landed headers that hold
 * some, by name, and how many of the rest wait for a block journal.
 *
 * A foreign landed header holding an orphan includes an event no longer on
 * L1: it is fault-provable, and every block built on it shares that fault,
 * so the node commits nothing (the gate stays pending) until the header
 * leaves the landed queue. The detail names the header and that the wait
 * is for a fault proof or an L1 rollback, not for this node.
 */
export const describeOrphansAwaitingRecovery = (held: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const orphans = sql.join(
      " + ",
      false,
    )(
      ORPHAN_TABLES.map(
        ({ table, kind }) => sql`(SELECT count(*) FROM ${sql(table)} o
          WHERE o.projected_header_hash = b.header_hash
            AND ${orphanedAdmission(sql, "o", kind)})`,
      ),
    );
    const rows = yield* sql<{ header_hash: Buffer; orphans: string }>`
      SELECT header_hash, orphans::text AS orphans FROM (
        SELECT b.header_hash, ${orphans} AS orphans
        FROM node_landed_blocks b
        WHERE b.kind = 'foreign' AND b.state = 'processed') counted
      WHERE orphans > 0
      ORDER BY header_hash`;
    const foreign = rows.reduce((sum, row) => sum + Number(row.orphans), 0);
    const journal = held - foreign;
    const journalPart = `${journal.toString()} orphaned event admission(s) wait for their block journal's disposition or the landed-block rebase`;
    if (rows.length === 0) return journalPart;
    const named = rows
      .slice(0, 3)
      .map((row) => `${row.header_hash.toString("hex")} (${row.orphans})`)
      .join(", ");
    const more =
      rows.length > 3 ? ` and ${(rows.length - 3).toString()} more` : "";
    return `foreign landed block ${named}${more} includes ${foreign.toString()} event(s) no longer on L1: it is fault-provable (fabricated deposit or withdrawal) and the node does not build on it; the hold clears when a fault proof or an L1 rollback removes the header from the landed queue${journal > 0 ? `; ${journalPart}` : ""}`;
  }).pipe(
    sqlErrorToDatabaseError(
      table,
      "Failed to describe the orphans awaiting recovery",
    ),
  );

/**
 * Journal orphans: every other held orphan, forced ones included (see the
 * module doc).
 */
export const countJournalOrphans = countHeld(
  (sql) =>
    sql`NOT ${onLandedBlock(sql, "o", "own")} AND NOT ${onLandedBlock(sql, "o", "foreign")}`,
  true,
  "Failed to count journal orphans",
);

/**
 * Every orphan the recompute holds the follower write gate for: all of them
 * (unheld ones included, which the repair deletes) except the own-landed
 * kind.
 */
export const countOrphansAwaitingRecovery = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const counts = [
    ...ORPHAN_TABLES.map(
      ({ table, kind }) => sql`(SELECT count(*) FROM ${sql(table)} o
        WHERE ${orphanedAdmission(sql, "o", kind)}
          AND NOT ${onLandedBlock(sql, "o", "own")})`,
    ),
    sql`(SELECT count(*) FROM forced_transaction_utxos f
      WHERE ${orphanedForcedAdmission(sql, "f")})`,
  ];
  return yield* sumOf(sql, counts);
}).pipe(
  sqlErrorToDatabaseError(table, "Failed to count orphans awaiting recovery"),
);
