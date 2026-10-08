import { SqlClient } from "@effect/sql";
import { Context, Effect, Option } from "effect";

import type { HistoryOwnerChange } from "../services/event-history-owner.js";
import { releaseAdmissionOwnership } from "./cekProgramMaterial.js";
import {
  requireRecoveryTransaction,
  requireSourceTransaction,
} from "./eventHistoryAuthority.js";
import {
  type AdmissionKind,
  orphanedAdmission,
  orphanedForcedAdmission,
  sameAdmission,
} from "./l1-admission-identity.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** Private production recovery capability. Supplied only after fresh signed
 * non-inclusion evidence, durable native restore and exact journal revalidation.
 * It authorizes the one retained header; memberships themselves remain archived.
 */
export const AuthorizedHistoryHeaderRetirement = Context.GenericTag<{
  readonly headerHash: Buffer;
}>("midgard/AuthorizedHistoryHeaderRetirement");

const table = "event_history_l2_ledger_receipts";
const refuse = (message: string) =>
  Effect.fail(new DatabaseError({ table, message, cause: undefined }));

/** An event row whose follower admission L1 no longer holds in its key set. */
export type OrphanAdmission = {
  readonly kind: AdmissionKind;
  readonly event_id: Buffer;
  readonly l1_event_key: Buffer;
  readonly l1_origin_outref: Buffer;
};

/** Every orphaned deposit and withdrawal row, row-locked. */
const lockOrphanedAdmissions = (sql: SqlClient.SqlClient) =>
  Effect.gen(function* () {
    const deposits = yield* sql<OrphanAdmission>`
      SELECT 'deposit' AS kind, d.event_id, d.l1_event_key, d.l1_origin_outref
      FROM deposits_utxos d WHERE ${orphanedAdmission(sql, "d", "deposit")}
      ORDER BY d.l1_event_key FOR UPDATE OF d`;
    const withdrawals = yield* sql<OrphanAdmission>`
      SELECT 'withdrawal' AS kind, w.event_id, w.l1_event_key, w.l1_origin_outref
      FROM withdrawal_utxos w WHERE ${orphanedAdmission(sql, "w", "withdrawal")}
      ORDER BY w.l1_event_key FOR UPDATE OF w`;
    return [...deposits, ...withdrawals];
  });

/** An unreversed acceptance receipt consumed the deposit with follower
 * admission `orphan` and some transaction of its batch is no longer pending
 * in the mempool (gone, or marked by a block that includes it): the
 * dependency already left the unpublished overlay, so the orphan cannot be
 * repaired from retained receipts. */
export const orphanHasPublishedDependency = (
  binding: Buffer,
  orphan: OrphanAdmission,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql`SELECT 1 FROM event_history_l2_ledger_receipts r
        WHERE r.binding_digest = ${binding} AND r.reversed_at_revision IS NULL
          AND EXISTS (SELECT 1 FROM jsonb_populate_recordset(NULL::deposits_utxos, r.deposits_before) d
            WHERE d.l1_event_key = ${orphan.l1_event_key} AND d.l1_origin_outref = ${orphan.l1_origin_outref})
          AND EXISTS (SELECT 1 FROM unnest(r.tx_ids) AS ids(tx_id)
            WHERE NOT EXISTS (SELECT 1 FROM mempool m WHERE m.tx_id = ids.tx_id AND m.included_by IS NULL)) LIMIT 1`;
    return rows.length !== 0;
  });

/** Receipt `sequence` cannot be inverted as one unpublished batch: it lacks
 * its after-image or payloads, or one of its transactions is no longer an
 * accepted, unassigned, unmarked mempool entry. */
export const ledgerReceiptIsIncomplete = (sequence: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql`SELECT 1 FROM event_history_l2_ledger_receipts r
        WHERE sequence = ${sequence} AND (ledger_after IS NULL OR
          EXISTS (SELECT 1 FROM unnest(r.tx_ids) AS ids(tx_id)
            LEFT JOIN mempool m ON m.tx_id = ids.tx_id
            LEFT JOIN tx_admissions a ON a.tx_id = ids.tx_id
            WHERE m.tx_id IS NULL OR m.included_by IS NOT NULL
              OR a.status IS DISTINCT FROM 'accepted'
              OR EXISTS (SELECT 1 FROM blocks b WHERE b.tx_id = ids.tx_id)
              OR EXISTS (SELECT 1 FROM immutable i WHERE i.tx_id = ids.tx_id)
              OR EXISTS (SELECT 1 FROM pending_block_finalization_txs p WHERE p.member_id = ids.tx_id))
          OR jsonb_array_length(payloads_before) <> cardinality(tx_ids))`;
    return rows.length !== 0;
  });

/** Detect dispositions that require additional canonical source evidence before
 * touching dependent SQL. This is used only by the production source owner;
 * the strict inverse/materialization APIs still refuse such state. Keeping the
 * journal moving while Recovering permits collection of expiry/finality proof.
 */
export const pendingHistoryLedgerDisposition = (change: HistoryOwnerChange) =>
  Effect.gen(function* () {
    const token = yield* requireSourceTransaction;
    if (token.deploymentIdentity !== change.after.manifestId)
      return yield* refuse("History disposition deployment changed");
    const sql = yield* SqlClient.SqlClient;
    const binding = Buffer.from(change.after.bindingDigest, "hex");
    const current = yield* sql`SELECT 1 FROM event_history_cursor
      WHERE binding_digest = ${binding} AND revision = ${change.after.revision}
        AND head_hash = ${Buffer.from(change.after.head.id, "hex")}
        AND snapshot_digest = ${Buffer.from(change.after.capture.snapshotDigest, "hex")} FOR UPDATE`;
    if (current.length !== 1)
      return yield* refuse("History disposition checkpoint changed");
    // Native CAS may have committed just before a source generation changed.
    // This obligation survives even when a later branch makes every origin
    // canonical again; orphan flags alone cannot authorize reopening the gate.
    const outstanding = yield* sql`SELECT 1 FROM event_history_recovery_plans
      WHERE binding_digest = ${binding} AND state = 'prepared' LIMIT 1`;
    if (outstanding.length !== 0)
      return {
        status: "pending" as const,
        reason:
          "A durable native/SQL recovery operation requires current-branch disposition",
      };
    const assigned = yield* sql`WITH orphans AS (
      SELECT d.l1_event_key, d.l1_origin_outref, d.projected_header_hash, d.status::text AS status
        FROM deposits_utxos d WHERE ${orphanedAdmission(sql, "d", "deposit")}
      UNION ALL
      SELECT w.l1_event_key, w.l1_origin_outref, w.projected_header_hash, w.status::text AS status
        FROM withdrawal_utxos w WHERE ${orphanedAdmission(sql, "w", "withdrawal")}
    ) SELECT 1 FROM orphans o WHERE
      EXISTS (SELECT 1 FROM pending_block_finalizations WHERE status NOT IN ('locally_applied', 'abandoned'))
      OR EXISTS (SELECT 1 FROM processed_mempool WHERE included_by IS NULL)
      OR o.projected_header_hash IS NOT NULL OR o.status = 'finalized'
      OR EXISTS (SELECT 1 FROM pending_block_finalization_deposits m WHERE ${sameAdmission(sql, "m", "o")})
      OR EXISTS (SELECT 1 FROM pending_block_finalization_withdrawals m WHERE ${sameAdmission(sql, "m", "o")})
      LIMIT 1`;
    // A forced orphan is counted only while an unfinished block journal
    // holds it, so it is always assigned: the journal's disposition clears it.
    const forced = yield* sql`SELECT 1 FROM forced_transaction_utxos f
      WHERE ${orphanedForcedAdmission(sql, "f")} LIMIT 1`;
    return assigned.length === 0 && forced.length === 0
      ? undefined
      : {
          status: "pending" as const,
          reason:
            "Canonical history advanced; dependent L2 state awaits authenticated candidate, signed-submission or published-header disposition",
        };
  }).pipe(
    sqlErrorToDatabaseError(
      table,
      "Failed to inspect dependent history disposition",
    ),
  );

/** Requeue only complete, unassigned acceptance batches from their exact
 * inverse receipts. The caller owns a drained canonical recovery generation;
 * assigned or possibly submitted state requires its existing disposition first.
 */
export const requeueUnpublishedHistoryLedger = (input: {
  readonly bindingDigest: string;
  readonly checkpointRevision: string;
}) =>
  Effect.gen(function* () {
    yield* requireRecoveryTransaction;
    const sql = yield* SqlClient.SqlClient;
    const binding = Buffer.from(input.bindingDigest, "hex");
    const assigned = yield* sql`SELECT 1 FROM pending_block_finalizations
    WHERE status NOT IN ('locally_applied', 'abandoned')
    UNION ALL SELECT 1 FROM processed_mempool WHERE included_by IS NULL
    LIMIT 1`;
    if (assigned.length !== 0)
      return yield* refuse(
        "Unpublished overlay requeue requires pending-candidate disposition",
      );
    const receipts = yield* sql<{
      sequence: string;
      tx_ids: Buffer[];
    }>`SELECT sequence::text, tx_ids
      FROM event_history_l2_ledger_receipts r WHERE binding_digest = ${binding}
        AND reversed_at_revision IS NULL AND EXISTS (SELECT 1 FROM mempool m
          WHERE m.tx_id = ANY(r.tx_ids) AND m.included_by IS NULL)
      ORDER BY r.sequence DESC FOR UPDATE`;
    const uncovered = yield* sql`SELECT 1 FROM mempool m
      WHERE m.included_by IS NULL AND (SELECT count(*) FROM event_history_l2_ledger_receipts r WHERE binding_digest = ${binding}
        AND reversed_at_revision IS NULL AND m.tx_id = ANY(r.tx_ids)) <> 1 LIMIT 1`;
    if (uncovered.length !== 0)
      return yield* refuse(
        "Unpublished suffix lacks a unique complete inverse receipt",
      );

    for (const receipt of receipts) {
      if (yield* ledgerReceiptIsIncomplete(receipt.sequence))
        return yield* refuse(
          "Ledger batch is incomplete, assigned or only partially unpublished",
        );
      const differentAfter =
        yield* sql`SELECT 1 FROM event_history_l2_ledger_receipts r,
        LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.ledger_after) expected
        LEFT JOIN mempool_ledger actual ON actual.outref = expected.outref
        WHERE r.sequence = ${receipt.sequence} AND to_jsonb(actual) IS DISTINCT FROM to_jsonb(expected) LIMIT 1`;
      const occupiedBefore =
        yield* sql`SELECT 1 FROM event_history_l2_ledger_receipts r,
        LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.ledger_before) expected
        JOIN mempool_ledger actual ON actual.outref = expected.outref
        WHERE r.sequence = ${receipt.sequence} LIMIT 1`;
      const changedPayload =
        yield* sql`SELECT 1 FROM event_history_l2_ledger_receipts r,
        LATERAL jsonb_populate_recordset(NULL::tx_admission_payloads, r.payloads_before) expected
        LEFT JOIN tx_admission_payloads actual ON actual.tx_id = expected.tx_id
        LEFT JOIN mempool m ON m.tx_id = expected.tx_id
        WHERE r.sequence = ${receipt.sequence} AND (actual.tx_canonical_cbor IS DISTINCT FROM expected.tx_canonical_cbor
          OR actual.cek_program_material_sidecar_sha256 IS DISTINCT FROM expected.cek_program_material_sidecar_sha256
          OR m.tx IS DISTINCT FROM expected.tx_canonical_cbor) LIMIT 1`;
      if (
        differentAfter.length ||
        occupiedBefore.length ||
        changedPayload.length
      )
        return yield* refuse(
          "Unpublished inverse receipt no longer matches current ledger or payload bytes",
        );
      // Only consumed deposit statuses change during acceptance. Reference-only
      // rows and L1 inclusion/pointer fields must never be restored from history.
      const changedDeposit =
        yield* sql`SELECT 1 FROM event_history_l2_ledger_receipts r,
        LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.ledger_before) spent
        JOIN jsonb_populate_recordset(NULL::deposits_utxos, r.deposits_before) old
          ON old.event_id = spent.source_event_id
        LEFT JOIN deposits_utxos current ON current.event_id = old.event_id
        WHERE r.sequence = ${receipt.sequence} AND (current.status IS DISTINCT FROM 'consumed'
          OR current.l1_event_key IS DISTINCT FROM old.l1_event_key
          OR current.l1_origin_outref IS DISTINCT FROM old.l1_origin_outref
          OR current.projected_header_hash IS DISTINCT FROM old.projected_header_hash) LIMIT 1`;
      if (changedDeposit.length)
        return yield* refuse(
          "Consumed deposit incarnation changed before inverse application",
        );
      yield* sql`DELETE FROM mempool_ledger actual USING event_history_l2_ledger_receipts r,
        LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.ledger_after) expected
        WHERE r.sequence = ${receipt.sequence} AND actual.outref = expected.outref`;
      yield* sql`INSERT INTO mempool_ledger SELECT old.* FROM event_history_l2_ledger_receipts r,
        LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.ledger_before) old WHERE r.sequence = ${receipt.sequence}`;
      yield* sql`UPDATE deposits_utxos current SET status = old.status
        FROM event_history_l2_ledger_receipts r,
          LATERAL jsonb_populate_recordset(NULL::mempool_ledger, r.ledger_before) spent,
          LATERAL jsonb_populate_recordset(NULL::deposits_utxos, r.deposits_before) old
        WHERE r.sequence = ${receipt.sequence} AND old.event_id = spent.source_event_id AND current.event_id = old.event_id`;
      yield* sql`UPDATE tx_admission_payloads current SET cek_program_material_sidecar_cbor = old.cek_program_material_sidecar_cbor
        FROM event_history_l2_ledger_receipts r,
          LATERAL jsonb_populate_recordset(NULL::tx_admission_payloads, r.payloads_before) old
        WHERE r.sequence = ${receipt.sequence} AND current.tx_id = old.tx_id`;
      yield* releaseAdmissionOwnership(receipt.tx_ids);
      yield* sql`UPDATE tx_admissions SET status = 'queued', terminal_at = NULL,
        validation_started_at = NULL, lease_owner = NULL, lease_expires_at = NULL,
        reject_code = NULL, reject_detail = NULL,
        next_attempt_at = GREATEST(NOW(), first_seen_at, last_seen_at, updated_at),
        updated_at = GREATEST(NOW(), first_seen_at, last_seen_at, updated_at)
        WHERE tx_id IN (SELECT unnest(tx_ids) FROM event_history_l2_ledger_receipts WHERE sequence = ${receipt.sequence})`;
      for (const name of ["mempool", "mempool_tx_deltas", "address_history"]) {
        yield* sql`DELETE FROM ${sql(name)} WHERE tx_id IN (SELECT unnest(tx_ids) FROM event_history_l2_ledger_receipts WHERE sequence = ${receipt.sequence})`;
      }
      yield* sql`UPDATE event_history_l2_ledger_receipts SET reversed_at_revision = ${input.checkpointRevision}
        WHERE sequence = ${receipt.sequence} AND reversed_at_revision IS NULL`;
    }
  });

/** First repair slice: undo the complete, proved-unpublished acceptance overlay.
 * The source journal's orphan flags supply authority; inverse receipts supply
 * bytes. Published/possibly broadcast state and missing inverse evidence remain
 * fenced. The owner must drain producers and deferred writes before calling.
 */
export const repairUnpublishedHistoryLedger = (change: HistoryOwnerChange) =>
  Effect.gen(function* () {
    const token = yield* requireSourceTransaction;
    if (token.deploymentIdentity !== change.after.manifestId)
      return yield* refuse("History ledger repair deployment changed");
    const sql = yield* SqlClient.SqlClient;
    const binding = Buffer.from(change.after.bindingDigest, "hex");
    const current =
      yield* sql`SELECT 1 FROM event_history_cursor WHERE binding_digest = ${binding}
      AND manifest_id = ${Buffer.from(change.after.manifestId, "hex")}
      AND revision = ${change.after.revision} AND head_hash = ${Buffer.from(change.after.head.id, "hex")}
      AND snapshot_digest = ${Buffer.from(change.after.capture.snapshotDigest, "hex")} FOR UPDATE`;
    if (current.length !== 1)
      return yield* refuse("History ledger repair checkpoint changed");
    const orphans = yield* lockOrphanedAdmissions(sql);
    if (orphans.length === 0) return;
    // Orphans exist only after a rewind; their repair is recovery work, never
    // part of a Ready append.
    yield* requireRecoveryTransaction;

    const pending = yield* sql`SELECT 1 FROM pending_block_finalizations
      WHERE status NOT IN ('locally_applied', 'abandoned') LIMIT 1`;
    const processed = yield* sql`SELECT 1 FROM processed_mempool
      WHERE included_by IS NULL LIMIT 1`;
    if (pending.length !== 0 || processed.length !== 0)
      return yield* refuse(
        "Orphan repair requires explicit pending-candidate or signed-submission disposition",
      );
    // Without retained receipts, absence of a current spend is not proof that a
    // published transaction did not reference an orphan. Require reconstruction
    // of that accepted baseline rather than guessing from the present UTxO set.
    const missingBaseline = yield* sql`SELECT 1 FROM (
      SELECT tx_id FROM tx_admissions WHERE status = 'accepted'
      UNION SELECT tx_id FROM blocks
      UNION SELECT tx_id FROM immutable
    ) accepted WHERE NOT EXISTS (SELECT 1 FROM event_history_l2_ledger_receipts r
      WHERE r.binding_digest = ${binding} AND r.reversed_at_revision IS NULL
        AND accepted.tx_id = ANY(r.tx_ids)) LIMIT 1`;
    if (missingBaseline.length !== 0)
      return yield* refuse(
        "Orphan repair requires retained inverse evidence or authenticated accepted-baseline reconstruction",
      );
    const retirement = yield* Effect.serviceOption(
      AuthorizedHistoryHeaderRetirement,
    );
    const retiredHeader = Option.isSome(retirement)
      ? retirement.value.headerHash
      : null;
    for (const orphan of orphans) {
      const eventTable =
        orphan.kind === "deposit" ? "deposits_utxos" : "withdrawal_utxos";
      const identity = sql`l1_event_key = ${orphan.l1_event_key} AND l1_origin_outref = ${orphan.l1_origin_outref}`;
      const assigned = yield* sql`SELECT 1 FROM ${sql(eventTable)}
        WHERE ${identity}
          AND (projected_header_hash IS NOT NULL OR status = 'finalized')
          AND (${retiredHeader}::bytea IS NULL OR projected_header_hash IS DISTINCT FROM ${retiredHeader}) LIMIT 1`;
      const memberships =
        yield* sql`SELECT 1 FROM pending_block_finalization_deposits
        WHERE ${identity}
          AND (${retiredHeader}::bytea IS NULL OR header_hash <> ${retiredHeader})
        UNION ALL SELECT 1 FROM pending_block_finalization_withdrawals
        WHERE ${identity}
          AND (${retiredHeader}::bytea IS NULL OR header_hash <> ${retiredHeader})`;
      if (assigned.length !== 0 || memberships.length !== 0)
        return yield* refuse(
          "Orphan admission has retained header membership requiring authenticated published correction",
        );
      if (yield* orphanHasPublishedDependency(binding, orphan))
        return yield* refuse(
          "Orphan dependency already left the unpublished ledger overlay",
        );
    }

    yield* requeueUnpublishedHistoryLedger({
      bindingDigest: change.after.bindingDigest,
      checkpointRevision: change.after.revision,
    });
    if (retiredHeader !== null) {
      // Clear assignments only AFTER inverse receipts checked/restored their exact
      // preimages. Header membership rows and signed intent are never deleted.
      yield* sql`UPDATE deposits_utxos d SET projected_header_hash = NULL
        WHERE d.projected_header_hash = ${retiredHeader}
          AND ${orphanedAdmission(sql, "d", "deposit")}`;
    }
    for (const orphan of orphans) {
      if (orphan.kind === "deposit") {
        const unsafe = yield* sql`SELECT 1 FROM deposits_utxos d
          WHERE d.l1_event_key = ${orphan.l1_event_key} AND d.l1_origin_outref = ${orphan.l1_origin_outref}
            AND (status NOT IN ('awaiting', 'projected') OR projected_header_hash IS NOT NULL
              OR EXISTS (SELECT 1 FROM mempool_ledger l WHERE l.source_event_id = d.event_id AND l.output <> d.ledger_output))`;
        if (unsafe.length)
          return yield* refuse(
            "Orphan deposit remains dependent on published or unproven ledger state",
          );
        yield* sql`DELETE FROM mempool_ledger WHERE source_event_id = ${orphan.event_id}`;
        yield* sql`DELETE FROM deposits_utxos WHERE l1_event_key = ${orphan.l1_event_key} AND l1_origin_outref = ${orphan.l1_origin_outref}`;
      } else {
        yield* sql`DELETE FROM withdrawal_utxos WHERE l1_event_key = ${orphan.l1_event_key} AND l1_origin_outref = ${orphan.l1_origin_outref}`;
      }
    }
    // Re-run classification against the repaired ledger; preserve submitted raw
    // withdrawal bodies and all assignments to published headers.
    yield* sql`UPDATE withdrawal_utxos SET settlement_event_info = NULL,
      validity = NULL, validity_detail = '{}'::jsonb, status = 'awaiting',
      classification_revision = classification_revision + 1, updated_at = NOW()
      WHERE projected_header_hash IS NULL AND status <> 'finalized'
        AND (status <> 'awaiting' OR validity IS NOT NULL OR settlement_event_info IS NOT NULL)`;
  }).pipe(
    sqlErrorToDatabaseError(
      table,
      "Failed unpublished history-dependent ledger repair",
    ),
  );
