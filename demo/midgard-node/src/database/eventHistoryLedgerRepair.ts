import { SqlClient } from "@effect/sql";
import { Context, Effect, Option } from "effect";

import type { HistoryOwnerChange } from "../services/event-history-owner.js";
import { releaseAdmissionOwnership } from "./cekProgramMaterial.js";
import { requireRecoveryTransaction } from "./eventHistoryAuthority.js";
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

/** Detect dispositions that require additional canonical source evidence before
 * touching dependent SQL. This is used only by the production source owner;
 * the strict inverse/materialization APIs still refuse such state. Keeping the
 * journal moving while Recovering permits collection of expiry/finality proof.
 */
export const pendingHistoryLedgerDisposition = (change: HistoryOwnerChange) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
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
      SELECT i.incarnation_id, i.kind FROM event_history_incarnations i
      WHERE i.binding_digest = ${binding} AND NOT i.origin_canonical
        AND (EXISTS (SELECT 1 FROM deposits_utxos d WHERE d.history_binding_digest = i.binding_digest AND d.history_incarnation_id = i.incarnation_id)
          OR EXISTS (SELECT 1 FROM withdrawal_utxos w WHERE w.history_binding_digest = i.binding_digest AND w.history_incarnation_id = i.incarnation_id))
    ) SELECT 1 FROM orphans o WHERE
      EXISTS (SELECT 1 FROM pending_block_finalizations WHERE status NOT IN ('finalized', 'abandoned'))
      OR EXISTS (SELECT 1 FROM processed_mempool)
      OR EXISTS (SELECT 1 FROM deposits_utxos d WHERE d.history_binding_digest = ${binding} AND d.history_incarnation_id = o.incarnation_id AND (d.projected_header_hash IS NOT NULL OR d.status = 'finalized'))
      OR EXISTS (SELECT 1 FROM withdrawal_utxos w WHERE w.history_binding_digest = ${binding} AND w.history_incarnation_id = o.incarnation_id AND (w.projected_header_hash IS NOT NULL OR w.status = 'finalized'))
      OR EXISTS (SELECT 1 FROM pending_block_finalization_deposits d WHERE d.history_binding_digest = ${binding} AND d.history_incarnation_id = o.incarnation_id)
      OR EXISTS (SELECT 1 FROM pending_block_finalization_withdrawals w WHERE w.history_binding_digest = ${binding} AND w.history_incarnation_id = o.incarnation_id)
      LIMIT 1`;
    return assigned.length === 0
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

/** First repair slice: undo the complete, proved-unpublished acceptance overlay.
 * The source journal's orphan flags supply authority; inverse receipts supply
 * bytes. Published/possibly broadcast state and missing inverse evidence remain
 * fenced. The owner must drain producers and deferred writes before calling.
 */
export const repairUnpublishedHistoryLedger = (change: HistoryOwnerChange) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
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
    const orphans = yield* sql<{
      kind: string;
      event_id: Buffer;
      incarnation_id: Buffer;
    }>`
      SELECT i.kind, i.event_id, i.incarnation_id FROM event_history_incarnations i
      WHERE i.binding_digest = ${binding} AND NOT i.origin_canonical
        AND (EXISTS (SELECT 1 FROM deposits_utxos d WHERE d.history_binding_digest = i.binding_digest
          AND d.history_incarnation_id = i.incarnation_id)
          OR EXISTS (SELECT 1 FROM withdrawal_utxos w WHERE w.history_binding_digest = i.binding_digest
          AND w.history_incarnation_id = i.incarnation_id))
      ORDER BY i.incarnation_id FOR UPDATE`;
    if (orphans.length === 0) return;

    const pending = yield* sql`SELECT 1 FROM pending_block_finalizations
      WHERE status NOT IN ('finalized', 'abandoned') LIMIT 1`;
    const processed = yield* sql`SELECT 1 FROM processed_mempool LIMIT 1`;
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
      const assigned = yield* sql`SELECT 1 FROM ${sql(eventTable)}
        WHERE history_binding_digest = ${binding} AND history_incarnation_id = ${orphan.incarnation_id}
          AND (projected_header_hash IS NOT NULL OR status = 'finalized')
          AND (${retiredHeader}::bytea IS NULL OR projected_header_hash IS DISTINCT FROM ${retiredHeader}) LIMIT 1`;
      const memberships =
        yield* sql`SELECT 1 FROM pending_block_finalization_deposits
        WHERE history_binding_digest = ${binding} AND history_incarnation_id = ${orphan.incarnation_id}
          AND (${retiredHeader}::bytea IS NULL OR header_hash <> ${retiredHeader})
        UNION ALL SELECT 1 FROM pending_block_finalization_withdrawals
        WHERE history_binding_digest = ${binding} AND history_incarnation_id = ${orphan.incarnation_id}
          AND (${retiredHeader}::bytea IS NULL OR header_hash <> ${retiredHeader})`;
      if (assigned.length !== 0 || memberships.length !== 0)
        return yield* refuse(
          "Orphan admission has retained header membership requiring authenticated published correction",
        );
      const publishedDependency =
        yield* sql`SELECT 1 FROM event_history_l2_ledger_receipts r
        WHERE r.binding_digest = ${binding} AND r.reversed_at_revision IS NULL
          AND EXISTS (SELECT 1 FROM jsonb_populate_recordset(NULL::deposits_utxos, r.deposits_before) d
            WHERE d.history_binding_digest = ${binding} AND d.history_incarnation_id = ${orphan.incarnation_id})
          AND EXISTS (SELECT 1 FROM unnest(r.tx_ids) AS ids(tx_id)
            WHERE NOT EXISTS (SELECT 1 FROM mempool m WHERE m.tx_id = ids.tx_id)) LIMIT 1`;
      if (publishedDependency.length !== 0)
        return yield* refuse(
          "Orphan dependency already left the unpublished ledger overlay",
        );
    }

    const receipts = yield* sql<{
      sequence: string;
      tx_ids: Buffer[];
    }>`SELECT sequence::text, tx_ids
      FROM event_history_l2_ledger_receipts r WHERE binding_digest = ${binding}
        AND reversed_at_revision IS NULL AND EXISTS (SELECT 1 FROM mempool m WHERE m.tx_id = ANY(r.tx_ids))
      ORDER BY r.sequence DESC FOR UPDATE`;
    const uncovered = yield* sql`SELECT 1 FROM mempool m WHERE
      (SELECT count(*) FROM event_history_l2_ledger_receipts r WHERE binding_digest = ${binding}
        AND reversed_at_revision IS NULL AND m.tx_id = ANY(r.tx_ids)) <> 1 LIMIT 1`;
    if (uncovered.length !== 0)
      return yield* refuse(
        "Unpublished suffix lacks a unique complete inverse receipt",
      );

    for (const receipt of receipts) {
      const unsafe = yield* sql`SELECT 1 FROM event_history_l2_ledger_receipts r
        WHERE sequence = ${receipt.sequence} AND (ledger_after IS NULL OR
          EXISTS (SELECT 1 FROM unnest(r.tx_ids) AS ids(tx_id)
            LEFT JOIN mempool m ON m.tx_id = ids.tx_id
            LEFT JOIN tx_admissions a ON a.tx_id = ids.tx_id
            WHERE m.tx_id IS NULL OR a.status IS DISTINCT FROM 'accepted'
              OR EXISTS (SELECT 1 FROM blocks b WHERE b.tx_id = ids.tx_id)
              OR EXISTS (SELECT 1 FROM immutable i WHERE i.tx_id = ids.tx_id)
              OR EXISTS (SELECT 1 FROM pending_block_finalization_txs p WHERE p.member_id = ids.tx_id))
          OR jsonb_array_length(payloads_before) <> cardinality(tx_ids))`;
      if (unsafe.length !== 0)
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
          OR current.history_binding_digest IS DISTINCT FROM old.history_binding_digest
          OR current.history_incarnation_id IS DISTINCT FROM old.history_incarnation_id
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
      yield* sql`UPDATE event_history_l2_ledger_receipts SET reversed_at_revision = ${change.after.revision}
        WHERE sequence = ${receipt.sequence} AND reversed_at_revision IS NULL`;
    }
    if (retiredHeader !== null) {
      // Clear assignments only AFTER inverse receipts checked/restored their exact
      // preimages. Header membership rows and signed intent are never deleted.
      yield* sql`UPDATE deposits_utxos d SET projected_header_hash = NULL
        WHERE d.history_binding_digest = ${binding} AND d.projected_header_hash = ${retiredHeader}
          AND EXISTS (SELECT 1 FROM event_history_incarnations i
            WHERE i.binding_digest = d.history_binding_digest AND i.incarnation_id = d.history_incarnation_id
              AND NOT i.origin_canonical)`;
    }
    for (const orphan of orphans) {
      if (orphan.kind === "deposit") {
        const unsafe = yield* sql`SELECT 1 FROM deposits_utxos d
          WHERE history_binding_digest = ${binding} AND history_incarnation_id = ${orphan.incarnation_id}
            AND (status NOT IN ('awaiting', 'projected') OR projected_header_hash IS NOT NULL
              OR EXISTS (SELECT 1 FROM mempool_ledger l WHERE l.source_event_id = d.event_id AND l.output <> d.ledger_output))`;
        if (unsafe.length)
          return yield* refuse(
            "Orphan deposit remains dependent on published or unproven ledger state",
          );
        yield* sql`DELETE FROM mempool_ledger WHERE source_event_id = ${orphan.event_id}`;
        yield* sql`DELETE FROM deposits_utxos WHERE history_binding_digest = ${binding} AND history_incarnation_id = ${orphan.incarnation_id}`;
      } else {
        yield* sql`DELETE FROM withdrawal_utxos WHERE history_binding_digest = ${binding} AND history_incarnation_id = ${orphan.incarnation_id}`;
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
