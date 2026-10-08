import { sha256Hex } from "../../sha256.js";
import initialSchemaSql from "./sql/0001_initial_schema.sql";
import automaticSettlementSql from "./sql/0002_automatic_settlement.sql";
import operatorMembershipSql from "./sql/0003_operator_membership.sql";
import retainedScriptMaterialSql from "./sql/0004_retained_script_material.sql";
import foreignEventCensusSql from "./sql/0005_foreign_event_census.sql";
import foreignNativeAdoptionSql from "./sql/0006_foreign_native_adoption.sql";
import dropForeignTipReconciliationsSql from "./sql/0007_drop_foreign_tip_reconciliations.sql";
import followerAdmissionIdentitySql from "./sql/0008_follower_admission_identity.sql";
import landedBlocksSql from "./sql/0009_landed_blocks.sql";
import intentRefusalHoldsSql from "./sql/0010_intent_refusal_holds.sql";
import locallyAppliedBlockStatusSql from "./sql/0011_locally_applied_block_status.sql";
import receiptSettlementsSql from "./sql/0012_receipt_settlements.sql";
import mempoolInclusionMarksSql from "./sql/0013_mempool_inclusion_marks.sql";
import confirmedLedgerMergesSql from "./sql/0014_confirmed_ledger_merges.sql";
import settlementStatusDerivedSql from "./sql/0015_settlement_status_derived.sql";
import receiptRejectionsSql from "./sql/0016_receipt_rejections.sql";
import dropSettlementHoldSlotSql from "./sql/0017_drop_settlement_hold_slot.sql";
import landedBlocksOwnRemovedSql from "./sql/0018_landed_blocks_own_removed.sql";

export type Migration = {
  readonly version: number;
  readonly name: string;
  readonly checksumSha256: string;
  readonly sql: string;
  readonly transactional: true;
};

export const MIGRATIONS: readonly Migration[] = [
  {
    version: 1,
    name: "initial_schema",
    checksumSha256: sha256Hex(initialSchemaSql),
    sql: initialSchemaSql,
    transactional: true,
  },
  {
    version: 2,
    name: "automatic_settlement",
    checksumSha256: sha256Hex(automaticSettlementSql),
    sql: automaticSettlementSql,
    transactional: true,
  },
  {
    version: 3,
    name: "operator_membership",
    checksumSha256: sha256Hex(operatorMembershipSql),
    sql: operatorMembershipSql,
    transactional: true,
  },
  {
    version: 4,
    name: "retained_script_material",
    checksumSha256: sha256Hex(retainedScriptMaterialSql),
    sql: retainedScriptMaterialSql,
    transactional: true,
  },
  {
    version: 5,
    name: "foreign_event_census",
    checksumSha256: sha256Hex(foreignEventCensusSql),
    sql: foreignEventCensusSql,
    transactional: true,
  },
  {
    version: 6,
    name: "foreign_native_adoption",
    checksumSha256: sha256Hex(foreignNativeAdoptionSql),
    sql: foreignNativeAdoptionSql,
    transactional: true,
  },
  {
    version: 7,
    name: "drop_foreign_tip_reconciliations",
    checksumSha256: sha256Hex(dropForeignTipReconciliationsSql),
    sql: dropForeignTipReconciliationsSql,
    transactional: true,
  },
  {
    version: 8,
    name: "follower_admission_identity",
    checksumSha256: sha256Hex(followerAdmissionIdentitySql),
    sql: followerAdmissionIdentitySql,
    transactional: true,
  },
  {
    version: 9,
    name: "landed_blocks",
    checksumSha256: sha256Hex(landedBlocksSql),
    sql: landedBlocksSql,
    transactional: true,
  },
  {
    version: 10,
    name: "intent_refusal_holds",
    checksumSha256: sha256Hex(intentRefusalHoldsSql),
    sql: intentRefusalHoldsSql,
    transactional: true,
  },
  {
    version: 11,
    name: "locally_applied_block_status",
    checksumSha256: sha256Hex(locallyAppliedBlockStatusSql),
    sql: locallyAppliedBlockStatusSql,
    transactional: true,
  },
  {
    version: 12,
    name: "receipt_settlements",
    checksumSha256: sha256Hex(receiptSettlementsSql),
    sql: receiptSettlementsSql,
    transactional: true,
  },
  {
    version: 13,
    name: "mempool_inclusion_marks",
    checksumSha256: sha256Hex(mempoolInclusionMarksSql),
    sql: mempoolInclusionMarksSql,
    transactional: true,
  },
  {
    version: 14,
    name: "confirmed_ledger_merges",
    checksumSha256: sha256Hex(confirmedLedgerMergesSql),
    sql: confirmedLedgerMergesSql,
    transactional: true,
  },
  {
    version: 15,
    name: "settlement_status_derived",
    checksumSha256: sha256Hex(settlementStatusDerivedSql),
    sql: settlementStatusDerivedSql,
    transactional: true,
  },
  {
    version: 16,
    name: "receipt_rejections",
    checksumSha256: sha256Hex(receiptRejectionsSql),
    sql: receiptRejectionsSql,
    transactional: true,
  },
  {
    version: 17,
    name: "drop_settlement_hold_slot",
    checksumSha256: sha256Hex(dropSettlementHoldSlotSql),
    sql: dropSettlementHoldSlotSql,
    transactional: true,
  },
  {
    version: 18,
    name: "landed_blocks_own_removed",
    checksumSha256: sha256Hex(landedBlocksOwnRemovedSql),
    sql: landedBlocksOwnRemovedSql,
    transactional: true,
  },
] as const;

export const EXPECTED_SCHEMA_VERSION =
  MIGRATIONS[MIGRATIONS.length - 1]!.version;

const manifestHashOf = (migrations: readonly Migration[]): string =>
  sha256Hex(
    migrations
      .map(
        (migration) =>
          `${migration.version}:${migration.name}:${migration.checksumSha256}`,
      )
      .join("\n"),
  );

export const MIGRATION_MANIFEST_HASH = manifestHashOf(MIGRATIONS);

/**
 * The manifest hash an earlier release stamped, for each prefix of
 * MIGRATIONS, mapped to the last version that prefix covers. Rows keep the
 * hash of the release that applied them.
 */
export const MIGRATION_PREFIX_MANIFEST_HASHES: ReadonlyMap<string, number> =
  new Map(
    MIGRATIONS.map((migration, index) => [
      manifestHashOf(MIGRATIONS.slice(0, index + 1)),
      migration.version,
    ]),
  );

export const APPLICATION_TABLE_NAMES = [
  "operator_membership_observations",
  "settlement_jobs",
  "settlement_attempts",
  "settlement_owners",
  "address_history",
  "blocks",
  "cek_program_material_admission_owners",
  "cek_program_material_retained_state_owners",
  "cek_program_material_entries",
  "cek_program_material_memberships",
  "confirmed_ledger",
  "commit_build_calibration",
  "deposits_utxos",
  "forced_transaction_utxos",
  "withdrawal_utxos",
  "immutable",
  "mempool",
  "processed_mempool",
  "mempool_ledger",
  "mempool_tx_deltas",
  "mpf_engine_state",
  "tx_rejections",
  "pending_block_finalizations",
  "pending_block_finalization_deposits",
  "pending_block_finalization_forced_transactions",
  "pending_block_finalization_withdrawals",
  "pending_block_finalization_txs",
  "pending_block_finalization_transition_trace",
  "pending_block_finalization_event_to_step",
  "pending_block_finalization_validation_traces",
  "pending_block_finalization_validation_trace_witnesses",
  "tx_admissions",
  "tx_admission_payloads",
  "local_mutation_jobs",
  "state_queue_mutation_leases",
  "da_payloads",
  "da_payload_terminal_outcomes",
  "state_queue_terminal_observer_states",
  "da_payload_publications",
  "da_payload_announcements",
  "deposit_submission_attempts",
  "event_history_submissions",
  "event_history_submission_inputs",
  "event_history_authority",
  "event_history_replay_receipts",
  "event_history_cursor",
  "event_history_block_applications",
  "event_history_live_outputs",
  "event_history_incarnations",
  "event_history_l2_ledger_receipts",
  "event_history_l2_ledger_receipt_settlements",
  "event_history_l2_ledger_receipt_rejections",
  "event_history_recovery_plans",
  "follower_event_ingestion",
  "node_landed_blocks",
  "node_confirmed_ledger_frontier",
  "node_confirmed_merges",
  "node_confirmed_ledger_spent",
  "intent_refusal_holds",
] as const;

export const APPLICATION_INDEX_NAMES = [
  "settlement_jobs_due",
  "settlement_attempts_event",
  "settlement_attempts_open",
  "idx_address_history_created_at",
  "idx_blocks_header_hash",
  "idx_blocks_tx_id",
  "idx_confirmed_ledger_address",
  "idx_deposits_utxos_status_inclusion_time_event_id",
  "idx_deposits_utxos_projected_header_hash",
  "idx_deposits_utxos_deposit_l1_tx_hash",
  "idx_forced_transaction_utxos_status_inclusion_time_tx_order_id",
  "idx_forced_transaction_utxos_projected_header_hash",
  "idx_forced_transaction_utxos_tx_id",
  "idx_withdrawal_utxos_status_inclusion_time_event_id",
  "idx_withdrawal_utxos_projected_header_hash",
  "idx_withdrawal_utxos_withdrawal_l1_tx_hash",
  "idx_withdrawal_utxos_l2_outref",
  "idx_immutable_time_stamp_tz",
  "idx_mempool_time_stamp_tz_tx_id",
  "idx_processed_mempool_time_stamp_tz_tx_id",
  "idx_mempool_ledger_address",
  "uniq_mempool_ledger_source_event_id",
  "idx_tx_rejections_tx_id",
  "idx_tx_rejections_created_at",
  "uniq_pending_block_finalizations_single_active",
  "idx_pending_block_finalizations_status",
  "idx_tx_admissions_lease",
  "idx_tx_admissions_active_lease",
  "idx_tx_admissions_queued_arrival",
  "uniq_tx_rejections_tx_id",
  "idx_local_mutation_jobs_status_updated",
  "uniq_state_queue_mutation_leases_active_scope",
  "idx_state_queue_mutation_leases_status_updated",
  "idx_da_payloads_created_at",
  "idx_da_payload_terminal_outcomes_authority",
  "idx_da_payload_publications_retry",
  "idx_da_payload_announcements_retry",
  "idx_deposit_submission_attempts_deposit_event_id",
  "idx_deposit_submission_attempts_status_submitted_at",
  "uniq_event_history_canonical_block",
  "uniq_event_history_canonical_height",
  "uniq_event_history_incarnation_event",
  "uniq_event_history_canonical_event",
  "uniq_event_history_canonical_key",
  "idx_event_history_incarnations_orphans",
  "idx_deposits_utxos_l1_admission",
  "idx_withdrawal_utxos_l1_admission",
  "idx_withdrawal_utxos_l1_admission_tx",
  "event_history_l2_ledger_receipts_unreversed",
  "event_history_recovery_plans_prepared",
  "uniq_node_landed_blocks_processed_parent",
  "idx_receipt_settlements_settled_by",
  "idx_mempool_included_by",
  "idx_processed_mempool_included_by",
  "idx_node_confirmed_merges_merge_slot",
] as const;

export const migrationByVersion = new Map(
  MIGRATIONS.map((migration) => [migration.version, migration]),
);
