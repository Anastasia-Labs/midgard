import { sha256Hex } from "@/sha256.js";

import initialSchemaSql from "./sql/0001_initial_schema.sql";

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
] as const;

export const EXPECTED_SCHEMA_VERSION =
  MIGRATIONS[MIGRATIONS.length - 1]!.version;

export const MIGRATION_MANIFEST_HASH = sha256Hex(
  MIGRATIONS.map(
    (migration) =>
      `${migration.version}:${migration.name}:${migration.checksumSha256}`,
  ).join("\n"),
);

export const APPLICATION_TABLE_NAMES = [
  "address_history",
  "blocks",
  "cek_program_material_admission_owners",
  "cek_program_material_entries",
  "cek_program_material_memberships",
  "confirmed_ledger",
  "commit_build_calibration",
  "deposits_utxos",
  "forced_transaction_utxos",
  "foreign_tip_reconciliations",
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
  "tx_admissions",
  "tx_admission_payloads",
  "local_mutation_jobs",
  "state_queue_mutation_leases",
  "da_payloads",
  "da_payload_publications",
  "da_payload_announcements",
  "deposit_submission_attempts",
] as const;

export const APPLICATION_INDEX_NAMES = [
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
  "idx_foreign_tip_reconciliations_status_updated",
  "idx_foreign_tip_reconciliations_window",
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
  "idx_da_payload_publications_retry",
  "idx_da_payload_announcements_retry",
  "idx_deposit_submission_attempts_deposit_event_id",
  "idx_deposit_submission_attempts_status_submitted_at",
] as const;

export const migrationByVersion = new Map(
  MIGRATIONS.map((migration) => [migration.version, migration]),
);
