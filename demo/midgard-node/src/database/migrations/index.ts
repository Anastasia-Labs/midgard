import { createHash } from "node:crypto";

import initialSchemaSql from "./sql/0001_initial_schema.sql";
import durableTxAdmissionsSql from "./sql/0002_durable_tx_admissions.sql";
import localMutationJobsSql from "./sql/0003_local_mutation_jobs.sql";
import withdrawalEventsSql from "./sql/0004_withdrawal_events.sql";
import pendingFinalizationJournalPayloadsSql from "./sql/0005_pending_finalization_journal_payloads.sql";
import stateQueueMutationLeasesSql from "./sql/0006_state_queue_mutation_leases.sql";
import daPayloadsSql from "./sql/0007_da_payloads.sql";
import pendingFinalizationUtxoPayloadsSql from "./sql/0008_pending_finalization_utxo_payloads.sql";
import depositSubmissionAttemptsSql from "./sql/0009_deposit_submission_attempts.sql";

export type Migration = {
  readonly version: number;
  readonly name: string;
  readonly checksumSha256: string;
  readonly sql: string;
  readonly transactional: true;
};

const sha256Hex = (value: string): string =>
  createHash("sha256").update(value, "utf8").digest("hex");

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
    name: "durable_tx_admissions",
    checksumSha256: sha256Hex(durableTxAdmissionsSql),
    sql: durableTxAdmissionsSql,
    transactional: true,
  },
  {
    version: 3,
    name: "local_mutation_jobs",
    checksumSha256: sha256Hex(localMutationJobsSql),
    sql: localMutationJobsSql,
    transactional: true,
  },
  {
    version: 4,
    name: "withdrawal_events",
    checksumSha256: sha256Hex(withdrawalEventsSql),
    sql: withdrawalEventsSql,
    transactional: true,
  },
  {
    version: 5,
    name: "pending_finalization_journal_payloads",
    checksumSha256: sha256Hex(pendingFinalizationJournalPayloadsSql),
    sql: pendingFinalizationJournalPayloadsSql,
    transactional: true,
  },
  {
    version: 6,
    name: "state_queue_mutation_leases",
    checksumSha256: sha256Hex(stateQueueMutationLeasesSql),
    sql: stateQueueMutationLeasesSql,
    transactional: true,
  },
  {
    version: 7,
    name: "da_payloads",
    checksumSha256: sha256Hex(daPayloadsSql),
    sql: daPayloadsSql,
    transactional: true,
  },
  {
    version: 8,
    name: "pending_finalization_utxo_payloads",
    checksumSha256: sha256Hex(pendingFinalizationUtxoPayloadsSql),
    sql: pendingFinalizationUtxoPayloadsSql,
    transactional: true,
  },
  {
    version: 9,
    name: "deposit_submission_attempts",
    checksumSha256: sha256Hex(depositSubmissionAttemptsSql),
    sql: depositSubmissionAttemptsSql,
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
  "confirmed_ledger",
  "latest_ledger",
  "deposits_utxos",
  "withdrawal_utxos",
  "immutable",
  "mempool",
  "processed_mempool",
  "mempool_ledger",
  "mempool_tx_deltas",
  "tx_rejections",
  "deposit_ingestion_cursor",
  "pending_block_finalizations",
  "pending_block_finalization_deposits",
  "pending_block_finalization_withdrawals",
  "pending_block_finalization_txs",
  "pending_block_finalization_utxos",
  "tx_admissions",
  "local_mutation_jobs",
  "state_queue_mutation_leases",
  "da_payloads",
  "deposit_submission_attempts",
] as const;

export const APPLICATION_INDEX_NAMES = [
  "idx_address_history_created_at",
  "idx_blocks_header_hash",
  "idx_blocks_tx_id",
  "idx_confirmed_ledger_address",
  "idx_latest_ledger_address",
  "idx_deposits_utxos_status_inclusion_time_event_id",
  "idx_deposits_utxos_projected_header_hash",
  "idx_deposits_utxos_deposit_l1_tx_hash",
  "idx_withdrawal_utxos_status_inclusion_time_event_id",
  "idx_withdrawal_utxos_projected_header_hash",
  "idx_withdrawal_utxos_withdrawal_l1_tx_hash",
  "idx_withdrawal_utxos_l2_outref",
  "idx_immutable_time_stamp_tz",
  "idx_mempool_time_stamp_tz",
  "idx_processed_mempool_time_stamp_tz",
  "idx_mempool_ledger_address",
  "idx_mempool_ledger_source_event_id",
  "uniq_mempool_ledger_source_event_id",
  "idx_tx_rejections_tx_id",
  "idx_tx_rejections_created_at",
  "uniq_pending_block_finalizations_single_active",
  "idx_pending_block_finalizations_status",
  "idx_tx_admissions_dequeue",
  "idx_tx_admissions_status_updated",
  "idx_tx_admissions_lease",
  "uniq_tx_rejections_tx_id",
  "idx_local_mutation_jobs_status_updated",
  "uniq_state_queue_mutation_leases_active_scope",
  "idx_state_queue_mutation_leases_status_updated",
  "idx_da_payloads_created_at",
  "idx_deposit_submission_attempts_deposit_event_id",
  "idx_deposit_submission_attempts_status_submitted_at",
] as const;

export const migrationByVersion = new Map(
  MIGRATIONS.map((migration) => [migration.version, migration]),
);
