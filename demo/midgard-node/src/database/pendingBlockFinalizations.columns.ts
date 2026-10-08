import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION } from "@al-ft/midgard-core/deployment-manifest-identity";

import * as WithdrawalsDB from "./withdrawals.js";

export const tableName = "pending_block_finalizations";

export const depositsTableName = "pending_block_finalization_deposits";

export const forcedTransactionsTableName =
  "pending_block_finalization_forced_transactions";

export const withdrawalsTableName = "pending_block_finalization_withdrawals";

export const txsTableName = "pending_block_finalization_txs";

export const transitionTraceTableName =
  "pending_block_finalization_transition_trace";

export const eventToStepTableName = "pending_block_finalization_event_to_step";

export const validationTracesTableName =
  "pending_block_finalization_validation_traces";

export const validationTraceWitnessesTableName =
  "pending_block_finalization_validation_trace_witnesses";

export enum Columns {
  HEADER_HASH = "header_hash",
  HEADER_CBOR = "header_cbor",
  FORMAT_VERSION = "format_version",
  REPLAY_KIND = "replay_kind",
  DEPLOYMENT_MARKER_SCHEMA_VERSION = "deployment_marker_schema_version",
  DEPLOYMENT_MANIFEST_ID = "deployment_manifest_id",
  CONSENSUS_PROFILE_ID = "consensus_profile_id",
  SUBMITTED_TX_HASH = "submitted_tx_hash",
  PREPARED_TX_HASH = "prepared_tx_hash",
  INTENDED_TX_HASH = "intended_tx_hash",
  SIGNED_TX_CBOR = "signed_tx_cbor",
  CORRECTION_TRANSITION_DIGEST = "correction_transition_digest",
  STATE_QUEUE_LEASE_TOKEN = "state_queue_lease_token",
  BASE_SNAPSHOT_ID = "base_snapshot_id",
  BASE_TAIL_OUT_REF = "base_tail_out_ref",
  BASE_TAIL_HEADER_HASH = "base_tail_header_hash",
  BASE_TAIL_DATUM_CBOR = "base_tail_datum_cbor",
  BASE_UTXOS_ROOT = "base_utxos_root",
  BASE_FORCED_TRANSACTIONS_ROOT = "base_forced_transactions_root",
  BASE_TRANSACTIONS_ROOT = "base_transactions_root",
  BASE_DEPOSITS_ROOT = "base_deposits_root",
  BASE_WITHDRAWALS_ROOT = "base_withdrawals_root",
  BLOCK_START_TIME = "block_start_time",
  BLOCK_END_TIME = "block_end_time",
  EXPECTED_UTXOS_ROOT = "expected_utxos_root",
  EXPECTED_FORCED_TRANSACTIONS_ROOT = "expected_forced_transactions_root",
  EXPECTED_TRANSACTIONS_ROOT = "expected_transactions_root",
  EXPECTED_DEPOSITS_ROOT = "expected_deposits_root",
  EXPECTED_WITHDRAWALS_ROOT = "expected_withdrawals_root",
  EXPECTED_TRANSITION_TRACE_ROOT = "expected_transition_trace_root",
  EXPECTED_EVENT_TO_STEP_ROOT = "expected_event_to_step_root",
  EXPECTED_VALIDATION_TRACES_ROOT = "expected_validation_traces_root",
  EXPECTED_WITHDRAWAL_COUNT = "expected_withdrawal_count",
  EXPECTED_FORCED_TRANSACTION_COUNT = "expected_forced_transaction_count",
  EXPECTED_L2_TRANSACTION_COUNT = "expected_l2_transaction_count",
  EXPECTED_DEPOSIT_COUNT = "expected_deposit_count",
  EXPECTED_TOTAL_EVENT_COUNT = "expected_total_event_count",
  EXPECTED_TRANSITION_STEP_COUNT = "expected_transition_step_count",
  EXPECTED_VALIDATION_TRACE_COUNT = "expected_validation_trace_count",
  LEDGER_DELTA_SPENT = "ledger_delta_spent",
  LEDGER_DELTA_PRODUCED = "ledger_delta_produced",
  UTXO_PAYLOAD_ENTRY_COUNT = "utxo_payload_entry_count",
  UTXO_PAYLOAD_ENCODED_TUPLE_BYTES = "utxo_payload_encoded_tuple_bytes",
  MPF_OWNER_SCHEMA = "mpf_owner_schema",
  MPF_OWNER_BINARY_SHA256 = "mpf_owner_binary_sha256",
  MPF_REPLAY_BASE_ROOT = "mpf_replay_base_root",
  MPF_REPLAY_CANDIDATE_ROOT = "mpf_replay_candidate_root",
  MPF_REPLAY_EVENT_LOG = "mpf_replay_event_log",
  MPF_REPLAY_EVENT_LOG_DIGEST = "mpf_replay_event_log_digest",
  MPF_REPLAY_EVENT_ROOTS = "mpf_replay_event_roots",
  MPF_REPLAY_EVENT_COUNT = "mpf_replay_event_count",
  STATUS = "status",
  OBSERVED_CONFIRMED_AT_MS = "observed_confirmed_at_ms",
  CREATED_AT = "created_at",
  UPDATED_AT = "updated_at",
}

export enum MemberColumns {
  HEADER_HASH = "header_hash",
  MEMBER_ID = "member_id",
  ORDINAL = "ordinal",
  PAYLOAD_CBOR = "payload_cbor",
  PAYLOAD_SHA256 = "payload_sha256",
  CEK_PROGRAM_MATERIAL_SIDECAR_CBOR = "cek_program_material_sidecar_cbor",
  CEK_PROGRAM_MATERIAL_SIDECAR_SHA256 = "cek_program_material_sidecar_sha256",
  SOURCE_TABLE = "source_table",
  SOURCE_ID = "source_id",
  SOURCE_TIMESTAMP = "source_time_stamp_tz",
}

export enum WithdrawalMemberColumns {
  CLASSIFICATION_REVISION = "classification_revision",
  VALIDITY = "validity",
  VALIDITY_DETAIL = "validity_detail",
  CLASSIFICATION_SHA256 = "classification_sha256",
}

export enum UtxoColumns {
  OUTREF = "outref",
  OUTPUT = "output",
}

export const Status = {
  PendingSubmission: "pending_submission",
  SubmittedLocalFinalizationPending: "submitted_local_finalization_pending",
  SubmittedUnconfirmed: "submitted_unconfirmed",
  ObservedWaitingStability: "observed_waiting_stability",
  /**
   * The node applied the block locally (plan §13.1: formerly `finalized`).
   * Whether it is final on L1 is derived from the follower's facts.
   */
  LocallyApplied: "locally_applied",
  Abandoned: "abandoned",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export const ACTIVE_STATUSES: readonly Status[] = [
  Status.PendingSubmission,
  Status.SubmittedLocalFinalizationPending,
  Status.SubmittedUnconfirmed,
  Status.ObservedWaitingStability,
];

export type Row = {
  [Columns.HEADER_HASH]: Buffer;
  [Columns.HEADER_CBOR]: Buffer;
  [Columns.FORMAT_VERSION]: typeof PENDING_BLOCK_FINALIZATION_VERSION;
  [Columns.REPLAY_KIND]: PendingBlockFinalizationReplayKind;
  [Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION]: typeof MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION;
  [Columns.DEPLOYMENT_MANIFEST_ID]: string;
  [Columns.CONSENSUS_PROFILE_ID]: typeof MIDGARD_CONSENSUS_PROFILE_ID;
  [Columns.SUBMITTED_TX_HASH]: Buffer | null;
  [Columns.PREPARED_TX_HASH]?: Buffer | null;
  [Columns.INTENDED_TX_HASH]?: Buffer | null;
  [Columns.SIGNED_TX_CBOR]?: Buffer | null;
  [Columns.CORRECTION_TRANSITION_DIGEST]?: string | null;
  [Columns.STATE_QUEUE_LEASE_TOKEN]: string;
  [Columns.BASE_SNAPSHOT_ID]: string;
  [Columns.BASE_TAIL_OUT_REF]: string;
  [Columns.BASE_TAIL_HEADER_HASH]: Buffer;
  [Columns.BASE_TAIL_DATUM_CBOR]: string;
  [Columns.BASE_UTXOS_ROOT]: string;
  [Columns.BASE_FORCED_TRANSACTIONS_ROOT]: string;
  [Columns.BASE_TRANSACTIONS_ROOT]: string;
  [Columns.BASE_DEPOSITS_ROOT]: string;
  [Columns.BASE_WITHDRAWALS_ROOT]: string;
  [Columns.BLOCK_START_TIME]: Date;
  [Columns.BLOCK_END_TIME]: Date;
  [Columns.EXPECTED_UTXOS_ROOT]: string;
  [Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT]: string;
  [Columns.EXPECTED_TRANSACTIONS_ROOT]: string;
  [Columns.EXPECTED_DEPOSITS_ROOT]: string;
  [Columns.EXPECTED_WITHDRAWALS_ROOT]: string;
  [Columns.EXPECTED_TRANSITION_TRACE_ROOT]: string;
  [Columns.EXPECTED_EVENT_TO_STEP_ROOT]: string;
  [Columns.EXPECTED_VALIDATION_TRACES_ROOT]: string;
  [Columns.EXPECTED_WITHDRAWAL_COUNT]: bigint;
  [Columns.EXPECTED_FORCED_TRANSACTION_COUNT]: bigint;
  [Columns.EXPECTED_L2_TRANSACTION_COUNT]: bigint;
  [Columns.EXPECTED_DEPOSIT_COUNT]: bigint;
  [Columns.EXPECTED_TOTAL_EVENT_COUNT]: bigint;
  [Columns.EXPECTED_TRANSITION_STEP_COUNT]: bigint;
  [Columns.EXPECTED_VALIDATION_TRACE_COUNT]: bigint;
  [Columns.LEDGER_DELTA_SPENT]: unknown;
  [Columns.LEDGER_DELTA_PRODUCED]: unknown;
  [Columns.UTXO_PAYLOAD_ENTRY_COUNT]?: number | null;
  [Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES]?: number | null;
  [Columns.MPF_OWNER_SCHEMA]?: number | null;
  [Columns.MPF_OWNER_BINARY_SHA256]?: Buffer | null;
  [Columns.MPF_REPLAY_BASE_ROOT]?: Buffer | null;
  [Columns.MPF_REPLAY_CANDIDATE_ROOT]?: Buffer | null;
  [Columns.MPF_REPLAY_EVENT_LOG]?: Buffer | null;
  [Columns.MPF_REPLAY_EVENT_LOG_DIGEST]?: Buffer | null;
  [Columns.MPF_REPLAY_EVENT_ROOTS]?: Buffer | null;
  [Columns.MPF_REPLAY_EVENT_COUNT]?: number | null;
  [Columns.STATUS]: Status;
  [Columns.OBSERVED_CONFIRMED_AT_MS]: bigint | null;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
};

type PgBigInt = bigint | number | string;

export type RawRow = Omit<
  Row,
  | Columns.EXPECTED_WITHDRAWAL_COUNT
  | Columns.EXPECTED_FORCED_TRANSACTION_COUNT
  | Columns.EXPECTED_L2_TRANSACTION_COUNT
  | Columns.EXPECTED_DEPOSIT_COUNT
  | Columns.EXPECTED_TOTAL_EVENT_COUNT
  | Columns.EXPECTED_TRANSITION_STEP_COUNT
  | Columns.EXPECTED_VALIDATION_TRACE_COUNT
  | Columns.UTXO_PAYLOAD_ENTRY_COUNT
  | Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES
  | Columns.MPF_OWNER_SCHEMA
  | Columns.MPF_REPLAY_EVENT_COUNT
  | Columns.OBSERVED_CONFIRMED_AT_MS
> & {
  [Columns.EXPECTED_WITHDRAWAL_COUNT]: PgBigInt;
  [Columns.EXPECTED_FORCED_TRANSACTION_COUNT]: PgBigInt;
  [Columns.EXPECTED_L2_TRANSACTION_COUNT]: PgBigInt;
  [Columns.EXPECTED_DEPOSIT_COUNT]: PgBigInt;
  [Columns.EXPECTED_TOTAL_EVENT_COUNT]: PgBigInt;
  [Columns.EXPECTED_TRANSITION_STEP_COUNT]: PgBigInt;
  [Columns.EXPECTED_VALIDATION_TRACE_COUNT]: PgBigInt;
  [Columns.UTXO_PAYLOAD_ENTRY_COUNT]?: PgBigInt | null;
  [Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES]?: PgBigInt | null;
  [Columns.MPF_OWNER_SCHEMA]?: PgBigInt | null;
  [Columns.MPF_REPLAY_EVENT_COUNT]?: PgBigInt | null;
  [Columns.OBSERVED_CONFIRMED_AT_MS]: PgBigInt | null;
};

export type MemberRecord = {
  /** The follower admission identity of a deposit or withdrawal member. */
  readonly l1_event_key?: Buffer | null;
  readonly l1_origin_outref?: Buffer | null;
  [MemberColumns.HEADER_HASH]: Buffer;
  [MemberColumns.MEMBER_ID]: Buffer;
  [MemberColumns.ORDINAL]: number;
  [MemberColumns.PAYLOAD_CBOR]: Buffer;
  [MemberColumns.PAYLOAD_SHA256]: Buffer;
  [MemberColumns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]?: Buffer | null;
  [MemberColumns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]?: Buffer | null;
  [MemberColumns.SOURCE_TABLE]: string;
  [MemberColumns.SOURCE_ID]: Buffer;
  [MemberColumns.SOURCE_TIMESTAMP]: Date;
};

export type WithdrawalMemberRecord = MemberRecord & {
  [WithdrawalMemberColumns.CLASSIFICATION_REVISION]: number;
  [WithdrawalMemberColumns.VALIDITY]: WithdrawalsDB.Validity;
  [WithdrawalMemberColumns.VALIDITY_DETAIL]: unknown;
  [WithdrawalMemberColumns.CLASSIFICATION_SHA256]: Buffer;
};

export type UtxoInput = {
  [UtxoColumns.OUTREF]: Buffer;
  [UtxoColumns.OUTPUT]: Buffer;
};

export type RetainedRootMemberInput = {
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
};

const toBigInt = (value: PgBigInt): bigint =>
  typeof value === "bigint" ? value : BigInt(value);

const toSafeNumber = (value: PgBigInt | null | undefined): number | null => {
  if (value == null) return null;
  const normalized = Number(value);
  if (!Number.isSafeInteger(normalized) || normalized < 0) {
    throw new Error(
      `Database aggregate is not a non-negative safe integer: ${String(value)}`,
    );
  }
  return normalized;
};

export const normalizeRow = (row: RawRow): Row => ({
  ...row,
  [Columns.EXPECTED_WITHDRAWAL_COUNT]: toBigInt(
    row[Columns.EXPECTED_WITHDRAWAL_COUNT],
  ),
  [Columns.EXPECTED_FORCED_TRANSACTION_COUNT]: toBigInt(
    row[Columns.EXPECTED_FORCED_TRANSACTION_COUNT],
  ),
  [Columns.EXPECTED_L2_TRANSACTION_COUNT]: toBigInt(
    row[Columns.EXPECTED_L2_TRANSACTION_COUNT],
  ),
  [Columns.EXPECTED_DEPOSIT_COUNT]: toBigInt(
    row[Columns.EXPECTED_DEPOSIT_COUNT],
  ),
  [Columns.EXPECTED_TOTAL_EVENT_COUNT]: toBigInt(
    row[Columns.EXPECTED_TOTAL_EVENT_COUNT],
  ),
  [Columns.EXPECTED_TRANSITION_STEP_COUNT]: toBigInt(
    row[Columns.EXPECTED_TRANSITION_STEP_COUNT],
  ),
  [Columns.EXPECTED_VALIDATION_TRACE_COUNT]: toBigInt(
    row[Columns.EXPECTED_VALIDATION_TRACE_COUNT],
  ),
  [Columns.UTXO_PAYLOAD_ENTRY_COUNT]: toSafeNumber(
    row[Columns.UTXO_PAYLOAD_ENTRY_COUNT],
  ),
  [Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES]: toSafeNumber(
    row[Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES],
  ),
  [Columns.MPF_OWNER_SCHEMA]: toSafeNumber(row[Columns.MPF_OWNER_SCHEMA]),
  [Columns.MPF_REPLAY_EVENT_COUNT]: toSafeNumber(
    row[Columns.MPF_REPLAY_EVENT_COUNT],
  ),
  [Columns.OBSERVED_CONFIRMED_AT_MS]:
    row[Columns.OBSERVED_CONFIRMED_AT_MS] === null
      ? null
      : toBigInt(row[Columns.OBSERVED_CONFIRMED_AT_MS]),
});

export const PENDING_BLOCK_FINALIZATION_VERSION = 1 as const;

export const PendingBlockFinalizationReplayKind = {
  LedgerDelta: "ledger_delta_v1",
  LedgerDeltaWithNativeMpf: "ledger_delta_native_mpf_v1",
} as const;

export type PendingBlockFinalizationReplayKind =
  (typeof PendingBlockFinalizationReplayKind)[keyof typeof PendingBlockFinalizationReplayKind];
