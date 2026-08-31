import { decodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE_V1_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  type DeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
  parseDeploymentMarkerV1,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import * as DaPayloadsDB from "@/database/daPayloads.js";
import * as DepositsDB from "@/database/deposits.js";
import * as ForcedTransactionsDB from "@/database/forcedTransactions.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { exactRecord } from "@/database/utils/exact-record.js";
import * as TxTable from "@/database/utils/tx.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { Database } from "@/services/database.js";
import { sha256 } from "@/sha256.js";

export const tableName = "pending_block_finalizations";
const depositsTableName = "pending_block_finalization_deposits";
const forcedTransactionsTableName =
  "pending_block_finalization_forced_transactions";
const withdrawalsTableName = "pending_block_finalization_withdrawals";
const txsTableName = "pending_block_finalization_txs";
const transitionTraceTableName = "pending_block_finalization_transition_trace";
const eventToStepTableName = "pending_block_finalization_event_to_step";
const validationTracesTableName =
  "pending_block_finalization_validation_traces";

export enum Columns {
  HEADER_HASH = "header_hash",
  HEADER_CBOR = "header_cbor",
  FORMAT_VERSION = "format_version",
  REPLAY_KIND = "replay_kind",
  DEPLOYMENT_MARKER_SCHEMA_VERSION = "deployment_marker_schema_version",
  DEPLOYMENT_MANIFEST_ID = "deployment_manifest_id",
  CONSENSUS_PROFILE_ID = "consensus_profile_id",
  SUBMITTED_TX_HASH = "submitted_tx_hash",
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

export enum UtxoColumns {
  OUTREF = "outref",
  OUTPUT = "output",
}

export const Status = {
  PendingSubmission: "pending_submission",
  SubmittedLocalFinalizationPending: "submitted_local_finalization_pending",
  SubmittedUnconfirmed: "submitted_unconfirmed",
  ObservedWaitingStability: "observed_waiting_stability",
  Finalized: "finalized",
  Abandoned: "abandoned",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

const ACTIVE_STATUSES: readonly Status[] = [
  Status.PendingSubmission,
  Status.SubmittedLocalFinalizationPending,
  Status.SubmittedUnconfirmed,
  Status.ObservedWaitingStability,
];

export type Row = {
  [Columns.HEADER_HASH]: Buffer;
  [Columns.HEADER_CBOR]: Buffer;
  [Columns.FORMAT_VERSION]: typeof PENDING_BLOCK_FINALIZATION_V1_VERSION;
  [Columns.REPLAY_KIND]: PendingBlockFinalizationReplayKindV1;
  [Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION]: typeof MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION;
  [Columns.DEPLOYMENT_MANIFEST_ID]: string;
  [Columns.CONSENSUS_PROFILE_ID]: typeof MIDGARD_CONSENSUS_PROFILE_V1_ID;
  [Columns.SUBMITTED_TX_HASH]: Buffer | null;
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

type RawRow = Omit<
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

const normalizeRow = (row: RawRow): Row => ({
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

export type Record = Row & {
  readonly depositEventIds: readonly Buffer[];
  readonly forcedTransactionEventIds: readonly Buffer[];
  readonly withdrawalEventIds: readonly Buffer[];
  readonly mempoolTxIds: readonly Buffer[];
  readonly depositMembers: readonly MemberRecord[];
  readonly forcedTransactionMembers: readonly MemberRecord[];
  readonly withdrawalMembers: readonly MemberRecord[];
  readonly txMembers: readonly MemberRecord[];
  readonly transitionTraceMembers: readonly MemberRecord[];
  readonly eventToStepMembers: readonly MemberRecord[];
  readonly validationTraceMembers: readonly MemberRecord[];
  readonly ledgerDelta: LedgerDeltaInput;
  readonly utxoPayloadAggregate?: UtxoPayloadSizeAggregate;
  readonly nativeMpfReplay?: NativeMpfReplayInput;
};

export type UtxoPayloadSizeAggregate = {
  readonly entryCount: number;
  readonly encodedTupleBytes: number;
};

export type LedgerDeltaInput = {
  readonly spent: readonly Buffer[];
  readonly produced: readonly UtxoInput[];
};

export type NativeMpfReplayInput = {
  readonly schema: 1;
  readonly ownerBinarySha256: Buffer;
  readonly baseRoot: Buffer;
  readonly candidateRoot: Buffer;
  readonly eventLog: Buffer;
  readonly eventLogDigest: Buffer;
  readonly eventRoots: Buffer;
  readonly eventCount: number;
};

export const PENDING_BLOCK_FINALIZATION_V1_VERSION = 1 as const;

export const PendingBlockFinalizationReplayKindV1 = {
  LedgerDelta: "ledger_delta_v1",
  LedgerDeltaWithNativeMpf: "ledger_delta_native_mpf_v1",
} as const;

export type PendingBlockFinalizationReplayKindV1 =
  (typeof PendingBlockFinalizationReplayKindV1)[keyof typeof PendingBlockFinalizationReplayKindV1];

export type PendingBlockFinalizationMetadata = {
  readonly deploymentMarker: DeploymentMarkerV1;
  readonly consensusProfileId: typeof MIDGARD_CONSENSUS_PROFILE_V1_ID;
  readonly stateQueueLeaseToken: string;
  readonly baseSnapshotId: string;
  readonly baseTailOutRef: string;
  readonly baseTailHeaderHash: Buffer;
  readonly baseTailDatumCbor: string;
  readonly baseRoots: {
    readonly utxosRoot: string;
    readonly forcedTransactionsRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
  };
  readonly blockStartTime: Date;
  readonly expectedRoots: {
    readonly utxosRoot: string;
    readonly forcedTransactionsRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
    readonly transitionTraceRoot: string;
    readonly eventToStepRoot: string;
    readonly validationTracesRoot: string;
  };
  readonly expectedCounts: {
    readonly withdrawalCount: bigint;
    readonly forcedTransactionCount: bigint;
    readonly l2TransactionCount: bigint;
    readonly depositCount: bigint;
    readonly totalEventCount: bigint;
    readonly transitionStepCount: bigint;
    readonly validationTraceCount: bigint;
  };
};

export type PendingBlockFinalizationV1 = {
  readonly version: typeof PENDING_BLOCK_FINALIZATION_V1_VERSION;
  readonly metadata: PendingBlockFinalizationMetadata;
  readonly replay:
    | {
        readonly kind: typeof PendingBlockFinalizationReplayKindV1.LedgerDelta;
        readonly ledgerDelta: LedgerDeltaInput;
      }
    | {
        readonly kind: typeof PendingBlockFinalizationReplayKindV1.LedgerDeltaWithNativeMpf;
        readonly ledgerDelta: LedgerDeltaInput;
        readonly nativeMpfReplay: NativeMpfReplayInput;
      };
};

export type PrepareInput = {
  readonly headerHash: Buffer;
  readonly headerCbor: Buffer;
  readonly metadata: PendingBlockFinalizationMetadata;
  readonly blockEndTime: Date;
  readonly depositEventIds: readonly Buffer[];
  readonly depositEntries: readonly DepositsDB.Entry[];
  readonly forcedTransactionEventIds: readonly Buffer[];
  readonly forcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly withdrawalEventIds: readonly Buffer[];
  readonly withdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly mempoolTxIds: readonly Buffer[];
  readonly mempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxProgramMaterialSidecars?: readonly {
    readonly txId: Buffer;
    readonly sidecarCbor: Buffer;
  }[];
  readonly mempoolTxSourceTable: string;
  readonly transitionTraceMembers: readonly RetainedRootMemberInput[];
  readonly eventToStepMembers: readonly RetainedRootMemberInput[];
  readonly validationTraceMembers: readonly RetainedRootMemberInput[];
  readonly ledgerDelta: LedgerDeltaInput;
  readonly utxoPayloadAggregate?: UtxoPayloadSizeAggregate;
  readonly nativeMpfReplay?: NativeMpfReplayInput;
};

const exactBytes = (value: unknown, label: string, length?: number): Buffer => {
  if (
    !(value instanceof Uint8Array) ||
    (length === undefined ? value.length === 0 : value.length !== length)
  ) {
    throw new Error(
      length === undefined
        ? `${label} must be non-empty bytes`
        : `${label} must contain exactly ${length.toString()} bytes`,
    );
  }
  return Buffer.from(value);
};

const exactNonEmptyString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`${label} must be a non-empty string`);
  }
  return value;
};

const exactHex = (value: unknown, bytes: number, label: string): string => {
  if (
    typeof value !== "string" ||
    !new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)
  ) {
    throw new Error(
      `${label} must be exactly ${bytes.toString()} lowercase hex bytes`,
    );
  }
  return value;
};

const exactDate = (value: unknown, label: string): Date => {
  if (!(value instanceof Date) || !Number.isFinite(value.getTime())) {
    throw new Error(`${label} must be a valid Date`);
  }
  return new Date(value.getTime());
};

const exactNonNegativeBigInt = (value: unknown, label: string): bigint => {
  if (typeof value !== "bigint" || value < 0n) {
    throw new Error(`${label} must be a non-negative bigint`);
  }
  return value;
};

const parseLedgerDeltaV1 = (value: unknown): LedgerDeltaInput => {
  const candidate = exactRecord(
    value,
    ["spent", "produced"],
    "PendingBlockFinalizationV1 ledgerDelta",
  );
  if (!Array.isArray(candidate.spent)) {
    throw new Error(
      "PendingBlockFinalizationV1 ledgerDelta.spent must be an array",
    );
  }
  if (!Array.isArray(candidate.produced)) {
    throw new Error(
      "PendingBlockFinalizationV1 ledgerDelta.produced must be an array",
    );
  }
  const spent = candidate.spent.map((outref, index) =>
    exactBytes(
      outref,
      `PendingBlockFinalizationV1 ledgerDelta.spent[${index.toString()}]`,
    ),
  );
  const produced = candidate.produced.map((entry, index) => {
    const item = exactRecord(
      entry,
      [UtxoColumns.OUTREF, UtxoColumns.OUTPUT],
      `PendingBlockFinalizationV1 ledgerDelta.produced[${index.toString()}]`,
    );
    return {
      [UtxoColumns.OUTREF]: exactBytes(
        item[UtxoColumns.OUTREF],
        `PendingBlockFinalizationV1 ledgerDelta.produced[${index.toString()}].outref`,
      ),
      [UtxoColumns.OUTPUT]: exactBytes(
        item[UtxoColumns.OUTPUT],
        `PendingBlockFinalizationV1 ledgerDelta.produced[${index.toString()}].output`,
      ),
    };
  });
  const spentIds = new Set(spent.map((outref) => outref.toString("hex")));
  const producedIds = new Set(
    produced.map((entry) => entry[UtxoColumns.OUTREF].toString("hex")),
  );
  if (
    spentIds.size !== spent.length ||
    producedIds.size !== produced.length ||
    [...spentIds].some((outref) => producedIds.has(outref))
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 ledgerDelta outrefs must be unique and disjoint",
    );
  }
  return { spent, produced };
};

const assertNativeMpfReplay = (replay: NativeMpfReplayInput): void => {
  const exact = (value: Buffer, bytes: number, field: string): void => {
    if (value.byteLength !== bytes) {
      throw new Error(
        `${field} must contain exactly ${bytes.toString()} bytes`,
      );
    }
  };
  if (replay.schema !== 1) throw new Error("mpf_owner_schema must equal 1");
  exact(replay.ownerBinarySha256, 32, "mpf_owner_binary_sha256");
  exact(replay.baseRoot, 32, "mpf_replay_base_root");
  exact(replay.candidateRoot, 32, "mpf_replay_candidate_root");
  exact(replay.eventLogDigest, 32, "mpf_replay_event_log_digest");
  if (replay.eventLog.byteLength < 92) {
    throw new Error("mpf_replay_event_log is truncated");
  }
  if (
    !Number.isSafeInteger(replay.eventCount) ||
    replay.eventCount < 0 ||
    replay.eventRoots.byteLength !== replay.eventCount * 32
  ) {
    throw new Error("mpf_replay_event_roots/count mismatch");
  }
};

const parseNativeMpfReplayV1 = (value: unknown): NativeMpfReplayInput => {
  const candidate = exactRecord(
    value,
    [
      "schema",
      "ownerBinarySha256",
      "baseRoot",
      "candidateRoot",
      "eventLog",
      "eventLogDigest",
      "eventRoots",
      "eventCount",
    ],
    "PendingBlockFinalizationV1 nativeMpfReplay",
  );
  if (candidate.schema !== 1) {
    throw new Error(
      "PendingBlockFinalizationV1 nativeMpfReplay.schema must equal 1",
    );
  }
  if (
    typeof candidate.eventCount !== "number" ||
    !Number.isSafeInteger(candidate.eventCount)
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 nativeMpfReplay.eventCount must be a safe integer",
    );
  }
  const replay: NativeMpfReplayInput = {
    schema: 1,
    ownerBinarySha256: exactBytes(
      candidate.ownerBinarySha256,
      "PendingBlockFinalizationV1 nativeMpfReplay.ownerBinarySha256",
      32,
    ),
    baseRoot: exactBytes(
      candidate.baseRoot,
      "PendingBlockFinalizationV1 nativeMpfReplay.baseRoot",
      32,
    ),
    candidateRoot: exactBytes(
      candidate.candidateRoot,
      "PendingBlockFinalizationV1 nativeMpfReplay.candidateRoot",
      32,
    ),
    eventLog: exactBytes(
      candidate.eventLog,
      "PendingBlockFinalizationV1 nativeMpfReplay.eventLog",
    ),
    eventLogDigest: exactBytes(
      candidate.eventLogDigest,
      "PendingBlockFinalizationV1 nativeMpfReplay.eventLogDigest",
      32,
    ),
    eventRoots: exactBytes(
      candidate.eventRoots,
      "PendingBlockFinalizationV1 nativeMpfReplay.eventRoots",
      candidate.eventCount * 32,
    ),
    eventCount: candidate.eventCount,
  };
  assertNativeMpfReplay(replay);
  return replay;
};

const parsePendingBlockFinalizationMetadataV1 = (
  value: unknown,
): PendingBlockFinalizationMetadata => {
  const candidate = exactRecord(
    value,
    [
      "deploymentMarker",
      "consensusProfileId",
      "stateQueueLeaseToken",
      "baseSnapshotId",
      "baseTailOutRef",
      "baseTailHeaderHash",
      "baseTailDatumCbor",
      "baseRoots",
      "blockStartTime",
      "expectedRoots",
      "expectedCounts",
    ],
    "PendingBlockFinalizationV1 metadata",
  );
  const baseRoots = exactRecord(
    candidate.baseRoots,
    [
      "utxosRoot",
      "forcedTransactionsRoot",
      "transactionsRoot",
      "depositsRoot",
      "withdrawalsRoot",
    ],
    "PendingBlockFinalizationV1 metadata.baseRoots",
  );
  const expectedRoots = exactRecord(
    candidate.expectedRoots,
    [
      "utxosRoot",
      "forcedTransactionsRoot",
      "transactionsRoot",
      "depositsRoot",
      "withdrawalsRoot",
      "transitionTraceRoot",
      "eventToStepRoot",
      "validationTracesRoot",
    ],
    "PendingBlockFinalizationV1 metadata.expectedRoots",
  );
  const expectedCounts = exactRecord(
    candidate.expectedCounts,
    [
      "withdrawalCount",
      "forcedTransactionCount",
      "l2TransactionCount",
      "depositCount",
      "totalEventCount",
      "transitionStepCount",
      "validationTraceCount",
    ],
    "PendingBlockFinalizationV1 metadata.expectedCounts",
  );
  const deploymentMarker = parseDeploymentMarkerV1(candidate.deploymentMarker);
  if (candidate.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_V1_ID) {
    throw new Error(
      `PendingBlockFinalizationV1 metadata.consensusProfileId must equal ${MIDGARD_CONSENSUS_PROFILE_V1_ID}`,
    );
  }
  const counts = {
    withdrawalCount: exactNonNegativeBigInt(
      expectedCounts.withdrawalCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.withdrawalCount",
    ),
    forcedTransactionCount: exactNonNegativeBigInt(
      expectedCounts.forcedTransactionCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.forcedTransactionCount",
    ),
    l2TransactionCount: exactNonNegativeBigInt(
      expectedCounts.l2TransactionCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.l2TransactionCount",
    ),
    depositCount: exactNonNegativeBigInt(
      expectedCounts.depositCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.depositCount",
    ),
    totalEventCount: exactNonNegativeBigInt(
      expectedCounts.totalEventCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.totalEventCount",
    ),
    transitionStepCount: exactNonNegativeBigInt(
      expectedCounts.transitionStepCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.transitionStepCount",
    ),
    validationTraceCount: exactNonNegativeBigInt(
      expectedCounts.validationTraceCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.validationTraceCount",
    ),
  };
  if (
    counts.totalEventCount !==
      counts.withdrawalCount +
        counts.forcedTransactionCount +
        counts.l2TransactionCount +
        counts.depositCount ||
    counts.transitionStepCount !== counts.totalEventCount ||
    counts.validationTraceCount !==
      counts.forcedTransactionCount + counts.l2TransactionCount
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 metadata expected counts are inconsistent",
    );
  }
  return {
    deploymentMarker,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
    stateQueueLeaseToken: exactNonEmptyString(
      candidate.stateQueueLeaseToken,
      "PendingBlockFinalizationV1 metadata.stateQueueLeaseToken",
    ),
    baseSnapshotId: exactNonEmptyString(
      candidate.baseSnapshotId,
      "PendingBlockFinalizationV1 metadata.baseSnapshotId",
    ),
    baseTailOutRef: exactNonEmptyString(
      candidate.baseTailOutRef,
      "PendingBlockFinalizationV1 metadata.baseTailOutRef",
    ),
    baseTailHeaderHash: exactBytes(
      candidate.baseTailHeaderHash,
      "PendingBlockFinalizationV1 metadata.baseTailHeaderHash",
      28,
    ),
    baseTailDatumCbor: exactNonEmptyString(
      candidate.baseTailDatumCbor,
      "PendingBlockFinalizationV1 metadata.baseTailDatumCbor",
    ),
    baseRoots: {
      utxosRoot: exactHex(
        baseRoots.utxosRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.utxosRoot",
      ),
      forcedTransactionsRoot: exactHex(
        baseRoots.forcedTransactionsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.forcedTransactionsRoot",
      ),
      transactionsRoot: exactHex(
        baseRoots.transactionsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.transactionsRoot",
      ),
      depositsRoot: exactHex(
        baseRoots.depositsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.depositsRoot",
      ),
      withdrawalsRoot: exactHex(
        baseRoots.withdrawalsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.withdrawalsRoot",
      ),
    },
    blockStartTime: exactDate(
      candidate.blockStartTime,
      "PendingBlockFinalizationV1 metadata.blockStartTime",
    ),
    expectedRoots: {
      utxosRoot: exactHex(
        expectedRoots.utxosRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.utxosRoot",
      ),
      forcedTransactionsRoot: exactHex(
        expectedRoots.forcedTransactionsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.forcedTransactionsRoot",
      ),
      transactionsRoot: exactHex(
        expectedRoots.transactionsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.transactionsRoot",
      ),
      depositsRoot: exactHex(
        expectedRoots.depositsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.depositsRoot",
      ),
      withdrawalsRoot: exactHex(
        expectedRoots.withdrawalsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.withdrawalsRoot",
      ),
      transitionTraceRoot: exactHex(
        expectedRoots.transitionTraceRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.transitionTraceRoot",
      ),
      eventToStepRoot: exactHex(
        expectedRoots.eventToStepRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.eventToStepRoot",
      ),
      validationTracesRoot: exactHex(
        expectedRoots.validationTracesRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.validationTracesRoot",
      ),
    },
    expectedCounts: counts,
  };
};

export const parsePendingBlockFinalizationV1 = (
  value: unknown,
): PendingBlockFinalizationV1 => {
  const candidate = exactRecord(
    value,
    ["version", "metadata", "replay"],
    "PendingBlockFinalizationV1",
  );
  if (candidate.version !== PENDING_BLOCK_FINALIZATION_V1_VERSION) {
    throw new Error(
      `PendingBlockFinalizationV1 version must equal ${PENDING_BLOCK_FINALIZATION_V1_VERSION.toString()}`,
    );
  }
  const metadata = parsePendingBlockFinalizationMetadataV1(candidate.metadata);
  const replayCandidate = exactRecord(
    candidate.replay,
    (candidate.replay as { readonly kind?: unknown })?.kind ===
      PendingBlockFinalizationReplayKindV1.LedgerDeltaWithNativeMpf
      ? ["kind", "ledgerDelta", "nativeMpfReplay"]
      : ["kind", "ledgerDelta"],
    "PendingBlockFinalizationV1 replay",
  );
  const ledgerDelta = parseLedgerDeltaV1(replayCandidate.ledgerDelta);
  if (
    replayCandidate.kind === PendingBlockFinalizationReplayKindV1.LedgerDelta
  ) {
    return {
      version: PENDING_BLOCK_FINALIZATION_V1_VERSION,
      metadata,
      replay: {
        kind: PendingBlockFinalizationReplayKindV1.LedgerDelta,
        ledgerDelta,
      },
    };
  }
  if (
    replayCandidate.kind !==
    PendingBlockFinalizationReplayKindV1.LedgerDeltaWithNativeMpf
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 replay kind must be an exact V1 discriminator",
    );
  }
  const nativeMpfReplay = parseNativeMpfReplayV1(
    replayCandidate.nativeMpfReplay,
  );
  if (
    nativeMpfReplay.baseRoot.toString("hex") !== metadata.baseRoots.utxosRoot ||
    nativeMpfReplay.candidateRoot.toString("hex") !==
      metadata.expectedRoots.utxosRoot
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 native MPF replay roots do not match metadata",
    );
  }
  return {
    version: PENDING_BLOCK_FINALIZATION_V1_VERSION,
    metadata,
    replay: {
      kind: PendingBlockFinalizationReplayKindV1.LedgerDeltaWithNativeMpf,
      ledgerDelta,
      nativeMpfReplay,
    },
  };
};

const decodeNativeMpfReplay = (row: Row): NativeMpfReplayInput | undefined => {
  const values = [
    row[Columns.MPF_OWNER_SCHEMA],
    row[Columns.MPF_OWNER_BINARY_SHA256],
    row[Columns.MPF_REPLAY_BASE_ROOT],
    row[Columns.MPF_REPLAY_CANDIDATE_ROOT],
    row[Columns.MPF_REPLAY_EVENT_LOG],
    row[Columns.MPF_REPLAY_EVENT_LOG_DIGEST],
    row[Columns.MPF_REPLAY_EVENT_ROOTS],
    row[Columns.MPF_REPLAY_EVENT_COUNT],
  ];
  if (
    row[Columns.REPLAY_KIND] ===
    PendingBlockFinalizationReplayKindV1.LedgerDelta
  ) {
    if (values.some((value) => value != null)) {
      throw new Error(
        "PendingBlockFinalizationV1 delta-only replay contains native MPF fields",
      );
    }
    return undefined;
  }
  if (
    row[Columns.REPLAY_KIND] !==
    PendingBlockFinalizationReplayKindV1.LedgerDeltaWithNativeMpf
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 persisted replay kind is unsupported",
    );
  }
  if (values.some((value) => value == null)) {
    throw new Error(
      "PendingBlockFinalizationV1 native MPF replay fields are partially null",
    );
  }
  const replay: NativeMpfReplayInput = {
    schema: row[Columns.MPF_OWNER_SCHEMA] as 1,
    ownerBinarySha256: Buffer.from(row[Columns.MPF_OWNER_BINARY_SHA256]!),
    baseRoot: Buffer.from(row[Columns.MPF_REPLAY_BASE_ROOT]!),
    candidateRoot: Buffer.from(row[Columns.MPF_REPLAY_CANDIDATE_ROOT]!),
    eventLog: Buffer.from(row[Columns.MPF_REPLAY_EVENT_LOG]!),
    eventLogDigest: Buffer.from(row[Columns.MPF_REPLAY_EVENT_LOG_DIGEST]!),
    eventRoots: Buffer.from(row[Columns.MPF_REPLAY_EVENT_ROOTS]!),
    eventCount: row[Columns.MPF_REPLAY_EVENT_COUNT]!,
  };
  return parseNativeMpfReplayV1(replay);
};

const decodeHexArray = (value: unknown, label: string): readonly Buffer[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must be an array of exact lowercase hex strings`);
  }
  return value.map((item, index) =>
    Buffer.from(
      exactNonEmptyCanonicalHex(item, `${label}[${index.toString()}]`),
      "hex",
    ),
  );
};

const exactNonEmptyCanonicalHex = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.length % 2 !== 0 ||
    !/^[0-9a-f]+$/u.test(value)
  ) {
    throw new Error(`${label} must be non-empty even-length lowercase hex`);
  }
  return value;
};

const decodeLedgerDelta = (row: Row): LedgerDeltaInput => {
  const parseJson = (value: unknown): unknown =>
    typeof value === "string" ? JSON.parse(value) : value;
  const spent = parseJson(row[Columns.LEDGER_DELTA_SPENT]);
  const produced = parseJson(row[Columns.LEDGER_DELTA_PRODUCED]);
  if (!Array.isArray(produced)) {
    throw new Error("ledger_delta_produced must be an array");
  }
  return parseLedgerDeltaV1({
    spent: decodeHexArray(spent, "ledger_delta_spent"),
    produced: produced.map((entry) => {
      const candidate = exactRecord(
        entry,
        ["outref", "output"],
        "ledger_delta_produced entry",
      );
      return {
        [UtxoColumns.OUTREF]: Buffer.from(
          exactNonEmptyCanonicalHex(
            candidate.outref,
            "ledger_delta_produced.outref",
          ),
          "hex",
        ),
        [UtxoColumns.OUTPUT]: Buffer.from(
          exactNonEmptyCanonicalHex(
            candidate.output,
            "ledger_delta_produced.output",
          ),
          "hex",
        ),
      };
    }),
  });
};

const pendingBlockFinalizationMetadataFromRow = (
  row: Row,
): PendingBlockFinalizationMetadata => ({
  deploymentMarker: {
    schemaVersion: row[Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION],
    manifestId: row[Columns.DEPLOYMENT_MANIFEST_ID],
  },
  consensusProfileId: row[Columns.CONSENSUS_PROFILE_ID],
  stateQueueLeaseToken: row[Columns.STATE_QUEUE_LEASE_TOKEN],
  baseSnapshotId: row[Columns.BASE_SNAPSHOT_ID],
  baseTailOutRef: row[Columns.BASE_TAIL_OUT_REF],
  baseTailHeaderHash: row[Columns.BASE_TAIL_HEADER_HASH],
  baseTailDatumCbor: row[Columns.BASE_TAIL_DATUM_CBOR],
  baseRoots: {
    utxosRoot: row[Columns.BASE_UTXOS_ROOT],
    forcedTransactionsRoot: row[Columns.BASE_FORCED_TRANSACTIONS_ROOT],
    transactionsRoot: row[Columns.BASE_TRANSACTIONS_ROOT],
    depositsRoot: row[Columns.BASE_DEPOSITS_ROOT],
    withdrawalsRoot: row[Columns.BASE_WITHDRAWALS_ROOT],
  },
  blockStartTime: row[Columns.BLOCK_START_TIME],
  expectedRoots: {
    utxosRoot: row[Columns.EXPECTED_UTXOS_ROOT],
    forcedTransactionsRoot: row[Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT],
    transactionsRoot: row[Columns.EXPECTED_TRANSACTIONS_ROOT],
    depositsRoot: row[Columns.EXPECTED_DEPOSITS_ROOT],
    withdrawalsRoot: row[Columns.EXPECTED_WITHDRAWALS_ROOT],
    transitionTraceRoot: row[Columns.EXPECTED_TRANSITION_TRACE_ROOT],
    eventToStepRoot: row[Columns.EXPECTED_EVENT_TO_STEP_ROOT],
    validationTracesRoot: row[Columns.EXPECTED_VALIDATION_TRACES_ROOT],
  },
  expectedCounts: {
    withdrawalCount: row[Columns.EXPECTED_WITHDRAWAL_COUNT],
    forcedTransactionCount: row[Columns.EXPECTED_FORCED_TRANSACTION_COUNT],
    l2TransactionCount: row[Columns.EXPECTED_L2_TRANSACTION_COUNT],
    depositCount: row[Columns.EXPECTED_DEPOSIT_COUNT],
    totalEventCount: row[Columns.EXPECTED_TOTAL_EVENT_COUNT],
    transitionStepCount: row[Columns.EXPECTED_TRANSITION_STEP_COUNT],
    validationTraceCount: row[Columns.EXPECTED_VALIDATION_TRACE_COUNT],
  },
});

const assertSameIdSet = (
  table: string,
  label: string,
  expected: readonly Buffer[],
  actual: readonly Buffer[],
): Effect.Effect<void, DatabaseError> =>
  Effect.gen(function* () {
    const expectedSet = new Set(expected.map((value) => value.toString("hex")));
    const actualSet = new Set(actual.map((value) => value.toString("hex")));
    if (
      expectedSet.size !== actualSet.size ||
      [...expectedSet].some((hex) => !actualSet.has(hex))
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table,
          message: `Refusing to prepare pending journal because ${label} ids do not match the provided payload entries`,
          cause: `expected=[${[...expectedSet].join(",")}],actual=[${[
            ...actualSet,
          ].join(",")}]`,
        }),
      );
    }
  });

const txMemberEntry = (
  headerHash: Buffer,
  entry: TxTable.EntryWithTimeStamp,
  ordinal: number,
  sourceTable: string,
  programMaterialSidecarCbor?: Buffer,
): MemberRecord => {
  const payload = Buffer.from(entry[TxTable.Columns.TX]);
  const memberId = Buffer.from(entry[TxTable.Columns.TX_ID]);
  return {
    [MemberColumns.HEADER_HASH]: headerHash,
    [MemberColumns.MEMBER_ID]: memberId,
    [MemberColumns.ORDINAL]: ordinal,
    [MemberColumns.PAYLOAD_CBOR]: payload,
    [MemberColumns.PAYLOAD_SHA256]: sha256(payload),
    ...(programMaterialSidecarCbor === undefined
      ? {}
      : {
          [MemberColumns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]: Buffer.from(
            programMaterialSidecarCbor,
          ),
          [MemberColumns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]: sha256(
            programMaterialSidecarCbor,
          ),
        }),
    [MemberColumns.SOURCE_TABLE]: sourceTable,
    [MemberColumns.SOURCE_ID]: memberId,
    [MemberColumns.SOURCE_TIMESTAMP]: entry[TxTable.Columns.TIMESTAMPTZ],
  };
};

const depositMemberEntry = (
  headerHash: Buffer,
  entry: DepositsDB.Entry,
  ordinal: number,
): MemberRecord => {
  const payload = Buffer.from(entry[DepositsDB.Columns.INFO]);
  const memberId = Buffer.from(entry[DepositsDB.Columns.ID]);
  return {
    [MemberColumns.HEADER_HASH]: headerHash,
    [MemberColumns.MEMBER_ID]: memberId,
    [MemberColumns.ORDINAL]: ordinal,
    [MemberColumns.PAYLOAD_CBOR]: payload,
    [MemberColumns.PAYLOAD_SHA256]: sha256(payload),
    [MemberColumns.SOURCE_TABLE]: DepositsDB.tableName,
    [MemberColumns.SOURCE_ID]: memberId,
    [MemberColumns.SOURCE_TIMESTAMP]: entry[DepositsDB.Columns.INCLUSION_TIME],
  };
};

const forcedTransactionMemberEntry = (
  headerHash: Buffer,
  entry: ForcedTransactionsDB.Entry,
  ordinal: number,
): Effect.Effect<MemberRecord, DatabaseError> =>
  Effect.gen(function* () {
    const sourceValueCbor = Buffer.from(
      entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
    );
    const canonicalTransactionCbor =
      entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR];
    const programMaterialSidecarCbor =
      entry[ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR];
    const programMaterialSidecarSha256 =
      entry[ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256];
    if (
      sourceValueCbor.length === 0 ||
      canonicalTransactionCbor.length === 0 ||
      programMaterialSidecarCbor.length === 0 ||
      programMaterialSidecarSha256.length !== 32 ||
      !sha256(programMaterialSidecarCbor).equals(programMaterialSidecarSha256)
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "Refusing to journal a V1 forced transaction without its exact source value, canonical transaction preimage, and authenticated CEK material sidecar",
          cause: `tx_order_id=${entry[
            ForcedTransactionsDB.Columns.TX_ORDER_ID
          ].toString("hex")}`,
        }),
      );
    }
    const payload = ForcedTransactionsDB.encodeForcedTransactionJournalMemberV1(
      {
        sourceValueCbor,
        canonicalTransactionCbor,
        programMaterialSidecarCbor,
      },
    );
    const memberId = Buffer.from(
      entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
    );
    return {
      [MemberColumns.HEADER_HASH]: headerHash,
      [MemberColumns.MEMBER_ID]: memberId,
      [MemberColumns.ORDINAL]: ordinal,
      [MemberColumns.PAYLOAD_CBOR]: payload,
      [MemberColumns.PAYLOAD_SHA256]: sha256(payload),
      [MemberColumns.SOURCE_TABLE]: ForcedTransactionsDB.tableName,
      [MemberColumns.SOURCE_ID]: memberId,
      [MemberColumns.SOURCE_TIMESTAMP]:
        entry[ForcedTransactionsDB.Columns.INCLUSION_TIME],
    };
  });

const withdrawalMemberEntry = (
  headerHash: Buffer,
  entry: WithdrawalsDB.Entry,
  ordinal: number,
): Effect.Effect<MemberRecord, DatabaseError> =>
  Effect.gen(function* () {
    const payload = entry[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
    const memberId = Buffer.from(entry[WithdrawalsDB.Columns.ID]);
    if (payload === null) {
      return yield* Effect.fail(
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message:
            "Refusing to prepare pending journal for an unclassified withdrawal",
          cause: `event_id=${memberId.toString("hex")}`,
        }),
      );
    }
    return {
      [MemberColumns.HEADER_HASH]: headerHash,
      [MemberColumns.MEMBER_ID]: memberId,
      [MemberColumns.ORDINAL]: ordinal,
      [MemberColumns.PAYLOAD_CBOR]: Buffer.from(payload),
      [MemberColumns.PAYLOAD_SHA256]: sha256(Buffer.from(payload)),
      [MemberColumns.SOURCE_TABLE]: WithdrawalsDB.tableName,
      [MemberColumns.SOURCE_ID]: memberId,
      [MemberColumns.SOURCE_TIMESTAMP]:
        entry[WithdrawalsDB.Columns.INCLUSION_TIME],
    };
  });

const retainedRootMemberEntry = ({
  headerHash,
  entry,
  ordinal,
  sourceTable,
  blockEndTime,
}: {
  readonly headerHash: Buffer;
  readonly entry: RetainedRootMemberInput;
  readonly ordinal: number;
  readonly sourceTable: string;
  readonly blockEndTime: Date;
}): MemberRecord => {
  const memberId = Buffer.from(entry.keyCbor);
  const payload = Buffer.from(entry.valueCbor);
  return {
    [MemberColumns.HEADER_HASH]: headerHash,
    [MemberColumns.MEMBER_ID]: memberId,
    [MemberColumns.ORDINAL]: ordinal,
    [MemberColumns.PAYLOAD_CBOR]: payload,
    [MemberColumns.PAYLOAD_SHA256]: sha256(payload),
    [MemberColumns.SOURCE_TABLE]: sourceTable,
    [MemberColumns.SOURCE_ID]: memberId,
    [MemberColumns.SOURCE_TIMESTAMP]: blockEndTime,
  };
};

const retrieveMembers = (
  sql: SqlClient.SqlClient,
  memberTableName: string,
  headerHash: Buffer,
): Effect.Effect<readonly MemberRecord[], never, never> =>
  Effect.gen(function* () {
    return yield* sql<MemberRecord>`SELECT * FROM ${sql(memberTableName)}
      WHERE ${sql(MemberColumns.HEADER_HASH)} = ${headerHash}
      ORDER BY ${sql(MemberColumns.ORDINAL)} ASC`;
  }).pipe(Effect.orDie);

export const validateForcedTransactionJournalMembersV1 = (
  members: readonly MemberRecord[],
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError> =>
  Effect.try({
    try: () => {
      for (const member of members) {
        const memberId = member[MemberColumns.MEMBER_ID];
        const payload = member[MemberColumns.PAYLOAD_CBOR];
        const payloadSha256 = member[MemberColumns.PAYLOAD_SHA256];
        if (
          member[MemberColumns.SOURCE_TABLE] !==
            ForcedTransactionsDB.tableName ||
          !member[MemberColumns.SOURCE_ID].equals(memberId) ||
          payloadSha256.length !== 32 ||
          !sha256(payload).equals(payloadSha256)
        ) {
          throw new Error(
            `forced journal member identity or payload digest mismatch: member_id=${memberId.toString("hex")}`,
          );
        }
        ForcedTransactionsDB.decodeForcedTransactionJournalMemberV1(payload);
      }
    },
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message:
          "Refusing to load a pending-finalization journal containing a non-canonical ForcedTransactionJournalMemberV1",
        cause: `header_hash=${headerHash.toString("hex")}; ${String(cause)}`,
      }),
  });

const decodePendingBlockFinalizationRowV1 = (
  row: RawRow,
): Effect.Effect<
  {
    readonly normalizedRow: Row;
    readonly ledgerDelta: LedgerDeltaInput;
    readonly nativeMpfReplay?: NativeMpfReplayInput;
  },
  DatabaseError
> =>
  Effect.try({
    try: () => {
      const persistedFormatVersion: unknown = row[Columns.FORMAT_VERSION];
      if (persistedFormatVersion !== PENDING_BLOCK_FINALIZATION_V1_VERSION) {
        throw new Error(
          `persisted format_version must equal ${PENDING_BLOCK_FINALIZATION_V1_VERSION.toString()}`,
        );
      }
      const normalizedRow = normalizeRow(row);
      const ledgerDelta = decodeLedgerDelta(normalizedRow);
      const nativeMpfReplay = decodeNativeMpfReplay(normalizedRow);
      const pending = parsePendingBlockFinalizationV1({
        version: normalizedRow[Columns.FORMAT_VERSION],
        metadata: pendingBlockFinalizationMetadataFromRow(normalizedRow),
        replay:
          nativeMpfReplay === undefined
            ? {
                kind: normalizedRow[Columns.REPLAY_KIND],
                ledgerDelta,
              }
            : {
                kind: normalizedRow[Columns.REPLAY_KIND],
                ledgerDelta,
                nativeMpfReplay,
              },
      });
      if (
        normalizedRow[Columns.HEADER_HASH].length !== 28 ||
        normalizedRow[Columns.HEADER_CBOR].length === 0 ||
        (normalizedRow[Columns.SUBMITTED_TX_HASH] !== null &&
          normalizedRow[Columns.SUBMITTED_TX_HASH].length !== 32) ||
        normalizedRow[Columns.BLOCK_END_TIME].getTime() <=
          pending.metadata.blockStartTime.getTime() ||
        !Object.values(Status).includes(normalizedRow[Columns.STATUS])
      ) {
        throw new Error(
          "persisted header, status, transaction hash, or block window is invalid",
        );
      }
      return {
        normalizedRow,
        ledgerDelta: pending.replay.ledgerDelta,
        nativeMpfReplay:
          pending.replay.kind ===
          PendingBlockFinalizationReplayKindV1.LedgerDeltaWithNativeMpf
            ? pending.replay.nativeMpfReplay
            : undefined,
      };
    },
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message: "Refusing to load a non-canonical PendingBlockFinalizationV1",
        cause: String(cause),
      }),
  });

const retrieveRecord = (
  sql: SqlClient.SqlClient,
  row: RawRow,
): Effect.Effect<Record, DatabaseError, never> =>
  Effect.gen(function* () {
    const decoded = yield* decodePendingBlockFinalizationRowV1(row);
    const { normalizedRow, ledgerDelta, nativeMpfReplay } = decoded;
    const [
      depositEventIds,
      forcedTransactionEventIds,
      withdrawalEventIds,
      mempoolTxIds,
      transitionTraceMembers,
      eventToStepMembers,
      validationTraceMembers,
    ] = yield* Effect.all(
      [
        retrieveMembers(
          sql,
          depositsTableName,
          normalizedRow[Columns.HEADER_HASH],
        ),
        retrieveMembers(
          sql,
          forcedTransactionsTableName,
          normalizedRow[Columns.HEADER_HASH],
        ),
        retrieveMembers(
          sql,
          withdrawalsTableName,
          normalizedRow[Columns.HEADER_HASH],
        ),
        retrieveMembers(sql, txsTableName, normalizedRow[Columns.HEADER_HASH]),
        retrieveMembers(
          sql,
          transitionTraceTableName,
          normalizedRow[Columns.HEADER_HASH],
        ),
        retrieveMembers(
          sql,
          eventToStepTableName,
          normalizedRow[Columns.HEADER_HASH],
        ),
        retrieveMembers(
          sql,
          validationTracesTableName,
          normalizedRow[Columns.HEADER_HASH],
        ),
      ],
      { concurrency: 1 },
    );
    yield* validateForcedTransactionJournalMembersV1(
      forcedTransactionEventIds,
      normalizedRow[Columns.HEADER_HASH],
    );
    return {
      ...normalizedRow,
      ledgerDelta,
      nativeMpfReplay,
      utxoPayloadAggregate:
        normalizedRow[Columns.UTXO_PAYLOAD_ENTRY_COUNT] == null ||
        normalizedRow[Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES] == null
          ? undefined
          : {
              entryCount: normalizedRow[Columns.UTXO_PAYLOAD_ENTRY_COUNT],
              encodedTupleBytes:
                normalizedRow[Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES],
            },
      depositEventIds: depositEventIds.map(
        (member) => member[MemberColumns.MEMBER_ID],
      ),
      forcedTransactionEventIds: forcedTransactionEventIds.map(
        (member) => member[MemberColumns.MEMBER_ID],
      ),
      withdrawalEventIds: withdrawalEventIds.map(
        (member) => member[MemberColumns.MEMBER_ID],
      ),
      mempoolTxIds: mempoolTxIds.map(
        (member) => member[MemberColumns.MEMBER_ID],
      ),
      depositMembers: depositEventIds,
      forcedTransactionMembers: forcedTransactionEventIds,
      withdrawalMembers: withdrawalEventIds,
      txMembers: mempoolTxIds,
      transitionTraceMembers,
      eventToStepMembers,
      validationTraceMembers,
    };
  });

export const retrieveActive = (): Effect.Effect<
  Option.Option<Record>,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
      ORDER BY ${sql(Columns.CREATED_AT)} ASC
      LIMIT 1`;
    return rows.length === 0
      ? Option.none()
      : Option.some(yield* retrieveRecord(sql, rows[0]!));
  }).pipe(
    Effect.withLogSpan(`retrieveActive ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve active pending-finalization record",
    ),
  );

export const hasActive: Effect.Effect<boolean, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<{ readonly present: boolean }>`SELECT EXISTS (
      SELECT 1 FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
    ) AS present`;
    return row?.present === true;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to check active pending-finalization state",
    ),
  );

export const retrieveByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
      LIMIT 1`;
    return rows.length === 0
      ? Option.none()
      : Option.some(yield* retrieveRecord(sql, rows[0]!));
  }).pipe(
    Effect.withLogSpan(`retrieveByHeaderHash ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve pending-finalization record by header hash",
    ),
  );

export const retrieveActiveByStateQueueLeaseToken = (
  stateQueueLeaseToken: string,
): Effect.Effect<readonly Row[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATE_QUEUE_LEASE_TOKEN)} = ${stateQueueLeaseToken}
        AND ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
      ORDER BY ${sql(Columns.CREATED_AT)} ASC`;
    return yield* Effect.forEach(
      rows,
      (row) =>
        decodePendingBlockFinalizationRowV1(row).pipe(
          Effect.map(({ normalizedRow }) => normalizedRow),
        ),
      { concurrency: 1 },
    );
  }).pipe(
    Effect.withLogSpan(`retrieveActiveByStateQueueLeaseToken ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve active pending-finalization records by state-queue lease token",
    ),
  );

export const retrieveByStateQueueLeaseToken = (
  stateQueueLeaseToken: string,
): Effect.Effect<readonly Row[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATE_QUEUE_LEASE_TOKEN)} = ${stateQueueLeaseToken}
      ORDER BY ${sql(Columns.CREATED_AT)} ASC`;
    return yield* Effect.forEach(
      rows,
      (row) =>
        decodePendingBlockFinalizationRowV1(row).pipe(
          Effect.map(({ normalizedRow }) => normalizedRow),
        ),
      { concurrency: 1 },
    );
  }).pipe(
    Effect.withLogSpan(`retrieveByStateQueueLeaseToken ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve pending-finalization records by state-queue lease token",
    ),
  );

export const retrieveFinalizedMissingDaPayloads = ({
  headerHash,
  limit = 100,
}: {
  readonly headerHash?: Buffer;
  readonly limit?: number;
} = {}): Effect.Effect<readonly Record[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const safeLimit = Math.max(1, Math.floor(limit));
    const sql = yield* SqlClient.SqlClient;
    const rows =
      headerHash === undefined
        ? yield* sql<RawRow>`SELECT ${sql(tableName)}.* FROM ${sql(tableName)}
            LEFT JOIN ${sql(DaPayloadsDB.tableName)}
              ON ${sql(DaPayloadsDB.tableName)}.${sql(
                DaPayloadsDB.Columns.HEADER_HASH,
              )} = ${sql(tableName)}.${sql(Columns.HEADER_HASH)}
            WHERE ${sql(tableName)}.${sql(Columns.STATUS)} = ${Status.Finalized}
              AND ${sql(DaPayloadsDB.tableName)}.${sql(
                DaPayloadsDB.Columns.HEADER_HASH,
              )} IS NULL
            ORDER BY ${sql(tableName)}.${sql(Columns.CREATED_AT)} ASC
            LIMIT ${safeLimit}`
        : yield* sql<RawRow>`SELECT ${sql(tableName)}.* FROM ${sql(tableName)}
            LEFT JOIN ${sql(DaPayloadsDB.tableName)}
              ON ${sql(DaPayloadsDB.tableName)}.${sql(
                DaPayloadsDB.Columns.HEADER_HASH,
              )} = ${sql(tableName)}.${sql(Columns.HEADER_HASH)}
            WHERE ${sql(tableName)}.${sql(Columns.STATUS)} = ${Status.Finalized}
              AND ${sql(DaPayloadsDB.tableName)}.${sql(
                DaPayloadsDB.Columns.HEADER_HASH,
              )} IS NULL
              AND ${sql(tableName)}.${sql(Columns.HEADER_HASH)} = ${headerHash}
            ORDER BY ${sql(tableName)}.${sql(Columns.CREATED_AT)} ASC
            LIMIT ${safeLimit}`;
    return yield* Effect.forEach(rows, (row) => retrieveRecord(sql, row), {
      concurrency: 1,
    });
  }).pipe(
    Effect.withLogSpan(`retrieveFinalizedMissingDaPayloads ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve finalized journals missing DA payloads",
    ),
  );

export const preparePendingSubmission = (
  input: PrepareInput,
  options?: {
    /**
     * Runs after the active-journal guard and inside the same SQL transaction
     * as the pending journal insert. Used by speculative submission to lock,
     * revalidate, and project its exact source snapshot atomically.
     */
    readonly beforeJournalInsert?: Effect.Effect<void, DatabaseError, Database>;
  },
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pendingV1 = yield* Effect.try({
      try: () => {
        const pending = parsePendingBlockFinalizationV1({
          version: PENDING_BLOCK_FINALIZATION_V1_VERSION,
          metadata: input.metadata,
          replay:
            input.nativeMpfReplay === undefined
              ? {
                  kind: PendingBlockFinalizationReplayKindV1.LedgerDelta,
                  ledgerDelta: input.ledgerDelta,
                }
              : {
                  kind: PendingBlockFinalizationReplayKindV1.LedgerDeltaWithNativeMpf,
                  ledgerDelta: input.ledgerDelta,
                  nativeMpfReplay: input.nativeMpfReplay,
                },
        });
        exactBytes(
          input.headerHash,
          "PendingBlockFinalizationV1 headerHash",
          28,
        );
        exactBytes(input.headerCbor, "PendingBlockFinalizationV1 headerCbor");
        const blockEndTime = exactDate(
          input.blockEndTime,
          "PendingBlockFinalizationV1 blockEndTime",
        );
        if (
          blockEndTime.getTime() <= pending.metadata.blockStartTime.getTime()
        ) {
          throw new Error(
            "PendingBlockFinalizationV1 block window must be increasing",
          );
        }
        return pending;
      },
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to prepare a non-canonical PendingBlockFinalizationV1",
          cause,
        }),
    });
    const metadata = pendingV1.metadata;
    const ledgerDelta = pendingV1.replay.ledgerDelta;
    const nativeMpfReplay =
      pendingV1.replay.kind ===
      PendingBlockFinalizationReplayKindV1.LedgerDeltaWithNativeMpf
        ? pendingV1.replay.nativeMpfReplay
        : undefined;
    const deploymentMarker = metadata.deploymentMarker;
    yield* assertSameIdSet(
      tableName,
      "deposit",
      input.depositEventIds,
      input.depositEntries.map((entry) => entry[DepositsDB.Columns.ID]),
    );
    yield* assertSameIdSet(
      tableName,
      "forced transaction",
      input.forcedTransactionEventIds,
      input.forcedTransactionEntries.map(
        (entry) => entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
      ),
    );
    yield* assertSameIdSet(
      tableName,
      "withdrawal",
      input.withdrawalEventIds,
      input.withdrawalEntries.map((entry) => entry[WithdrawalsDB.Columns.ID]),
    );
    yield* assertSameIdSet(
      tableName,
      "mempool tx",
      input.mempoolTxIds,
      input.mempoolTxs.map((entry) => entry[TxTable.Columns.TX_ID]),
    );
    const depositMembers = input.depositEntries.map((entry, ordinal) =>
      depositMemberEntry(input.headerHash, entry, ordinal),
    );
    const forcedTransactionMembers = yield* Effect.forEach(
      input.forcedTransactionEntries,
      (entry, ordinal) =>
        forcedTransactionMemberEntry(input.headerHash, entry, ordinal),
    );
    const withdrawalMembers = yield* Effect.forEach(
      input.withdrawalEntries,
      (entry, ordinal) =>
        withdrawalMemberEntry(input.headerHash, entry, ordinal),
    );
    const programMaterialByTxId = new Map<string, Buffer>();
    for (const material of input.mempoolTxProgramMaterialSidecars ?? []) {
      const txIdHex = material.txId.toString("hex");
      if (programMaterialByTxId.has(txIdHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: txsTableName,
            message:
              "Refusing to prepare duplicate V1 transaction program material",
            cause: `tx_id=${txIdHex}`,
          }),
        );
      }
      yield* Effect.try({
        try: () =>
          decodeMidgardCekProgramMaterialSidecarV1(material.sidecarCbor),
        catch: (cause) =>
          new DatabaseError({
            table: txsTableName,
            message:
              "Refusing to journal malformed V1 transaction program material",
            cause,
          }),
      });
      programMaterialByTxId.set(txIdHex, Buffer.from(material.sidecarCbor));
    }
    if (
      programMaterialByTxId.size !== input.mempoolTxs.length ||
      input.mempoolTxs.some(
        (entry) =>
          !programMaterialByTxId.has(
            entry[TxTable.Columns.TX_ID].toString("hex"),
          ),
      )
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: txsTableName,
          message:
            "V1 pending journal requires one canonical program-material sidecar per normal transaction",
          cause: `transactions=${input.mempoolTxs.length.toString()},sidecars=${programMaterialByTxId.size.toString()}`,
        }),
      );
    }
    const txMembers = input.mempoolTxs.map((entry, ordinal) =>
      txMemberEntry(
        input.headerHash,
        entry,
        ordinal,
        input.mempoolTxSourceTable,
        programMaterialByTxId.get(entry[TxTable.Columns.TX_ID].toString("hex")),
      ),
    );
    const transitionTraceMembers = input.transitionTraceMembers.map(
      (entry, ordinal) =>
        retainedRootMemberEntry({
          headerHash: input.headerHash,
          entry,
          ordinal,
          sourceTable: transitionTraceTableName,
          blockEndTime: input.blockEndTime,
        }),
    );
    const eventToStepMembers = input.eventToStepMembers.map((entry, ordinal) =>
      retainedRootMemberEntry({
        headerHash: input.headerHash,
        entry,
        ordinal,
        sourceTable: eventToStepTableName,
        blockEndTime: input.blockEndTime,
      }),
    );
    const validationTraceMembers = input.validationTraceMembers.map(
      (entry, ordinal) =>
        retainedRootMemberEntry({
          headerHash: input.headerHash,
          entry,
          ordinal,
          sourceTable: validationTracesTableName,
          blockEndTime: input.blockEndTime,
        }),
    );
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const activeRows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
          LIMIT 1`;
        const active = activeRows[0];
        if (
          active !== undefined &&
          !active[Columns.HEADER_HASH].equals(input.headerHash)
        ) {
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Refusing to prepare a new pending block while another active pending-finalization record exists",
              cause: `active_header_hash=${active[Columns.HEADER_HASH].toString(
                "hex",
              )},requested_header_hash=${input.headerHash.toString("hex")}`,
            }),
          );
        }
        if (options?.beforeJournalInsert !== undefined) {
          yield* options.beforeJournalInsert;
        }
        if (active !== undefined) {
          yield* sql`DELETE FROM ${sql(tableName)}
            WHERE ${sql(Columns.HEADER_HASH)} = ${input.headerHash}
              AND ${sql(Columns.STATUS)} = ${Status.PendingSubmission}`;
        }
        yield* sql`DELETE FROM ${sql(tableName)}
          WHERE ${sql(Columns.HEADER_HASH)} = ${input.headerHash}
            AND ${sql(Columns.STATUS)} = ${Status.Abandoned}
            AND ${sql(Columns.SUBMITTED_TX_HASH)} IS NULL`;
        yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert({
          [Columns.HEADER_HASH]: input.headerHash,
          [Columns.HEADER_CBOR]: input.headerCbor,
          [Columns.FORMAT_VERSION]: pendingV1.version,
          [Columns.REPLAY_KIND]: pendingV1.replay.kind,
          [Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION]:
            deploymentMarker.schemaVersion,
          [Columns.DEPLOYMENT_MANIFEST_ID]: deploymentMarker.manifestId,
          [Columns.CONSENSUS_PROFILE_ID]: metadata.consensusProfileId,
          [Columns.SUBMITTED_TX_HASH]: null,
          [Columns.STATE_QUEUE_LEASE_TOKEN]: metadata.stateQueueLeaseToken,
          [Columns.BASE_SNAPSHOT_ID]: metadata.baseSnapshotId,
          [Columns.BASE_TAIL_OUT_REF]: metadata.baseTailOutRef,
          [Columns.BASE_TAIL_HEADER_HASH]: metadata.baseTailHeaderHash,
          [Columns.BASE_TAIL_DATUM_CBOR]: metadata.baseTailDatumCbor,
          [Columns.BASE_UTXOS_ROOT]: metadata.baseRoots.utxosRoot,
          [Columns.BASE_FORCED_TRANSACTIONS_ROOT]:
            metadata.baseRoots.forcedTransactionsRoot,
          [Columns.BASE_TRANSACTIONS_ROOT]: metadata.baseRoots.transactionsRoot,
          [Columns.BASE_DEPOSITS_ROOT]: metadata.baseRoots.depositsRoot,
          [Columns.BASE_WITHDRAWALS_ROOT]: metadata.baseRoots.withdrawalsRoot,
          [Columns.BLOCK_START_TIME]: metadata.blockStartTime,
          [Columns.BLOCK_END_TIME]: input.blockEndTime,
          [Columns.EXPECTED_UTXOS_ROOT]: metadata.expectedRoots.utxosRoot,
          [Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT]:
            metadata.expectedRoots.forcedTransactionsRoot,
          [Columns.EXPECTED_TRANSACTIONS_ROOT]:
            metadata.expectedRoots.transactionsRoot,
          [Columns.EXPECTED_DEPOSITS_ROOT]: metadata.expectedRoots.depositsRoot,
          [Columns.EXPECTED_WITHDRAWALS_ROOT]:
            metadata.expectedRoots.withdrawalsRoot,
          [Columns.EXPECTED_TRANSITION_TRACE_ROOT]:
            metadata.expectedRoots.transitionTraceRoot,
          [Columns.EXPECTED_EVENT_TO_STEP_ROOT]:
            metadata.expectedRoots.eventToStepRoot,
          [Columns.EXPECTED_VALIDATION_TRACES_ROOT]:
            metadata.expectedRoots.validationTracesRoot,
          [Columns.EXPECTED_WITHDRAWAL_COUNT]:
            metadata.expectedCounts.withdrawalCount,
          [Columns.EXPECTED_FORCED_TRANSACTION_COUNT]:
            metadata.expectedCounts.forcedTransactionCount,
          [Columns.EXPECTED_L2_TRANSACTION_COUNT]:
            metadata.expectedCounts.l2TransactionCount,
          [Columns.EXPECTED_DEPOSIT_COUNT]:
            metadata.expectedCounts.depositCount,
          [Columns.EXPECTED_TOTAL_EVENT_COUNT]:
            metadata.expectedCounts.totalEventCount,
          [Columns.EXPECTED_TRANSITION_STEP_COUNT]:
            metadata.expectedCounts.transitionStepCount,
          [Columns.EXPECTED_VALIDATION_TRACE_COUNT]:
            metadata.expectedCounts.validationTraceCount,
          [Columns.LEDGER_DELTA_SPENT]: JSON.stringify(
            input.ledgerDelta.spent.map((outref) => outref.toString("hex")),
          ),
          [Columns.LEDGER_DELTA_PRODUCED]: JSON.stringify(
            ledgerDelta.produced.map((entry) => ({
              outref: entry[UtxoColumns.OUTREF].toString("hex"),
              output: entry[UtxoColumns.OUTPUT].toString("hex"),
            })),
          ),
          [Columns.UTXO_PAYLOAD_ENTRY_COUNT]:
            input.utxoPayloadAggregate?.entryCount ?? null,
          [Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES]:
            input.utxoPayloadAggregate?.encodedTupleBytes ?? null,
          [Columns.MPF_OWNER_SCHEMA]: nativeMpfReplay?.schema ?? null,
          [Columns.MPF_OWNER_BINARY_SHA256]:
            nativeMpfReplay?.ownerBinarySha256 ?? null,
          [Columns.MPF_REPLAY_BASE_ROOT]: nativeMpfReplay?.baseRoot ?? null,
          [Columns.MPF_REPLAY_CANDIDATE_ROOT]:
            nativeMpfReplay?.candidateRoot ?? null,
          [Columns.MPF_REPLAY_EVENT_LOG]: nativeMpfReplay?.eventLog ?? null,
          [Columns.MPF_REPLAY_EVENT_LOG_DIGEST]:
            nativeMpfReplay?.eventLogDigest ?? null,
          [Columns.MPF_REPLAY_EVENT_ROOTS]: nativeMpfReplay?.eventRoots ?? null,
          [Columns.MPF_REPLAY_EVENT_COUNT]: nativeMpfReplay?.eventCount ?? null,
          [Columns.STATUS]: Status.PendingSubmission,
          [Columns.OBSERVED_CONFIRMED_AT_MS]: null,
        })}`;
        if (depositMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(depositsTableName)} ${sql.insert(
            depositMembers,
          )}`;
        }
        if (forcedTransactionMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(
            forcedTransactionsTableName,
          )} ${sql.insert(forcedTransactionMembers)}`;
        }
        if (withdrawalMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(withdrawalsTableName)} ${sql.insert(
            withdrawalMembers,
          )}`;
        }
        if (txMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(txsTableName)} ${sql.insert(txMembers)}`;
        }
        if (transitionTraceMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(
            transitionTraceTableName,
          )} ${sql.insert(transitionTraceMembers)}`;
        }
        if (eventToStepMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(eventToStepTableName)} ${sql.insert(
            eventToStepMembers,
          )}`;
        }
        if (validationTraceMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(
            validationTracesTableName,
          )} ${sql.insert(validationTraceMembers)}`;
        }
      }),
    );
  }).pipe(
    Effect.withLogSpan(`preparePendingSubmission ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to prepare pending block finalization",
    ),
  );

export const markSubmitted = (
  headerHash: Buffer,
  submittedTxHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.SUBMITTED_TX_HASH)} = ${submittedTxHash},
          ${sql(Columns.STATUS)} = ${Status.SubmittedLocalFinalizationPending},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} = ${Status.PendingSubmission}
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to mark pending block as submitted",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markSubmitted ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark pending block as submitted",
    ),
  );

export const discardUnsubmittedPendingSubmission = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} = ${Status.PendingSubmission}
        AND ${sql(Columns.SUBMITTED_TX_HASH)} IS NULL`;
  }).pipe(
    Effect.withLogSpan(`discardUnsubmittedPendingSubmission ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to discard unsubmitted pending block journal",
    ),
  );

export const markLocalFinalizationComplete = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.SubmittedUnconfirmed},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.SubmittedLocalFinalizationPending},
          ${Status.SubmittedUnconfirmed}
        )
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to mark pending block as locally finalized and awaiting confirmation",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markLocalFinalizationComplete ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark pending block local finalization complete",
    ),
  );

export const markObservedWaitingStability = (
  headerHash: Buffer,
  observedConfirmedAtMs: bigint,
  submittedTxHash?: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.SUBMITTED_TX_HASH)} = COALESCE(
            ${sql(Columns.SUBMITTED_TX_HASH)},
            ${submittedTxHash ?? null}
          ),
          ${sql(Columns.STATUS)} = ${Status.ObservedWaitingStability},
          ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)} = COALESCE(
            ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)},
            ${observedConfirmedAtMs}
          ),
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.PendingSubmission},
          ${Status.SubmittedLocalFinalizationPending},
          ${Status.SubmittedUnconfirmed},
          ${Status.ObservedWaitingStability}
        )
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to mark pending block as observed waiting stability",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markObservedWaitingStability ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark pending block as observed waiting stability",
    ),
  );

export const reviveAbandonedCanonical = (
  headerHash: Buffer,
  observedConfirmedAtMs: bigint,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.ObservedWaitingStability},
          ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)} = COALESCE(
            ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)},
            ${observedConfirmedAtMs}
          ),
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} = ${Status.Abandoned}
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to revive abandoned canonical pending block journal",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`reviveAbandonedCanonical ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to revive abandoned canonical pending block journal",
    ),
  );

export const markFinalized = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql.withTransaction(
      Effect.gen(function* () {
        const updated = yield* sql<Row>`UPDATE ${sql(tableName)}
          SET ${sql(Columns.STATUS)} = ${Status.Finalized},
              ${sql(Columns.UPDATED_AT)} = NOW()
          WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
            AND ${sql(Columns.STATUS)} IN (
              ${Status.SubmittedUnconfirmed},
              ${Status.ObservedWaitingStability}
            )
          RETURNING *`;
        const finalized = updated[0];
        if (finalized !== undefined) {
          yield* deleteSupersededAbandonedUnsubmittedJournals(sql, finalized);
        }
        return updated;
      }),
    );
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to finalize pending block journal",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markFinalized ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to finalize pending block journal",
    ),
  );

const deleteSupersededAbandonedUnsubmittedJournals = (
  sql: SqlClient.SqlClient,
  finalized: Row,
) =>
  sql<{ readonly header_hash: Buffer }>`DELETE FROM ${sql(tableName)}
    WHERE ${sql(Columns.STATUS)} = ${Status.Abandoned}
      AND ${sql(Columns.SUBMITTED_TX_HASH)} IS NULL
      AND ${sql(Columns.HEADER_HASH)} <> ${finalized[Columns.HEADER_HASH]}
      AND ${sql(Columns.BASE_TAIL_HEADER_HASH)} = ${
        finalized[Columns.BASE_TAIL_HEADER_HASH]
      }
      AND ${sql(Columns.BASE_UTXOS_ROOT)} = ${
        finalized[Columns.BASE_UTXOS_ROOT]
      }
      AND ${sql(Columns.BASE_FORCED_TRANSACTIONS_ROOT)} = ${
        finalized[Columns.BASE_FORCED_TRANSACTIONS_ROOT]
      }
      AND ${sql(Columns.BASE_TRANSACTIONS_ROOT)} = ${
        finalized[Columns.BASE_TRANSACTIONS_ROOT]
      }
      AND ${sql(Columns.BASE_DEPOSITS_ROOT)} = ${
        finalized[Columns.BASE_DEPOSITS_ROOT]
      }
      AND ${sql(Columns.BASE_WITHDRAWALS_ROOT)} = ${
        finalized[Columns.BASE_WITHDRAWALS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_UTXOS_ROOT)} = ${
        finalized[Columns.EXPECTED_UTXOS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT)} = ${
        finalized[Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_TRANSACTIONS_ROOT)} = ${
        finalized[Columns.EXPECTED_TRANSACTIONS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_DEPOSITS_ROOT)} = ${
        finalized[Columns.EXPECTED_DEPOSITS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_WITHDRAWALS_ROOT)} = ${
        finalized[Columns.EXPECTED_WITHDRAWALS_ROOT]
      }
      AND ${sql(Columns.EXPECTED_TRANSITION_TRACE_ROOT)} = ${
        finalized[Columns.EXPECTED_TRANSITION_TRACE_ROOT]
      }
      AND ${sql(Columns.EXPECTED_EVENT_TO_STEP_ROOT)} = ${
        finalized[Columns.EXPECTED_EVENT_TO_STEP_ROOT]
      }
      AND ${sql(Columns.EXPECTED_WITHDRAWAL_COUNT)} = ${
        finalized[Columns.EXPECTED_WITHDRAWAL_COUNT]
      }
      AND ${sql(Columns.EXPECTED_FORCED_TRANSACTION_COUNT)} = ${
        finalized[Columns.EXPECTED_FORCED_TRANSACTION_COUNT]
      }
      AND ${sql(Columns.EXPECTED_L2_TRANSACTION_COUNT)} = ${
        finalized[Columns.EXPECTED_L2_TRANSACTION_COUNT]
      }
      AND ${sql(Columns.EXPECTED_DEPOSIT_COUNT)} = ${
        finalized[Columns.EXPECTED_DEPOSIT_COUNT]
      }
      AND ${sql(Columns.EXPECTED_TOTAL_EVENT_COUNT)} = ${
        finalized[Columns.EXPECTED_TOTAL_EVENT_COUNT]
      }
      AND ${sql(Columns.EXPECTED_TRANSITION_STEP_COUNT)} = ${
        finalized[Columns.EXPECTED_TRANSITION_STEP_COUNT]
      }
    RETURNING ${sql(Columns.HEADER_HASH)}`;

export const deleteSupersededAbandonedUnsubmitted = (): Effect.Effect<
  number,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql.withTransaction(
      Effect.gen(function* () {
        const finalizedRows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} = ${Status.Finalized}
          ORDER BY ${sql(Columns.CREATED_AT)} ASC`;
        const deletedRows = yield* Effect.forEach(
          finalizedRows,
          (finalized) =>
            decodePendingBlockFinalizationRowV1(finalized).pipe(
              Effect.flatMap(({ normalizedRow }) =>
                deleteSupersededAbandonedUnsubmittedJournals(
                  sql,
                  normalizedRow,
                ),
              ),
            ),
          { concurrency: 1 },
        );
        return deletedRows.reduce((total, rows) => total + rows.length, 0);
      }),
    );
    return deleted;
  }).pipe(
    Effect.withLogSpan(`deleteSupersededAbandonedUnsubmitted ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to delete superseded pre-submit pending block journals",
    ),
  );

export const markAbandoned = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Abandoned},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to abandon pending block journal",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markAbandoned ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to abandon pending block journal",
    ),
  );

/**
 * Terminal transition for a locally known block removed by authenticated
 * state-queue correction. Idempotence is restricted to the resulting
 * Abandoned state; every other missing/conflicting status fails closed.
 */
export const markCorrectedAfterStateQueueRemoval = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const existing = yield* sql<Pick<Row, Columns.STATUS>>`SELECT ${sql(
      Columns.STATUS,
    )} FROM ${sql(tableName)} WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}`;
    if (existing[0]?.[Columns.STATUS] === Status.Abandoned) return;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Abandoned},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.SubmittedLocalFinalizationPending},
          ${Status.SubmittedUnconfirmed},
          ${Status.ObservedWaitingStability},
          ${Status.Finalized}
        )
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to mark state-queue-corrected block abandoned",
          cause: `header_hash=${headerHash.toString("hex")},status=${existing[0]?.[Columns.STATUS] ?? "missing"}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markCorrectedAfterStateQueueRemoval ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark state-queue-corrected journal",
    ),
  );

export const markUnsubmittedAbandoned = (
  headerHash: Buffer,
): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Abandoned},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} = ${Status.PendingSubmission}
        AND ${sql(Columns.SUBMITTED_TX_HASH)} IS NULL
      RETURNING *`;
    return rows.length === 1;
  }).pipe(
    Effect.withLogSpan(`markUnsubmittedAbandoned ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to abandon unsubmitted pending block journal",
    ),
  );

export const txMemberToEntry = (
  member: MemberRecord,
): TxTable.EntryWithTimeStamp => ({
  [TxTable.Columns.TX_ID]: Buffer.from(member[MemberColumns.MEMBER_ID]),
  [TxTable.Columns.TX]: Buffer.from(member[MemberColumns.PAYLOAD_CBOR]),
  [TxTable.Columns.TIMESTAMPTZ]: member[MemberColumns.SOURCE_TIMESTAMP],
});

export const assertActiveJournalPayloadsComplete: Effect.Effect<
  void,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const active = yield* retrieveActive();
  if (Option.isNone(active)) {
    return;
  }
  const record = active.value;
  const hasIncompletePayload = (members: readonly MemberRecord[]): boolean =>
    members.some(
      (member) =>
        member[MemberColumns.PAYLOAD_CBOR].length <= 0 ||
        member[MemberColumns.PAYLOAD_SHA256].length !== 32,
    );
  if (
    record[Columns.HEADER_CBOR].length <= 0 ||
    [
      record.txMembers,
      record.depositMembers,
      record.forcedTransactionMembers,
      record.withdrawalMembers,
      record.transitionTraceMembers,
      record.eventToStepMembers,
      record.validationTraceMembers,
    ].some(hasIncompletePayload) ||
    BigInt(record.validationTraceMembers.length) !==
      record[Columns.EXPECTED_VALIDATION_TRACE_COUNT]
  ) {
    return yield* Effect.fail(
      new DatabaseError({
        table: tableName,
        message:
          "Active pending-finalization journal has incomplete durable payload members",
        cause: `header_hash=${record[Columns.HEADER_HASH].toString("hex")}`,
      }),
    );
  }
}).pipe(Effect.withLogSpan(`assertActiveJournalPayloadsComplete ${tableName}`));

export const clear = Effect.all(
  [
    clearTable(depositsTableName),
    clearTable(forcedTransactionsTableName),
    clearTable(withdrawalsTableName),
    clearTable(txsTableName),
    clearTable(transitionTraceTableName),
    clearTable(eventToStepTableName),
    clearTable(validationTracesTableName),
    clearTable(tableName),
  ],
  { discard: true },
);
