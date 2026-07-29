import { createHash } from "node:crypto";

import { decodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
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
import * as TxTable from "@/database/utils/tx.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { Database } from "@/services/database.js";

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
  [Columns.CONSENSUS_PROFILE_ID]: "midgard-consensus-v1";
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

export type PendingBlockFinalizationMetadata = {
  readonly consensusProfileId: "midgard-consensus-v1";
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
  if (values.every((value) => value == null)) return undefined;
  if (values.some((value) => value == null)) {
    throw new Error("Architecture G replay journal fields are partially null");
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
  assertNativeMpfReplay(replay);
  return replay;
};

const decodeHexArray = (value: unknown, label: string): readonly Buffer[] => {
  if (!Array.isArray(value) || value.some((item) => typeof item !== "string")) {
    throw new Error(`${label} must be an array of hex strings`);
  }
  return value.map((item) => Buffer.from(item as string, "hex"));
};

const decodeLedgerDelta = (row: Row): LedgerDeltaInput => {
  const parseJson = (value: unknown): unknown =>
    typeof value === "string" ? JSON.parse(value) : value;
  const spent = parseJson(row[Columns.LEDGER_DELTA_SPENT]);
  const produced = parseJson(row[Columns.LEDGER_DELTA_PRODUCED]);
  if (!Array.isArray(produced)) {
    throw new Error("ledger_delta_produced must be an array");
  }
  return {
    spent: decodeHexArray(spent, "ledger_delta_spent"),
    produced: produced.map((entry) => {
      if (
        typeof entry !== "object" ||
        entry === null ||
        !("outref" in entry) ||
        !("output" in entry) ||
        typeof entry.outref !== "string" ||
        typeof entry.output !== "string"
      ) {
        throw new Error("Invalid ledger_delta_produced entry");
      }
      return {
        [UtxoColumns.OUTREF]: Buffer.from(entry.outref, "hex"),
        [UtxoColumns.OUTPUT]: Buffer.from(entry.output, "hex"),
      };
    }),
  };
};

const sha256 = (payload: Buffer): Buffer =>
  createHash("sha256").update(payload).digest();

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
      canonicalTransactionCbor.length === 0 ||
      programMaterialSidecarCbor.length === 0 ||
      programMaterialSidecarSha256.length !== 32 ||
      !sha256(programMaterialSidecarCbor).equals(programMaterialSidecarSha256)
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "Refusing to journal a V1 forced transaction without its canonical transaction preimage and authenticated CEK material sidecar",
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

const retrieveRecord = (
  sql: SqlClient.SqlClient,
  row: RawRow,
): Effect.Effect<Record, never, never> =>
  Effect.gen(function* () {
    const normalizedRow = normalizeRow(row);
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
    return {
      ...normalizedRow,
      ledgerDelta: decodeLedgerDelta(normalizedRow),
      nativeMpfReplay: decodeNativeMpfReplay(normalizedRow),
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
  }).pipe(Effect.orDie);

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
    return rows.map(normalizeRow);
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
    return rows.map(normalizeRow);
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
    if (input.nativeMpfReplay !== undefined) {
      yield* Effect.try({
        try: () => assertNativeMpfReplay(input.nativeMpfReplay!),
        catch: (cause) =>
          new DatabaseError({
            table: tableName,
            message:
              "Refusing to prepare an invalid Architecture G replay journal",
            cause,
          }),
      });
    }
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
          [Columns.CONSENSUS_PROFILE_ID]: input.metadata.consensusProfileId,
          [Columns.SUBMITTED_TX_HASH]: null,
          [Columns.STATE_QUEUE_LEASE_TOKEN]:
            input.metadata.stateQueueLeaseToken,
          [Columns.BASE_SNAPSHOT_ID]: input.metadata.baseSnapshotId,
          [Columns.BASE_TAIL_OUT_REF]: input.metadata.baseTailOutRef,
          [Columns.BASE_TAIL_HEADER_HASH]: input.metadata.baseTailHeaderHash,
          [Columns.BASE_TAIL_DATUM_CBOR]: input.metadata.baseTailDatumCbor,
          [Columns.BASE_UTXOS_ROOT]: input.metadata.baseRoots.utxosRoot,
          [Columns.BASE_FORCED_TRANSACTIONS_ROOT]:
            input.metadata.baseRoots.forcedTransactionsRoot,
          [Columns.BASE_TRANSACTIONS_ROOT]:
            input.metadata.baseRoots.transactionsRoot,
          [Columns.BASE_DEPOSITS_ROOT]: input.metadata.baseRoots.depositsRoot,
          [Columns.BASE_WITHDRAWALS_ROOT]:
            input.metadata.baseRoots.withdrawalsRoot,
          [Columns.BLOCK_START_TIME]: input.metadata.blockStartTime,
          [Columns.BLOCK_END_TIME]: input.blockEndTime,
          [Columns.EXPECTED_UTXOS_ROOT]: input.metadata.expectedRoots.utxosRoot,
          [Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT]:
            input.metadata.expectedRoots.forcedTransactionsRoot,
          [Columns.EXPECTED_TRANSACTIONS_ROOT]:
            input.metadata.expectedRoots.transactionsRoot,
          [Columns.EXPECTED_DEPOSITS_ROOT]:
            input.metadata.expectedRoots.depositsRoot,
          [Columns.EXPECTED_WITHDRAWALS_ROOT]:
            input.metadata.expectedRoots.withdrawalsRoot,
          [Columns.EXPECTED_TRANSITION_TRACE_ROOT]:
            input.metadata.expectedRoots.transitionTraceRoot,
          [Columns.EXPECTED_EVENT_TO_STEP_ROOT]:
            input.metadata.expectedRoots.eventToStepRoot,
          [Columns.EXPECTED_VALIDATION_TRACES_ROOT]:
            input.metadata.expectedRoots.validationTracesRoot,
          [Columns.EXPECTED_WITHDRAWAL_COUNT]:
            input.metadata.expectedCounts.withdrawalCount,
          [Columns.EXPECTED_FORCED_TRANSACTION_COUNT]:
            input.metadata.expectedCounts.forcedTransactionCount,
          [Columns.EXPECTED_L2_TRANSACTION_COUNT]:
            input.metadata.expectedCounts.l2TransactionCount,
          [Columns.EXPECTED_DEPOSIT_COUNT]:
            input.metadata.expectedCounts.depositCount,
          [Columns.EXPECTED_TOTAL_EVENT_COUNT]:
            input.metadata.expectedCounts.totalEventCount,
          [Columns.EXPECTED_TRANSITION_STEP_COUNT]:
            input.metadata.expectedCounts.transitionStepCount,
          [Columns.EXPECTED_VALIDATION_TRACE_COUNT]:
            input.metadata.expectedCounts.validationTraceCount,
          [Columns.LEDGER_DELTA_SPENT]: JSON.stringify(
            input.ledgerDelta.spent.map((outref) => outref.toString("hex")),
          ),
          [Columns.LEDGER_DELTA_PRODUCED]: JSON.stringify(
            input.ledgerDelta.produced.map((entry) => ({
              outref: entry[UtxoColumns.OUTREF].toString("hex"),
              output: entry[UtxoColumns.OUTPUT].toString("hex"),
            })),
          ),
          [Columns.UTXO_PAYLOAD_ENTRY_COUNT]:
            input.utxoPayloadAggregate?.entryCount ?? null,
          [Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES]:
            input.utxoPayloadAggregate?.encodedTupleBytes ?? null,
          [Columns.MPF_OWNER_SCHEMA]: input.nativeMpfReplay?.schema ?? null,
          [Columns.MPF_OWNER_BINARY_SHA256]:
            input.nativeMpfReplay?.ownerBinarySha256 ?? null,
          [Columns.MPF_REPLAY_BASE_ROOT]:
            input.nativeMpfReplay?.baseRoot ?? null,
          [Columns.MPF_REPLAY_CANDIDATE_ROOT]:
            input.nativeMpfReplay?.candidateRoot ?? null,
          [Columns.MPF_REPLAY_EVENT_LOG]:
            input.nativeMpfReplay?.eventLog ?? null,
          [Columns.MPF_REPLAY_EVENT_LOG_DIGEST]:
            input.nativeMpfReplay?.eventLogDigest ?? null,
          [Columns.MPF_REPLAY_EVENT_ROOTS]:
            input.nativeMpfReplay?.eventRoots ?? null,
          [Columns.MPF_REPLAY_EVENT_COUNT]:
            input.nativeMpfReplay?.eventCount ?? null,
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
            deleteSupersededAbandonedUnsubmittedJournals(
              sql,
              normalizeRow(finalized),
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
