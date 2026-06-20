import { createHash } from "node:crypto";

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
const utxosTableName = "pending_block_finalization_utxos";
const transitionTraceTableName = "pending_block_finalization_transition_trace";
const eventToStepTableName = "pending_block_finalization_event_to_step";

export enum Columns {
  HEADER_HASH = "header_hash",
  HEADER_CBOR = "header_cbor",
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
  EXPECTED_WITHDRAWAL_COUNT = "expected_withdrawal_count",
  EXPECTED_FORCED_TRANSACTION_COUNT = "expected_forced_transaction_count",
  EXPECTED_L2_TRANSACTION_COUNT = "expected_l2_transaction_count",
  EXPECTED_DEPOSIT_COUNT = "expected_deposit_count",
  EXPECTED_TOTAL_EVENT_COUNT = "expected_total_event_count",
  EXPECTED_TRANSITION_STEP_COUNT = "expected_transition_step_count",
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
  SOURCE_TABLE = "source_table",
  SOURCE_ID = "source_id",
  SOURCE_TIMESTAMP = "source_time_stamp_tz",
}

export enum UtxoColumns {
  HEADER_HASH = "header_hash",
  OUTREF = "outref",
  ORDINAL = "ordinal",
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
  [Columns.EXPECTED_WITHDRAWAL_COUNT]: bigint;
  [Columns.EXPECTED_FORCED_TRANSACTION_COUNT]: bigint;
  [Columns.EXPECTED_L2_TRANSACTION_COUNT]: bigint;
  [Columns.EXPECTED_DEPOSIT_COUNT]: bigint;
  [Columns.EXPECTED_TOTAL_EVENT_COUNT]: bigint;
  [Columns.EXPECTED_TRANSITION_STEP_COUNT]: bigint;
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
  | Columns.OBSERVED_CONFIRMED_AT_MS
> & {
  [Columns.EXPECTED_WITHDRAWAL_COUNT]: PgBigInt;
  [Columns.EXPECTED_FORCED_TRANSACTION_COUNT]: PgBigInt;
  [Columns.EXPECTED_L2_TRANSACTION_COUNT]: PgBigInt;
  [Columns.EXPECTED_DEPOSIT_COUNT]: PgBigInt;
  [Columns.EXPECTED_TOTAL_EVENT_COUNT]: PgBigInt;
  [Columns.EXPECTED_TRANSITION_STEP_COUNT]: PgBigInt;
  [Columns.OBSERVED_CONFIRMED_AT_MS]: PgBigInt | null;
};

export type MemberRecord = {
  [MemberColumns.HEADER_HASH]: Buffer;
  [MemberColumns.MEMBER_ID]: Buffer;
  [MemberColumns.ORDINAL]: number;
  [MemberColumns.PAYLOAD_CBOR]: Buffer;
  [MemberColumns.PAYLOAD_SHA256]: Buffer;
  [MemberColumns.SOURCE_TABLE]: string;
  [MemberColumns.SOURCE_ID]: Buffer;
  [MemberColumns.SOURCE_TIMESTAMP]: Date;
};

export type UtxoRecord = {
  [UtxoColumns.HEADER_HASH]: Buffer;
  [UtxoColumns.OUTREF]: Buffer;
  [UtxoColumns.ORDINAL]: number;
  [UtxoColumns.OUTPUT]: Buffer;
};

export type UtxoInput = Omit<
  UtxoRecord,
  UtxoColumns.HEADER_HASH | UtxoColumns.ORDINAL
>;

export type RetainedRootMemberInput = {
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
};

const toBigInt = (value: PgBigInt): bigint =>
  typeof value === "bigint" ? value : BigInt(value);

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
  readonly utxoMembers: readonly UtxoRecord[];
};

export type PendingBlockFinalizationMetadata = {
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
  };
  readonly expectedCounts: {
    readonly withdrawalCount: bigint;
    readonly forcedTransactionCount: bigint;
    readonly l2TransactionCount: bigint;
    readonly depositCount: bigint;
    readonly totalEventCount: bigint;
    readonly transitionStepCount: bigint;
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
  readonly mempoolTxSourceTable: string;
  readonly transitionTraceMembers: readonly RetainedRootMemberInput[];
  readonly eventToStepMembers: readonly RetainedRootMemberInput[];
  readonly utxoEntries: readonly UtxoInput[];
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
): MemberRecord => {
  const payload = Buffer.from(entry[TxTable.Columns.TX]);
  const memberId = Buffer.from(entry[TxTable.Columns.TX_ID]);
  return {
    [MemberColumns.HEADER_HASH]: headerHash,
    [MemberColumns.MEMBER_ID]: memberId,
    [MemberColumns.ORDINAL]: ordinal,
    [MemberColumns.PAYLOAD_CBOR]: payload,
    [MemberColumns.PAYLOAD_SHA256]: sha256(payload),
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
): MemberRecord => {
  const payload = Buffer.from(
    entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
  );
  const memberId = Buffer.from(entry[ForcedTransactionsDB.Columns.TX_ORDER_ID]);
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
};

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

const utxoMemberEntry = (
  headerHash: Buffer,
  entry: UtxoInput,
  ordinal: number,
): UtxoRecord => ({
  [UtxoColumns.HEADER_HASH]: headerHash,
  [UtxoColumns.OUTREF]: Buffer.from(entry[UtxoColumns.OUTREF]),
  [UtxoColumns.ORDINAL]: ordinal,
  [UtxoColumns.OUTPUT]: Buffer.from(entry[UtxoColumns.OUTPUT]),
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

const retrieveUtxoMembers = (
  sql: SqlClient.SqlClient,
  headerHash: Buffer,
): Effect.Effect<readonly UtxoRecord[], never, never> =>
  Effect.gen(function* () {
    return yield* sql<UtxoRecord>`SELECT * FROM ${sql(utxosTableName)}
      WHERE ${sql(UtxoColumns.HEADER_HASH)} = ${headerHash}
      ORDER BY ${sql(UtxoColumns.ORDINAL)} ASC`;
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
      utxoMembers,
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
        retrieveUtxoMembers(sql, normalizedRow[Columns.HEADER_HASH]),
      ],
      { concurrency: "unbounded" },
    );
    return {
      ...normalizedRow,
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
      utxoMembers,
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
    const depositMembers = input.depositEntries.map((entry, ordinal) =>
      depositMemberEntry(input.headerHash, entry, ordinal),
    );
    const forcedTransactionMembers = input.forcedTransactionEntries.map(
      (entry, ordinal) =>
        forcedTransactionMemberEntry(input.headerHash, entry, ordinal),
    );
    const withdrawalMembers = yield* Effect.forEach(
      input.withdrawalEntries,
      (entry, ordinal) =>
        withdrawalMemberEntry(input.headerHash, entry, ordinal),
    );
    const txMembers = input.mempoolTxs.map((entry, ordinal) =>
      txMemberEntry(
        input.headerHash,
        entry,
        ordinal,
        input.mempoolTxSourceTable,
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
    const utxoMembers = input.utxoEntries.map((entry, ordinal) =>
      utxoMemberEntry(input.headerHash, entry, ordinal),
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
        if (utxoMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(utxosTableName)} ${sql.insert(
            utxoMembers,
          )}`;
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
    ].some(hasIncompletePayload)
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
    clearTable(utxosTableName),
    clearTable(tableName),
  ],
  { discard: true },
);
