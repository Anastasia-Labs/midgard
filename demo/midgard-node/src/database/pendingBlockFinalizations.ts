import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { createHash } from "node:crypto";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as DepositsDB from "@/database/deposits.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import * as TxTable from "@/database/utils/tx.js";

export const tableName = "pending_block_finalizations";
const depositsTableName = "pending_block_finalization_deposits";
const withdrawalsTableName = "pending_block_finalization_withdrawals";
const txsTableName = "pending_block_finalization_txs";

export enum Columns {
  HEADER_HASH = "header_hash",
  SUBMITTED_TX_HASH = "submitted_tx_hash",
  STATE_QUEUE_LEASE_TOKEN = "state_queue_lease_token",
  BASE_SNAPSHOT_ID = "base_snapshot_id",
  BASE_TAIL_OUT_REF = "base_tail_out_ref",
  BASE_TAIL_HEADER_HASH = "base_tail_header_hash",
  BASE_TAIL_DATUM_CBOR = "base_tail_datum_cbor",
  BASE_UTXOS_ROOT = "base_utxos_root",
  BASE_TRANSACTIONS_ROOT = "base_transactions_root",
  BASE_DEPOSITS_ROOT = "base_deposits_root",
  BASE_WITHDRAWALS_ROOT = "base_withdrawals_root",
  BLOCK_START_TIME = "block_start_time",
  BLOCK_END_TIME = "block_end_time",
  EXPECTED_UTXOS_ROOT = "expected_utxos_root",
  EXPECTED_TRANSACTIONS_ROOT = "expected_transactions_root",
  EXPECTED_DEPOSITS_ROOT = "expected_deposits_root",
  EXPECTED_WITHDRAWALS_ROOT = "expected_withdrawals_root",
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
  [Columns.SUBMITTED_TX_HASH]: Buffer | null;
  [Columns.STATE_QUEUE_LEASE_TOKEN]: string;
  [Columns.BASE_SNAPSHOT_ID]: string;
  [Columns.BASE_TAIL_OUT_REF]: string;
  [Columns.BASE_TAIL_HEADER_HASH]: Buffer;
  [Columns.BASE_TAIL_DATUM_CBOR]: string;
  [Columns.BASE_UTXOS_ROOT]: string;
  [Columns.BASE_TRANSACTIONS_ROOT]: string;
  [Columns.BASE_DEPOSITS_ROOT]: string;
  [Columns.BASE_WITHDRAWALS_ROOT]: string;
  [Columns.BLOCK_START_TIME]: Date;
  [Columns.BLOCK_END_TIME]: Date;
  [Columns.EXPECTED_UTXOS_ROOT]: string;
  [Columns.EXPECTED_TRANSACTIONS_ROOT]: string;
  [Columns.EXPECTED_DEPOSITS_ROOT]: string;
  [Columns.EXPECTED_WITHDRAWALS_ROOT]: string;
  [Columns.STATUS]: Status;
  [Columns.OBSERVED_CONFIRMED_AT_MS]: bigint | null;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
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

export type Record = Row & {
  readonly depositEventIds: readonly Buffer[];
  readonly withdrawalEventIds: readonly Buffer[];
  readonly mempoolTxIds: readonly Buffer[];
  readonly depositMembers: readonly MemberRecord[];
  readonly withdrawalMembers: readonly MemberRecord[];
  readonly txMembers: readonly MemberRecord[];
};

export type PendingBlockFinalizationMetadata = {
  readonly stateQueueLeaseToken: string;
  readonly baseSnapshotId: string;
  readonly baseTailOutRef: string;
  readonly baseTailHeaderHash: Buffer;
  readonly baseTailDatumCbor: string;
  readonly baseRoots: {
    readonly utxosRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
  };
  readonly blockStartTime: Date;
  readonly expectedRoots: {
    readonly utxosRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
  };
};

export type PrepareInput = {
  readonly headerHash: Buffer;
  readonly metadata: PendingBlockFinalizationMetadata;
  readonly blockEndTime: Date;
  readonly depositEventIds: readonly Buffer[];
  readonly depositEntries: readonly DepositsDB.Entry[];
  readonly withdrawalEventIds?: readonly Buffer[];
  readonly withdrawalEntries?: readonly WithdrawalsDB.Entry[];
  readonly mempoolTxIds: readonly Buffer[];
  readonly mempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxSourceTable: string;
};

const uniqueBuffers = (values: readonly Buffer[]): readonly Buffer[] =>
  Array.from(new Set(values.map((value) => value.toString("hex")))).map((hex) =>
    Buffer.from(hex, "hex"),
  );

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

const retrieveMembers = (
  sql: SqlClient.SqlClient,
  memberTableName: string,
  headerHash: Buffer,
): Effect.Effect<readonly MemberRecord[], never, never> =>
  Effect.gen(function* () {
    const rows = yield* sql<MemberRecord>`SELECT * FROM ${sql(memberTableName)}
      WHERE ${sql(MemberColumns.HEADER_HASH)} = ${headerHash}
      ORDER BY ${sql(MemberColumns.ORDINAL)} ASC`;
    return rows;
  }).pipe(Effect.orDie);

const retrieveRecord = (
  sql: SqlClient.SqlClient,
  row: Row,
): Effect.Effect<Record, never, never> =>
  Effect.gen(function* () {
    const [depositEventIds, withdrawalEventIds, mempoolTxIds] = yield* Effect.all(
      [
        retrieveMembers(sql, depositsTableName, row[Columns.HEADER_HASH]),
        retrieveMembers(sql, withdrawalsTableName, row[Columns.HEADER_HASH]),
        retrieveMembers(sql, txsTableName, row[Columns.HEADER_HASH]),
      ],
      { concurrency: "unbounded" },
    );
    return {
      ...row,
      depositEventIds: depositEventIds.map(
        (member) => member[MemberColumns.MEMBER_ID],
      ),
      withdrawalEventIds: withdrawalEventIds.map(
        (member) => member[MemberColumns.MEMBER_ID],
      ),
      mempoolTxIds: mempoolTxIds.map((member) => member[MemberColumns.MEMBER_ID]),
      depositMembers: depositEventIds,
      withdrawalMembers: withdrawalEventIds,
      txMembers: mempoolTxIds,
    };
  }).pipe(Effect.orDie);

export const retrieveActive = (): Effect.Effect<
  Option.Option<Record>,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
      ORDER BY ${sql(Columns.CREATED_AT)} ASC
      LIMIT 1`;
    if (rows.length <= 0) {
      return Option.none();
    }
    return Option.some(yield* retrieveRecord(sql, rows[0]!));
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
    const rows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
      LIMIT 1`;
    if (rows.length <= 0) {
      return Option.none();
    }
    return Option.some(yield* retrieveRecord(sql, rows[0]!));
  }).pipe(
    Effect.withLogSpan(`retrieveByHeaderHash ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve pending-finalization record by header hash",
    ),
  );

export const preparePendingSubmission = (
  input: PrepareInput,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const depositEventIds = uniqueBuffers(input.depositEventIds);
    const withdrawalEventIds = uniqueBuffers(input.withdrawalEventIds ?? []);
    const mempoolTxIds = uniqueBuffers(input.mempoolTxIds);
    yield* assertSameIdSet(
      tableName,
      "deposit",
      depositEventIds,
      input.depositEntries.map((entry) => entry[DepositsDB.Columns.ID]),
    );
    yield* assertSameIdSet(
      tableName,
      "withdrawal",
      withdrawalEventIds,
      (input.withdrawalEntries ?? []).map(
        (entry) => entry[WithdrawalsDB.Columns.ID],
      ),
    );
    yield* assertSameIdSet(
      tableName,
      "mempool tx",
      mempoolTxIds,
      input.mempoolTxs.map((entry) => entry[TxTable.Columns.TX_ID]),
    );
    const depositMembers = input.depositEntries.map((entry, ordinal) =>
      depositMemberEntry(input.headerHash, entry, ordinal),
    );
    const withdrawalMembers = yield* Effect.forEach(
      input.withdrawalEntries ?? [],
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
        yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert({
          [Columns.HEADER_HASH]: input.headerHash,
          [Columns.SUBMITTED_TX_HASH]: null,
          [Columns.STATE_QUEUE_LEASE_TOKEN]:
            input.metadata.stateQueueLeaseToken,
          [Columns.BASE_SNAPSHOT_ID]: input.metadata.baseSnapshotId,
          [Columns.BASE_TAIL_OUT_REF]: input.metadata.baseTailOutRef,
          [Columns.BASE_TAIL_HEADER_HASH]:
            input.metadata.baseTailHeaderHash,
          [Columns.BASE_TAIL_DATUM_CBOR]: input.metadata.baseTailDatumCbor,
          [Columns.BASE_UTXOS_ROOT]: input.metadata.baseRoots.utxosRoot,
          [Columns.BASE_TRANSACTIONS_ROOT]:
            input.metadata.baseRoots.transactionsRoot,
          [Columns.BASE_DEPOSITS_ROOT]: input.metadata.baseRoots.depositsRoot,
          [Columns.BASE_WITHDRAWALS_ROOT]:
            input.metadata.baseRoots.withdrawalsRoot,
          [Columns.BLOCK_START_TIME]: input.metadata.blockStartTime,
          [Columns.BLOCK_END_TIME]: input.blockEndTime,
          [Columns.EXPECTED_UTXOS_ROOT]:
            input.metadata.expectedRoots.utxosRoot,
          [Columns.EXPECTED_TRANSACTIONS_ROOT]:
            input.metadata.expectedRoots.transactionsRoot,
          [Columns.EXPECTED_DEPOSITS_ROOT]:
            input.metadata.expectedRoots.depositsRoot,
          [Columns.EXPECTED_WITHDRAWALS_ROOT]:
            input.metadata.expectedRoots.withdrawalsRoot,
          [Columns.STATUS]: Status.PendingSubmission,
          [Columns.OBSERVED_CONFIRMED_AT_MS]: null,
        })}`;
        if (depositMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(depositsTableName)} ${sql.insert(
            depositMembers,
          )}`;
        }
        if (withdrawalMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(withdrawalsTableName)} ${sql.insert(
            withdrawalMembers,
          )}`;
        }
        if (txMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(txsTableName)} ${sql.insert(
            txMembers,
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
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Finalized},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.SubmittedUnconfirmed},
          ${Status.ObservedWaitingStability}
        )
      RETURNING *`;
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
  const incompleteTxMember = record.txMembers.find(
    (member) =>
      member[MemberColumns.PAYLOAD_CBOR].length <= 0 ||
      member[MemberColumns.PAYLOAD_SHA256].length !== 32,
  );
  const incompleteDepositMember = record.depositMembers.find(
    (member) =>
      member[MemberColumns.PAYLOAD_CBOR].length <= 0 ||
      member[MemberColumns.PAYLOAD_SHA256].length !== 32,
  );
  const incompleteWithdrawalMember = record.withdrawalMembers.find(
    (member) =>
      member[MemberColumns.PAYLOAD_CBOR].length <= 0 ||
      member[MemberColumns.PAYLOAD_SHA256].length !== 32,
  );
  if (
    incompleteTxMember !== undefined ||
    incompleteDepositMember !== undefined ||
    incompleteWithdrawalMember !== undefined
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
}).pipe(
  Effect.withLogSpan(`assertActiveJournalPayloadsComplete ${tableName}`),
);

export const clear = Effect.all(
  [
    clearTable(depositsTableName),
    clearTable(withdrawalsTableName),
    clearTable(txsTableName),
    clearTable(tableName),
  ],
  { discard: true },
);
