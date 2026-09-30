import { SqlClient, type Statement } from "@effect/sql";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import { sha256 } from "../sha256.js";
import {
  ACTIVE_STATUSES,
  Columns,
  depositsTableName,
  eventToStepTableName,
  forcedTransactionsTableName,
  MemberColumns,
  type RawRow,
  type Row,
  Status,
  tableName,
  transitionTraceTableName,
  txsTableName,
  validationTracesTableName,
  validationTraceWitnessesTableName,
  WithdrawalMemberColumns,
  type WithdrawalMemberRecord,
  withdrawalsTableName,
} from "./pendingBlockFinalizations.columns.js";
import {
  decodePendingBlockFinalizationRow,
  retrieveMembers,
  validateForcedTransactionJournalMembers,
  withdrawalClassificationDigest,
} from "./pendingBlockFinalizations.decode-pending-block-finalization-row.js";
import { type Record } from "./pendingBlockFinalizations.parse-ledger-delta.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as WithdrawalsDB from "./withdrawals.js";

export const retrieveRecord = (
  sql: SqlClient.SqlClient,
  row: RawRow,
): Effect.Effect<Record, DatabaseError, never> =>
  Effect.gen(function* () {
    const decoded = yield* decodePendingBlockFinalizationRow(row);
    const { normalizedRow, ledgerDelta, nativeMpfReplay } = decoded;
    const [
      depositEventIds,
      forcedTransactionEventIds,
      withdrawalEventIds,
      mempoolTxIds,
      transitionTraceMembers,
      eventToStepMembers,
      validationTraceMembers,
      validationTraceWitnessMembers,
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
        retrieveMembers<WithdrawalMemberRecord>(
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
        retrieveMembers(
          sql,
          validationTraceWitnessesTableName,
          normalizedRow[Columns.HEADER_HASH],
        ),
      ],
      { concurrency: 1 },
    );
    yield* validateForcedTransactionJournalMembers(
      forcedTransactionEventIds,
      normalizedRow[Columns.HEADER_HASH],
    );
    yield* Effect.try({
      try: () => {
        for (const member of withdrawalEventIds) {
          if (
            !member[MemberColumns.HEADER_HASH].equals(
              normalizedRow[Columns.HEADER_HASH],
            ) ||
            member[MemberColumns.SOURCE_TABLE] !== WithdrawalsDB.tableName ||
            !member[MemberColumns.SOURCE_ID].equals(
              member[MemberColumns.MEMBER_ID],
            ) ||
            !sha256(member[MemberColumns.PAYLOAD_CBOR]).equals(
              member[MemberColumns.PAYLOAD_SHA256],
            ) ||
            !Object.values(WithdrawalsDB.Validity).includes(
              member[WithdrawalMemberColumns.VALIDITY],
            ) ||
            !withdrawalClassificationDigest(member).equals(
              member[WithdrawalMemberColumns.CLASSIFICATION_SHA256],
            )
          )
            throw new Error(
              "Withdrawal journal classification identity or digest mismatch",
            );
        }
      },
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to load an invalid withdrawal classification journal",
          cause,
        }),
    });
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
      validationTraceWitnessMembers,
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
  lock = false,
): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
      LIMIT 1 ${lock ? sql`FOR UPDATE` : sql``}`;
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

/**
 * The newest finalized journal matching `filter`, newest by block window (the
 * state queue orders blocks by time, so this is chain order), or none.
 */
const retrieveNewestFinalizedWhere = (
  label: string,
  filter: (sql: SqlClient.SqlClient) => Statement.Fragment,
): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = ${Status.Finalized} AND ${filter(sql)}
      ORDER BY ${sql(Columns.BLOCK_END_TIME)} DESC, ${sql(Columns.CREATED_AT)} DESC
      LIMIT 1`;
    return rows.length === 0
      ? Option.none()
      : Option.some(yield* retrieveRecord(sql, rows[0]!));
  }).pipe(
    Effect.withLogSpan(`${label} ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      `Failed to retrieve finalized pending-finalization record (${label})`,
    ),
  );

/**
 * This node's newest finalized block, by block window. It is not necessarily
 * the native committed point: a correction rewind or signed-header recovery
 * applied after it resets the native root to that recovery's target root,
 * which can be a foreign block's post-state rather than any journal's expected
 * root (see `recomputeCommittedTip`).
 */
export const retrieveNewestFinalized = (): Effect.Effect<
  Option.Option<Record>,
  DatabaseError,
  Database
> =>
  retrieveNewestFinalizedWhere("retrieveNewestFinalized", (sql) => sql`TRUE`);

/** The finalized journal for `headerHash`; abandoned or active ones are not. */
export const retrieveFinalizedByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  retrieveNewestFinalizedWhere(
    "retrieveFinalizedByHeaderHash",
    (sql) => sql`${sql(Columns.HEADER_HASH)} = ${headerHash}`,
  );

/**
 * The newest finalized journal whose expected UTxO root is `expectedUtxosRoot`
 * (and whose block ended by `endedBy`, when given): the local ledger point a
 * block built on a foreign tail started from, when that tail left the ledger
 * at a root this node had itself reached.
 */
export const retrieveNewestFinalizedWithExpectedRoot = ({
  expectedUtxosRoot,
  endedBy,
}: {
  readonly expectedUtxosRoot: string;
  readonly endedBy?: Date;
}): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  retrieveNewestFinalizedWhere(
    "retrieveNewestFinalizedWithExpectedRoot",
    (sql) =>
      endedBy === undefined
        ? sql`${sql(Columns.EXPECTED_UTXOS_ROOT)} = ${expectedUtxosRoot}`
        : sql`${sql(Columns.EXPECTED_UTXOS_ROOT)} = ${expectedUtxosRoot}
            AND ${sql(Columns.BLOCK_END_TIME)} <= ${endedBy}`,
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
        decodePendingBlockFinalizationRow(row).pipe(
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
