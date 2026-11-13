import { SqlClient, type Statement } from "@effect/sql";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import { sha256 } from "../sha256.js";
import {
  Columns,
  depositsTableName,
  eventToStepTableName,
  forcedTransactionsTableName,
  MemberColumns,
  type RawRow,
  tableName,
  transitionTraceTableName,
  txsTableName,
  validationTracesTableName,
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
import {
  deadHeadersOf,
  deadJournals,
  liveJournalSql,
} from "./pendingBlockFinalizations.derived-status.js";
import { type Record } from "./pendingBlockFinalizations.parse-ledger-delta.js";
import {
  appliedOrFolded,
  type DeadHeaders,
} from "./pendingBlockFinalizations.status-sql.js";
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

/** The live journal over `dead` (`liveJournalSql`), if any. */
export const retrieveLiveOver = (
  dead: DeadHeaders,
): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`${liveJournalSql(sql, dead)}`;
    return rows.length === 0
      ? Option.none()
      : Option.some(yield* retrieveRecord(sql, rows[0]!));
  }).pipe(
    Effect.withLogSpan(`retrieveLive ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve the live journal"),
  );

/** The node's live journal, if any: its newest unlanded journal not dead. */
export const retrieveLive = (): Effect.Effect<
  Option.Option<Record>,
  DatabaseError,
  Database
> =>
  deadJournals.pipe(
    Effect.flatMap((dead) => retrieveLiveOver(deadHeadersOf(dead))),
  );

export const hasLive: Effect.Effect<boolean, DatabaseError, Database> =
  Effect.gen(function* () {
    const dead = deadHeadersOf(yield* deadJournals);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
      FROM (${liveJournalSql(sql, dead)}) live_journal`;
    return rows.length > 0;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to check for a live journal"),
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
 * The newest applied (or folded) journal matching `filter`, newest by block
 * window (the state queue orders blocks by time, so this is chain order), or
 * none.
 */
const retrieveNewestAppliedWhere = (
  label: string,
  filter: (sql: SqlClient.SqlClient) => Statement.Fragment,
): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT p.* FROM ${sql(tableName)} p
      WHERE ${filter(sql)} AND ${appliedOrFolded(sql, "p")}
      ORDER BY p.${sql(Columns.BLOCK_END_TIME)} DESC, p.${sql(Columns.CREATED_AT)} DESC
      LIMIT 1`;
    return rows.length === 0
      ? Option.none()
      : Option.some(yield* retrieveRecord(sql, rows[0]!));
  }).pipe(
    Effect.withLogSpan(`${label} ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      `Failed to retrieve an applied journal (${label})`,
    ),
  );

/**
 * This node's newest applied (or folded) block, by block window. It is not
 * necessarily the native committed point: a landed-block rebase applied
 * after it resets the native root to that move's target root, which can be a
 * foreign block's post-state rather than any journal's expected root (see
 * `recomputeCommittedTip`).
 */
export const retrieveNewestApplied = (): Effect.Effect<
  Option.Option<Record>,
  DatabaseError,
  Database
> => retrieveNewestAppliedWhere("retrieveNewestApplied", (sql) => sql`TRUE`);

/** The journal of `headerHash` if its block is applied or folded. */
export const retrieveAppliedByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  retrieveNewestAppliedWhere(
    "retrieveAppliedByHeaderHash",
    (sql) => sql`p.${sql(Columns.HEADER_HASH)} = ${headerHash}`,
  );

/**
 * The newest applied (or folded) journal whose expected UTxO root is
 * `expectedUtxosRoot` (and whose block ended by `endedBy`, when given): the
 * local ledger point a block built on a foreign tail started from, when that
 * tail left the ledger at a root this node had itself reached.
 */
export const retrieveNewestAppliedWithExpectedRoot = ({
  expectedUtxosRoot,
  endedBy,
}: {
  readonly expectedUtxosRoot: string;
  readonly endedBy?: Date;
}): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  retrieveNewestAppliedWhere("retrieveNewestAppliedWithExpectedRoot", (sql) =>
    endedBy === undefined
      ? sql`p.${sql(Columns.EXPECTED_UTXOS_ROOT)} = ${expectedUtxosRoot}`
      : sql`p.${sql(Columns.EXPECTED_UTXOS_ROOT)} = ${expectedUtxosRoot}
            AND p.${sql(Columns.BLOCK_END_TIME)} <= ${endedBy}`,
  );
