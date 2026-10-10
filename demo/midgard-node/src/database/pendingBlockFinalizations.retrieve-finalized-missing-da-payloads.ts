import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import * as DaPayloadsDB from "./daPayloads.js";
import * as MutationJobsDB from "./mutationJobs.js";
import {
  ACTIVE_STATUSES,
  Columns,
  MemberColumns,
  type RawRow,
  type Row,
  Status,
  tableName,
  WithdrawalMemberColumns,
  type WithdrawalMemberRecord,
} from "./pendingBlockFinalizations.columns.js";
import { decodePendingBlockFinalizationRow } from "./pendingBlockFinalizations.decode-pending-block-finalization-row.js";
import { type Record } from "./pendingBlockFinalizations.parse-ledger-delta.js";
import { retrieveRecord } from "./pendingBlockFinalizations.retrieve-record.js";
import { ACTIVE_PENDING_JOURNAL_REFUSAL } from "./pendingBlockFinalizations.single-active-refusal.js";
import {
  challengeRelevantHeader,
  orphanMemberJournal,
  pruneInBatches,
  recoveryRelevantJournal,
} from "./retention-holds.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as WithdrawalsDB from "./withdrawals.js";

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
        decodePendingBlockFinalizationRow(row).pipe(
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
            WHERE ${sql(tableName)}.${sql(Columns.STATUS)} = ${Status.LocallyApplied}
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
            WHERE ${sql(tableName)}.${sql(Columns.STATUS)} = ${Status.LocallyApplied}
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

/** A signed commit intent no acknowledgement, landing or disposal has
 * resolved: S6 derives its status, and the landed-block rebase disposes of
 * its journal once it is dead or its base left (whichever lands wins). */
const unreconciledSignedSubmission = (sql: SqlClient.SqlClient) =>
  sql`status = ${Status.PendingSubmission} AND intended_tx_hash IS NOT NULL`;

/** How long an active journal may stay unresolved before the node reports it:
 * well past the time S6 takes to derive a missed commit dead, so an honest
 * replacement never trips it. */
export const PENDING_FINALIZATION_AGE_BOUND_MS = 15 * 60_000;

export type UnreconciledSignedSubmission = {
  readonly headerHash: Buffer;
  /** Since the journal was prepared, on the database clock. */
  readonly ageMs: number;
};

/** The journal `assertNoUnreconciledSignedSubmission` refuses on, if any. A
 * read for deciding whether a commit attempt can do anything; it never stands
 * in for that assertion or for the prepare guard. */
export const retrieveUnreconciledSignedSubmission: Effect.Effect<
  UnreconciledSignedSubmission | undefined,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
    age_ms: number;
  }>`SELECT header_hash, GREATEST(0, EXTRACT(EPOCH FROM (NOW() - ${sql(
    Columns.CREATED_AT,
  )})) * 1000)::float8 AS age_ms
    FROM ${sql(tableName)} WHERE ${unreconciledSignedSubmission(sql)}
    ORDER BY ${sql(Columns.CREATED_AT)} ASC LIMIT 1`;
  const row = rows[0];
  return row === undefined
    ? undefined
    : { headerHash: row.header_hash, ageMs: Math.floor(Number(row.age_ms)) };
}).pipe(sqlErrorToDatabaseError(tableName, "Failed signed submission lookup"));

export type ActiveJournalAges = {
  /** The oldest journal not yet finalized or abandoned; null when none. */
  readonly pendingFinalizationAgeMs: number | null;
  /** The oldest unreconciled signed commit intent; null when none. */
  readonly signedIntentUnresolvedAgeMs: number | null;
};

/** Ages of the journals that hold back the next commit, on the database
 * clock. */
export const retrieveActiveJournalAges: Effect.Effect<
  ActiveJournalAges,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  // NULL over no rows; GREATEST would turn that into 0, so clamp below.
  const age = sql`(EXTRACT(EPOCH FROM (NOW() - MIN(${sql(
    Columns.CREATED_AT,
  )}))) * 1000)::float8`;
  const [row] = yield* sql<{
    active_age_ms: number | null;
    signed_age_ms: number | null;
  }>`SELECT
      (SELECT ${age} FROM ${sql(tableName)}
        WHERE ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}) AS active_age_ms,
      (SELECT ${age} FROM ${sql(tableName)}
        WHERE ${unreconciledSignedSubmission(sql)}) AS signed_age_ms`;
  const toMs = (value: number | null | undefined) =>
    value === null || value === undefined
      ? null
      : Math.max(0, Math.floor(Number(value)));
  return {
    pendingFinalizationAgeMs: toMs(row?.active_age_ms),
    signedIntentUnresolvedAgeMs: toMs(row?.signed_age_ms),
  };
}).pipe(
  sqlErrorToDatabaseError(tableName, "Failed to read active journal ages"),
);

/** A lost submit response requires reconciliation before another candidate build.
 * The transactional prepare guard remains authoritative against later races. */
export const assertNoUnreconciledSignedSubmission = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
  }>`SELECT header_hash FROM pending_block_finalizations
    WHERE ${unreconciledSignedSubmission(sql)} LIMIT 1`;
  if (rows.length !== 0)
    return yield* Effect.fail(
      new DatabaseError({
        table: tableName,
        message: ACTIVE_PENDING_JOURNAL_REFUSAL,
        cause: `signed_header=${rows[0]!.header_hash.toString("hex")}; canonical reconciliation required`,
      }),
    );
}).pipe(
  withFollowerWrite,
  sqlErrorToDatabaseError(tableName, "Failed signed submission preflight"),
);

export const withdrawalMemberToAssignment = (
  member: WithdrawalMemberRecord,
): WithdrawalsDB.SettlementInfoAssignment => ({
  eventId: member[MemberColumns.MEMBER_ID],
  expectedClassificationRevision:
    member[WithdrawalMemberColumns.CLASSIFICATION_REVISION],
  settlementEventInfo: member[MemberColumns.PAYLOAD_CBOR],
  validity: member[WithdrawalMemberColumns.VALIDITY],
  validityDetail: member[WithdrawalMemberColumns.VALIDITY_DETAIL],
});

/**
 * Deletes finalized journals whose block ended before `challengeableCutoff`,
 * `batchLimit` rows per statement until a batch comes up short, `maxBatches`
 * ran, or the clock passes `deadlineMs`; each journal's member rows go with it
 * by cascade. Never removed: any journal not finalized (an abandoned one may
 * still be revived), the newest finalized journal (the local block boundary),
 * any journal whose confirmed-merge finalization job has not completed (the
 * landed-merge walk stops at a header with no journal, so pruning one before
 * its merge is folded locally would skip that merge silently), and any
 * journal whose header is still challenge-relevant (`challengeRelevantHeader`:
 * the confirmed head, a live queue header, or a header DA retention holds for
 * finality: live in the follower's facts, or merged or removed by a landed tx
 * that is not final yet, so a landed merge keeps its journal until it is
 * final at k), and any journal with an orphaned event member
 * (`orphanMemberJournal`). Recovery dependencies are also kept:
 * unfinished/abandoned journals' bases, same-base siblings and descendants,
 * and every retained native recovery plan's primary/member headers. Each batch
 * is its own gated write, so it needs the follower write gate and holds it
 * for one statement at a time. Returns the number removed.
 */
export const pruneFinalizedBeyondChallengeability = ({
  challengeableCutoff,
  view,
  deploymentIdentityDigest,
  // Each batch cascades into the journal's tx, trace and witness rows while
  // holding the history write lock, so batches stay small.
  batchLimit = 50,
  maxBatches = 100,
  deadlineMs,
}: {
  readonly challengeableCutoff: Date;
  readonly view: DaPayloadsDB.RetentionL1View;
  readonly deploymentIdentityDigest: Buffer;
  readonly batchLimit?: number;
  readonly maxBatches?: number;
  readonly deadlineMs?: number;
}): Effect.Effect<number, DatabaseError, Database> =>
  pruneInBatches({
    batchLimit,
    maxBatches,
    deadlineMs,
    batch: (limit) =>
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const rows = yield* sql<{ header_hash: Buffer }>`DELETE FROM ${sql(
          tableName,
        )}
        WHERE ${sql(Columns.HEADER_HASH)} IN (
          SELECT ${sql(Columns.HEADER_HASH)} FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} = ${Status.LocallyApplied}
            AND ${sql(Columns.BLOCK_END_TIME)} < ${challengeableCutoff}
            AND NOT ${challengeRelevantHeader(sql, `${tableName}.${Columns.HEADER_HASH}`, { view, deploymentIdentityDigest })}
            AND NOT ${recoveryRelevantJournal(sql, `${tableName}.${Columns.HEADER_HASH}`)}
            AND NOT ${orphanMemberJournal(sql, `${tableName}.${Columns.HEADER_HASH}`)}
            AND EXISTS (
              SELECT 1 FROM ${sql(MutationJobsDB.tableName)} AS job
              WHERE job.${sql(MutationJobsDB.Columns.JOB_ID)} =
                  ${MutationJobsDB.confirmedMergeFinalizationJobId("")}::text ||
                  encode(${sql(tableName)}.${sql(Columns.HEADER_HASH)}, 'hex')
                AND job.${sql(MutationJobsDB.Columns.STATUS)} = ${MutationJobsDB.Status.Completed})
            AND ${sql(Columns.HEADER_HASH)} <> (
              SELECT newest.${sql(Columns.HEADER_HASH)} FROM ${sql(
                tableName,
              )} AS newest
              WHERE newest.${sql(Columns.STATUS)} = ${Status.LocallyApplied}
              ORDER BY newest.${sql(Columns.BLOCK_END_TIME)} DESC,
                newest.${sql(Columns.CREATED_AT)} DESC
              LIMIT 1)
          ORDER BY ${sql(Columns.BLOCK_END_TIME)} ASC
          LIMIT ${limit})
        RETURNING ${sql(Columns.HEADER_HASH)}`;
        return rows.length;
      }).pipe(
        withFollowerWrite,
        Effect.withLogSpan(`pruneFinalizedBeyondChallengeability ${tableName}`),
        sqlErrorToDatabaseError(
          tableName,
          "Failed to prune finalized journals beyond challengeability",
        ),
      ),
  });
