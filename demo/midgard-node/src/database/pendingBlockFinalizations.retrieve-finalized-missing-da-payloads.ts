import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { withHistoryWrite } from "../services/event-history-producer.js";
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

/** A signed commit intent no acknowledgement, landing or replacement has
 * resolved: only the history owner's signed-intent reconciliation clears it. */
const unreconciledSignedSubmission = (sql: SqlClient.SqlClient) =>
  sql`status = ${Status.PendingSubmission} AND intended_tx_hash IS NOT NULL`;

/** How long an active journal may stay unresolved before the node reports it:
 * well past the signed-intent replacement window, so an honest replacement
 * never trips it. */
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
        message:
          "Refusing to prepare a new pending block while another active pending-finalization record exists",
        cause: `signed_header=${rows[0]!.header_hash.toString("hex")}; canonical reconciliation required`,
      }),
    );
}).pipe(
  withHistoryWrite,
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
 * `batchLimit` rows per statement until a batch comes up short or
 * `maxBatches` ran; each journal's member rows go with it by cascade. Never
 * removed: any journal not finalized (an abandoned one may still be revived),
 * the confirmed head and every header live in the L1 state queue, every
 * header DA retention still holds for finality (`finalityHeldPayload`, the
 * exemption set the DA payload prune applies), the newest
 * finalized journal (the local block boundary), and any journal whose
 * confirmed-merge finalization job has not completed: the landed-merge walk
 * stops at a header with no journal, so pruning one before its merge is
 * folded locally would skip that merge silently. Runs as a history write,
 * so it needs the history producer permit. Returns the number removed.
 */
export const pruneFinalizedBeyondChallengeability = ({
  challengeableCutoff,
  view,
  deploymentIdentityDigest,
  batchLimit = 500,
  maxBatches = 100,
}: {
  readonly challengeableCutoff: Date;
  readonly view: DaPayloadsDB.RetentionL1View;
  readonly deploymentIdentityDigest: Buffer | undefined;
  readonly batchLimit?: number;
  readonly maxBatches?: number;
}): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const exempt = [view.confirmedHeadHash, ...view.liveQueueHeaderHashes];
    const limit = Math.max(1, Math.floor(batchLimit));
    let removed = 0;
    for (let batch = 0; batch < Math.max(1, maxBatches); batch++) {
      const rows = yield* sql<{ header_hash: Buffer }>`DELETE FROM ${sql(
        tableName,
      )}
        WHERE ${sql(Columns.HEADER_HASH)} IN (
          SELECT ${sql(Columns.HEADER_HASH)} FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} = ${Status.Finalized}
            AND ${sql(Columns.BLOCK_END_TIME)} < ${challengeableCutoff}
            AND NOT ${sql.in(Columns.HEADER_HASH, exempt)}
            AND NOT ${DaPayloadsDB.finalityHeldPayload(sql, deploymentIdentityDigest)}
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
              WHERE newest.${sql(Columns.STATUS)} = ${Status.Finalized}
              ORDER BY newest.${sql(Columns.BLOCK_END_TIME)} DESC,
                newest.${sql(Columns.CREATED_AT)} DESC
              LIMIT 1)
          ORDER BY ${sql(Columns.BLOCK_END_TIME)} ASC
          LIMIT ${limit})
        RETURNING ${sql(Columns.HEADER_HASH)}`;
      removed += rows.length;
      if (rows.length < limit) break;
    }
    return removed;
  }).pipe(
    withHistoryWrite,
    Effect.withLogSpan(`pruneFinalizedBeyondChallengeability ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to prune finalized journals beyond challengeability",
    ),
  );
