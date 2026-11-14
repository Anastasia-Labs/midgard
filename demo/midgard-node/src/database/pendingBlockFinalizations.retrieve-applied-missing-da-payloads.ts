import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import * as DaPayloadsDB from "./daPayloads.js";
import {
  Columns,
  MemberColumns,
  type RawRow,
  type Row,
  tableName,
  WithdrawalMemberColumns,
  type WithdrawalMemberRecord,
} from "./pendingBlockFinalizations.columns.js";
import { decodePendingBlockFinalizationRow } from "./pendingBlockFinalizations.decode-pending-block-finalization-row.js";
import {
  deadHeadersOf,
  deadJournals,
  LIVE_JOURNAL_REFUSAL,
  liveJournalSql,
} from "./pendingBlockFinalizations.derived-status.js";
import { type Record } from "./pendingBlockFinalizations.parse-ledger-delta.js";
import { retrieveRecord } from "./pendingBlockFinalizations.retrieve-record.js";
import {
  appliedOrFolded,
  deadEffectPending,
  deadHeader,
  type DeadHeaders,
  type OwnBlockStatus,
  ownBlockStatusSql,
} from "./pendingBlockFinalizations.status-sql.js";
import {
  challengeRelevantHeader,
  orphanMemberJournal,
  pruneInBatches,
  recoveryRelevantJournal,
} from "./retention-holds.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as WithdrawalsDB from "./withdrawals.js";

/** A journal of a state-queue mutation lease, with its derived status. */
export type LeaseJournal = Readonly<{
  row: Row;
  status: OwnBlockStatus;
  /** Whether its block can no longer land (`deadJournals`). */
  dead: boolean;
}>;

export const retrieveByStateQueueLeaseToken = (
  stateQueueLeaseToken: string,
): Effect.Effect<readonly LeaseJournal[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const dead = deadHeadersOf(yield* deadJournals);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<
      RawRow & { own_block_status: OwnBlockStatus; dead: boolean }
    >`SELECT p.*, ${ownBlockStatusSql(sql, "p")} AS own_block_status,
        ${deadHeader(sql, "p.header_hash", dead)} AS dead
      FROM ${sql(tableName)} p
      WHERE p.${sql(Columns.STATE_QUEUE_LEASE_TOKEN)} = ${stateQueueLeaseToken}
      ORDER BY p.${sql(Columns.CREATED_AT)} ASC`;
    return yield* Effect.forEach(
      rows,
      ({ own_block_status, dead: isDead, ...row }) =>
        decodePendingBlockFinalizationRow(row).pipe(
          Effect.map(({ normalizedRow }) => ({
            row: normalizedRow,
            status: own_block_status,
            dead: isDead,
          })),
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

export const retrieveAppliedMissingDaPayloads = ({
  headerHash,
  limit = 100,
}: {
  readonly headerHash?: Buffer;
  readonly limit?: number;
} = {}): Effect.Effect<readonly Record[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const safeLimit = Math.max(1, Math.floor(limit));
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT p.* FROM ${sql(tableName)} p
      WHERE NOT EXISTS (SELECT 1 FROM ${sql(DaPayloadsDB.tableName)} payload
          WHERE payload.${sql(DaPayloadsDB.Columns.HEADER_HASH)} = p.${sql(
            Columns.HEADER_HASH,
          )})
        ${headerHash === undefined ? sql`` : sql`AND p.${sql(Columns.HEADER_HASH)} = ${headerHash}`}
        AND ${appliedOrFolded(sql, "p")}
      ORDER BY p.${sql(Columns.CREATED_AT)} ASC
      LIMIT ${safeLimit}`;
    return yield* Effect.forEach(rows, (row) => retrieveRecord(sql, row), {
      concurrency: 1,
    });
  }).pipe(
    Effect.withLogSpan(`retrieveAppliedMissingDaPayloads ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve applied journals missing DA payloads",
    ),
  );

/** The live journal over `dead` if it is signed (alias `live`): a signed
 * commit intent no landing or death has resolved. The landed-block rebase
 * takes its block in when it lands, and disposes of its effects once it is
 * dead (whichever lands wins). */
const liveSignedJournalSql = (sql: SqlClient.SqlClient, dead: DeadHeaders) =>
  sql`SELECT live_journal.* FROM (${liveJournalSql(sql, dead)}) live_journal
    WHERE live_journal.intended_tx_hash IS NOT NULL`;

/** How long a live journal may stay unresolved before the node reports it:
 * well past the time S6 takes to derive a missed commit dead, so an honest
 * replacement never trips it. */
export const PENDING_FINALIZATION_AGE_BOUND_MS = 15 * 60_000;

/** The age of a journal row aliased `alias`, from its preparation to the
 * database clock now. `clock_timestamp()`, not the transaction's start
 * time: a row written after the reading transaction began never reads
 * younger than it is. */
const ageMs = (sql: SqlClient.SqlClient, alias: string) =>
  sql`(EXTRACT(EPOCH FROM (clock_timestamp() - ${sql(alias)}.${sql(
    Columns.CREATED_AT,
  )})) * 1000)::float8`;

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
  const dead = deadHeadersOf(yield* deadJournals);
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
    age_ms: number;
  }>`SELECT signed.header_hash, GREATEST(0, ${ageMs(sql, "signed")}) AS age_ms
    FROM (${liveSignedJournalSql(sql, dead)}) signed`;
  const row = rows[0];
  return row === undefined
    ? undefined
    : { headerHash: row.header_hash, ageMs: Math.floor(Number(row.age_ms)) };
}).pipe(sqlErrorToDatabaseError(tableName, "Failed signed submission lookup"));

export type LiveJournalAges = {
  /** The live journal's age; null when there is none. */
  readonly pendingFinalizationAgeMs: number | null;
  /** The live journal's age when it is signed; null otherwise. */
  readonly signedIntentUnresolvedAgeMs: number | null;
};

/** Ages of the live journal, which holds back the next commit, on the
 * database clock. */
export const retrieveLiveJournalAges: Effect.Effect<
  LiveJournalAges,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const dead = deadHeadersOf(yield* deadJournals);
  const sql = yield* SqlClient.SqlClient;
  const [row] = yield* sql<{
    age_ms: number;
    signed: boolean;
  }>`SELECT ${ageMs(sql, "live_journal")} AS age_ms,
      live_journal.intended_tx_hash IS NOT NULL AS signed
    FROM (${liveJournalSql(sql, dead)}) live_journal`;
  const age =
    row === undefined ? null : Math.max(0, Math.floor(Number(row.age_ms)));
  return {
    pendingFinalizationAgeMs: age,
    signedIntentUnresolvedAgeMs: row?.signed === true ? age : null,
  };
}).pipe(sqlErrorToDatabaseError(tableName, "Failed to read live journal ages"));

/** A lost submit response requires reconciliation before another candidate
 * build. The transactional prepare guard remains authoritative against later
 * races. */
export const assertNoUnreconciledSignedSubmission = Effect.gen(function* () {
  const dead = deadHeadersOf(yield* deadJournals);
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
  }>`SELECT signed.header_hash FROM (${liveSignedJournalSql(sql, dead)}) signed`;
  if (rows.length !== 0)
    return yield* Effect.fail(
      new DatabaseError({
        table: tableName,
        message: LIVE_JOURNAL_REFUSAL,
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
 * Deletes folded journals whose block ended before `challengeableCutoff`,
 * `batchLimit` rows per statement until a batch comes up short, `maxBatches`
 * ran, or the clock passes `deadlineMs`; each journal's member rows go with it
 * by cascade. A dead journal (`deadJournals`, read in the batch's gated
 * transaction) goes too, whatever its age, once its signed commit's intent
 * row is gone, no disposal effect is pending
 * (`deadEffectPending`) and its header is not challenge-relevant: its block
 * can no longer land and nothing is left to undo. Never removed: any other
 * journal not folded (an unlanded one that is not dead may still land, a
 * dead one whose intent row or an effect is left is kept until neither is,
 * and an unsigned one is the commit tick's to discard),
 * the newest applied or folded journal (the local block boundary), any journal
 * whose header is still challenge-relevant (`challengeRelevantHeader`: the
 * confirmed head, a live queue header, or a header DA retention holds for
 * finality: live in the follower's facts, or merged or removed by a landed
 * tx that is not final yet, so a landed merge keeps its journal until it is
 * final at k), and any folded journal with an orphaned event member
 * (`orphanMemberJournal`). Folded recovery dependencies are also kept: the bases,
 * same-base siblings and descendants of journals not applied or folded, and
 * every retained native recovery plan's primary/member headers. Each batch
 * is its own gated write, so it needs the follower write gate and holds it
 * for one statement at a time. Returns the number removed.
 */
export const pruneFoldedBeyondChallengeability = ({
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
        const dead = deadHeadersOf(yield* deadJournals);
        const sql = yield* SqlClient.SqlClient;
        // Without the intent journal's table no intent can be read gone.
        const [intents] = yield* sql<{ present: boolean }>`SELECT
          to_regclass('l1_intents') IS NOT NULL AS present`;
        const spentDead =
          dead.length === 0 || intents?.present !== true
            ? sql`FALSE`
            : sql`(${deadHeader(sql, `p.${Columns.HEADER_HASH}`, dead)}
              AND p.${sql(Columns.INTENDED_TX_HASH)} IS NOT NULL
              AND NOT EXISTS (SELECT 1 FROM l1_intents intent
                WHERE intent.tx_hash = p.${sql(Columns.INTENDED_TX_HASH)})
              AND NOT ${deadEffectPending(sql, dead)})`;
        const rows = yield* sql<{ header_hash: Buffer }>`DELETE FROM ${sql(
          tableName,
        )}
        WHERE ${sql(Columns.HEADER_HASH)} IN (
          SELECT p.${sql(Columns.HEADER_HASH)} FROM ${sql(tableName)} p
          WHERE NOT ${challengeRelevantHeader(sql, `p.${Columns.HEADER_HASH}`, { view, deploymentIdentityDigest })}
            AND (${spentDead} OR (p.${sql(Columns.BLOCK_TIME)} < ${challengeableCutoff}
            AND ${ownBlockStatusSql(sql, "p")} = 'folded'
            AND NOT ${recoveryRelevantJournal(sql, `p.${Columns.HEADER_HASH}`)}
            AND NOT ${orphanMemberJournal(sql, `p.${Columns.HEADER_HASH}`)}
            AND p.${sql(Columns.HEADER_HASH)} <> (
              SELECT newest.${sql(Columns.HEADER_HASH)} FROM ${sql(
                tableName,
              )} AS newest
              WHERE ${appliedOrFolded(sql, "newest")}
              ORDER BY newest.${sql(Columns.BLOCK_END_TIME)} DESC,
                newest.${sql(Columns.CREATED_AT)} DESC
              LIMIT 1)))
          ORDER BY p.${sql(Columns.BLOCK_END_TIME)} ASC
          LIMIT ${limit})
        RETURNING ${sql(Columns.HEADER_HASH)}`;
        return rows.length;
      }).pipe(
        withFollowerWrite,
        Effect.withLogSpan(`pruneFoldedBeyondChallengeability ${tableName}`),
        sqlErrorToDatabaseError(
          tableName,
          "Failed to prune folded journals beyond challengeability",
        ),
      ),
  });
