import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "local_mutation_jobs";

export const Kind = {
  LocalBlockFinalization: "local_block_finalization",
  ConfirmedMergeFinalization: "confirmed_merge_finalization",
} as const;

export type Kind = (typeof Kind)[keyof typeof Kind];

export const Status = {
  Running: "running",
  Completed: "completed",
  Failed: "failed",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export enum Columns {
  JOB_ID = "job_id",
  KIND = "kind",
  STATUS = "status",
  PLAN_HASH = "plan_hash",
  PAYLOAD = "payload",
  ATTEMPTS = "attempts",
  LAST_ERROR = "last_error",
  CREATED_AT = "created_at",
  UPDATED_AT = "updated_at",
  COMPLETED_AT = "completed_at",
}

export type Entry = {
  [Columns.JOB_ID]: string;
  [Columns.KIND]: Kind;
  [Columns.STATUS]: Status;
  [Columns.PLAN_HASH]: Buffer | null;
  [Columns.PAYLOAD]: unknown;
  [Columns.ATTEMPTS]: number;
  [Columns.LAST_ERROR]: string | null;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
  [Columns.COMPLETED_AT]: Date | null;
};

export const start = ({
  jobId,
  kind,
  payload,
}: {
  readonly jobId: string;
  readonly kind: Kind;
  readonly payload?: unknown;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} (
      ${sql(Columns.JOB_ID)},
      ${sql(Columns.KIND)},
      ${sql(Columns.STATUS)},
      ${sql(Columns.PAYLOAD)},
      ${sql(Columns.ATTEMPTS)}
    ) VALUES (
      ${jobId},
      ${kind},
      ${Status.Running},
      CAST(${JSON.stringify(payload ?? {})} AS JSONB),
      1
    )
    ON CONFLICT (${sql(Columns.JOB_ID)}) DO UPDATE SET
      ${sql(Columns.STATUS)} = ${Status.Running},
      ${sql(Columns.PAYLOAD)} = EXCLUDED.${sql(Columns.PAYLOAD)},
      ${sql(Columns.ATTEMPTS)} = ${sql(tableName)}.${sql(Columns.ATTEMPTS)} + 1,
      ${sql(Columns.LAST_ERROR)} = NULL,
      ${sql(Columns.UPDATED_AT)} = NOW()
    WHERE ${sql(tableName)}.${sql(Columns.KIND)} = EXCLUDED.${sql(Columns.KIND)}
      AND ${sql(tableName)}.${sql(Columns.STATUS)} <> ${Status.Completed}`;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to start local mutation job"),
  );

export const markCompleted = (
  jobId: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Completed},
          ${sql(Columns.LAST_ERROR)} = NULL,
          ${sql(Columns.COMPLETED_AT)} = NOW(),
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.JOB_ID)} = ${jobId}
        AND ${sql(Columns.STATUS)} <> ${Status.Completed}`;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to complete local mutation job"),
  );

export const markFailed = (
  jobId: string,
  error: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Failed},
          ${sql(Columns.LAST_ERROR)} = ${error.slice(0, 4000)},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.JOB_ID)} = ${jobId}
        AND ${sql(Columns.STATUS)} <> ${Status.Completed}`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark local mutation job failed",
    ),
  );

/** The job that finalizes one committed block into the local database. */
export const localBlockFinalizationJobId = (headerHashHex: string): string =>
  `${Kind.LocalBlockFinalization}:${headerHashHex}`;

/**
 * Removes the unfinished local-finalization job of a block whose
 * pending-finalization journal is being abandoned. Callers run it in the same
 * SQL transaction as that abandonment, so a refused abandonment keeps the job.
 *
 * This is the bounded-retention rule applied, not a workaround: rows are
 * prunable once past their horizon or removed, never retained without bound.
 * The block no longer exists on L1 (or provably never reached it), so its
 * finalization is moot, and the abandoned journal row, with its correction
 * digest where a correction removed it, is the durable record. The job's
 * attempts and last error (already cause-chained) are logged with the reason
 * so the diagnosis survives the row.
 *
 * Only this header's job of this kind is removed, and only while running or
 * failed; a completed job keeps its record. A late markFailed/markCompleted
 * from an attempt still in flight is an UPDATE and cannot bring the row back;
 * `start` inserts a fresh row only when a revived journal is replayed.
 */
export const abandonLocalBlockFinalization = (
  headerHash: Buffer,
  reason: string,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const headerHashHex = headerHash.toString("hex");
    const removed = yield* sql<Entry>`DELETE FROM ${sql(tableName)}
      WHERE ${sql(Columns.JOB_ID)} = ${localBlockFinalizationJobId(headerHashHex)}
        AND ${sql(Columns.KIND)} = ${Kind.LocalBlockFinalization}
        AND ${sql(Columns.STATUS)} IN (${Status.Running}, ${Status.Failed})
      RETURNING *`;
    for (const job of removed)
      yield* Effect.logWarning(
        `Removing moot local block finalization job with its journal abandonment (rolled back with it if the abandonment fails): header=${headerHashHex},status=${job[Columns.STATUS]},attempts=${job[Columns.ATTEMPTS].toString()},reason=${reason},last_error=${job[Columns.LAST_ERROR] ?? "none"}`,
      );
    return removed;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to remove abandoned block's local finalization job",
    ),
  );

/** The job that finalizes one L1-confirmed merge into the local database. */
export const confirmedMergeFinalizationJobId = (
  headerHashHex: string,
): string => `${Kind.ConfirmedMergeFinalization}:${headerHashHex}`;

export const retrieveByJobId = (
  jobId: string,
): Effect.Effect<Entry | undefined, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.JOB_ID)} = ${jobId}`;
    return rows[0];
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to retrieve local mutation job"),
  );

export const retrieveUnfinished: Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
    WHERE ${sql(Columns.STATUS)} <> ${Status.Completed}
    ORDER BY ${sql(Columns.UPDATED_AT)} ASC, ${sql(Columns.JOB_ID)} ASC`;
}).pipe(
  sqlErrorToDatabaseError(
    tableName,
    "Failed to retrieve unfinished local mutation jobs",
  ),
);

export const countUnfinished: Effect.Effect<bigint, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ count: string }>`SELECT COUNT(*)::bigint AS count
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} <> ${Status.Completed}`;
    return BigInt(rows[0].count);
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to count unfinished local mutation jobs",
    ),
  );
