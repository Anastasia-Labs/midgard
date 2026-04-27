import { Database } from "@/services/database.js";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";

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
    return BigInt(rows[0]?.count ?? "0");
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to count unfinished local mutation jobs",
    ),
  );
