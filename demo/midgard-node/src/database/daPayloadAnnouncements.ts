import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

export const tableName = "da_payload_announcements";

export type Status = "pending" | "failed" | "published";

export type Row = {
  readonly header_hash: Buffer;
  readonly status: Status;
  readonly attempts: number;
  readonly last_attempt_at: Date | null;
  readonly next_retry_at: Date | null;
  readonly published_at: Date | null;
  readonly last_error: string | null;
  readonly lease_owner: string | null;
  readonly lease_token: string | null;
  readonly lease_expires_at: Date | null;
  readonly created_at: Date;
  readonly updated_at: Date;
};

export const seedForPayload = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`
      INSERT INTO ${sql(tableName)} (
        header_hash, status, attempts, next_retry_at
      ) VALUES (${headerHash}, 'pending', 0, NOW())
      ON CONFLICT (header_hash) DO NOTHING
    `;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to seed DA announcement outbox"),
  );

export const seedRecentPayloads = (
  retentionDays: number,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`
      INSERT INTO ${sql(tableName)} (
        header_hash, status, attempts, next_retry_at
      )
      SELECT payload.header_hash, 'pending', 0, NOW()
      FROM da_payloads payload
      WHERE payload.created_at >= NOW() - (${retentionDays} * INTERVAL '1 day')
      ON CONFLICT (header_hash) DO NOTHING
    `;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to seed recent DA announcement outbox rows",
    ),
  );

export const claimDue = ({
  retentionDays,
  limit,
  leaseOwner,
  leaseToken,
  leaseMs,
}: {
  readonly retentionDays: number;
  readonly limit: number;
  readonly leaseOwner: string;
  readonly leaseToken: string;
  readonly leaseMs: number;
}): Effect.Effect<readonly Row[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Row>`
      WITH due AS (
        SELECT announcement.header_hash
        FROM ${sql(tableName)} announcement
        INNER JOIN da_payloads payload
          ON payload.header_hash = announcement.header_hash
        WHERE announcement.status IN ('pending', 'failed')
          AND announcement.next_retry_at <= NOW()
          AND (
            announcement.lease_expires_at IS NULL
            OR announcement.lease_expires_at <= NOW()
          )
          AND payload.created_at >= NOW() - (${retentionDays} * INTERVAL '1 day')
        ORDER BY announcement.next_retry_at ASC, announcement.header_hash ASC
        FOR UPDATE OF announcement SKIP LOCKED
        LIMIT ${limit}
      )
      UPDATE ${sql(tableName)} announcement
      SET lease_owner = ${leaseOwner},
        lease_token = ${leaseToken},
        lease_expires_at = NOW() + (${leaseMs} * INTERVAL '1 millisecond'),
        updated_at = NOW()
      FROM due
      WHERE announcement.header_hash = due.header_hash
      RETURNING announcement.*
    `;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to claim due DA announcements"),
  );

export const recordAttempt = ({
  headerHash,
  published,
  error,
  retryBackoffMs,
  retryBackoffMaxMs,
  lease,
}: {
  readonly headerHash: Buffer;
  readonly published: boolean;
  readonly error?: string;
  readonly retryBackoffMs: number;
  readonly retryBackoffMaxMs: number;
  readonly lease?: { readonly owner: string; readonly token: string };
}): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const now = new Date();
    const leaseOwner = lease?.owner ?? null;
    const leaseToken = lease?.token ?? null;
    const affected = yield* sql<{ readonly header_hash: Buffer }>`
      INSERT INTO ${sql(tableName)} (
        header_hash, status, attempts, last_attempt_at, next_retry_at,
        published_at, last_error
      ) SELECT
        ${headerHash},
        ${published ? "published" : "failed"},
        1,
        ${now},
        ${published ? null : new Date(now.getTime() + retryBackoffMs)},
        ${published ? now : null},
        ${error ?? null}
      WHERE ${leaseOwner}::text IS NULL
        OR EXISTS (
          SELECT 1
          FROM ${sql(tableName)} claimed
          WHERE claimed.header_hash = ${headerHash}
            AND claimed.lease_owner = ${leaseOwner}
            AND claimed.lease_token = ${leaseToken}
            AND claimed.lease_expires_at > NOW()
        )
      ON CONFLICT (header_hash) DO UPDATE SET
        status = CASE
          WHEN ${sql(tableName)}.status = 'published' THEN 'published'
          ELSE EXCLUDED.status
        END,
        attempts = ${sql(tableName)}.attempts + 1,
        last_attempt_at = NOW(),
        next_retry_at = CASE
          WHEN ${sql(tableName)}.status = 'published' OR EXCLUDED.status = 'published'
            THEN NULL
          ELSE NOW() + (
            LEAST(
              ${retryBackoffMaxMs},
              ${retryBackoffMs} * POWER(2, LEAST(${sql(tableName)}.attempts, 30))
            ) * INTERVAL '1 millisecond'
          )
        END,
        published_at = CASE
          WHEN EXCLUDED.status = 'published'
            THEN COALESCE(${sql(tableName)}.published_at, NOW())
          ELSE ${sql(tableName)}.published_at
        END,
        last_error = CASE
          WHEN ${sql(tableName)}.status = 'published' THEN ${sql(tableName)}.last_error
          ELSE EXCLUDED.last_error
        END,
        lease_owner = NULL,
        lease_token = NULL,
        lease_expires_at = NULL,
        updated_at = NOW()
      WHERE (
          ${leaseOwner}::text IS NULL
          AND (
            ${sql(tableName)}.lease_owner IS NULL
            OR ${sql(tableName)}.lease_expires_at <= NOW()
          )
        )
        OR (
          ${sql(tableName)}.lease_owner = ${leaseOwner}
          AND ${sql(tableName)}.lease_token = ${leaseToken}
          AND ${sql(tableName)}.lease_expires_at > NOW()
        )
      RETURNING ${sql(tableName)}.header_hash
    `;
    return affected.length === 1;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to record DA announcement attempt",
    ),
  );

export const releaseClaim = ({
  headerHash,
  leaseOwner,
  leaseToken,
}: {
  readonly headerHash: Buffer;
  readonly leaseOwner: string;
  readonly leaseToken: string;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`
      UPDATE ${sql(tableName)}
      SET lease_owner = NULL,
        lease_token = NULL,
        lease_expires_at = NULL,
        updated_at = NOW()
      WHERE header_hash = ${headerHash}
        AND lease_owner = ${leaseOwner}
        AND lease_token = ${leaseToken}
    `;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to release DA announcement claim",
    ),
  );

export const backlogCount = (
  retentionDays: number,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly count: string | number | bigint }>`
      SELECT COUNT(*) AS count
      FROM ${sql(tableName)} announcement
      INNER JOIN da_payloads payload ON payload.header_hash = announcement.header_hash
      WHERE announcement.status IN ('pending', 'failed')
        AND payload.created_at >= NOW() - (${retentionDays} * INTERVAL '1 day')
    `;
    return Number(rows[0]?.count ?? 0);
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to count DA announcement backlog",
    ),
  );
