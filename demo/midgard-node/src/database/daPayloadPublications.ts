import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import type { DaProducerCommitteePeer } from "@/da/libp2p-producer.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

export const tableName = "da_payload_publications";

export type PublicationStatus =
  | "pending"
  | "accepted"
  | "duplicate"
  | "conflict"
  | "rejected"
  | "transport_error";

export type Row = {
  readonly header_hash: Buffer;
  readonly peer_id: string;
  readonly signer_index: number;
  readonly status: PublicationStatus;
  readonly attempts: number;
  readonly last_attempt_at: Date | null;
  readonly next_retry_at: Date | null;
  readonly accepted_at: Date | null;
  readonly last_error: string | null;
  readonly lease_owner: string | null;
  readonly lease_token: string | null;
  readonly lease_expires_at: Date | null;
  readonly created_at: Date;
  readonly updated_at: Date;
};

export const seedForPayload = (
  headerHash: Buffer,
  peers: readonly DaProducerCommitteePeer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (peers.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.forEach(
      peers,
      (peer) =>
        sql`
          INSERT INTO ${sql(tableName)} (
            header_hash,
            peer_id,
            signer_index,
            status,
            attempts,
            next_retry_at
          ) VALUES (
            ${headerHash},
            ${peer.peerId},
            ${peer.signerIndex},
            'pending',
            0,
            NOW()
          )
          ON CONFLICT (header_hash, peer_id) DO NOTHING
        `,
      { concurrency: 1, discard: true },
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to seed durable DA publication peer rows",
    ),
  );

export const seedRecentPayloads = ({
  peers,
  retentionDays,
}: {
  readonly peers: readonly DaProducerCommitteePeer[];
  readonly retentionDays: number;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.forEach(
      peers,
      (peer) => sql`
        INSERT INTO ${sql(tableName)} (
          header_hash,
          peer_id,
          signer_index,
          status,
          attempts,
          next_retry_at
        )
        SELECT
          payload.header_hash,
          ${peer.peerId},
          ${peer.signerIndex},
          'pending',
          0,
          NOW()
        FROM da_payloads payload
        WHERE payload.created_at >= NOW() - (${retentionDays} * INTERVAL '1 day')
        ON CONFLICT (header_hash, peer_id) DO NOTHING
      `,
      { concurrency: 1, discard: true },
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to seed recent durable DA payloads for reconciliation",
    ),
  );

export const recordAttempt = ({
  headerHash,
  peer,
  status,
  error,
  retryBackoffMs,
  retryBackoffMaxMs,
  lease,
}: {
  readonly headerHash: Buffer;
  readonly peer: DaProducerCommitteePeer;
  readonly status: Exclude<PublicationStatus, "pending">;
  readonly error?: string;
  readonly retryBackoffMs: number;
  readonly retryBackoffMaxMs: number;
  readonly lease?: {
    readonly owner: string;
    readonly token: string;
  };
}): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const terminal =
      status === "accepted" || status === "duplicate" || status === "conflict";
    const now = new Date();
    const leaseOwner = lease?.owner ?? null;
    const leaseToken = lease?.token ?? null;
    const affected = yield* sql<{ readonly header_hash: Buffer }>`
      INSERT INTO ${sql(tableName)} (
        header_hash,
        peer_id,
        signer_index,
        status,
        attempts,
        last_attempt_at,
        next_retry_at,
        accepted_at,
        last_error
      ) SELECT
        ${headerHash},
        ${peer.peerId},
        ${peer.signerIndex},
        ${status},
        1,
        ${now},
        ${terminal ? null : new Date(now.getTime() + retryBackoffMs)},
        ${status === "accepted" || status === "duplicate" ? now : null},
        ${error ?? null}
      WHERE ${leaseOwner}::text IS NULL
        OR EXISTS (
          SELECT 1
          FROM ${sql(tableName)} claimed
          WHERE claimed.header_hash = ${headerHash}
            AND claimed.peer_id = ${peer.peerId}
            AND claimed.lease_owner = ${leaseOwner}
            AND claimed.lease_token = ${leaseToken}
            AND claimed.lease_expires_at > NOW()
        )
      ON CONFLICT (header_hash, peer_id) DO UPDATE SET
        signer_index = EXCLUDED.signer_index,
        status = CASE
          WHEN EXCLUDED.status = 'conflict' THEN 'conflict'
          WHEN ${sql(tableName)}.status = 'conflict' THEN 'conflict'
          WHEN ${sql(tableName)}.status IN ('accepted', 'duplicate')
            THEN ${sql(tableName)}.status
          ELSE EXCLUDED.status
        END,
        attempts = ${sql(tableName)}.attempts + 1,
        last_attempt_at = NOW(),
        next_retry_at = CASE
          WHEN EXCLUDED.status = 'conflict'
            OR ${sql(tableName)}.status IN ('accepted', 'duplicate', 'conflict')
            THEN NULL
          ELSE NOW() + (
            LEAST(
              ${retryBackoffMaxMs},
              ${retryBackoffMs} * POWER(2, LEAST(${sql(tableName)}.attempts, 30))
            ) * INTERVAL '1 millisecond'
          )
        END,
        accepted_at = CASE
          WHEN EXCLUDED.status IN ('accepted', 'duplicate')
            THEN COALESCE(${sql(tableName)}.accepted_at, NOW())
          ELSE ${sql(tableName)}.accepted_at
        END,
        last_error = CASE
          WHEN EXCLUDED.status = 'conflict' THEN EXCLUDED.last_error
          WHEN ${sql(tableName)}.status IN ('accepted', 'duplicate', 'conflict')
            THEN ${sql(tableName)}.last_error
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
      "Failed to record DA publication attempt",
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
        SELECT publication.header_hash, publication.peer_id
        FROM ${sql(tableName)} publication
        INNER JOIN da_payloads payload
          ON payload.header_hash = publication.header_hash
        WHERE publication.status IN ('pending', 'rejected', 'transport_error')
          AND publication.next_retry_at <= NOW()
          AND (publication.lease_expires_at IS NULL OR publication.lease_expires_at <= NOW())
          AND payload.created_at >= NOW() - (${retentionDays} * INTERVAL '1 day')
        ORDER BY publication.next_retry_at ASC,
          publication.header_hash ASC,
          publication.peer_id ASC
        FOR UPDATE OF publication SKIP LOCKED
        LIMIT ${limit}
      )
      UPDATE ${sql(tableName)} publication
      SET lease_owner = ${leaseOwner},
        lease_token = ${leaseToken},
        lease_expires_at = NOW() + (${leaseMs} * INTERVAL '1 millisecond'),
        updated_at = NOW()
      FROM due
      WHERE publication.header_hash = due.header_hash
        AND publication.peer_id = due.peer_id
      RETURNING publication.*
    `;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to claim due DA publication attempts",
    ),
  );

export const backlogCount = (
  retentionDays: number,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly count: string | number | bigint }>`
      SELECT COUNT(*) AS count
      FROM ${sql(tableName)} publication
      INNER JOIN da_payloads payload
        ON payload.header_hash = publication.header_hash
      WHERE publication.status IN ('pending', 'rejected', 'transport_error')
        AND payload.created_at >= NOW() - (${retentionDays} * INTERVAL '1 day')
    `;
    return Number(rows[0]?.count ?? 0);
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to count DA publication reconciliation backlog",
    ),
  );

export const releaseClaim = ({
  headerHash,
  peerId,
  leaseOwner,
  leaseToken,
}: {
  readonly headerHash: Buffer;
  readonly peerId: string;
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
        AND peer_id = ${peerId}
        AND lease_owner = ${leaseOwner}
        AND lease_token = ${leaseToken}
    `;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to release DA publication claim",
    ),
  );

export const conflictCount = (
  retentionDays: number,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly count: string | number | bigint }>`
      SELECT COUNT(*) AS count
      FROM ${sql(tableName)} publication
      INNER JOIN da_payloads payload
        ON payload.header_hash = publication.header_hash
      WHERE publication.status = 'conflict'
        AND payload.created_at >= NOW() - (${retentionDays} * INTERVAL '1 day')
    `;
    return Number(rows[0]?.count ?? 0);
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to count DA publication conflicts",
    ),
  );

export const acceptedCount = (
  headerHash: Buffer,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly count: string | number | bigint }>`
      SELECT COUNT(DISTINCT signer_index) AS count
      FROM ${sql(tableName)}
      WHERE header_hash = ${headerHash}
        AND status IN ('accepted', 'duplicate')
    `;
    return Number(rows[0]?.count ?? 0);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to count accepted DA peers"),
  );
