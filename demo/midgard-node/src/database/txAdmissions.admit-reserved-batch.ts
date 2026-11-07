import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { persistReservedAdmissionBatch } from "./txAdmissions.persist-reserved-admission-batch.js";
import {
  type ClaimedEntry,
  Columns,
  type Entry,
  normalizeRow,
  payloadTableName,
  type RawClaimedPayloadEntry,
  type RawEntry,
  type ReservedAdmissionOutcome,
  type ReservedAdmissionRequest,
  tableName,
  verifyClaimedPayloadRows,
} from "./txAdmissions.verify-claimed-payload-rows.js";
import {
  DatabaseError,
  logDatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";

/**
 * Durably admits one batch of reserved requests in one transaction. The caller
 * reserved each request's backlog row and bytes in memory beforehand, so the
 * batch takes no lock and reads no backlog total: shards write in parallel.
 */
export const admitReservedBatch = (
  requests: readonly ReservedAdmissionRequest[],
): Effect.Effect<
  readonly ReservedAdmissionOutcome[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (requests.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(persistReservedAdmissionBatch(requests));
  }).pipe(
    Effect.tapErrorTag("SqlError", (error) =>
      logDatabaseError(tableName, "admitReservedBatch", error),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to durably admit reserved transaction batch",
    ),
  );

export const requeueExpiredLeases: Effect.Effect<
  number,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<
    Pick<RawEntry, Columns.TX_ID>
  >`UPDATE ${sql(tableName)}
    SET
      ${sql(Columns.STATUS)} = 'queued',
      ${sql(Columns.LEASE_OWNER)} = NULL,
      ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
      ${sql(Columns.NEXT_ATTEMPT_AT)} = NOW(),
      ${sql(Columns.UPDATED_AT)} = GREATEST(
        NOW(),
        ${sql(Columns.FIRST_SEEN_AT)},
        ${sql(Columns.LAST_SEEN_AT)},
        ${sql(Columns.UPDATED_AT)}
      )
    WHERE ${sql(Columns.STATUS)} = 'validating'
      AND ${sql(Columns.LEASE_EXPIRES_AT)} < NOW()
    RETURNING ${sql(Columns.TX_ID)}`;
  return rows.length;
}).pipe(
  sqlErrorToDatabaseError(tableName, "Failed to requeue expired admissions"),
);

export const getByTxId = (
  txId: Buffer,
): Effect.Effect<Entry | null, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawEntry>`SELECT *
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.TX_ID)} = ${txId}`;
    return rows.length === 0 ? null : normalizeRow(rows[0]!);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to retrieve tx admission"),
  );

export const claimBatch = ({
  limit,
  leaseOwner,
  leaseDurationMs,
}: {
  readonly limit: number;
  readonly leaseOwner: string;
  readonly leaseDurationMs: number;
}): Effect.Effect<readonly ClaimedEntry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql.withTransaction(
      Effect.gen(function* () {
        // A lost claim commit after a database crash only leaves the row
        // queued for safe revalidation. A later synchronous terminal commit
        // WAL-orders and flushes this lease transition first.
        yield* sql`SET LOCAL synchronous_commit = off`;
        return yield* sql<RawClaimedPayloadEntry>`WITH candidates AS (
          SELECT ${sql(Columns.TX_ID)}
          FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} = 'queued'
            AND ${sql(Columns.NEXT_ATTEMPT_AT)} <= NOW()
          ORDER BY
            ${sql(Columns.ARRIVAL_SEQ)} ASC,
            ${sql(Columns.TX_ID)} ASC
          FOR UPDATE SKIP LOCKED
          LIMIT ${Math.max(1, limit)}
        ), claimed AS (
          UPDATE ${sql(tableName)} admissions
          SET
            ${sql(Columns.STATUS)} = 'validating',
            ${sql(Columns.LEASE_OWNER)} = ${leaseOwner},
            ${sql(Columns.LEASE_EXPIRES_AT)} = GREATEST(
              NOW(),
              admissions.${sql(Columns.FIRST_SEEN_AT)},
              admissions.${sql(Columns.LAST_SEEN_AT)},
              admissions.${sql(Columns.UPDATED_AT)}
            ) + (${Math.max(1, leaseDurationMs)} * INTERVAL '1 millisecond'),
            ${sql(Columns.VALIDATION_STARTED_AT)} =
              COALESCE(
                ${sql(Columns.VALIDATION_STARTED_AT)},
                GREATEST(
                  NOW(),
                  admissions.${sql(Columns.FIRST_SEEN_AT)},
                  admissions.${sql(Columns.LAST_SEEN_AT)},
                  admissions.${sql(Columns.UPDATED_AT)}
                )
              ),
            ${sql(Columns.ATTEMPT_COUNT)} = ${sql(Columns.ATTEMPT_COUNT)} + 1,
            ${sql(Columns.UPDATED_AT)} = GREATEST(
              NOW(),
              admissions.${sql(Columns.FIRST_SEEN_AT)},
              admissions.${sql(Columns.LAST_SEEN_AT)},
              admissions.${sql(Columns.UPDATED_AT)}
            )
          FROM candidates, ${sql(payloadTableName)} AS payload
          WHERE admissions.${sql(Columns.TX_ID)} = candidates.${sql(Columns.TX_ID)}
            AND payload.${sql(Columns.TX_ID)} = admissions.${sql(Columns.TX_ID)}
          RETURNING
            admissions.${sql(Columns.TX_ID)},
            payload.${sql(Columns.TX_CANONICAL_CBOR)},
            admissions.${sql(Columns.TX_FULL_HASH_V1)},
            admissions.${sql(Columns.ARRIVAL_SEQ)},
            admissions.${sql(Columns.FIRST_SEEN_AT)},
            admissions.${sql(Columns.VALIDATION_STARTED_AT)}
        )
        SELECT *
        FROM claimed
        ORDER BY
          ${sql(Columns.ARRIVAL_SEQ)} ASC,
          ${sql(Columns.TX_ID)} ASC`;
      }),
    );
    return yield* verifyClaimedPayloadRows(rows);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to claim admitted transactions"),
  );
