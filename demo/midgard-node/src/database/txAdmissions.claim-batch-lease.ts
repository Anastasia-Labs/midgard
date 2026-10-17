import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import {
  type ClaimedEntry,
  type ClaimedLeaseEntry,
  Columns,
  normalizeClaimedLeaseEntry,
  payloadTableName,
  type RawClaimedLeaseEntry,
  type RawClaimedPayloadEntry,
  tableName,
  verifyClaimedPayloadRows,
} from "./txAdmissions.verify-claimed-payload-rows.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/**
 * Claims one oldest-first batch without transferring its payload blobs. This
 * is deliberately separate from {@link claimBatch}: callers that need an
 * atomic claim-plus-payload result use that operation, while the ordered
 * validation pipeline can keep its sequencing lock around only the small
 * lease update.
 */
export const claimBatchLease = ({
  limit,
  leaseOwner,
  leaseDurationMs,
}: {
  readonly limit: number;
  readonly leaseOwner: string;
  readonly leaseDurationMs: number;
}): Effect.Effect<readonly ClaimedLeaseEntry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql.withTransaction(
      Effect.gen(function* () {
        // A lost claim commit after a database crash only leaves the row
        // queued for safe revalidation. A later synchronous terminal commit
        // WAL-orders and flushes this lease transition first.
        yield* sql`SET LOCAL synchronous_commit = off`;
        // Candidates are identified by tx_id, never by ctid: a duplicate
        // submission of a still-queued transaction rewrites the row (and so
        // moves it physically) while leaving it queued and claimable. A ctid
        // join would then target a tuple version this statement's snapshot
        // cannot see and silently drop the locked row from the batch.
        return yield* sql<RawClaimedLeaseEntry>`WITH candidates AS (
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
          FROM candidates
          WHERE admissions.${sql(Columns.TX_ID)} = candidates.${sql(Columns.TX_ID)}
          RETURNING
            admissions.${sql(Columns.TX_ID)},
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
    return rows.map(normalizeClaimedLeaseEntry);
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to claim admitted transaction leases",
    ),
  );

/**
 * Loads payload bytes only for the exact rows that this owner still leases.
 * A missing payload or lost lease is an infrastructure failure, not a
 * rejection: the queue processor releases the whole batch for retry and the
 * terminal acceptance path independently rechecks payload durability.
 */
export const loadClaimedPayloads = ({
  claimed,
  leaseOwner,
}: {
  readonly claimed: readonly ClaimedLeaseEntry[];
  readonly leaseOwner: string;
}): Effect.Effect<readonly ClaimedEntry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (claimed.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const claimedIds = claimed.map((entry) => entry[Columns.TX_ID]);
    const rows = yield* sql<RawClaimedPayloadEntry>`SELECT
        admission.${sql(Columns.TX_ID)},
        payload.${sql(Columns.TX_CANONICAL_CBOR)},
        payload.${sql(Columns.TX_FULL_HASH_V1)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)},
        admission.${sql(Columns.ARRIVAL_SEQ)},
        admission.${sql(Columns.FIRST_SEEN_AT)},
        admission.${sql(Columns.VALIDATION_STARTED_AT)}
      FROM ${sql(tableName)} AS admission
      INNER JOIN ${sql(payloadTableName)} AS payload
        ON payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
      WHERE admission.${sql(Columns.STATUS)} = 'validating'
        AND admission.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
      ORDER BY
        admission.${sql(Columns.ARRIVAL_SEQ)} ASC,
        admission.${sql(Columns.TX_ID)} ASC`;
    const entries = yield* verifyClaimedPayloadRows(rows);
    const expectedIds = new Set(claimedIds.map((txId) => txId.toString("hex")));
    const actualIds = new Set(
      entries.map((entry) => entry[Columns.TX_ID].toString("hex")),
    );
    if (
      entries.length !== claimed.length ||
      actualIds.size !== expectedIds.size ||
      [...expectedIds].some((txId) => !actualIds.has(txId))
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: payloadTableName,
          message:
            "Failed to load every claimed admission payload under the active validation lease",
          cause: `expected=${claimed.length},loaded=${entries.length}`,
        }),
      );
    }
    return entries;
  }).pipe(
    sqlErrorToDatabaseError(
      payloadTableName,
      "Failed to load claimed admission payloads",
    ),
  );

/**
 * Requeues admissions after an infrastructure failure without converting the
 * failure into a ledger verdict. The persisted claim count drives capped
 * exponential backoff across worker and process restarts; retry exhaustion is
 * intentionally non-terminal and prolonged stalls are surfaced by readiness.
 */
export const releaseForRetry = ({
  txIds,
  leaseOwner,
  baseDelayMs,
  maxDelayMs,
}: {
  readonly txIds: readonly Buffer[];
  readonly leaseOwner: string;
  readonly baseDelayMs: number;
  readonly maxDelayMs: number;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) {
      return;
    }
    const normalizedBaseDelayMs =
      Number.isSafeInteger(baseDelayMs) && baseDelayMs > 0 ? baseDelayMs : 0;
    const normalizedMaxDelayMs =
      Number.isSafeInteger(maxDelayMs) && maxDelayMs > 0 ? maxDelayMs : 0;
    const retryBackoffExponentCap =
      normalizedBaseDelayMs === 0 ||
      normalizedMaxDelayMs <= normalizedBaseDelayMs
        ? 0
        : Math.ceil(Math.log2(normalizedMaxDelayMs / normalizedBaseDelayMs));
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET
        ${sql(Columns.STATUS)} = 'queued',
        ${sql(Columns.LEASE_OWNER)} = NULL,
        ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
        ${sql(Columns.NEXT_ATTEMPT_AT)} = GREATEST(
          NOW(),
          ${sql(Columns.FIRST_SEEN_AT)},
          ${sql(Columns.LAST_SEEN_AT)},
          ${sql(Columns.UPDATED_AT)}
        ) + (
          LEAST(
            ${normalizedMaxDelayMs}::double precision,
            ${normalizedBaseDelayMs}::double precision * POWER(
              2::double precision,
              LEAST(
                GREATEST(${sql(Columns.ATTEMPT_COUNT)} - 1, 0),
                ${retryBackoffExponentCap}
              )
            )
          ) * INTERVAL '1 millisecond'
        ),
        ${sql(Columns.UPDATED_AT)} = GREATEST(
          NOW(),
          ${sql(Columns.FIRST_SEEN_AT)},
          ${sql(Columns.LAST_SEEN_AT)},
          ${sql(Columns.UPDATED_AT)}
        )
      WHERE ${sql.in(Columns.TX_ID, txIds)}
        AND ${sql(Columns.STATUS)} = 'validating'
        AND ${sql(Columns.LEASE_OWNER)} = ${leaseOwner}`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to release admissions for retry",
    ),
  );
