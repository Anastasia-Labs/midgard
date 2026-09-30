import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import {
  ADMISSION_BYTE_QUOTA_LOCK_KEY,
  currentBacklogPayloadBytes,
  normalizedMaxBacklogBytes,
} from "./txAdmissions.admit-without-byte-quota.js";
import { persistReservedAdmissionBatch } from "./txAdmissions.persist-reserved-admission-batch.js";
import {
  type ClaimedEntry,
  Columns,
  type Entry,
  normalizeRow,
  payloadTableName,
  postgresByteaArray,
  type RawClaimedPayloadEntry,
  type RawEntry,
  type ReservedAdmissionOutcome,
  type ReservedAdmissionRequest,
  tableName,
  TxAdmissionBacklogFullError,
  verifyClaimedPayloadRows,
} from "./txAdmissions.verify-claimed-payload-rows.js";
import {
  DatabaseError,
  logDatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";

export const admitReservedBatch = (
  requests: readonly ReservedAdmissionRequest[],
): Effect.Effect<
  readonly ReservedAdmissionOutcome[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (requests.length === 0) return [];
    const maxBacklogBytes = requests
      .slice(1)
      .reduce(
        (minimum, request) =>
          minimum < normalizedMaxBacklogBytes(request.maxBacklogBytes)
            ? minimum
            : normalizedMaxBacklogBytes(request.maxBacklogBytes),
        normalizedMaxBacklogBytes(requests[0]?.maxBacklogBytes),
      );
    const outerSql = yield* SqlClient.SqlClient;
    return yield* outerSql.withTransaction(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`SELECT pg_advisory_xact_lock(${ADMISSION_BYTE_QUOTA_LOCK_KEY})`;
        const uniqueTxIds = [
          ...new Map(
            requests.map((request) => [
              request.txId.toString("hex"),
              request.txId,
            ]),
          ).values(),
        ];
        const existingRows = yield* sql<{
          readonly tx_id: Buffer;
        }>`SELECT ${sql(Columns.TX_ID)} AS tx_id
          FROM ${sql(tableName)}
          WHERE ${sql.in(Columns.TX_ID, uniqueTxIds)}`;
        const existingTxIds = new Set(
          existingRows.map((row) => row.tx_id.toString("hex")),
        );
        let projectedBacklogBytes = yield* currentBacklogPayloadBytes;
        const admittedTxIds = new Set<string>();
        const deniedTxIds = new Set<string>();
        for (const request of requests) {
          const txIdHex = request.txId.toString("hex");
          if (
            existingTxIds.has(txIdHex) ||
            admittedTxIds.has(txIdHex) ||
            deniedTxIds.has(txIdHex)
          ) {
            continue;
          }
          const requestedBytes = BigInt(
            request.programMaterialSidecarCbor.length,
          );
          if (projectedBacklogBytes + requestedBytes > maxBacklogBytes) {
            deniedTxIds.add(txIdHex);
          } else {
            admittedTxIds.add(txIdHex);
            projectedBacklogBytes += requestedBytes;
          }
        }
        const eligibleRequests = requests.filter(
          (request) => !deniedTxIds.has(request.txId.toString("hex")),
        );
        const eligibleOutcomes =
          yield* persistReservedAdmissionBatch(eligibleRequests);
        let eligibleIndex = 0;
        return requests.map((request): ReservedAdmissionOutcome => {
          if (!deniedTxIds.has(request.txId.toString("hex"))) {
            return eligibleOutcomes[eligibleIndex++]!;
          }
          return {
            _tag: "Conflict",
            error: new TxAdmissionBacklogFullError({
              backlog: projectedBacklogBytes,
              maxBacklog: maxBacklogBytes,
              unit: "bytes",
              message:
                "Durable submission admission byte backlog is full; retry later",
            }),
          };
        });
      }),
    );
  }).pipe(
    Effect.tapErrorTag("SqlError", (error) =>
      logDatabaseError(tableName, "admitReservedBatchByteQuota", error),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to enforce reserved admission byte backlog",
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
    const rows = yield* sql<RawEntry>`SELECT
        admission.*,
        payload.${sql(Columns.TX_CANONICAL_CBOR)},
        payload.${sql(Columns.TX_FULL_HASH_V1)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
      FROM ${sql(tableName)} AS admission
      INNER JOIN ${sql(payloadTableName)} AS payload
        ON payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
      WHERE admission.${sql(Columns.TX_ID)} = ${txId}
      LIMIT 1`;
    return rows.length === 0 ? null : normalizeRow(rows[0]!);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to retrieve tx admission"),
  );

export type ProgramMaterialSidecarRecord = {
  readonly txId: Buffer;
  readonly sidecarCbor: Buffer;
};

/**
 * Loads immutable V1 sidecars for an exact transaction set. Missing rows are
 * omitted so callers can compare cardinality and fail closed.
 */
export const retrieveProgramMaterialSidecars = (
  txIds: readonly Buffer[],
): Effect.Effect<
  readonly ProgramMaterialSidecarRecord[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (txIds.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const rows = yield* sql<{
      readonly tx_id: Buffer;
      readonly cek_program_material_sidecar_cbor: Buffer;
    }>`SELECT
        ${sql(Columns.TX_ID)},
        ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)}
      FROM ${sql(payloadTableName)}
      WHERE ${sql(Columns.TX_ID)} =
        ANY(${pg.array(postgresByteaArray(txIds))}::bytea[])
      ORDER BY ${sql(Columns.TX_ID)} ASC`;
    return rows.map((row) => ({
      txId: Buffer.from(row.tx_id),
      sidecarCbor: Buffer.from(row.cek_program_material_sidecar_cbor),
    }));
  }).pipe(
    sqlErrorToDatabaseError(
      payloadTableName,
      "Failed to retrieve V1 program-material sidecars",
    ),
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
            payload.${sql(Columns.TX_FULL_HASH_V1)},
            payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
            payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)},
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
