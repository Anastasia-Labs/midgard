import { computeMidgardNativeTxFullHashFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";

import { Database } from "../services/database.js";
import { sha256 } from "../sha256.js";
import {
  admissionBacklogRejectCounter,
  admissionDuplicatePathCounter,
  type AdmitResult,
  Columns,
  type Entry,
  normalizeRow,
  payloadTableName,
  type RawEntry,
  type SubmitSource,
  tableName,
  toBigInt,
  TxAdmissionBacklogFullError,
  TxAdmissionConflictError,
} from "./txAdmissions.verify-claimed-payload-rows.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const tryInsert = ({
  txId,
  txCanonicalCbor,
  programMaterialSidecarCbor,
  submitSource,
}: {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly submitSource: SubmitSource;
}): Effect.Effect<Entry | null, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const txFullHash =
      computeMidgardNativeTxFullHashFromCanonicalCbor(txCanonicalCbor);
    const materialSidecarSha256 = sha256(programMaterialSidecarCbor);
    const inserted = yield* sql<RawEntry>`WITH inserted_admission AS (
        INSERT INTO ${sql(tableName)} (
          ${sql(Columns.TX_ID)},
          ${sql(Columns.STATUS)},
          ${sql(Columns.SUBMIT_SOURCE)}
        ) VALUES (
          ${txId},
          'queued',
          ${submitSource}
        )
        ON CONFLICT (${sql(Columns.TX_ID)}) DO NOTHING
        RETURNING *
      ), inserted_payload AS (
        INSERT INTO ${sql(payloadTableName)} (
          ${sql(Columns.TX_ID)},
          ${sql(Columns.TX_CANONICAL_CBOR)},
          ${sql(Columns.TX_FULL_HASH_V1)},
          ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
          ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
        )
        SELECT
          ${sql(Columns.TX_ID)},
          ${txCanonicalCbor},
          ${txFullHash},
          ${programMaterialSidecarCbor},
          ${materialSidecarSha256}
        FROM inserted_admission
        RETURNING *
      )
      SELECT
        admission.*,
        payload.${sql(Columns.TX_CANONICAL_CBOR)},
        payload.${sql(Columns.TX_FULL_HASH_V1)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
      FROM inserted_admission admission
      INNER JOIN inserted_payload payload
        ON payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}`;
    return inserted.length === 0 ? null : normalizeRow(inserted[0]!);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to durably admit transaction"),
  );

export const touchDuplicateCount = ({
  txId,
  txFullHashV1: txFullHash,
  txCanonicalCbor,
  programMaterialSidecarCbor,
  programMaterialSidecarSha256,
  requestCount,
}: {
  readonly txId: Buffer;
  readonly txFullHashV1: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly programMaterialSidecarSha256: Buffer;
  readonly requestCount: number;
}): Effect.Effect<Entry | null, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const updated = yield* sql<RawEntry>`UPDATE ${sql(tableName)} AS admission
      SET
        ${sql(Columns.LAST_SEEN_AT)} = GREATEST(
          NOW(),
          admission.${sql(Columns.FIRST_SEEN_AT)},
          admission.${sql(Columns.LAST_SEEN_AT)}
        ),
        ${sql(Columns.UPDATED_AT)} = GREATEST(
          NOW(),
          admission.${sql(Columns.FIRST_SEEN_AT)},
          admission.${sql(Columns.LAST_SEEN_AT)},
          admission.${sql(Columns.UPDATED_AT)}
        ),
        ${sql(Columns.REQUEST_COUNT)} =
          admission.${sql(Columns.REQUEST_COUNT)} + ${requestCount}
      FROM ${sql(payloadTableName)} AS payload
      WHERE admission.${sql(Columns.TX_ID)} = ${txId}
        AND payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
        AND payload.${sql(Columns.TX_FULL_HASH_V1)} = ${txFullHash}
        AND payload.${sql(Columns.TX_CANONICAL_CBOR)} = ${txCanonicalCbor}
        AND payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)} =
          ${programMaterialSidecarSha256}
        AND (
          payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)} =
            ${programMaterialSidecarCbor}
          OR admission.${sql(Columns.STATUS)} IN ('accepted', 'rejected')
        )
      RETURNING
        admission.*,
        payload.${sql(Columns.TX_CANONICAL_CBOR)},
        payload.${sql(Columns.TX_FULL_HASH_V1)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}`;
    return updated.length === 0 ? null : normalizeRow(updated[0]!);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to touch duplicate admission"),
  );

export const touchDuplicate = ({
  txId,
  txFullHashV1: txFullHash,
  txCanonicalCbor,
  programMaterialSidecarCbor,
}: {
  readonly txId: Buffer;
  readonly txFullHashV1: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
}): Effect.Effect<Entry | null, DatabaseError, Database> =>
  touchDuplicateCount({
    txId,
    txFullHashV1: txFullHash,
    txCanonicalCbor,
    programMaterialSidecarCbor,
    programMaterialSidecarSha256: sha256(programMaterialSidecarCbor),
    requestCount: 1,
  });

const admitWithoutByteQuota = ({
  txId,
  txCanonicalCbor,
  programMaterialSidecarCbor,
  submitSource,
  currentBacklog,
  maxBacklog,
}: {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly submitSource: Exclude<SubmitSource, "backfill">;
  readonly currentBacklog: bigint;
  readonly maxBacklog: number;
}): Effect.Effect<
  AdmitResult,
  DatabaseError | TxAdmissionConflictError | TxAdmissionBacklogFullError,
  Database
> =>
  Effect.gen(function* () {
    const txFullHash =
      computeMidgardNativeTxFullHashFromCanonicalCbor(txCanonicalCbor);
    const max = BigInt(Math.max(0, maxBacklog));
    if (currentBacklog >= max) {
      const duplicate = yield* touchDuplicate({
        txId,
        txFullHashV1: txFullHash,
        txCanonicalCbor,
        programMaterialSidecarCbor,
      });
      if (duplicate !== null) {
        yield* Metric.increment(admissionDuplicatePathCounter);
        return { entry: duplicate, kind: "duplicate" as const };
      }
      yield* Metric.increment(admissionBacklogRejectCounter);
      return yield* Effect.fail(
        new TxAdmissionBacklogFullError({
          backlog: currentBacklog,
          maxBacklog: max,
          message: "Durable submission admission backlog is full; retry later",
        }),
      );
    }

    const inserted = yield* tryInsert({
      txId,
      txCanonicalCbor,
      programMaterialSidecarCbor,
      submitSource,
    });
    if (inserted !== null) {
      return { entry: inserted, kind: "new" as const };
    }

    const duplicate = yield* touchDuplicate({
      txId,
      txFullHashV1: txFullHash,
      txCanonicalCbor,
      programMaterialSidecarCbor,
    });
    if (duplicate !== null) {
      yield* Metric.increment(admissionDuplicatePathCounter);
      return { entry: duplicate, kind: "duplicate" as const };
    }

    return yield* Effect.fail(
      new TxAdmissionConflictError({
        txIdHex: txId.toString("hex"),
        message: `Refusing to admit transaction ${txId.toString("hex")}: tx_id already exists with different normalized bytes`,
      }),
    );
  });

export const ADMISSION_BYTE_QUOTA_LOCK_KEY = 0x4d_49_44_47_41_52_44n;

const DEFAULT_MAX_DURABLE_ADMISSION_BACKLOG_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes;

export const normalizedMaxBacklogBytes = (
  value: number | undefined,
): bigint => {
  const normalized = value ?? DEFAULT_MAX_DURABLE_ADMISSION_BACKLOG_BYTES;
  if (!Number.isSafeInteger(normalized) || normalized <= 0) {
    throw new Error("maxBacklogBytes must be a positive safe integer");
  }
  return BigInt(normalized);
};

export const currentBacklogPayloadBytes = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    readonly bytes: bigint | number | string;
  }>`SELECT COALESCE(
      SUM(octet_length(payload.${sql(
        Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR,
      )})),
      0
    )::bigint AS bytes
    FROM ${sql(tableName)} admission
    INNER JOIN ${sql(payloadTableName)} payload
      ON payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
    WHERE admission.${sql(Columns.STATUS)} IN ('queued', 'validating')`;
  return toBigInt(rows[0]?.bytes ?? 0);
});

export const admit = ({
  maxBacklogBytes,
  ...request
}: {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly submitSource: Exclude<SubmitSource, "backfill">;
  readonly currentBacklog: bigint;
  readonly maxBacklog: number;
  readonly maxBacklogBytes?: number;
}): Effect.Effect<
  AdmitResult,
  DatabaseError | TxAdmissionConflictError | TxAdmissionBacklogFullError,
  Database
> =>
  Effect.gen(function* () {
    const outerSql = yield* SqlClient.SqlClient;
    return yield* outerSql.withTransaction(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`SELECT pg_advisory_xact_lock(${ADMISSION_BYTE_QUOTA_LOCK_KEY})`;
        const existing = yield* sql<{ readonly exists: boolean }>`SELECT EXISTS(
          SELECT 1
          FROM ${sql(tableName)}
          WHERE ${sql(Columns.TX_ID)} = ${request.txId}
        ) AS exists`;
        if (existing[0]?.exists !== true) {
          const backlog = yield* currentBacklogPayloadBytes;
          const maxBacklog = normalizedMaxBacklogBytes(maxBacklogBytes);
          const requestedBytes = BigInt(
            request.programMaterialSidecarCbor.length,
          );
          if (backlog + requestedBytes > maxBacklog) {
            return yield* Effect.fail(
              new TxAdmissionBacklogFullError({
                backlog,
                maxBacklog,
                unit: "bytes",
                message:
                  "Durable submission admission byte backlog is full; retry later",
              }),
            );
          }
        }
        return yield* admitWithoutByteQuota(request);
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to enforce durable admission byte backlog",
    ),
  );
