import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect, Metric } from "effect";

import { Database } from "../services/database.js";
import { touchDuplicateCount } from "./txAdmissions.admit-without-byte-quota.js";
import {
  type BatchResolvedRawEntry,
  groupReservedAdmissionVariants,
} from "./txAdmissions.group-reserved-admission-variants.js";
import {
  admissionDuplicatePathCounter,
  type AdmitResult,
  Columns,
  normalizeRow,
  payloadTableName,
  postgresByteaArray,
  type ReservedAdmissionOutcome,
  type ReservedAdmissionRequest,
  tableName,
  TxAdmissionConflictError,
} from "./txAdmissions.verify-claimed-payload-rows.js";
import {
  DatabaseError,
  logDatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";

/**
 * Resolves reserved admission requests in one atomic PostgreSQL statement on
 * the uncontended path. A concurrent ON CONFLICT loser may need one bounded
 * follow-up touch because PostgreSQL's statement snapshot cannot see the row
 * whose conflicting insert it just waited for.
 * The first byte variant for an absent tx id wins; existing rows instead match
 * their persisted bytes. Deterministic tx-id ordering prevents opposite-order
 * microbatches from acquiring conflicting unique-index locks in opposite order.
 */
export const persistReservedAdmissionBatch = (
  requests: readonly ReservedAdmissionRequest[],
): Effect.Effect<
  readonly ReservedAdmissionOutcome[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    if (requests.length === 0) return [];
    const variants = groupReservedAdmissionVariants(requests);
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const resolved = yield* sql<BatchResolvedRawEntry>`WITH input AS (
        SELECT *
        FROM unnest(
          ${pg.array(postgresByteaArray(variants.map((value) => value.txId)))}::bytea[],
          ${pg.array(postgresByteaArray(variants.map((value) => value.txCanonicalCbor)))}::bytea[],
          ${pg.array(postgresByteaArray(variants.map((value) => value.txFullHashV1)))}::bytea[],
          ${pg.array(
            postgresByteaArray(
              variants.map((value) => value.programMaterialSidecarCbor),
            ),
          )}::bytea[],
          ${pg.array(
            postgresByteaArray(
              variants.map((value) => value.programMaterialSidecarSha256),
            ),
          )}::bytea[],
          ${pg.array(variants.map((value) => value.submitSource))}::text[],
          ${pg.array(variants.map((value) => value.requestIndices.length))}::integer[],
          ${pg.array(variants.map((value) => value.variantOrdinal))}::integer[],
          ${pg.array(variants.map((value) => value.firstVariantForTxId))}::boolean[]
        ) AS batch_input(
          tx_id,
          tx_canonical_cbor,
          tx_full_hash_v1,
          cek_program_material_sidecar_cbor,
          cek_program_material_sidecar_sha256,
          submit_source,
          request_count,
          variant_ordinal,
          first_variant_for_tx_id
        )
      ), inserted_admission AS (
        INSERT INTO ${sql(tableName)} (
          ${sql(Columns.TX_ID)},
          ${sql(Columns.STATUS)},
          ${sql(Columns.SUBMIT_SOURCE)},
          ${sql(Columns.REQUEST_COUNT)}
        )
        SELECT
          input.tx_id,
          'queued',
          input.submit_source,
          input.request_count
        FROM input
        WHERE input.first_variant_for_tx_id
        ORDER BY input.variant_ordinal
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
          inserted.${sql(Columns.TX_ID)},
          input.tx_canonical_cbor,
          input.tx_full_hash_v1,
          input.cek_program_material_sidecar_cbor,
          input.cek_program_material_sidecar_sha256
        FROM inserted_admission inserted
        INNER JOIN input
          ON input.tx_id = inserted.${sql(Columns.TX_ID)}
          AND input.first_variant_for_tx_id
        RETURNING *
      ), updated_existing AS (
        UPDATE ${sql(tableName)} admissions
        SET
          ${sql(Columns.LAST_SEEN_AT)} = GREATEST(
            NOW(),
            admissions.${sql(Columns.FIRST_SEEN_AT)},
            admissions.${sql(Columns.LAST_SEEN_AT)}
          ),
          ${sql(Columns.UPDATED_AT)} = GREATEST(
            NOW(),
            admissions.${sql(Columns.FIRST_SEEN_AT)},
            admissions.${sql(Columns.LAST_SEEN_AT)},
            admissions.${sql(Columns.UPDATED_AT)}
          ),
          ${sql(Columns.REQUEST_COUNT)} =
            admissions.${sql(Columns.REQUEST_COUNT)} + input.request_count
        FROM input
        INNER JOIN ${sql(payloadTableName)} payload
          ON payload.${sql(Columns.TX_ID)} = input.tx_id
          AND payload.${sql(Columns.TX_FULL_HASH_V1)} =
            input.tx_full_hash_v1
          AND payload.${sql(Columns.TX_CANONICAL_CBOR)} =
            input.tx_canonical_cbor
          AND payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
            = input.cek_program_material_sidecar_sha256
        WHERE admissions.${sql(Columns.TX_ID)} = input.tx_id
          AND (
            payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)}
              = input.cek_program_material_sidecar_cbor
            OR admissions.${sql(Columns.STATUS)} IN ('accepted', 'rejected')
          )
          AND NOT EXISTS (
            SELECT 1
            FROM inserted_admission inserted
            WHERE inserted.${sql(Columns.TX_ID)} = input.tx_id
          )
        RETURNING input.variant_ordinal, admissions.*
      )
      SELECT
        input.variant_ordinal,
        'new'::text AS result_kind,
        inserted.*,
        payload.${sql(Columns.TX_CANONICAL_CBOR)},
        payload.${sql(Columns.TX_FULL_HASH_V1)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
      FROM input
      INNER JOIN inserted_admission inserted
        ON inserted.${sql(Columns.TX_ID)} = input.tx_id
        AND input.first_variant_for_tx_id
      INNER JOIN inserted_payload payload
        ON payload.${sql(Columns.TX_ID)} = inserted.${sql(Columns.TX_ID)}
      UNION ALL
      SELECT
        updated.variant_ordinal,
        'duplicate'::text AS result_kind,
        updated.${sql(Columns.TX_ID)},
        updated.${sql(Columns.ARRIVAL_SEQ)},
        updated.${sql(Columns.STATUS)},
        updated.${sql(Columns.FIRST_SEEN_AT)},
        updated.${sql(Columns.LAST_SEEN_AT)},
        updated.${sql(Columns.UPDATED_AT)},
        updated.${sql(Columns.VALIDATION_STARTED_AT)},
        updated.${sql(Columns.TERMINAL_AT)},
        updated.${sql(Columns.LEASE_OWNER)},
        updated.${sql(Columns.LEASE_EXPIRES_AT)},
        updated.${sql(Columns.ATTEMPT_COUNT)},
        updated.${sql(Columns.NEXT_ATTEMPT_AT)},
        updated.${sql(Columns.REJECT_CODE)},
        updated.${sql(Columns.REJECT_DETAIL)},
        updated.${sql(Columns.SUBMIT_SOURCE)},
        updated.${sql(Columns.REQUEST_COUNT)},
        payload.${sql(Columns.TX_CANONICAL_CBOR)},
        payload.${sql(Columns.TX_FULL_HASH_V1)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
      FROM updated_existing updated
      INNER JOIN input ON input.variant_ordinal = updated.variant_ordinal
      INNER JOIN ${sql(payloadTableName)} payload
        ON payload.${sql(Columns.TX_ID)} = updated.${sql(Columns.TX_ID)}
      ORDER BY variant_ordinal`;

    const resolvedByVariant = new Map(
      resolved.map((row) => [
        Number(row.variant_ordinal),
        {
          entry: normalizeRow(row),
          kind: row.result_kind,
        } satisfies AdmitResult,
      ]),
    );
    for (const variant of variants) {
      if (resolvedByVariant.has(variant.variantOrdinal)) continue;
      const duplicate = yield* touchDuplicateCount({
        txId: variant.txId,
        txFullHashV1: variant.txFullHashV1,
        txCanonicalCbor: variant.txCanonicalCbor,
        programMaterialSidecarCbor: variant.programMaterialSidecarCbor,
        programMaterialSidecarSha256: variant.programMaterialSidecarSha256,
        requestCount: variant.requestIndices.length,
      });
      if (duplicate !== null) {
        resolvedByVariant.set(variant.variantOrdinal, {
          entry: duplicate,
          kind: "duplicate",
        });
      }
    }
    const outcomes: ReservedAdmissionOutcome[] = Array.from(
      { length: requests.length },
      () => ({
        _tag: "Conflict",
        error: new TxAdmissionConflictError({
          txIdHex: "",
          message: "Unresolved reserved admission variant",
        }),
      }),
    );
    let duplicateCount = 0;
    for (const variant of variants) {
      const result = resolvedByVariant.get(variant.variantOrdinal);
      for (
        let offset = 0;
        offset < variant.requestIndices.length;
        offset += 1
      ) {
        const requestIndex = variant.requestIndices[offset]!;
        if (result === undefined) {
          const txIdHex = variant.txId.toString("hex");
          outcomes[requestIndex] = {
            _tag: "Conflict",
            error: new TxAdmissionConflictError({
              txIdHex,
              message: `Refusing to admit transaction ${txIdHex}: tx_id already exists with different normalized bytes`,
            }),
          };
          continue;
        }
        const kind =
          result.kind === "new" && offset === 0 ? "new" : "duplicate";
        if (kind === "duplicate") duplicateCount += 1;
        outcomes[requestIndex] = {
          _tag: "Success",
          result: { entry: result.entry, kind },
        };
      }
    }
    if (duplicateCount > 0) {
      yield* Metric.incrementBy(
        admissionDuplicatePathCounter,
        BigInt(duplicateCount),
      );
    }
    return outcomes;
  }).pipe(
    Effect.tapErrorTag("SqlError", (error) =>
      logDatabaseError(tableName, "admitReservedBatch", error),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to durably admit reserved transaction batch",
    ),
  );
