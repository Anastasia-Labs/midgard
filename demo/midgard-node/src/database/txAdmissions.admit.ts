import { computeMidgardNativeTxFullHashFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import {
  type AdmitResult,
  Columns,
  type Entry,
  normalizeRow,
  payloadTableName,
  type RawEntry,
  type SubmitSource,
  tableName,
  TxAdmissionConflictError,
} from "./txAdmissions.verify-claimed-payload-rows.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/**
 * Inserts one queued admission row and its payload, or nothing when the tx id
 * is already admitted. It applies no backlog quota.
 */
export const tryInsert = ({
  txId,
  txCanonicalCbor,
  submitSource,
}: {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly submitSource: SubmitSource;
}): Effect.Effect<Entry | null, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const txFullHash =
      computeMidgardNativeTxFullHashFromCanonicalCbor(txCanonicalCbor);
    const inserted = yield* sql<RawEntry>`WITH inserted_admission AS (
        INSERT INTO ${sql(tableName)} (
          ${sql(Columns.TX_ID)},
          ${sql(Columns.STATUS)},
          ${sql(Columns.SUBMIT_SOURCE)},
          ${sql(Columns.PAYLOAD_BYTES)},
          ${sql(Columns.TX_FULL_HASH_V1)}
        ) VALUES (
          ${txId},
          'queued',
          ${submitSource},
          ${txCanonicalCbor.length},
          ${txFullHash}
        )
        ON CONFLICT (${sql(Columns.TX_ID)}) DO NOTHING
        RETURNING *
      ), inserted_payload AS (
        INSERT INTO ${sql(payloadTableName)} (
          ${sql(Columns.TX_ID)},
          ${sql(Columns.TX_CANONICAL_CBOR)}
        )
        SELECT ${sql(Columns.TX_ID)}, ${txCanonicalCbor}
        FROM inserted_admission
        RETURNING ${sql(Columns.TX_ID)}
      )
      SELECT admission.*
      FROM inserted_admission admission
      INNER JOIN inserted_payload payload
        ON payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}`;
    return inserted.length === 0 ? null : normalizeRow(inserted[0]!);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to durably admit transaction"),
  );

/**
 * Counts `requestCount` more submissions of an admitted tx whose stored full
 * hash equals `txFullHashV1`. The full hash covers the witnesses, so a match
 * means the same canonical bytes; the payload row is never read, and may
 * already be gone for a terminal row.
 */
export const touchDuplicateCount = ({
  txId,
  txFullHashV1: txFullHash,
  requestCount,
}: {
  readonly txId: Buffer;
  readonly txFullHashV1: Buffer;
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
      WHERE admission.${sql(Columns.TX_ID)} = ${txId}
        AND admission.${sql(Columns.TX_FULL_HASH_V1)} = ${txFullHash}
      RETURNING admission.*`;
    return updated.length === 0 ? null : normalizeRow(updated[0]!);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to touch duplicate admission"),
  );

export const touchDuplicate = ({
  txId,
  txCanonicalCbor,
}: {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
}): Effect.Effect<Entry | null, DatabaseError, Database> =>
  touchDuplicateCount({
    txId,
    txFullHashV1:
      computeMidgardNativeTxFullHashFromCanonicalCbor(txCanonicalCbor),
    requestCount: 1,
  });

/**
 * Admits one transaction outside the admission writer, with no backlog
 * quota: a new row, a counted duplicate, or a conflict when the tx id is
 * stored with other bytes. Tooling and tests use it; the HTTP routes reserve
 * the backlog in memory and go through the admission writer.
 */
export const admit = ({
  txId,
  txCanonicalCbor,
  submitSource,
}: {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly submitSource: Exclude<SubmitSource, "backfill">;
}): Effect.Effect<
  AdmitResult,
  DatabaseError | TxAdmissionConflictError,
  Database
> =>
  Effect.gen(function* () {
    const inserted = yield* tryInsert({ txId, txCanonicalCbor, submitSource });
    if (inserted !== null) return { entry: inserted, kind: "new" as const };
    const duplicate = yield* touchDuplicate({ txId, txCanonicalCbor });
    if (duplicate !== null)
      return { entry: duplicate, kind: "duplicate" as const };
    return yield* Effect.fail(
      new TxAdmissionConflictError({
        txIdHex: txId.toString("hex"),
        message: `Refusing to admit transaction ${txId.toString("hex")}: tx_id already exists with different normalized bytes`,
      }),
    );
  });
