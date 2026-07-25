import { createHash } from "node:crypto";

import { RejectedTx } from "@al-ft/midgard-validation/types";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Data, Duration, Effect, Metric } from "effect";

import * as DepositsDB from "@/database/deposits.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as TxRejectionsDB from "@/database/txRejections.js";
import {
  DatabaseError,
  logDatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import type { Entry as LedgerEntry } from "@/database/utils/ledger.js";
import { emitPhase1AcceptCommitCheckpoint } from "@/e2e/phase1-accept-crash-checkpoint.js";
import { Database } from "@/services/database.js";
import { WriteBehind } from "@/services/write-behind.js";
import { ProcessedTx } from "@/utils.js";

export const tableName = "tx_admissions";
export const payloadTableName = "tx_admission_payloads";

const txAdmissionAcceptedMempoolDurationTimer = Metric.timer(
  "tx_admission_mark_accepted_mempool_duration",
  "Duration of MempoolDB persistence inside TxAdmissionsDB.markAccepted",
);

const txAdmissionAcceptedTerminalDurationTimer = Metric.timer(
  "tx_admission_mark_accepted_terminal_duration",
  "Duration of accepted tx_admissions terminal status updates",
);

const txAdmissionAcceptedTotalDurationTimer = Metric.timer(
  "tx_admission_mark_accepted_total_duration",
  "Total duration of TxAdmissionsDB.markAccepted",
);

// @effect/sql-pg classifies arrays of Buffer values as text[] parameters.
// Encode each element using PostgreSQL's canonical bytea text form so the
// explicit bytea[] cast remains binary-safe while retaining one array bind.
const postgresByteaArray = (values: readonly Buffer[]): readonly string[] =>
  values.map((value) => `\\x${value.toString("hex")}`);

type AcceptedPersistenceCounts = {
  readonly accepted_count: string | number | bigint;
  readonly spent_count: string | number | bigint;
  readonly consumed_deposit_count: string | number | bigint;
  readonly updated_deposit_count: string | number | bigint;
};

const postgresLedgerTimestampArray = (
  values: readonly LedgerEntry[],
): readonly (string | null)[] =>
  values.map((value) =>
    "time_stamp_tz" in value ? value.time_stamp_tz.toISOString() : null,
  );

const txAdmissionMarkRejectedDurationTimer = Metric.timer(
  "tx_admission_mark_rejected_duration",
  "Total duration of TxAdmissionsDB.markRejected",
);

const admissionDuplicatePathCounter = Metric.counter(
  "admission_duplicate_path_total",
  {
    description: "Number of durable admission duplicate-path responses",
    bigint: true,
    incremental: true,
  },
);

const admissionBacklogRejectCounter = Metric.counter(
  "admission_backlog_reject_total",
  {
    description: "Number of durable admissions rejected by the backlog cap",
    bigint: true,
    incremental: true,
  },
);

export const Status = {
  Queued: "queued",
  Validating: "validating",
  Accepted: "accepted",
  Rejected: "rejected",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export type SubmitSource = "native" | "backfill";

export enum Columns {
  TX_ID = "tx_id",
  TX_CANONICAL_CBOR = "tx_canonical_cbor",
  TX_CANONICAL_CBOR_SHA256 = "tx_canonical_cbor_sha256",
  CEK_PROGRAM_MATERIAL_SIDECAR_CBOR = "cek_program_material_sidecar_cbor",
  CEK_PROGRAM_MATERIAL_SIDECAR_SHA256 = "cek_program_material_sidecar_sha256",
  ARRIVAL_SEQ = "arrival_seq",
  STATUS = "status",
  FIRST_SEEN_AT = "first_seen_at",
  LAST_SEEN_AT = "last_seen_at",
  UPDATED_AT = "updated_at",
  VALIDATION_STARTED_AT = "validation_started_at",
  TERMINAL_AT = "terminal_at",
  LEASE_OWNER = "lease_owner",
  LEASE_EXPIRES_AT = "lease_expires_at",
  ATTEMPT_COUNT = "attempt_count",
  NEXT_ATTEMPT_AT = "next_attempt_at",
  REJECT_CODE = "reject_code",
  REJECT_DETAIL = "reject_detail",
  SUBMIT_SOURCE = "submit_source",
  REQUEST_COUNT = "request_count",
}

type RawEntry = {
  readonly [Columns.TX_ID]: Buffer;
  readonly [Columns.TX_CANONICAL_CBOR]: Buffer;
  readonly [Columns.TX_CANONICAL_CBOR_SHA256]: Buffer;
  readonly [Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]: Buffer;
  readonly [Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]: Buffer;
  readonly [Columns.ARRIVAL_SEQ]: bigint | number | string;
  readonly [Columns.STATUS]: Status;
  readonly [Columns.FIRST_SEEN_AT]: Date;
  readonly [Columns.LAST_SEEN_AT]: Date;
  readonly [Columns.UPDATED_AT]: Date;
  readonly [Columns.VALIDATION_STARTED_AT]: Date | null;
  readonly [Columns.TERMINAL_AT]: Date | null;
  readonly [Columns.LEASE_OWNER]: string | null;
  readonly [Columns.LEASE_EXPIRES_AT]: Date | null;
  readonly [Columns.ATTEMPT_COUNT]: number;
  readonly [Columns.NEXT_ATTEMPT_AT]: Date;
  readonly [Columns.REJECT_CODE]: string | null;
  readonly [Columns.REJECT_DETAIL]: string | null;
  readonly [Columns.SUBMIT_SOURCE]: SubmitSource;
  readonly [Columns.REQUEST_COUNT]: bigint | number | string;
};

export type Entry = Omit<
  RawEntry,
  Columns.ARRIVAL_SEQ | Columns.REQUEST_COUNT
> & {
  readonly [Columns.ARRIVAL_SEQ]: bigint;
  readonly [Columns.REQUEST_COUNT]: bigint;
};

type RawClaimedEntry = Pick<
  RawEntry,
  | Columns.TX_ID
  | Columns.TX_CANONICAL_CBOR
  | Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR
  | Columns.ARRIVAL_SEQ
  | Columns.FIRST_SEEN_AT
  | Columns.VALIDATION_STARTED_AT
>;

export type ClaimedEntry = Omit<RawClaimedEntry, Columns.ARRIVAL_SEQ> & {
  readonly [Columns.ARRIVAL_SEQ]: bigint;
};

/**
 * The ordered, durable half of a claim.  It intentionally omits the payload:
 * callers that serialize claim order can release their short claim lock before
 * loading CBOR blobs, while still proving the payload belongs to this exact
 * validation lease before dispatching it to a worker.
 */
type RawClaimedLeaseEntry = Omit<
  RawClaimedEntry,
  Columns.TX_CANONICAL_CBOR | Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR
>;

export type ClaimedLeaseEntry = Omit<
  RawClaimedLeaseEntry,
  Columns.ARRIVAL_SEQ
> & {
  readonly [Columns.ARRIVAL_SEQ]: bigint;
};

export type AdmitResult = {
  readonly entry: Entry;
  readonly kind: "new" | "duplicate";
};

export type ReservedAdmissionRequest = {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly submitSource: Exclude<SubmitSource, "backfill">;
};

export type ReservedAdmissionOutcome =
  | { readonly _tag: "Success"; readonly result: AdmitResult }
  | { readonly _tag: "Conflict"; readonly error: TxAdmissionConflictError };

export class TxAdmissionConflictError extends Data.TaggedError(
  "TxAdmissionConflictError",
)<{
  readonly txIdHex: string;
  readonly message: string;
}> {}

export class TxAdmissionBacklogFullError extends Data.TaggedError(
  "TxAdmissionBacklogFullError",
)<{
  readonly backlog: bigint;
  readonly maxBacklog: bigint;
  readonly message: string;
}> {}

const toBigInt = (value: bigint | number | string): bigint =>
  typeof value === "bigint" ? value : BigInt(value);

const normalizeRow = (row: RawEntry): Entry => ({
  ...row,
  [Columns.ARRIVAL_SEQ]: toBigInt(row[Columns.ARRIVAL_SEQ]),
  [Columns.REQUEST_COUNT]: toBigInt(row[Columns.REQUEST_COUNT]),
});

const normalizeClaimedEntry = (row: RawClaimedEntry): ClaimedEntry => ({
  ...row,
  [Columns.ARRIVAL_SEQ]: toBigInt(row[Columns.ARRIVAL_SEQ]),
});

const normalizeClaimedLeaseEntry = (
  row: RawClaimedLeaseEntry,
): ClaimedLeaseEntry => ({
  ...row,
  [Columns.ARRIVAL_SEQ]: toBigInt(row[Columns.ARRIVAL_SEQ]),
});

const sha256 = (bytes: Buffer): Buffer =>
  createHash("sha256").update(bytes).digest();

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
    const txCanonicalCborSha256 = sha256(txCanonicalCbor);
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
          ${sql(Columns.TX_CANONICAL_CBOR_SHA256)},
          ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
          ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
        )
        SELECT
          ${sql(Columns.TX_ID)},
          ${txCanonicalCbor},
          ${txCanonicalCborSha256},
          ${programMaterialSidecarCbor},
          ${materialSidecarSha256}
        FROM inserted_admission
        RETURNING *
      )
      SELECT
        admission.*,
        payload.${sql(Columns.TX_CANONICAL_CBOR)},
        payload.${sql(Columns.TX_CANONICAL_CBOR_SHA256)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
      FROM inserted_admission admission
      INNER JOIN inserted_payload payload
        ON payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}`;
    return inserted.length === 0 ? null : normalizeRow(inserted[0]!);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to durably admit transaction"),
  );

const touchDuplicateCount = ({
  txId,
  txCanonicalCborSha256,
  txCanonicalCbor,
  programMaterialSidecarCbor,
  programMaterialSidecarSha256,
  requestCount,
}: {
  readonly txId: Buffer;
  readonly txCanonicalCborSha256: Buffer;
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
        AND payload.${sql(Columns.TX_CANONICAL_CBOR_SHA256)} = ${txCanonicalCborSha256}
        AND payload.${sql(Columns.TX_CANONICAL_CBOR)} = ${txCanonicalCbor}
        AND payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)} =
          ${programMaterialSidecarSha256}
        AND payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)} =
          ${programMaterialSidecarCbor}
      RETURNING
        admission.*,
        payload.${sql(Columns.TX_CANONICAL_CBOR)},
        payload.${sql(Columns.TX_CANONICAL_CBOR_SHA256)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}`;
    return updated.length === 0 ? null : normalizeRow(updated[0]!);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to touch duplicate admission"),
  );

export const touchDuplicate = ({
  txId,
  txCanonicalCborSha256,
  txCanonicalCbor,
  programMaterialSidecarCbor,
}: {
  readonly txId: Buffer;
  readonly txCanonicalCborSha256: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
}): Effect.Effect<Entry | null, DatabaseError, Database> =>
  touchDuplicateCount({
    txId,
    txCanonicalCborSha256,
    txCanonicalCbor,
    programMaterialSidecarCbor,
    programMaterialSidecarSha256: sha256(programMaterialSidecarCbor),
    requestCount: 1,
  });

export const admit = ({
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
    const txCanonicalCborSha256 = sha256(txCanonicalCbor);
    const max = BigInt(Math.max(0, maxBacklog));
    if (currentBacklog >= max) {
      const duplicate = yield* touchDuplicate({
        txId,
        txCanonicalCborSha256,
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
      txCanonicalCborSha256,
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

type ReservedAdmissionVariant = {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly txCanonicalCborSha256: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly programMaterialSidecarSha256: Buffer;
  readonly submitSource: Exclude<SubmitSource, "backfill">;
  readonly requestIndices: readonly number[];
  readonly variantOrdinal: number;
  readonly firstVariantForTxId: boolean;
};

type BatchResolvedRawEntry = RawEntry & {
  readonly variant_ordinal: number;
  readonly result_kind: "new" | "duplicate";
};

const groupReservedAdmissionVariants = (
  requests: readonly ReservedAdmissionRequest[],
): readonly ReservedAdmissionVariant[] => {
  const byTxId = new Map<string, ReservedAdmissionVariant[]>();
  for (let index = 0; index < requests.length; index += 1) {
    const request = requests[index]!;
    const txIdHex = request.txId.toString("hex");
    const variants = byTxId.get(txIdHex) ?? [];
    const matching = variants.find(
      (variant) =>
        variant.txCanonicalCbor.equals(request.txCanonicalCbor) &&
        variant.programMaterialSidecarCbor.equals(
          request.programMaterialSidecarCbor,
        ) &&
        variant.submitSource === request.submitSource,
    );
    if (matching === undefined) {
      variants.push({
        ...request,
        txCanonicalCborSha256: sha256(request.txCanonicalCbor),
        programMaterialSidecarSha256: sha256(
          request.programMaterialSidecarCbor,
        ),
        requestIndices: [index],
        variantOrdinal: -1,
        firstVariantForTxId: variants.length === 0,
      });
      byTxId.set(txIdHex, variants);
    } else {
      (matching.requestIndices as number[]).push(index);
    }
  }
  return [...byTxId.values()]
    .flat()
    .sort((left, right) => Buffer.compare(left.txId, right.txId))
    .map((variant, variantOrdinal) => ({ ...variant, variantOrdinal }));
};

/**
 * Resolves reserved admission requests in one atomic PostgreSQL statement on
 * the uncontended path. A concurrent ON CONFLICT loser may need one bounded
 * follow-up touch because PostgreSQL's statement snapshot cannot see the row
 * whose conflicting insert it just waited for.
 * The first byte variant for an absent tx id wins; existing rows instead match
 * their persisted bytes. Deterministic tx-id ordering prevents opposite-order
 * microbatches from acquiring conflicting unique-index locks in opposite order.
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
    const variants = groupReservedAdmissionVariants(requests);
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const resolved = yield* sql<BatchResolvedRawEntry>`WITH input AS (
        SELECT *
        FROM unnest(
          ${pg.array(postgresByteaArray(variants.map((value) => value.txId)))}::bytea[],
          ${pg.array(postgresByteaArray(variants.map((value) => value.txCanonicalCbor)))}::bytea[],
          ${pg.array(postgresByteaArray(variants.map((value) => value.txCanonicalCborSha256)))}::bytea[],
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
          tx_canonical_cbor_sha256,
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
          ${sql(Columns.TX_CANONICAL_CBOR_SHA256)},
          ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
          ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
        )
        SELECT
          inserted.${sql(Columns.TX_ID)},
          input.tx_canonical_cbor,
          input.tx_canonical_cbor_sha256,
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
          AND payload.${sql(Columns.TX_CANONICAL_CBOR_SHA256)} =
            input.tx_canonical_cbor_sha256
          AND payload.${sql(Columns.TX_CANONICAL_CBOR)} =
            input.tx_canonical_cbor
          AND payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)}
            = input.cek_program_material_sidecar_cbor
          AND payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}
            = input.cek_program_material_sidecar_sha256
        WHERE admissions.${sql(Columns.TX_ID)} = input.tx_id
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
        payload.${sql(Columns.TX_CANONICAL_CBOR_SHA256)},
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
        payload.${sql(Columns.TX_CANONICAL_CBOR_SHA256)},
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
        txCanonicalCborSha256: variant.txCanonicalCborSha256,
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
        payload.${sql(Columns.TX_CANONICAL_CBOR_SHA256)},
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
        return yield* sql<RawClaimedEntry>`WITH candidates AS (
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
            payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
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
    return rows.map(normalizeClaimedEntry);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to claim admitted transactions"),
  );

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
        return yield* sql<RawClaimedLeaseEntry>`WITH candidates AS (
          SELECT ctid AS row_ctid
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
          WHERE admissions.ctid = candidates.row_ctid
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
    const rows = yield* sql<RawClaimedEntry>`SELECT
        admission.${sql(Columns.TX_ID)},
        payload.${sql(Columns.TX_CANONICAL_CBOR)},
        payload.${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)},
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
    const entries = rows.map(normalizeClaimedEntry);
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

export const markAccepted = ({
  rows,
  leaseOwner,
  processedTxs,
}: {
  readonly rows: readonly Pick<Entry, Columns.TX_ID>[];
  readonly leaseOwner: string;
  readonly processedTxs: readonly ProcessedTx[];
}): Effect.Effect<void, DatabaseError, Database | WriteBehind> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const totalStartedAt = Date.now();
    const acceptedTxIds = processedTxs.map((tx) => tx.txId);
    const acceptedTxIdArray = postgresByteaArray(acceptedTxIds);
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const mempoolStartedAt = Date.now();
        const { produced, spent } =
          MempoolDB.compactLedgerEffects(processedTxs);
        const compactArrays =
          produced.length > 0 && spent.length > 0
            ? {
                producedTxIds: postgresByteaArray(
                  produced.map((entry) => entry[MempoolLedgerDB.Columns.TX_ID]),
                ),
                producedOutrefs: postgresByteaArray(
                  produced.map(
                    (entry) => entry[MempoolLedgerDB.Columns.OUTREF],
                  ),
                ),
                producedOutputs: postgresByteaArray(
                  produced.map(
                    (entry) => entry[MempoolLedgerDB.Columns.OUTPUT],
                  ),
                ),
                producedAddresses: produced.map(
                  (entry) => entry[MempoolLedgerDB.Columns.ADDRESS],
                ),
                producedTimestamps: postgresLedgerTimestampArray(produced),
                spentOutrefs: postgresByteaArray(spent),
              }
            : null;
        let fallbackMempoolCount = 0;
        if (compactArrays === null) {
          const insertedMemberships = yield* sql<
            Pick<RawEntry, Columns.TX_ID>
          >`INSERT INTO ${sql(MempoolDB.tableName)} (tx_id, tx)
            SELECT
              admission.${sql(Columns.TX_ID)},
              payload.${sql(Columns.TX_CANONICAL_CBOR)}
            FROM ${sql(tableName)} AS admission
            INNER JOIN ${sql(payloadTableName)} AS payload
              ON payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
            WHERE admission.${sql(Columns.STATUS)} = 'validating'
              AND admission.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
              AND admission.${sql(Columns.TX_ID)} =
                ANY(${pg.array(acceptedTxIdArray)}::bytea[])
            ON CONFLICT (tx_id) DO NOTHING
            RETURNING ${sql(Columns.TX_ID)}`;
          fallbackMempoolCount = insertedMemberships.length;
          if (fallbackMempoolCount !== processedTxs.length) {
            return yield* Effect.fail(
              new DatabaseError({
                table: MempoolDB.tableName,
                message:
                  "Failed to persist accepted mempool memberships exactly once with durable admission payloads",
                cause: `expected=${processedTxs.length},inserted=${fallbackMempoolCount}`,
              }),
            );
          }
          yield* MempoolDB.applyLedgerEffectsCore(processedTxs);
        }
        yield* txAdmissionAcceptedMempoolDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - mempoolStartedAt)),
        );
        const terminalStartedAt = Date.now();
        const counts =
          compactArrays !== null
            ? yield* sql<AcceptedPersistenceCounts>`
                WITH accepted_admissions AS (
                  UPDATE ${sql(tableName)} AS admission
                  SET
                    ${sql(Columns.STATUS)} = 'accepted',
                    ${sql(Columns.LEASE_OWNER)} = NULL,
                    ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
                    ${sql(Columns.TERMINAL_AT)} = GREATEST(
                      NOW(),
                      admission.${sql(Columns.FIRST_SEEN_AT)},
                      admission.${sql(Columns.LAST_SEEN_AT)},
                      admission.${sql(Columns.UPDATED_AT)},
                      COALESCE(
                        admission.${sql(Columns.VALIDATION_STARTED_AT)},
                        admission.${sql(Columns.FIRST_SEEN_AT)}
                      )
                    ),
                    ${sql(Columns.UPDATED_AT)} = GREATEST(
                      NOW(),
                      admission.${sql(Columns.FIRST_SEEN_AT)},
                      admission.${sql(Columns.LAST_SEEN_AT)},
                      admission.${sql(Columns.UPDATED_AT)},
                      COALESCE(
                        admission.${sql(Columns.VALIDATION_STARTED_AT)},
                        admission.${sql(Columns.FIRST_SEEN_AT)}
                      )
                    )
                  FROM ${sql(payloadTableName)} AS payload
                  WHERE admission.${sql(Columns.STATUS)} = 'validating'
                    AND admission.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
                    AND admission.${sql(Columns.TX_ID)} =
                      ANY(${pg.array(acceptedTxIdArray)}::bytea[])
                    AND payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
                  RETURNING
                    admission.${sql(Columns.TX_ID)},
                    payload.${sql(Columns.TX_CANONICAL_CBOR)}
                ),
                mempool_insert AS (
                  INSERT INTO ${sql(MempoolDB.tableName)} (tx_id, tx)
                  SELECT
                    ${sql(Columns.TX_ID)},
                    ${sql(Columns.TX_CANONICAL_CBOR)}
                  FROM accepted_admissions
                ),
                produced_insert AS (
                  INSERT INTO ${sql(MempoolLedgerDB.tableName)} (
                    ${sql(MempoolLedgerDB.Columns.TX_ID)},
                    ${sql(MempoolLedgerDB.Columns.OUTREF)},
                    ${sql(MempoolLedgerDB.Columns.OUTPUT)},
                    ${sql(MempoolLedgerDB.Columns.ADDRESS)},
                    ${sql(MempoolLedgerDB.Columns.TIMESTAMPTZ)}
                  )
                  SELECT
                    produced_input.tx_id,
                    produced_input.outref,
                    produced_input.output,
                    produced_input.address,
                    COALESCE(produced_input.time_stamp_tz, NOW())
                  FROM unnest(
                    ${pg.array(compactArrays.producedTxIds)}::bytea[],
                    ${pg.array(compactArrays.producedOutrefs)}::bytea[],
                    ${pg.array(compactArrays.producedOutputs)}::bytea[],
                    ${pg.array(compactArrays.producedAddresses)}::text[],
                    ${pg.array(compactArrays.producedTimestamps)}::timestamptz[]
                  ) AS produced_input(
                    tx_id,
                    outref,
                    output,
                    address,
                    time_stamp_tz
                  )
                ),
                spent_delete AS (
                  DELETE FROM ${sql(MempoolLedgerDB.tableName)}
                  WHERE ${sql(MempoolLedgerDB.Columns.OUTREF)} =
                    ANY(${pg.array(compactArrays.spentOutrefs)}::bytea[])
                  RETURNING ${sql(MempoolLedgerDB.Columns.SOURCE_EVENT_ID)}
                ),
                deposit_update AS (
                  UPDATE ${sql(DepositsDB.tableName)} deposits
                  SET ${sql(DepositsDB.Columns.STATUS)} = ${DepositsDB.Status.Consumed}
                  FROM spent_delete
                  WHERE spent_delete.${sql(
                    MempoolLedgerDB.Columns.SOURCE_EVENT_ID,
                  )} IS NOT NULL
                    AND deposits.${sql(DepositsDB.Columns.ID)} =
                      spent_delete.${sql(
                        MempoolLedgerDB.Columns.SOURCE_EVENT_ID,
                      )}
                    AND deposits.${sql(DepositsDB.Columns.STATUS)}
                      IN (${DepositsDB.Status.Projected}, ${DepositsDB.Status.Consumed})
                  RETURNING 1
                )
                SELECT
                  (SELECT COUNT(*)::bigint FROM accepted_admissions)
                    AS accepted_count,
                  (SELECT COUNT(*)::bigint FROM spent_delete)
                    AS spent_count,
                  (SELECT COUNT(*)::bigint FROM spent_delete
                    WHERE ${sql(MempoolLedgerDB.Columns.SOURCE_EVENT_ID)}
                      IS NOT NULL)
                    AS consumed_deposit_count,
                  (SELECT COUNT(*)::bigint FROM deposit_update)
                    AS updated_deposit_count
              `
            : yield* sql<AcceptedPersistenceCounts>`
                WITH accepted_admissions AS (
                  UPDATE ${sql(tableName)} AS admission
                  SET
                    ${sql(Columns.STATUS)} = 'accepted',
                    ${sql(Columns.LEASE_OWNER)} = NULL,
                    ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
                    ${sql(Columns.TERMINAL_AT)} = GREATEST(
                      NOW(),
                      admission.${sql(Columns.FIRST_SEEN_AT)},
                      admission.${sql(Columns.LAST_SEEN_AT)},
                      admission.${sql(Columns.UPDATED_AT)},
                      COALESCE(
                        admission.${sql(Columns.VALIDATION_STARTED_AT)},
                        admission.${sql(Columns.FIRST_SEEN_AT)}
                      )
                    ),
                    ${sql(Columns.UPDATED_AT)} = GREATEST(
                      NOW(),
                      admission.${sql(Columns.FIRST_SEEN_AT)},
                      admission.${sql(Columns.LAST_SEEN_AT)},
                      admission.${sql(Columns.UPDATED_AT)},
                      COALESCE(
                        admission.${sql(Columns.VALIDATION_STARTED_AT)},
                        admission.${sql(Columns.FIRST_SEEN_AT)}
                      )
                    )
                  FROM ${sql(payloadTableName)} AS payload
                  WHERE admission.${sql(Columns.STATUS)} = 'validating'
                    AND admission.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
                    AND admission.${sql(Columns.TX_ID)} =
                      ANY(${pg.array(acceptedTxIdArray)}::bytea[])
                    AND payload.${sql(Columns.TX_ID)} = admission.${sql(Columns.TX_ID)}
                  RETURNING 1
                )
                SELECT
                  (SELECT COUNT(*)::bigint FROM accepted_admissions)
                    AS accepted_count,
                  ${BigInt(spent.length)}::bigint AS spent_count,
                  0::bigint AS consumed_deposit_count,
                  0::bigint AS updated_deposit_count
              `;
        const result = counts[0];
        const expected = {
          accepted_count: processedTxs.length,
          ...(compactArrays === null ? {} : { spent_count: spent.length }),
        } as const;
        const mismatch = Object.entries(expected).find(
          ([key, value]) =>
            Number(result?.[key as keyof AcceptedPersistenceCounts] ?? -1) !==
            value,
        );
        const consumedDepositCount = Number(
          result?.consumed_deposit_count ?? -1,
        );
        const updatedDepositCount = Number(result?.updated_deposit_count ?? -1);
        if (
          mismatch !== undefined ||
          consumedDepositCount !== updatedDepositCount
        ) {
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Failed to mark accepted admissions exactly once under the active validation lease",
              cause: `expected=${JSON.stringify(expected)},actual=${JSON.stringify(result)},claimed=${rows.length}`,
            }),
          );
        }
        yield* txAdmissionAcceptedTerminalDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - terminalStartedAt)),
        );
      }),
    );
    yield* emitPhase1AcceptCommitCheckpoint(acceptedTxIds);
    yield* MempoolDB.enqueueAcceptedWriteBehind(processedTxs);
    yield* txAdmissionAcceptedTotalDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - totalStartedAt)),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to mark admissions accepted"),
  );

export const markRejected = ({
  rows,
  leaseOwner,
  rejectedTxs,
}: {
  readonly rows: readonly Pick<Entry, Columns.TX_ID>[];
  readonly leaseOwner: string;
  readonly rejectedTxs: readonly RejectedTx[];
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (rejectedTxs.length === 0) {
      return;
    }
    const startedAt = Date.now();
    const sql = yield* SqlClient.SqlClient;
    const rejectionRows = rejectedTxs.map((rejectedTx) => ({
      tx_id: rejectedTx.txId,
      reject_code: rejectedTx.code,
      reject_detail: rejectedTx.detail,
    }));
    const txIds = rejectedTxs.map((tx) => tx.txId);
    const rejectionValues = rejectedTxs.map(
      (rejectedTx) =>
        sql`(${rejectedTx.txId}, ${rejectedTx.code}, ${rejectedTx.detail})`,
    );
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const persistedRejections = yield* sql<
          Pick<TxRejectionsDB.Entry, TxRejectionsDB.Columns.TX_ID>
        >`INSERT INTO ${sql(TxRejectionsDB.tableName)} ${sql.insert(
          rejectionRows,
        )}
          ON CONFLICT (${sql(TxRejectionsDB.Columns.TX_ID)}) DO UPDATE SET
            ${sql(TxRejectionsDB.Columns.TX_ID)} = ${sql(
              TxRejectionsDB.tableName,
            )}.${sql(TxRejectionsDB.Columns.TX_ID)}
          WHERE
            ${sql(TxRejectionsDB.tableName)}.${sql(
              TxRejectionsDB.Columns.REJECT_CODE,
            )} = EXCLUDED.${sql(TxRejectionsDB.Columns.REJECT_CODE)}
            AND ${sql(TxRejectionsDB.tableName)}.${sql(
              TxRejectionsDB.Columns.REJECT_DETAIL,
            )} IS NOT DISTINCT FROM EXCLUDED.${sql(
              TxRejectionsDB.Columns.REJECT_DETAIL,
            )}
          RETURNING ${sql(TxRejectionsDB.Columns.TX_ID)}`;
        if (persistedRejections.length !== rejectedTxs.length) {
          return yield* Effect.fail(
            new DatabaseError({
              table: TxRejectionsDB.tableName,
              message:
                "Failed to persist rejected transactions exactly once with matching rejection metadata",
              cause: `expected=${rejectedTxs.length},persisted=${persistedRejections.length}`,
            }),
          );
        }
        const updated = yield* sql<Pick<RawEntry, Columns.TX_ID>>`
          UPDATE ${sql(tableName)} AS admissions
          SET
            ${sql(Columns.STATUS)} = 'rejected',
            ${sql(Columns.LEASE_OWNER)} = NULL,
            ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
            ${sql(Columns.TERMINAL_AT)} = GREATEST(
              NOW(),
              admissions.${sql(Columns.FIRST_SEEN_AT)},
              admissions.${sql(Columns.LAST_SEEN_AT)},
              admissions.${sql(Columns.UPDATED_AT)},
              COALESCE(
                admissions.${sql(Columns.VALIDATION_STARTED_AT)},
                admissions.${sql(Columns.FIRST_SEEN_AT)}
              )
            ),
            ${sql(Columns.REJECT_CODE)} = rejected.reject_code,
            ${sql(Columns.REJECT_DETAIL)} = rejected.reject_detail,
            ${sql(Columns.UPDATED_AT)} = GREATEST(
              NOW(),
              admissions.${sql(Columns.FIRST_SEEN_AT)},
              admissions.${sql(Columns.LAST_SEEN_AT)},
              admissions.${sql(Columns.UPDATED_AT)},
              COALESCE(
                admissions.${sql(Columns.VALIDATION_STARTED_AT)},
                admissions.${sql(Columns.FIRST_SEEN_AT)}
              )
            )
          FROM (VALUES ${sql.csv(rejectionValues)})
            AS rejected(tx_id, reject_code, reject_detail)
          WHERE admissions.${sql(Columns.TX_ID)} = rejected.tx_id
            AND admissions.${sql(Columns.STATUS)} = 'validating'
            AND admissions.${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
          RETURNING admissions.${sql(Columns.TX_ID)}`;
        if (updated.length !== txIds.length) {
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Failed to mark rejected admissions exactly once under the active validation lease",
              cause: `expected=${txIds.length},updated=${updated.length},claimed=${rows.length}`,
            }),
          );
        }
      }),
    );
    yield* txAdmissionMarkRejectedDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - startedAt)),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to mark admissions rejected"),
  );

export const countBacklog: Effect.Effect<bigint, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly count: bigint | number | string }>`
      SELECT (
        (SELECT COUNT(*) FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} = 'queued')
        +
        (SELECT COUNT(*) FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} = 'validating')
      )::bigint AS count`;
    return toBigInt(rows[0].count);
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to count backlog"));

export const oldestQueuedAgeMs: Effect.Effect<number, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly age_ms: number | string }>`
      SELECT (COALESCE(EXTRACT(EPOCH FROM (NOW() - MIN(${sql(
        Columns.FIRST_SEEN_AT,
      )}))), 0) * 1000)::double precision AS age_ms
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = 'queued'`;
    return Number(rows[0].age_ms);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to compute oldest queued age"),
  );
