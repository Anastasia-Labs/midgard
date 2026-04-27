import { createHash } from "node:crypto";
import { SqlClient } from "@effect/sql";
import { SqlError } from "@effect/sql";
import { Data, Effect } from "effect";
import { Database } from "@/services/database.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as MempoolDB from "@/database/mempool.js";
import * as TxRejectionsDB from "@/database/txRejections.js";
import { ProcessedTx } from "@/utils.js";
import { RejectedTx } from "@/validation/types.js";

export const tableName = "tx_admissions";

export const Status = {
  Queued: "queued",
  Validating: "validating",
  Accepted: "accepted",
  Rejected: "rejected",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export type SubmitSource = "native" | "cardano-converted" | "backfill";

export enum Columns {
  TX_ID = "tx_id",
  TX_CBOR = "tx_cbor",
  TX_CBOR_SHA256 = "tx_cbor_sha256",
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
  readonly [Columns.TX_CBOR]: Buffer;
  readonly [Columns.TX_CBOR_SHA256]: Buffer;
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

export type AdmitResult = {
  readonly entry: Entry;
  readonly kind: "new" | "duplicate";
};

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

const sha256 = (bytes: Buffer): Buffer =>
  createHash("sha256").update(bytes).digest();

const sameBytes = (left: Buffer, right: Buffer): boolean => left.equals(right);

const normalizeSqlError =
  <E>(message: string) =>
  <A, R>(
    effect: Effect.Effect<A, E | SqlError.SqlError | DatabaseError, R>,
  ): Effect.Effect<A, E | DatabaseError, R> =>
    Effect.mapError(effect, (error) =>
      error instanceof SqlError.SqlError
        ? new DatabaseError({
            table: tableName,
            message,
            cause: error,
          })
        : error,
    );

export const admit = ({
  txId,
  txCbor,
  submitSource,
  maxBacklog,
}: {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
  readonly submitSource: Exclude<SubmitSource, "backfill">;
  readonly maxBacklog: number;
}): Effect.Effect<
  AdmitResult,
  DatabaseError | TxAdmissionConflictError | TxAdmissionBacklogFullError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const txCborSha256 = sha256(txCbor);
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const existingRows = yield* sql<RawEntry>`SELECT *
          FROM ${sql(tableName)}
          WHERE ${sql(Columns.TX_ID)} = ${txId}
          FOR UPDATE`;
        const existing = existingRows[0];
        if (existing !== undefined) {
          if (
            !sameBytes(existing[Columns.TX_CBOR_SHA256], txCborSha256) ||
            !sameBytes(existing[Columns.TX_CBOR], txCbor)
          ) {
            return yield* Effect.fail(
              new TxAdmissionConflictError({
                txIdHex: txId.toString("hex"),
                message:
                  "Refusing to admit transaction: tx_id already exists with different normalized bytes",
              }),
            );
          }
          const updated = yield* sql<RawEntry>`UPDATE ${sql(tableName)}
            SET
              ${sql(Columns.LAST_SEEN_AT)} = NOW(),
              ${sql(Columns.UPDATED_AT)} = NOW(),
              ${sql(Columns.REQUEST_COUNT)} = ${sql(Columns.REQUEST_COUNT)} + 1
            WHERE ${sql(Columns.TX_ID)} = ${txId}
            RETURNING *`;
          return {
            entry: normalizeRow(updated[0]!),
            kind: "duplicate" as const,
          };
        }

        const backlogRows = yield* sql<{
          readonly count: bigint | number | string;
        }>`SELECT COUNT(*)::bigint AS count
          FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} IN ('queued', 'validating')`;
        const backlog = toBigInt(backlogRows[0]?.count ?? 0n);
        const max = BigInt(Math.max(0, maxBacklog));
        if (backlog >= max) {
          return yield* Effect.fail(
            new TxAdmissionBacklogFullError({
              backlog,
              maxBacklog: max,
              message:
                "Durable submission admission backlog is full; retry later",
            }),
          );
        }

        const inserted = yield* sql<RawEntry>`INSERT INTO ${sql(tableName)} (
            ${sql(Columns.TX_ID)},
            ${sql(Columns.TX_CBOR)},
            ${sql(Columns.TX_CBOR_SHA256)},
            ${sql(Columns.STATUS)},
            ${sql(Columns.SUBMIT_SOURCE)}
          ) VALUES (
            ${txId},
            ${txCbor},
            ${txCborSha256},
            'queued',
            ${submitSource}
          )
          RETURNING *`;
        return {
          entry: normalizeRow(inserted[0]!),
          kind: "new" as const,
        };
      }),
    );
  }).pipe(normalizeSqlError("Failed to durably admit transaction"));

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
      ${sql(Columns.UPDATED_AT)} = NOW()
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
      WHERE ${sql(Columns.TX_ID)} = ${txId}
      LIMIT 1`;
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
}): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawEntry>`WITH candidates AS (
        SELECT ${sql(Columns.TX_ID)}
        FROM ${sql(tableName)}
        WHERE ${sql(Columns.STATUS)} = 'queued'
          AND ${sql(Columns.NEXT_ATTEMPT_AT)} <= NOW()
        ORDER BY ${sql(Columns.ARRIVAL_SEQ)} ASC
        FOR UPDATE SKIP LOCKED
        LIMIT ${Math.max(1, limit)}
      )
      UPDATE ${sql(tableName)} admissions
      SET
        ${sql(Columns.STATUS)} = 'validating',
        ${sql(Columns.LEASE_OWNER)} = ${leaseOwner},
        ${sql(Columns.LEASE_EXPIRES_AT)} =
          NOW() + (${Math.max(1, leaseDurationMs)} * INTERVAL '1 millisecond'),
        ${sql(Columns.VALIDATION_STARTED_AT)} =
          COALESCE(${sql(Columns.VALIDATION_STARTED_AT)}, NOW()),
        ${sql(Columns.ATTEMPT_COUNT)} = ${sql(Columns.ATTEMPT_COUNT)} + 1,
        ${sql(Columns.UPDATED_AT)} = NOW()
      FROM candidates
      WHERE admissions.${sql(Columns.TX_ID)} = candidates.${sql(Columns.TX_ID)}
      RETURNING admissions.*`;
    return rows.map(normalizeRow);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to claim admitted transactions"),
  );

export const releaseForRetry = ({
  txIds,
  leaseOwner,
  delayMs,
}: {
  readonly txIds: readonly Buffer[];
  readonly leaseOwner: string;
  readonly delayMs: number;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET
        ${sql(Columns.STATUS)} = 'queued',
        ${sql(Columns.LEASE_OWNER)} = NULL,
        ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
        ${sql(Columns.NEXT_ATTEMPT_AT)} =
          NOW() + (${Math.max(0, delayMs)} * INTERVAL '1 millisecond'),
        ${sql(Columns.UPDATED_AT)} = NOW()
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
  readonly rows: readonly Entry[];
  readonly leaseOwner: string;
  readonly processedTxs: readonly ProcessedTx[];
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const txIds = processedTxs.map((tx) => tx.txId);
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* MempoolDB.insertMultiple([...processedTxs]);
        const updated = yield* sql<Pick<RawEntry, Columns.TX_ID>>`
          UPDATE ${sql(tableName)}
          SET
            ${sql(Columns.STATUS)} = 'accepted',
            ${sql(Columns.LEASE_OWNER)} = NULL,
            ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
            ${sql(Columns.TERMINAL_AT)} = NOW(),
            ${sql(Columns.UPDATED_AT)} = NOW()
          WHERE ${sql.in(Columns.TX_ID, txIds)}
            AND ${sql(Columns.STATUS)} = 'validating'
            AND ${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
          RETURNING ${sql(Columns.TX_ID)}`;
        if (updated.length !== txIds.length) {
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Failed to mark accepted admissions exactly once under the active validation lease",
              cause: `expected=${txIds.length},updated=${updated.length},claimed=${rows.length}`,
            }),
          );
        }
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to mark admissions accepted"),
  );

export const markRejected = ({
  rows,
  leaseOwner,
  rejectedTxs,
}: {
  readonly rows: readonly Entry[];
  readonly leaseOwner: string;
  readonly rejectedTxs: readonly RejectedTx[];
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (rejectedTxs.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const rejectionRows = rejectedTxs.map((rejectedTx) => ({
      tx_id: rejectedTx.txId,
      reject_code: rejectedTx.code,
      reject_detail: rejectedTx.detail,
    }));
    const txIds = rejectedTxs.map((tx) => tx.txId);
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
        let updatedCount = 0;
        for (const rejectedTx of rejectedTxs) {
          const updated = yield* sql<Pick<RawEntry, Columns.TX_ID>>`
            UPDATE ${sql(tableName)}
            SET
              ${sql(Columns.STATUS)} = 'rejected',
              ${sql(Columns.LEASE_OWNER)} = NULL,
              ${sql(Columns.LEASE_EXPIRES_AT)} = NULL,
              ${sql(Columns.TERMINAL_AT)} = NOW(),
              ${sql(Columns.REJECT_CODE)} = ${rejectedTx.code},
              ${sql(Columns.REJECT_DETAIL)} = ${rejectedTx.detail},
              ${sql(Columns.UPDATED_AT)} = NOW()
            WHERE ${sql(Columns.TX_ID)} = ${rejectedTx.txId}
              AND ${sql(Columns.STATUS)} = 'validating'
              AND ${sql(Columns.LEASE_OWNER)} = ${leaseOwner}
            RETURNING ${sql(Columns.TX_ID)}`;
          updatedCount += updated.length;
        }
        if (updatedCount !== txIds.length) {
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Failed to mark rejected admissions exactly once under the active validation lease",
              cause: `expected=${txIds.length},updated=${updatedCount},claimed=${rows.length}`,
            }),
          );
        }
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to mark admissions rejected"),
  );

export const countBacklog: Effect.Effect<bigint, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly count: bigint | number | string }>`
      SELECT COUNT(*)::bigint AS count
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} IN ('queued', 'validating')`;
    return toBigInt(rows[0]?.count ?? 0n);
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
    const value = rows[0]?.age_ms;
    return value === undefined ? 0 : Number(value);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to compute oldest queued age"),
  );
