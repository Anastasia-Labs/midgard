import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

export const tableName = "foreign_tip_reconciliations";

export const Status = {
  Awaiting: "awaiting",
  Resolved: "resolved",
} as const;

export enum Columns {
  FOREIGN_HEADER_HASH = "foreign_header_hash",
  REPLACED_BASE_HEADER_HASH = "replaced_base_header_hash",
  FOREIGN_HEADER_CBOR = "foreign_header_cbor",
  BLOCK_START_TIME = "block_start_time",
  BLOCK_END_TIME = "block_end_time",
  DEPOSITS_ROOT = "deposits_root",
  FORCED_TRANSACTIONS_ROOT = "forced_transactions_root",
  WITHDRAWALS_ROOT = "withdrawals_root",
  DEPOSIT_COUNT = "deposit_count",
  FORCED_TRANSACTION_COUNT = "forced_transaction_count",
  WITHDRAWAL_COUNT = "withdrawal_count",
  VERIFIED_DA_PAYLOAD_CBOR = "verified_da_payload_cbor",
  VERIFIED_DA_SCHEMA_VERSION = "verified_da_schema_version",
  STATUS = "status",
  BLOCKING_REASON = "blocking_reason",
  CREATED_AT = "created_at",
  UPDATED_AT = "updated_at",
  RESOLVED_AT = "resolved_at",
}

export type Entry = {
  [Columns.FOREIGN_HEADER_HASH]: Buffer;
  [Columns.REPLACED_BASE_HEADER_HASH]: Buffer;
  [Columns.FOREIGN_HEADER_CBOR]: Buffer;
  [Columns.BLOCK_START_TIME]: Date;
  [Columns.BLOCK_END_TIME]: Date;
  [Columns.DEPOSITS_ROOT]: string;
  [Columns.FORCED_TRANSACTIONS_ROOT]: string;
  [Columns.WITHDRAWALS_ROOT]: string;
  [Columns.DEPOSIT_COUNT]: bigint;
  [Columns.FORCED_TRANSACTION_COUNT]: bigint;
  [Columns.WITHDRAWAL_COUNT]: bigint;
  [Columns.VERIFIED_DA_PAYLOAD_CBOR]: Buffer | null;
  [Columns.VERIFIED_DA_SCHEMA_VERSION]: number | null;
  [Columns.STATUS]: (typeof Status)[keyof typeof Status];
  [Columns.BLOCKING_REASON]: string | null;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
  [Columns.RESOLVED_AT]: Date | null;
};

export const recordMismatch = ({
  foreignHeaderHash,
  replacedBaseHeaderHash,
  foreignHeader,
}: {
  readonly foreignHeaderHash: string;
  readonly replacedBaseHeaderHash: string;
  readonly foreignHeader: SDK.Header;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const recomputedHeaderHash = yield* SDK.hashBlockHeader(foreignHeader).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: tableName,
            message: "Failed to authenticate foreign-tip reconciliation header",
            cause,
          }),
      ),
    );
    if (recomputedHeaderHash !== foreignHeaderHash) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Foreign-tip reconciliation header hash does not match",
          cause: `provided=${foreignHeaderHash},recomputed=${recomputedHeaderHash}`,
        }),
      );
    }
    const foreignHeaderCbor = Buffer.from(
      LucidData.to(foreignHeader as never, SDK.Header as never),
      "hex",
    );
    const startTimeMs = Number(foreignHeader.startTime);
    const endTimeMs = Number(foreignHeader.endTime);
    const blockStartTime = new Date(startTimeMs);
    const blockEndTime = new Date(endTimeMs);
    if (
      !Number.isSafeInteger(startTimeMs) ||
      !Number.isSafeInteger(endTimeMs) ||
      !Number.isFinite(blockStartTime.getTime()) ||
      !Number.isFinite(blockEndTime.getTime()) ||
      endTimeMs <= startTimeMs
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Foreign-tip reconciliation header has an invalid window",
          cause: `start=${foreignHeader.startTime.toString()},end=${foreignHeader.endTime.toString()}`,
        }),
      );
    }

    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ [Columns.FOREIGN_HEADER_HASH]: Buffer }>`
      INSERT INTO ${sql(tableName)} (
        ${sql(Columns.FOREIGN_HEADER_HASH)},
        ${sql(Columns.REPLACED_BASE_HEADER_HASH)},
        ${sql(Columns.FOREIGN_HEADER_CBOR)},
        ${sql(Columns.BLOCK_START_TIME)},
        ${sql(Columns.BLOCK_END_TIME)},
        ${sql(Columns.DEPOSITS_ROOT)},
        ${sql(Columns.FORCED_TRANSACTIONS_ROOT)},
        ${sql(Columns.WITHDRAWALS_ROOT)},
        ${sql(Columns.DEPOSIT_COUNT)},
        ${sql(Columns.FORCED_TRANSACTION_COUNT)},
        ${sql(Columns.WITHDRAWAL_COUNT)},
        ${sql(Columns.STATUS)},
        ${sql(Columns.BLOCKING_REASON)}
      ) VALUES (
        ${Buffer.from(foreignHeaderHash, "hex")},
        ${Buffer.from(replacedBaseHeaderHash, "hex")},
        ${foreignHeaderCbor},
        ${blockStartTime},
        ${blockEndTime},
        ${foreignHeader.depositsRoot},
        ${foreignHeader.forcedTransactionsRoot},
        ${foreignHeader.withdrawalsRoot},
        ${foreignHeader.depositCount},
        ${foreignHeader.forcedTransactionCount},
        ${foreignHeader.withdrawalCount},
        ${Status.Awaiting},
        ${"pending_evidence"}
      )
      ON CONFLICT (${sql(Columns.FOREIGN_HEADER_HASH)}) DO UPDATE SET
        ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(tableName)}.${sql(Columns.REPLACED_BASE_HEADER_HASH)} = EXCLUDED.${sql(Columns.REPLACED_BASE_HEADER_HASH)}
        AND ${sql(tableName)}.${sql(Columns.FOREIGN_HEADER_CBOR)} = EXCLUDED.${sql(Columns.FOREIGN_HEADER_CBOR)}
        AND ${sql(tableName)}.${sql(Columns.BLOCK_START_TIME)} = EXCLUDED.${sql(Columns.BLOCK_START_TIME)}
        AND ${sql(tableName)}.${sql(Columns.BLOCK_END_TIME)} = EXCLUDED.${sql(Columns.BLOCK_END_TIME)}
        AND ${sql(tableName)}.${sql(Columns.DEPOSITS_ROOT)} = EXCLUDED.${sql(Columns.DEPOSITS_ROOT)}
        AND ${sql(tableName)}.${sql(Columns.FORCED_TRANSACTIONS_ROOT)} = EXCLUDED.${sql(Columns.FORCED_TRANSACTIONS_ROOT)}
        AND ${sql(tableName)}.${sql(Columns.WITHDRAWALS_ROOT)} = EXCLUDED.${sql(Columns.WITHDRAWALS_ROOT)}
        AND ${sql(tableName)}.${sql(Columns.DEPOSIT_COUNT)} = EXCLUDED.${sql(Columns.DEPOSIT_COUNT)}
        AND ${sql(tableName)}.${sql(Columns.FORCED_TRANSACTION_COUNT)} = EXCLUDED.${sql(Columns.FORCED_TRANSACTION_COUNT)}
        AND ${sql(tableName)}.${sql(Columns.WITHDRAWAL_COUNT)} = EXCLUDED.${sql(Columns.WITHDRAWAL_COUNT)}
      RETURNING ${sql(Columns.FOREIGN_HEADER_HASH)}
    `;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Foreign-tip mismatch conflicts with durable reconciliation context",
          cause: `foreign=${foreignHeaderHash},replaced=${replacedBaseHeaderHash}`,
        }),
      );
    }
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to record foreign-tip mismatch"),
  );

export const retrieveAwaitingByForeignHeaderHash = (
  foreignHeaderHash: string,
): Effect.Effect<Option.Option<Entry>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`
      SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.FOREIGN_HEADER_HASH)} = ${Buffer.from(foreignHeaderHash, "hex")}
        AND ${sql(Columns.STATUS)} = ${Status.Awaiting}
      LIMIT 1
    `;
    return rows.length === 0 ? Option.none() : Option.some(rows[0]!);
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve foreign-tip reconciliation",
    ),
  );

export const retrieveByForeignHeaderHash = (
  foreignHeaderHash: string,
): Effect.Effect<Option.Option<Entry>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`
      SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.FOREIGN_HEADER_HASH)} = ${Buffer.from(foreignHeaderHash, "hex")}
      LIMIT 1
    `;
    return rows.length === 0 ? Option.none() : Option.some(rows[0]!);
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve foreign-tip reconciliation evidence",
    ),
  );

export const retrieveEvidenceHistory: Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<Entry>`
    SELECT * FROM ${sql(tableName)}
    ORDER BY ${sql(Columns.BLOCK_START_TIME)} ASC,
             ${sql(Columns.BLOCK_END_TIME)} ASC,
             ${sql(Columns.CREATED_AT)} ASC
  `;
}).pipe(
  sqlErrorToDatabaseError(
    tableName,
    "Failed to retrieve foreign-tip reconciliation evidence history",
  ),
);

export const countAwaiting: Effect.Effect<number, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly count: string }>`
      SELECT COUNT(*)::text AS count
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = ${Status.Awaiting}
    `;
    return Number(rows[0]?.count ?? "0");
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to count awaiting foreign-tip reconciliations",
    ),
  );

export const markResolved = ({
  foreignHeaderHash,
  verifiedDaPayloadCbor,
  verifiedDaSchemaVersion,
}: {
  readonly foreignHeaderHash: string;
  readonly verifiedDaPayloadCbor?: Buffer;
  readonly verifiedDaSchemaVersion?: number;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (
      (verifiedDaPayloadCbor === undefined) !==
      (verifiedDaSchemaVersion === undefined)
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Verified foreign DA evidence must include payload and schema version",
          cause: "payload/schema pair mismatch",
        }),
      );
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ [Columns.FOREIGN_HEADER_HASH]: Buffer }>`
      UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Resolved},
          ${sql(Columns.RESOLVED_AT)} = COALESCE(${sql(Columns.RESOLVED_AT)}, NOW()),
          ${sql(Columns.BLOCKING_REASON)} = NULL,
          ${sql(Columns.VERIFIED_DA_PAYLOAD_CBOR)} = COALESCE(
            ${sql(Columns.VERIFIED_DA_PAYLOAD_CBOR)},
            ${verifiedDaPayloadCbor ?? null}
          ),
          ${sql(Columns.VERIFIED_DA_SCHEMA_VERSION)} = COALESCE(
            ${sql(Columns.VERIFIED_DA_SCHEMA_VERSION)},
            ${verifiedDaSchemaVersion ?? null}
          ),
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.FOREIGN_HEADER_HASH)} = ${Buffer.from(foreignHeaderHash, "hex")}
        AND (
          ${sql(Columns.VERIFIED_DA_PAYLOAD_CBOR)} IS NULL
          OR ${sql(Columns.VERIFIED_DA_PAYLOAD_CBOR)} = ${verifiedDaPayloadCbor ?? null}
        )
        AND (
          ${sql(Columns.VERIFIED_DA_SCHEMA_VERSION)} IS NULL
          OR ${sql(Columns.VERIFIED_DA_SCHEMA_VERSION)} = ${verifiedDaSchemaVersion ?? null}
        )
      RETURNING ${sql(Columns.FOREIGN_HEADER_HASH)}
    `;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Foreign-tip verified evidence conflicts with retained history",
          cause: foreignHeaderHash,
        }),
      );
    }
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to resolve foreign-tip reconciliation",
    ),
  );

export const markAwaiting = ({
  foreignHeaderHash,
  reason,
}: {
  readonly foreignHeaderHash: string;
  readonly reason: string;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`
      UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Awaiting},
          ${sql(Columns.RESOLVED_AT)} = NULL,
          ${sql(Columns.BLOCKING_REASON)} = ${reason},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.FOREIGN_HEADER_HASH)} = ${Buffer.from(foreignHeaderHash, "hex")}
    `;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark foreign-tip reconciliation awaiting evidence",
    ),
  );

export const clear = clearTable(tableName);
