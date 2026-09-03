import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";

export const tableName = "deposit_submission_attempts";

export enum Columns {
  TX_HASH = "tx_hash",
  DEPOSIT_EVENT_ID = "deposit_event_id",
  EXPECTED_DEPOSIT_OUT_REF = "expected_deposit_out_ref",
  EXPECTED_L2_ADDRESS = "expected_l2_address",
  EXPECTED_LOVELACE = "expected_lovelace",
  EXPECTED_ASSETS = "expected_assets",
  METADATA = "metadata",
  FUNDING_OUT_REFS = "funding_out_refs",
  SUBMITTED_AT = "submitted_at",
  CONFIRMATION_STATUS = "confirmation_status",
  CONFIRMED_AT = "confirmed_at",
  LAST_RECONCILED_AT = "last_reconciled_at",
  LAST_ERROR = "last_error",
  UPDATED_AT = "updated_at",
}

export const Status = {
  SubmittedConfirmationUnknown: "submitted_confirmation_unknown",
  Confirmed: "confirmed",
  ReconciledAfterTimeout: "reconciled_after_timeout",
  Ambiguous: "ambiguous",
  RetryAllowed: "retry_allowed",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export type SerializedAssets = Readonly<Record<string, string>>;

export type Metadata = {
  readonly depositAddress: string;
  readonly depositEventId: string;
  readonly depositAssetName: string;
  readonly depositAuthUnit: string;
  readonly nonceInput: {
    readonly txHash: string;
    readonly outputIndex: number;
  };
  readonly validTo: number;
  readonly inclusionTime: number;
};

export type Row = {
  [Columns.TX_HASH]: Buffer;
  [Columns.DEPOSIT_EVENT_ID]: Buffer;
  [Columns.EXPECTED_DEPOSIT_OUT_REF]: string;
  [Columns.EXPECTED_L2_ADDRESS]: string;
  [Columns.EXPECTED_LOVELACE]: string;
  [Columns.EXPECTED_ASSETS]: SerializedAssets;
  [Columns.METADATA]: Metadata;
  [Columns.FUNDING_OUT_REFS]: readonly string[];
  [Columns.SUBMITTED_AT]: Date;
  [Columns.CONFIRMATION_STATUS]: Status;
  [Columns.CONFIRMED_AT]: Date | null;
  [Columns.LAST_RECONCILED_AT]: Date | null;
  [Columns.LAST_ERROR]: string | null;
  [Columns.UPDATED_AT]: Date;
};

export type InsertSubmittedInput = Pick<
  Row,
  | Columns.TX_HASH
  | Columns.DEPOSIT_EVENT_ID
  | Columns.EXPECTED_DEPOSIT_OUT_REF
  | Columns.EXPECTED_L2_ADDRESS
  | Columns.EXPECTED_LOVELACE
  | Columns.EXPECTED_ASSETS
  | Columns.METADATA
  | Columns.FUNDING_OUT_REFS
>;

const stableStringify = (value: unknown): string => {
  if (typeof value === "bigint") {
    return JSON.stringify(value.toString(10));
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableStringify).join(",")}]`;
  }
  if (value !== null && typeof value === "object") {
    return `{${Object.entries(value as Record<string, unknown>)
      .sort(([left], [right]) => left.localeCompare(right))
      .map(
        ([key, nested]) => `${JSON.stringify(key)}:${stableStringify(nested)}`,
      )
      .join(",")}}`;
  }
  return JSON.stringify(value);
};

const normalizeJsonb = (value: unknown): unknown => {
  if (typeof value !== "string") {
    return value;
  }
  try {
    return JSON.parse(value);
  } catch {
    return value;
  }
};

const sameSubmittedPayload = (row: Row, input: InsertSubmittedInput): boolean =>
  row[Columns.TX_HASH].equals(input[Columns.TX_HASH]) &&
  row[Columns.DEPOSIT_EVENT_ID].equals(input[Columns.DEPOSIT_EVENT_ID]) &&
  row[Columns.EXPECTED_DEPOSIT_OUT_REF] ===
    input[Columns.EXPECTED_DEPOSIT_OUT_REF] &&
  row[Columns.EXPECTED_L2_ADDRESS] === input[Columns.EXPECTED_L2_ADDRESS] &&
  row[Columns.EXPECTED_LOVELACE] === input[Columns.EXPECTED_LOVELACE] &&
  stableStringify(normalizeJsonb(row[Columns.EXPECTED_ASSETS])) ===
    stableStringify(input[Columns.EXPECTED_ASSETS]) &&
  stableStringify(normalizeJsonb(row[Columns.METADATA])) ===
    stableStringify(input[Columns.METADATA]) &&
  stableStringify(normalizeJsonb(row[Columns.FUNDING_OUT_REFS])) ===
    stableStringify(input[Columns.FUNDING_OUT_REFS]);

export const retrieveByTxHash = (
  txHash: Buffer,
): Effect.Effect<Option.Option<Row>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.TX_HASH)} = ${txHash}
      LIMIT 1`;
    return rows.length === 0 ? Option.none() : Option.some(rows[0]!);
  }).pipe(
    Effect.withLogSpan(`retrieveByTxHash ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve deposit submission attempt by tx hash",
    ),
  );

export const retrieveByEventId = (
  eventId: Buffer,
): Effect.Effect<readonly Row[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.DEPOSIT_EVENT_ID)} = ${eventId}
      ORDER BY ${sql(Columns.SUBMITTED_AT)} ASC, ${sql(Columns.TX_HASH)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveByEventId ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve deposit submission attempts by event id",
    ),
  );

export const retrieveOpenAttempts = (): Effect.Effect<
  readonly Row[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.CONFIRMATION_STATUS)} IN (
        ${Status.SubmittedConfirmationUnknown},
        ${Status.Ambiguous},
        ${Status.RetryAllowed}
      )
      ORDER BY ${sql(Columns.SUBMITTED_AT)} ASC, ${sql(Columns.TX_HASH)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveOpenAttempts ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve open deposit submission attempts",
    ),
  );

export const insertSubmitted = (
  input: InsertSubmittedInput,
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const existing = yield* retrieveByTxHash(input[Columns.TX_HASH]);
    if (Option.isSome(existing)) {
      if (sameSubmittedPayload(existing.value, input)) {
        return existing.value;
      }
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to overwrite deposit submission attempt because the existing tx hash has different expected payload",
          cause: `tx_hash=${input[Columns.TX_HASH].toString("hex")}`,
        }),
      );
    }

    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`
      INSERT INTO ${sql(tableName)} (
        ${sql(Columns.TX_HASH)},
        ${sql(Columns.DEPOSIT_EVENT_ID)},
        ${sql(Columns.EXPECTED_DEPOSIT_OUT_REF)},
        ${sql(Columns.EXPECTED_L2_ADDRESS)},
        ${sql(Columns.EXPECTED_LOVELACE)},
        ${sql(Columns.EXPECTED_ASSETS)},
        ${sql(Columns.METADATA)},
        ${sql(Columns.FUNDING_OUT_REFS)},
        ${sql(Columns.CONFIRMATION_STATUS)}
      ) VALUES (
        ${input[Columns.TX_HASH]},
        ${input[Columns.DEPOSIT_EVENT_ID]},
        ${input[Columns.EXPECTED_DEPOSIT_OUT_REF]},
        ${input[Columns.EXPECTED_L2_ADDRESS]},
        ${input[Columns.EXPECTED_LOVELACE]},
        CAST(${stableStringify(input[Columns.EXPECTED_ASSETS])} AS JSONB),
        CAST(${stableStringify(input[Columns.METADATA])} AS JSONB),
        CAST(${stableStringify(input[Columns.FUNDING_OUT_REFS])} AS JSONB),
        ${Status.SubmittedConfirmationUnknown}
      )
      RETURNING *`;
    return rows[0]!;
  }).pipe(
    Effect.withLogSpan(`insertSubmitted ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to insert deposit submission attempt",
    ),
  );

const markStatus = ({
  txHash,
  status,
  confirmedAt,
  lastReconciledAt,
  lastError,
}: {
  readonly txHash: Buffer;
  readonly status: Status;
  readonly confirmedAt?: Date | null;
  readonly lastReconciledAt?: Date | null;
  readonly lastError?: string | null;
}): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`
      UPDATE ${sql(tableName)}
      SET
        ${sql(Columns.CONFIRMATION_STATUS)} = ${status},
        ${sql(Columns.CONFIRMED_AT)} = ${
          confirmedAt === undefined ? null : confirmedAt
        },
        ${sql(Columns.LAST_RECONCILED_AT)} = ${
          lastReconciledAt === undefined ? null : lastReconciledAt
        },
        ${sql(Columns.LAST_ERROR)} = ${
          lastError === undefined ? null : lastError
        },
        ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.TX_HASH)} = ${txHash}
      RETURNING *`;
    if (rows.length === 1) {
      return rows[0]!;
    }
    return yield* Effect.fail(
      new DatabaseError({
        table: tableName,
        message: "Deposit submission attempt does not exist",
        cause: `tx_hash=${txHash.toString("hex")}`,
      }),
    );
  }).pipe(
    Effect.withLogSpan(`markStatus ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to update deposit submission attempt status",
    ),
  );

export const markConfirmed = (
  txHash: Buffer,
  confirmedAt: Date = new Date(),
): Effect.Effect<Row, DatabaseError, Database> =>
  markStatus({
    txHash,
    status: Status.Confirmed,
    confirmedAt,
    lastReconciledAt: null,
    lastError: null,
  });

export const markReconciled = (
  txHash: Buffer,
  reconciledAt: Date = new Date(),
): Effect.Effect<Row, DatabaseError, Database> =>
  markStatus({
    txHash,
    status: Status.ReconciledAfterTimeout,
    confirmedAt: reconciledAt,
    lastReconciledAt: reconciledAt,
    lastError: null,
  });

export const markAmbiguous = (
  txHash: Buffer,
  error: string,
  reconciledAt: Date = new Date(),
): Effect.Effect<Row, DatabaseError, Database> =>
  markStatus({
    txHash,
    status: Status.Ambiguous,
    confirmedAt: null,
    lastReconciledAt: reconciledAt,
    lastError: error,
  });

export const clear: Effect.Effect<void, DatabaseError, Database> =
  clearTable(tableName);
