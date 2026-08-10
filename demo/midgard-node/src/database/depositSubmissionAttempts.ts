import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

export const tableName = "deposit_submission_attempts";

export enum Columns {
  TX_HASH = "tx_hash",
  DEPOSIT_EVENT_ID = "deposit_event_id",
  SIGNED_TX_CBOR = "signed_tx_cbor",
  EXPECTED_DEPOSIT_OUT_REF = "expected_deposit_out_ref",
  EXPECTED_L2_ADDRESS = "expected_l2_address",
  EXPECTED_LOVELACE = "expected_lovelace",
  EXPECTED_ASSETS = "expected_assets",
  METADATA = "metadata",
  DEPENDENCY_OUT_REFS = "dependency_out_refs",
  STATUS = "status",
  PREPARED_AT = "prepared_at",
  ATTEMPT_COUNT = "attempt_count",
  LAST_SUBMISSION_AT = "last_submission_at",
  SUBMITTED_AT = "submitted_at",
  PROVIDER_ACKNOWLEDGEMENT = "provider_acknowledgement",
  CONFIRMED_AT = "confirmed_at",
  LAST_RECONCILED_AT = "last_reconciled_at",
  LAST_ERROR = "last_error",
  UPDATED_AT = "updated_at",
}

export const Status = {
  Prepared: "prepared",
  SubmissionUnknown: "submission_unknown",
  Submitted: "submitted",
  Confirmed: "confirmed",
  ReconciledAfterTimeout: "reconciled_after_timeout",
  Ambiguous: "ambiguous",
  Expired: "expired",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export type SerializedAssets = Readonly<Record<string, string>>;

export type DependencyOutRefs = {
  readonly spend: readonly string[];
  readonly collateral: readonly string[];
  readonly reference: readonly string[];
};

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
  [Columns.SIGNED_TX_CBOR]: Buffer;
  [Columns.EXPECTED_DEPOSIT_OUT_REF]: string;
  [Columns.EXPECTED_L2_ADDRESS]: string;
  [Columns.EXPECTED_LOVELACE]: string;
  [Columns.EXPECTED_ASSETS]: SerializedAssets;
  [Columns.METADATA]: Metadata;
  [Columns.DEPENDENCY_OUT_REFS]: DependencyOutRefs;
  [Columns.STATUS]: Status;
  [Columns.PREPARED_AT]: Date;
  [Columns.ATTEMPT_COUNT]: number;
  [Columns.LAST_SUBMISSION_AT]: Date | null;
  [Columns.SUBMITTED_AT]: Date | null;
  [Columns.PROVIDER_ACKNOWLEDGEMENT]: string | null;
  [Columns.CONFIRMED_AT]: Date | null;
  [Columns.LAST_RECONCILED_AT]: Date | null;
  [Columns.LAST_ERROR]: string | null;
  [Columns.UPDATED_AT]: Date;
};

export type InsertPreparedInput = Pick<
  Row,
  | Columns.TX_HASH
  | Columns.DEPOSIT_EVENT_ID
  | Columns.SIGNED_TX_CBOR
  | Columns.EXPECTED_DEPOSIT_OUT_REF
  | Columns.EXPECTED_L2_ADDRESS
  | Columns.EXPECTED_LOVELACE
  | Columns.EXPECTED_ASSETS
  | Columns.METADATA
  | Columns.DEPENDENCY_OUT_REFS
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

const samePreparedPayload = (row: Row, input: InsertPreparedInput): boolean =>
  row[Columns.TX_HASH].equals(input[Columns.TX_HASH]) &&
  row[Columns.DEPOSIT_EVENT_ID].equals(input[Columns.DEPOSIT_EVENT_ID]) &&
  row[Columns.SIGNED_TX_CBOR].equals(input[Columns.SIGNED_TX_CBOR]) &&
  row[Columns.EXPECTED_DEPOSIT_OUT_REF] ===
    input[Columns.EXPECTED_DEPOSIT_OUT_REF] &&
  row[Columns.EXPECTED_L2_ADDRESS] === input[Columns.EXPECTED_L2_ADDRESS] &&
  row[Columns.EXPECTED_LOVELACE] === input[Columns.EXPECTED_LOVELACE] &&
  stableStringify(normalizeJsonb(row[Columns.EXPECTED_ASSETS])) ===
    stableStringify(input[Columns.EXPECTED_ASSETS]) &&
  stableStringify(normalizeJsonb(row[Columns.METADATA])) ===
    stableStringify(input[Columns.METADATA]) &&
  stableStringify(normalizeJsonb(row[Columns.DEPENDENCY_OUT_REFS])) ===
    stableStringify(input[Columns.DEPENDENCY_OUT_REFS]);

const transitionError = (txHash: Buffer, transition: string): DatabaseError =>
  new DatabaseError({
    table: tableName,
    message: `Deposit submission attempt cannot transition via ${transition}`,
    cause: `tx_hash=${txHash.toString("hex")}`,
  });

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
      ORDER BY ${sql(Columns.PREPARED_AT)} ASC, ${sql(Columns.TX_HASH)} ASC`;
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
      WHERE ${sql(Columns.STATUS)} IN (
        ${Status.Prepared},
        ${Status.SubmissionUnknown},
        ${Status.Submitted},
        ${Status.Ambiguous}
      )
      ORDER BY ${sql(Columns.PREPARED_AT)} ASC, ${sql(Columns.TX_HASH)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveOpenAttempts ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve open deposit submission attempts",
    ),
  );

export const insertPrepared = (
  input: InsertPreparedInput,
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`
      INSERT INTO ${sql(tableName)} (
        ${sql(Columns.TX_HASH)},
        ${sql(Columns.DEPOSIT_EVENT_ID)},
        ${sql(Columns.SIGNED_TX_CBOR)},
        ${sql(Columns.EXPECTED_DEPOSIT_OUT_REF)},
        ${sql(Columns.EXPECTED_L2_ADDRESS)},
        ${sql(Columns.EXPECTED_LOVELACE)},
        ${sql(Columns.EXPECTED_ASSETS)},
        ${sql(Columns.METADATA)},
        ${sql(Columns.DEPENDENCY_OUT_REFS)},
        ${sql(Columns.STATUS)}
      ) VALUES (
        ${input[Columns.TX_HASH]},
        ${input[Columns.DEPOSIT_EVENT_ID]},
        ${input[Columns.SIGNED_TX_CBOR]},
        ${input[Columns.EXPECTED_DEPOSIT_OUT_REF]},
        ${input[Columns.EXPECTED_L2_ADDRESS]},
        ${input[Columns.EXPECTED_LOVELACE]},
        CAST(CAST(${stableStringify(input[Columns.EXPECTED_ASSETS])} AS TEXT) AS JSONB),
        CAST(CAST(${stableStringify(input[Columns.METADATA])} AS TEXT) AS JSONB),
        CAST(CAST(${stableStringify(input[Columns.DEPENDENCY_OUT_REFS])} AS TEXT) AS JSONB),
        ${Status.Prepared}
      )
      ON CONFLICT (${sql(Columns.TX_HASH)}) DO NOTHING
      RETURNING *`;
    if (rows.length === 1) {
      return rows[0]!;
    }

    const existing = yield* retrieveByTxHash(input[Columns.TX_HASH]);
    if (Option.isSome(existing) && samePreparedPayload(existing.value, input)) {
      return existing.value;
    }
    return yield* Effect.fail(
      new DatabaseError({
        table: tableName,
        message:
          "Refusing to overwrite deposit submission attempt because the existing tx hash has different signed bytes or expected payload",
        cause: `tx_hash=${input[Columns.TX_HASH].toString("hex")}`,
      }),
    );
  }).pipe(
    Effect.withLogSpan(`insertPrepared ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to prepare deposit submission attempt",
    ),
  );

export const beginSubmission = (
  txHash: Buffer,
  submissionAt: Date = new Date(),
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`
      UPDATE ${sql(tableName)}
      SET
        ${sql(Columns.STATUS)} = ${Status.SubmissionUnknown},
        ${sql(Columns.ATTEMPT_COUNT)} = ${sql(Columns.ATTEMPT_COUNT)} + 1,
        ${sql(Columns.LAST_SUBMISSION_AT)} = ${submissionAt},
        ${sql(Columns.SUBMITTED_AT)} = NULL,
        ${sql(Columns.PROVIDER_ACKNOWLEDGEMENT)} = NULL,
        ${sql(Columns.UPDATED_AT)} = ${submissionAt}
      WHERE ${sql(Columns.TX_HASH)} = ${txHash}
        AND ${sql(Columns.STATUS)} = ${Status.Prepared}
      RETURNING *`;
    if (rows.length === 1) {
      return rows[0]!;
    }
    return yield* Effect.fail(transitionError(txHash, "beginSubmission"));
  }).pipe(
    Effect.withLogSpan(`beginSubmission ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to begin deposit transaction submission",
    ),
  );

export const markSubmitted = (
  txHash: Buffer,
  providerAcknowledgement: string,
  submittedAt: Date = new Date(),
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`
      UPDATE ${sql(tableName)}
      SET
        ${sql(Columns.STATUS)} = ${Status.Submitted},
        ${sql(Columns.SUBMITTED_AT)} = ${submittedAt},
        ${sql(Columns.PROVIDER_ACKNOWLEDGEMENT)} = ${providerAcknowledgement},
        ${sql(Columns.LAST_ERROR)} = NULL,
        ${sql(Columns.UPDATED_AT)} = ${submittedAt}
      WHERE ${sql(Columns.TX_HASH)} = ${txHash}
        AND ${sql(Columns.STATUS)} = ${Status.SubmissionUnknown}
      RETURNING *`;
    if (rows.length === 1) {
      return rows[0]!;
    }
    const existing = yield* retrieveByTxHash(txHash);
    if (
      Option.isSome(existing) &&
      (existing.value[Columns.STATUS] === Status.Submitted ||
        existing.value[Columns.STATUS] === Status.Confirmed ||
        existing.value[Columns.STATUS] === Status.ReconciledAfterTimeout)
    ) {
      return existing.value;
    }
    return yield* Effect.fail(transitionError(txHash, "markSubmitted"));
  }).pipe(
    Effect.withLogSpan(`markSubmitted ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to record provider acknowledgement for deposit submission",
    ),
  );

const evidenceStatuses = [
  Status.Prepared,
  Status.SubmissionUnknown,
  Status.Submitted,
  Status.Ambiguous,
] as const;

export const markConfirmed = (
  txHash: Buffer,
  confirmedAt: Date = new Date(),
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`
      UPDATE ${sql(tableName)}
      SET
        ${sql(Columns.STATUS)} = ${Status.Confirmed},
        ${sql(Columns.CONFIRMED_AT)} = ${confirmedAt},
        ${sql(Columns.LAST_ERROR)} = NULL,
        ${sql(Columns.UPDATED_AT)} = ${confirmedAt}
      WHERE ${sql(Columns.TX_HASH)} = ${txHash}
        AND ${sql(Columns.STATUS)} IN ${sql.in(evidenceStatuses)}
      RETURNING *`;
    if (rows.length === 1) {
      return rows[0]!;
    }
    const existing = yield* retrieveByTxHash(txHash);
    if (
      Option.isSome(existing) &&
      (existing.value[Columns.STATUS] === Status.Confirmed ||
        existing.value[Columns.STATUS] === Status.ReconciledAfterTimeout)
    ) {
      return existing.value;
    }
    return yield* Effect.fail(transitionError(txHash, "markConfirmed"));
  }).pipe(
    Effect.withLogSpan(`markConfirmed ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to confirm deposit submission attempt",
    ),
  );

export const markReconciled = (
  txHash: Buffer,
  reconciledAt: Date = new Date(),
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`
      UPDATE ${sql(tableName)}
      SET
        ${sql(Columns.STATUS)} = ${Status.ReconciledAfterTimeout},
        ${sql(Columns.CONFIRMED_AT)} = ${reconciledAt},
        ${sql(Columns.LAST_RECONCILED_AT)} = ${reconciledAt},
        ${sql(Columns.LAST_ERROR)} = NULL,
        ${sql(Columns.UPDATED_AT)} = ${reconciledAt}
      WHERE ${sql(Columns.TX_HASH)} = ${txHash}
        AND ${sql(Columns.STATUS)} IN ${sql.in(evidenceStatuses)}
      RETURNING *`;
    if (rows.length === 1) {
      return rows[0]!;
    }
    const existing = yield* retrieveByTxHash(txHash);
    if (
      Option.isSome(existing) &&
      (existing.value[Columns.STATUS] === Status.ReconciledAfterTimeout ||
        existing.value[Columns.STATUS] === Status.Confirmed)
    ) {
      return existing.value;
    }
    return yield* Effect.fail(transitionError(txHash, "markReconciled"));
  }).pipe(
    Effect.withLogSpan(`markReconciled ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to reconcile deposit submission attempt",
    ),
  );

export const markAmbiguous = (
  txHash: Buffer,
  error: string,
  reconciledAt: Date = new Date(),
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`
      UPDATE ${sql(tableName)}
      SET
        ${sql(Columns.STATUS)} = ${Status.Ambiguous},
        ${sql(Columns.CONFIRMED_AT)} = NULL,
        ${sql(Columns.LAST_RECONCILED_AT)} = ${reconciledAt},
        ${sql(Columns.LAST_ERROR)} = ${error},
        ${sql(Columns.UPDATED_AT)} = ${reconciledAt}
      WHERE ${sql(Columns.TX_HASH)} = ${txHash}
        AND ${sql(Columns.STATUS)} = ${Status.SubmissionUnknown}
      RETURNING *`;
    if (rows.length === 1) {
      return rows[0]!;
    }
    const existing = yield* retrieveByTxHash(txHash);
    if (
      Option.isSome(existing) &&
      (existing.value[Columns.STATUS] === Status.Ambiguous ||
        existing.value[Columns.STATUS] === Status.Submitted ||
        existing.value[Columns.STATUS] === Status.Confirmed ||
        existing.value[Columns.STATUS] === Status.ReconciledAfterTimeout)
    ) {
      return existing.value;
    }
    return yield* Effect.fail(transitionError(txHash, "markAmbiguous"));
  }).pipe(
    Effect.withLogSpan(`markAmbiguous ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark deposit submission attempt ambiguous",
    ),
  );

export const markExpired = (
  txHash: Buffer,
  error: string,
  reconciledAt: Date = new Date(),
): Effect.Effect<Row, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`
      UPDATE ${sql(tableName)}
      SET
        ${sql(Columns.STATUS)} = ${Status.Expired},
        ${sql(Columns.CONFIRMED_AT)} = NULL,
        ${sql(Columns.LAST_RECONCILED_AT)} = ${reconciledAt},
        ${sql(Columns.LAST_ERROR)} = ${error},
        ${sql(Columns.UPDATED_AT)} = ${reconciledAt}
      WHERE ${sql(Columns.TX_HASH)} = ${txHash}
        AND ${sql(Columns.STATUS)} = ${Status.Prepared}
      RETURNING *`;
    if (rows.length === 1) {
      return rows[0]!;
    }
    const existing = yield* retrieveByTxHash(txHash);
    if (
      Option.isSome(existing) &&
      existing.value[Columns.STATUS] === Status.Expired
    ) {
      return existing.value;
    }
    return yield* Effect.fail(transitionError(txHash, "markExpired"));
  }).pipe(
    Effect.withLogSpan(`markExpired ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark deposit submission attempt expired",
    ),
  );

export const clear: Effect.Effect<void, DatabaseError, Database> =
  clearTable(tableName);
