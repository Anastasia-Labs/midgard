import { computeMidgardNativeTxFullHashFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { Data, Effect, Metric } from "effect";

import { sha256 } from "../sha256.js";
import { DatabaseError } from "./utils/common.js";
import type { Entry as LedgerEntry } from "./utils/ledger.js";

export const tableName = "tx_admissions";

export const payloadTableName = "tx_admission_payloads";

export const txAdmissionAcceptedMempoolDurationTimer = Metric.timer(
  "tx_admission_mark_accepted_mempool_duration",
  "Duration of MempoolDB persistence inside TxAdmissionsDB.markAccepted",
);

export const txAdmissionAcceptedTerminalDurationTimer = Metric.timer(
  "tx_admission_mark_accepted_terminal_duration",
  "Duration of accepted tx_admissions terminal status updates",
);

export const txAdmissionAcceptedTotalDurationTimer = Metric.timer(
  "tx_admission_mark_accepted_total_duration",
  "Total duration of TxAdmissionsDB.markAccepted",
);

// @effect/sql-pg classifies arrays of Buffer values as text[] parameters.
// Encode each element using PostgreSQL's canonical bytea text form so the
// explicit bytea[] cast remains binary-safe while retaining one array bind.
export const postgresByteaArray = (
  values: readonly Buffer[],
): readonly string[] => values.map((value) => `\\x${value.toString("hex")}`);

export type AcceptedPersistenceCounts = {
  readonly accepted_count: string | number | bigint;
  readonly spent_count: string | number | bigint;
  readonly consumed_deposit_count: string | number | bigint;
  readonly updated_deposit_count: string | number | bigint;
};

export const postgresLedgerTimestampArray = (
  values: readonly LedgerEntry[],
): readonly (string | null)[] =>
  values.map((value) =>
    "time_stamp_tz" in value ? value.time_stamp_tz.toISOString() : null,
  );

export const txAdmissionMarkRejectedDurationTimer = Metric.timer(
  "tx_admission_mark_rejected_duration",
  "Total duration of TxAdmissionsDB.markRejected",
);

export const admissionDuplicatePathCounter = Metric.counter(
  "admission_duplicate_path_total",
  {
    description: "Number of durable admission duplicate-path responses",
    bigint: true,
    incremental: true,
  },
);

export const admissionBacklogRejectCounter = Metric.counter(
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
  TX_FULL_HASH_V1 = "tx_full_hash_v1",
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

export type RawEntry = {
  readonly [Columns.TX_ID]: Buffer;
  readonly [Columns.TX_CANONICAL_CBOR]: Buffer;
  readonly [Columns.TX_FULL_HASH_V1]: Buffer;
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

export type RawClaimedPayloadEntry = RawClaimedEntry &
  Pick<
    RawEntry,
    Columns.TX_FULL_HASH_V1 | Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256
  >;

/**
 * The ordered, durable half of a claim.  It intentionally omits the payload:
 * callers that serialize claim order can release their short claim lock before
 * loading CBOR blobs, while still proving the payload belongs to this exact
 * validation lease before dispatching it to a worker.
 */
export type RawClaimedLeaseEntry = Omit<
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
  readonly maxBacklogBytes?: number;
};

export type ReservedAdmissionOutcome =
  | { readonly _tag: "Success"; readonly result: AdmitResult }
  | {
      readonly _tag: "Conflict";
      readonly error: TxAdmissionConflictError | TxAdmissionBacklogFullError;
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
  readonly unit?: "rows" | "bytes";
  readonly message: string;
}> {}

export const toBigInt = (value: bigint | number | string): bigint =>
  typeof value === "bigint" ? value : BigInt(value);

export const normalizeRow = (row: RawEntry): Entry => ({
  ...row,
  [Columns.ARRIVAL_SEQ]: toBigInt(row[Columns.ARRIVAL_SEQ]),
  [Columns.REQUEST_COUNT]: toBigInt(row[Columns.REQUEST_COUNT]),
});

const normalizeClaimedEntry = (row: RawClaimedEntry): ClaimedEntry => ({
  [Columns.TX_ID]: row[Columns.TX_ID],
  [Columns.TX_CANONICAL_CBOR]: row[Columns.TX_CANONICAL_CBOR],
  [Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
    row[Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR],
  [Columns.ARRIVAL_SEQ]: toBigInt(row[Columns.ARRIVAL_SEQ]),
  [Columns.FIRST_SEEN_AT]: row[Columns.FIRST_SEEN_AT],
  [Columns.VALIDATION_STARTED_AT]: row[Columns.VALIDATION_STARTED_AT],
});

export const normalizeClaimedLeaseEntry = (
  row: RawClaimedLeaseEntry,
): ClaimedLeaseEntry => ({
  ...row,
  [Columns.ARRIVAL_SEQ]: toBigInt(row[Columns.ARRIVAL_SEQ]),
});

export const verifyClaimedPayloadRows = (
  rows: readonly RawClaimedPayloadEntry[],
): Effect.Effect<readonly ClaimedEntry[], DatabaseError> =>
  Effect.gen(function* () {
    for (const row of rows) {
      const txIdHex = row[Columns.TX_ID].toString("hex");
      const expectedFullHash = computeMidgardNativeTxFullHashFromCanonicalCbor(
        row[Columns.TX_CANONICAL_CBOR],
      );
      if (!expectedFullHash.equals(row[Columns.TX_FULL_HASH_V1])) {
        return yield* Effect.fail(
          new DatabaseError({
            table: payloadTableName,
            message:
              "Admission payload canonical V1 full-transaction commitment does not match its persisted bytes",
            cause: `tx_id=${txIdHex}`,
          }),
        );
      }
      const expectedSidecarHash = sha256(
        row[Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR],
      );
      if (
        !expectedSidecarHash.equals(
          row[Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256],
        )
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: payloadTableName,
            message:
              "Admission payload CEK program-material sidecar commitment does not match its persisted bytes",
            cause: `tx_id=${txIdHex}`,
          }),
        );
      }
    }
    return rows.map(normalizeClaimedEntry);
  });
