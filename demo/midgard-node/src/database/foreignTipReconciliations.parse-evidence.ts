import { createHash } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import {
  type DeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { exactRecord } from "./utils/exact-record.js";

export const tableName = "foreign_tip_reconciliations";

export const FOREIGN_TIP_RECONCILIATION_VERSION = 1 as const;

export const EvidenceKind = {
  Pending: "pending_v1",
  VerifiedEmpty: "verified_empty_v1",
  VerifiedDa: "verified_da_v1",
} as const;

export type EvidenceKind = (typeof EvidenceKind)[keyof typeof EvidenceKind];

export const Status = {
  Awaiting: "awaiting",
  Resolved: "resolved",
} as const;

export enum Columns {
  FOREIGN_HEADER_HASH = "foreign_header_hash",
  FORMAT_VERSION = "format_version",
  DEPLOYMENT_MARKER_SCHEMA_VERSION = "deployment_marker_schema_version",
  DEPLOYMENT_MANIFEST_ID = "deployment_manifest_id",
  CONSENSUS_PROFILE_ID = "consensus_profile_id",
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
  VERIFIED_DA_PAYLOAD_SHA256 = "verified_da_payload_sha256",
  EVIDENCE_KIND = "evidence_kind",
  STATUS = "status",
  BLOCKING_REASON = "blocking_reason",
  CREATED_AT = "created_at",
  UPDATED_AT = "updated_at",
  RESOLVED_AT = "resolved_at",
}

export type Entry = {
  [Columns.FOREIGN_HEADER_HASH]: Buffer;
  [Columns.FORMAT_VERSION]: typeof FOREIGN_TIP_RECONCILIATION_VERSION;
  [Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION]: typeof MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION;
  [Columns.DEPLOYMENT_MANIFEST_ID]: string;
  [Columns.CONSENSUS_PROFILE_ID]: typeof MIDGARD_CONSENSUS_PROFILE_ID;
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
  [Columns.VERIFIED_DA_PAYLOAD_SHA256]: Buffer | null;
  [Columns.EVIDENCE_KIND]: EvidenceKind;
  [Columns.STATUS]: (typeof Status)[keyof typeof Status];
  [Columns.BLOCKING_REASON]: string | null;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
  [Columns.RESOLVED_AT]: Date | null;
};

type PgBigInt = bigint | number | string;

export type RawEntry = Omit<
  Entry,
  | Columns.DEPOSIT_COUNT
  | Columns.FORCED_TRANSACTION_COUNT
  | Columns.WITHDRAWAL_COUNT
> & {
  [Columns.DEPOSIT_COUNT]: PgBigInt;
  [Columns.FORCED_TRANSACTION_COUNT]: PgBigInt;
  [Columns.WITHDRAWAL_COUNT]: PgBigInt;
};

export type ForeignTipReconciliation = {
  readonly version: typeof FOREIGN_TIP_RECONCILIATION_VERSION;
  readonly deploymentMarker: DeploymentMarker;
  readonly consensusProfileId: typeof MIDGARD_CONSENSUS_PROFILE_ID;
  readonly foreignHeaderHash: Buffer;
  readonly replacedBaseHeaderHash: Buffer;
  readonly foreignHeaderCbor: Buffer;
  readonly blockStartTime: Date;
  readonly blockEndTime: Date;
  readonly commitments: {
    readonly depositsRoot: string;
    readonly forcedTransactionsRoot: string;
    readonly withdrawalsRoot: string;
    readonly depositCount: bigint;
    readonly forcedTransactionCount: bigint;
    readonly withdrawalCount: bigint;
  };
  readonly evidence:
    | { readonly kind: typeof EvidenceKind.Pending }
    | { readonly kind: typeof EvidenceKind.VerifiedEmpty }
    | {
        readonly kind: typeof EvidenceKind.VerifiedDa;
        readonly schemaVersion: 1;
        readonly payloadCbor: Buffer;
        readonly payloadSha256: Buffer;
      };
  readonly resolution:
    | { readonly kind: typeof Status.Awaiting; readonly reason: string }
    | { readonly kind: typeof Status.Resolved };
};

export type ForeignTipDaIdentity = {
  readonly headerHash: Buffer;
  readonly schemaVersion: 1;
  readonly consensusProfileId: typeof MIDGARD_CONSENSUS_PROFILE_ID;
  readonly payloadCbor: Buffer;
  readonly payloadSha256: Buffer;
};

export type ResolvedForeignTipEvidence = Exclude<
  ForeignTipReconciliation["evidence"],
  { readonly kind: typeof EvidenceKind.Pending }
>;

export type ResolveForeignTipEvidence =
  | { readonly kind: typeof EvidenceKind.VerifiedEmpty }
  | {
      readonly kind: typeof EvidenceKind.VerifiedDa;
      readonly daIdentity: ForeignTipDaIdentity;
    };

export const exactBytes = (
  value: unknown,
  label: string,
  bytes?: number,
): Buffer => {
  if (
    !(value instanceof Uint8Array) ||
    (bytes === undefined ? value.length === 0 : value.length !== bytes)
  ) {
    throw new Error(
      bytes === undefined
        ? `${label} must be non-empty bytes`
        : `${label} must contain exactly ${bytes.toString()} bytes`,
    );
  }
  return Buffer.from(value);
};

export const exactRoot = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${label} must be exactly 32 lowercase hex bytes`);
  }
  return value;
};

export const exactDate = (value: unknown, label: string): Date => {
  if (!(value instanceof Date) || !Number.isFinite(value.getTime())) {
    throw new Error(`${label} must be a valid Date`);
  }
  return new Date(value.getTime());
};

export const exactCount = (value: unknown, label: string): bigint => {
  if (typeof value !== "bigint" || value < 0n) {
    throw new Error(`${label} must be a non-negative bigint`);
  }
  return value;
};

export const parseEvidence = (
  value: unknown,
): ForeignTipReconciliation["evidence"] => {
  const kind =
    typeof value === "object" && value !== null && "kind" in value
      ? value.kind
      : undefined;
  if (kind === EvidenceKind.Pending || kind === EvidenceKind.VerifiedEmpty) {
    exactRecord(value, ["kind"], "ForeignTipReconciliationV1 evidence");
    return { kind };
  }
  if (kind !== EvidenceKind.VerifiedDa) {
    throw new Error(
      "ForeignTipReconciliationV1 evidence kind must be an exact V1 discriminator",
    );
  }
  const candidate = exactRecord(
    value,
    ["kind", "schemaVersion", "payloadCbor", "payloadSha256"],
    "ForeignTipReconciliationV1 verified DA evidence",
  );
  if (candidate.schemaVersion !== 1) {
    throw new Error(
      "ForeignTipReconciliationV1 verified DA schemaVersion must equal 1",
    );
  }
  const payloadCbor = exactBytes(
    candidate.payloadCbor,
    "ForeignTipReconciliationV1 verified DA payloadCbor",
  );
  const payloadSha256 = exactBytes(
    candidate.payloadSha256,
    "ForeignTipReconciliationV1 verified DA payloadSha256",
    32,
  );
  if (
    !createHash("sha256").update(payloadCbor).digest().equals(payloadSha256)
  ) {
    throw new Error(
      "ForeignTipReconciliationV1 verified DA payload digest does not match",
    );
  }
  return {
    kind: EvidenceKind.VerifiedDa,
    schemaVersion: 1,
    payloadCbor,
    payloadSha256,
  };
};
