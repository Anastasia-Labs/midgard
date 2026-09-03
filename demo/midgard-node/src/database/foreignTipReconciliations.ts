import { createHash } from "node:crypto";

import {
  MIDGARD_CONSENSUS_PROFILE_ID,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  assertDeploymentMarkerMatches,
  type DeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import { sha256 } from "../sha256.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";
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

type RawEntry = Omit<
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

const exactBytes = (value: unknown, label: string, bytes?: number): Buffer => {
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

const exactRoot = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${label} must be exactly 32 lowercase hex bytes`);
  }
  return value;
};

const exactDate = (value: unknown, label: string): Date => {
  if (!(value instanceof Date) || !Number.isFinite(value.getTime())) {
    throw new Error(`${label} must be a valid Date`);
  }
  return new Date(value.getTime());
};

const exactCount = (value: unknown, label: string): bigint => {
  if (typeof value !== "bigint" || value < 0n) {
    throw new Error(`${label} must be a non-negative bigint`);
  }
  return value;
};

const parseEvidence = (
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

export const parseForeignTipReconciliation = (
  value: unknown,
): ForeignTipReconciliation => {
  const candidate = exactRecord(
    value,
    [
      "version",
      "deploymentMarker",
      "consensusProfileId",
      "foreignHeaderHash",
      "replacedBaseHeaderHash",
      "foreignHeaderCbor",
      "blockStartTime",
      "blockEndTime",
      "commitments",
      "evidence",
      "resolution",
    ],
    "ForeignTipReconciliationV1",
  );
  if (candidate.version !== FOREIGN_TIP_RECONCILIATION_VERSION) {
    throw new Error(
      `ForeignTipReconciliationV1 version must equal ${FOREIGN_TIP_RECONCILIATION_VERSION.toString()}`,
    );
  }
  if (candidate.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_ID) {
    throw new Error(
      `ForeignTipReconciliationV1 consensusProfileId must equal ${MIDGARD_CONSENSUS_PROFILE_ID}`,
    );
  }
  const commitments = exactRecord(
    candidate.commitments,
    [
      "depositsRoot",
      "forcedTransactionsRoot",
      "withdrawalsRoot",
      "depositCount",
      "forcedTransactionCount",
      "withdrawalCount",
    ],
    "ForeignTipReconciliationV1 commitments",
  );
  const resolutionKind =
    typeof candidate.resolution === "object" &&
    candidate.resolution !== null &&
    "kind" in candidate.resolution
      ? candidate.resolution.kind
      : undefined;
  const resolutionCandidate = exactRecord(
    candidate.resolution,
    resolutionKind === Status.Awaiting ? ["kind", "reason"] : ["kind"],
    "ForeignTipReconciliationV1 resolution",
  );
  const resolution: ForeignTipReconciliation["resolution"] =
    resolutionCandidate.kind === Status.Awaiting
      ? {
          kind: Status.Awaiting,
          reason:
            typeof resolutionCandidate.reason === "string" &&
            resolutionCandidate.reason.length > 0
              ? resolutionCandidate.reason
              : (() => {
                  throw new Error(
                    "ForeignTipReconciliationV1 awaiting reason must be non-empty",
                  );
                })(),
        }
      : resolutionCandidate.kind === Status.Resolved
        ? { kind: Status.Resolved }
        : (() => {
            throw new Error(
              "ForeignTipReconciliationV1 resolution kind is unsupported",
            );
          })();
  const evidence = parseEvidence(candidate.evidence);
  const parsedCommitments = {
    depositsRoot: exactRoot(
      commitments.depositsRoot,
      "ForeignTipReconciliationV1 commitments.depositsRoot",
    ),
    forcedTransactionsRoot: exactRoot(
      commitments.forcedTransactionsRoot,
      "ForeignTipReconciliationV1 commitments.forcedTransactionsRoot",
    ),
    withdrawalsRoot: exactRoot(
      commitments.withdrawalsRoot,
      "ForeignTipReconciliationV1 commitments.withdrawalsRoot",
    ),
    depositCount: exactCount(
      commitments.depositCount,
      "ForeignTipReconciliationV1 commitments.depositCount",
    ),
    forcedTransactionCount: exactCount(
      commitments.forcedTransactionCount,
      "ForeignTipReconciliationV1 commitments.forcedTransactionCount",
    ),
    withdrawalCount: exactCount(
      commitments.withdrawalCount,
      "ForeignTipReconciliationV1 commitments.withdrawalCount",
    ),
  };
  const emptyEvidence =
    parsedCommitments.depositsRoot === SDK.EMPTY_MERKLE_TREE_ROOT &&
    parsedCommitments.forcedTransactionsRoot === SDK.EMPTY_MERKLE_TREE_ROOT &&
    parsedCommitments.withdrawalsRoot === SDK.EMPTY_MERKLE_TREE_ROOT &&
    parsedCommitments.depositCount === 0n &&
    parsedCommitments.forcedTransactionCount === 0n &&
    parsedCommitments.withdrawalCount === 0n;
  if (
    (evidence.kind === EvidenceKind.VerifiedEmpty && !emptyEvidence) ||
    (evidence.kind === EvidenceKind.VerifiedDa && emptyEvidence) ||
    (resolution.kind === Status.Resolved &&
      evidence.kind === EvidenceKind.Pending)
  ) {
    throw new Error(
      "ForeignTipReconciliationV1 evidence does not match its commitments or resolution",
    );
  }
  const blockStartTime = exactDate(
    candidate.blockStartTime,
    "ForeignTipReconciliationV1 blockStartTime",
  );
  const blockEndTime = exactDate(
    candidate.blockEndTime,
    "ForeignTipReconciliationV1 blockEndTime",
  );
  if (blockEndTime.getTime() <= blockStartTime.getTime()) {
    throw new Error(
      "ForeignTipReconciliationV1 block window must be increasing",
    );
  }
  return {
    version: FOREIGN_TIP_RECONCILIATION_VERSION,
    deploymentMarker: parseDeploymentMarker(candidate.deploymentMarker),
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    foreignHeaderHash: exactBytes(
      candidate.foreignHeaderHash,
      "ForeignTipReconciliationV1 foreignHeaderHash",
      28,
    ),
    replacedBaseHeaderHash: exactBytes(
      candidate.replacedBaseHeaderHash,
      "ForeignTipReconciliationV1 replacedBaseHeaderHash",
      28,
    ),
    foreignHeaderCbor: exactBytes(
      candidate.foreignHeaderCbor,
      "ForeignTipReconciliationV1 foreignHeaderCbor",
    ),
    blockStartTime,
    blockEndTime,
    commitments: parsedCommitments,
    evidence,
    resolution,
  };
};

const foreignTipReconciliationFromEntry = (
  entry: Entry,
): ForeignTipReconciliation =>
  parseForeignTipReconciliation({
    version: entry[Columns.FORMAT_VERSION],
    deploymentMarker: {
      schemaVersion: entry[Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION],
      manifestId: entry[Columns.DEPLOYMENT_MANIFEST_ID],
    },
    consensusProfileId: entry[Columns.CONSENSUS_PROFILE_ID],
    foreignHeaderHash: entry[Columns.FOREIGN_HEADER_HASH],
    replacedBaseHeaderHash: entry[Columns.REPLACED_BASE_HEADER_HASH],
    foreignHeaderCbor: entry[Columns.FOREIGN_HEADER_CBOR],
    blockStartTime: entry[Columns.BLOCK_START_TIME],
    blockEndTime: entry[Columns.BLOCK_END_TIME],
    commitments: {
      depositsRoot: entry[Columns.DEPOSITS_ROOT],
      forcedTransactionsRoot: entry[Columns.FORCED_TRANSACTIONS_ROOT],
      withdrawalsRoot: entry[Columns.WITHDRAWALS_ROOT],
      depositCount: entry[Columns.DEPOSIT_COUNT],
      forcedTransactionCount: entry[Columns.FORCED_TRANSACTION_COUNT],
      withdrawalCount: entry[Columns.WITHDRAWAL_COUNT],
    },
    evidence:
      entry[Columns.EVIDENCE_KIND] === EvidenceKind.VerifiedDa
        ? {
            kind: EvidenceKind.VerifiedDa,
            schemaVersion: entry[Columns.VERIFIED_DA_SCHEMA_VERSION],
            payloadCbor: entry[Columns.VERIFIED_DA_PAYLOAD_CBOR],
            payloadSha256: entry[Columns.VERIFIED_DA_PAYLOAD_SHA256],
          }
        : { kind: entry[Columns.EVIDENCE_KIND] },
    resolution:
      entry[Columns.STATUS] === Status.Awaiting
        ? { kind: Status.Awaiting, reason: entry[Columns.BLOCKING_REASON] }
        : { kind: Status.Resolved },
  });

export const decodeForeignTipReconciliation = (
  entry: Entry,
): ForeignTipReconciliation => foreignTipReconciliationFromEntry(entry);

const normalizeEntry = (entry: RawEntry): Entry => {
  const normalized: Entry = {
    ...entry,
    [Columns.DEPOSIT_COUNT]: BigInt(entry[Columns.DEPOSIT_COUNT]),
    [Columns.FORCED_TRANSACTION_COUNT]: BigInt(
      entry[Columns.FORCED_TRANSACTION_COUNT],
    ),
    [Columns.WITHDRAWAL_COUNT]: BigInt(entry[Columns.WITHDRAWAL_COUNT]),
  };
  foreignTipReconciliationFromEntry(normalized);
  if (
    (normalized[Columns.STATUS] === Status.Awaiting &&
      (normalized[Columns.BLOCKING_REASON] === null ||
        normalized[Columns.RESOLVED_AT] !== null)) ||
    (normalized[Columns.STATUS] === Status.Resolved &&
      (normalized[Columns.BLOCKING_REASON] !== null ||
        normalized[Columns.RESOLVED_AT] === null))
  ) {
    throw new Error(
      "ForeignTipReconciliationV1 persisted lifecycle timestamps are inconsistent",
    );
  }
  return normalized;
};

const decodeEntry = (entry: RawEntry): Effect.Effect<Entry, DatabaseError> =>
  Effect.try({
    try: () => normalizeEntry(entry),
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message: "Refusing to load a non-canonical ForeignTipReconciliationV1",
        cause: String(cause),
      }),
  });

const decodeForeignHeaderHashKey = (
  value: string,
): Effect.Effect<Buffer, DatabaseError> =>
  Effect.try({
    try: () => {
      if (!/^[0-9a-f]{56}$/u.test(value)) {
        throw new Error(
          "foreign header hash must be exact lowercase 28-byte hex",
        );
      }
      return Buffer.from(value, "hex");
    },
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message: "Foreign-tip reconciliation key is not canonical V1 hex",
        cause,
      }),
  });

const parseDaIdentity = (value: unknown): ForeignTipDaIdentity => {
  const candidate = exactRecord(
    value,
    [
      "headerHash",
      "schemaVersion",
      "consensusProfileId",
      "payloadCbor",
      "payloadSha256",
    ],
    "ForeignTipReconciliationV1 DA identity",
  );
  if (candidate.schemaVersion !== 1) {
    throw new Error(
      "ForeignTipReconciliationV1 DA identity schemaVersion must equal 1",
    );
  }
  if (candidate.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_ID) {
    throw new Error(
      "ForeignTipReconciliationV1 DA identity consensus profile is unsupported",
    );
  }
  const payloadCbor = exactBytes(
    candidate.payloadCbor,
    "ForeignTipReconciliationV1 DA identity payloadCbor",
  );
  const payloadSha256 = exactBytes(
    candidate.payloadSha256,
    "ForeignTipReconciliationV1 DA identity payloadSha256",
    32,
  );
  if (!sha256(payloadCbor).equals(payloadSha256)) {
    throw new Error(
      "ForeignTipReconciliationV1 DA identity payload digest does not match",
    );
  }
  return {
    headerHash: exactBytes(
      candidate.headerHash,
      "ForeignTipReconciliationV1 DA identity headerHash",
      28,
    ),
    schemaVersion: 1,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    payloadCbor,
    payloadSha256,
  };
};

export const authenticateForeignTipDaEvidence = ({
  reconciliation,
  deploymentMarker,
  evidence,
}: {
  readonly reconciliation: ForeignTipReconciliation;
  readonly deploymentMarker: unknown;
  readonly evidence: unknown;
}): ForeignTipDaIdentity => {
  const canonicalReconciliation = parseForeignTipReconciliation(reconciliation);
  assertDeploymentMarkerMatches(
    canonicalReconciliation.deploymentMarker,
    deploymentMarker,
    "ForeignTipReconciliationV1 recovery",
  );
  const canonicalEvidence = parseDaIdentity(evidence);
  if (
    !canonicalEvidence.headerHash.equals(
      canonicalReconciliation.foreignHeaderHash,
    ) ||
    canonicalEvidence.consensusProfileId !==
      canonicalReconciliation.consensusProfileId
  ) {
    throw new Error(
      "ForeignTipReconciliationV1 DA identity does not match header/profile",
    );
  }
  if (canonicalReconciliation.evidence.kind === EvidenceKind.VerifiedEmpty) {
    throw new Error(
      "ForeignTipReconciliationV1 verified-empty evidence cannot be replaced by DA",
    );
  }
  if (
    canonicalReconciliation.evidence.kind === EvidenceKind.VerifiedDa &&
    (!canonicalEvidence.payloadCbor.equals(
      canonicalReconciliation.evidence.payloadCbor,
    ) ||
      !canonicalEvidence.payloadSha256.equals(
        canonicalReconciliation.evidence.payloadSha256,
      ))
  ) {
    throw new Error(
      "ForeignTipReconciliationV1 retained DA evidence substitution rejected",
    );
  }
  return canonicalEvidence;
};

export const recordMismatch = ({
  foreignHeaderHash,
  replacedBaseHeaderHash,
  foreignHeader,
  consensusProfile,
  deploymentMarker,
}: {
  readonly foreignHeaderHash: string;
  readonly replacedBaseHeaderHash: string;
  readonly foreignHeader: SDK.Header;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly deploymentMarker: DeploymentMarker;
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
      LucidData.to(foreignHeader, SDK.Header),
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
    const canonical = yield* Effect.try({
      try: () => {
        if (
          !/^[0-9a-f]{56}$/u.test(foreignHeaderHash) ||
          !/^[0-9a-f]{56}$/u.test(replacedBaseHeaderHash)
        ) {
          throw new Error(
            "foreign and replaced header hashes must be exact lowercase 28-byte hex",
          );
        }
        return parseForeignTipReconciliation({
          version: FOREIGN_TIP_RECONCILIATION_VERSION,
          deploymentMarker,
          consensusProfileId: consensusProfile.profileId,
          foreignHeaderHash: Buffer.from(foreignHeaderHash, "hex"),
          replacedBaseHeaderHash: Buffer.from(replacedBaseHeaderHash, "hex"),
          foreignHeaderCbor,
          blockStartTime,
          blockEndTime,
          commitments: {
            depositsRoot: foreignHeader.depositsRoot,
            forcedTransactionsRoot: foreignHeader.forcedTransactionsRoot,
            withdrawalsRoot: foreignHeader.withdrawalsRoot,
            depositCount: foreignHeader.depositCount,
            forcedTransactionCount: foreignHeader.forcedTransactionCount,
            withdrawalCount: foreignHeader.withdrawalCount,
          },
          evidence: { kind: EvidenceKind.Pending },
          resolution: {
            kind: Status.Awaiting,
            reason: "pending_evidence",
          },
        });
      },
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to persist a non-canonical ForeignTipReconciliationV1",
          cause,
        }),
    });

    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ [Columns.FOREIGN_HEADER_HASH]: Buffer }>`
      INSERT INTO ${sql(tableName)} (
        ${sql(Columns.FOREIGN_HEADER_HASH)},
        ${sql(Columns.FORMAT_VERSION)},
        ${sql(Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION)},
        ${sql(Columns.DEPLOYMENT_MANIFEST_ID)},
        ${sql(Columns.CONSENSUS_PROFILE_ID)},
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
        ${sql(Columns.EVIDENCE_KIND)},
        ${sql(Columns.STATUS)},
        ${sql(Columns.BLOCKING_REASON)}
      ) VALUES (
        ${canonical.foreignHeaderHash},
        ${canonical.version},
        ${canonical.deploymentMarker.schemaVersion},
        ${canonical.deploymentMarker.manifestId},
        ${canonical.consensusProfileId},
        ${canonical.replacedBaseHeaderHash},
        ${canonical.foreignHeaderCbor},
        ${canonical.blockStartTime},
        ${canonical.blockEndTime},
        ${canonical.commitments.depositsRoot},
        ${canonical.commitments.forcedTransactionsRoot},
        ${canonical.commitments.withdrawalsRoot},
        ${canonical.commitments.depositCount},
        ${canonical.commitments.forcedTransactionCount},
        ${canonical.commitments.withdrawalCount},
        ${canonical.evidence.kind},
        ${Status.Awaiting},
        ${"pending_evidence"}
      )
      ON CONFLICT (${sql(Columns.FOREIGN_HEADER_HASH)}) DO UPDATE SET
        ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(tableName)}.${sql(Columns.FORMAT_VERSION)} = EXCLUDED.${sql(Columns.FORMAT_VERSION)}
        AND ${sql(tableName)}.${sql(Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION)} = EXCLUDED.${sql(Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION)}
        AND ${sql(tableName)}.${sql(Columns.DEPLOYMENT_MANIFEST_ID)} = EXCLUDED.${sql(Columns.DEPLOYMENT_MANIFEST_ID)}
        AND ${sql(tableName)}.${sql(Columns.REPLACED_BASE_HEADER_HASH)} = EXCLUDED.${sql(Columns.REPLACED_BASE_HEADER_HASH)}
        AND ${sql(tableName)}.${sql(Columns.CONSENSUS_PROFILE_ID)} = EXCLUDED.${sql(Columns.CONSENSUS_PROFILE_ID)}
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
    const headerHash = yield* decodeForeignHeaderHashKey(foreignHeaderHash);
    const rows = yield* sql<RawEntry>`
      SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.FOREIGN_HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} = ${Status.Awaiting}
      LIMIT 1
    `;
    return rows.length === 0
      ? Option.none()
      : Option.some(yield* decodeEntry(rows[0]!));
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
    const headerHash = yield* decodeForeignHeaderHashKey(foreignHeaderHash);
    const rows = yield* sql<RawEntry>`
      SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.FOREIGN_HEADER_HASH)} = ${headerHash}
      LIMIT 1
    `;
    return rows.length === 0
      ? Option.none()
      : Option.some(yield* decodeEntry(rows[0]!));
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
  const rows = yield* sql<RawEntry>`
    SELECT * FROM ${sql(tableName)}
    ORDER BY ${sql(Columns.BLOCK_START_TIME)} ASC,
             ${sql(Columns.BLOCK_END_TIME)} ASC,
             ${sql(Columns.CREATED_AT)} ASC
  `;
  return yield* Effect.forEach(rows, decodeEntry, { concurrency: 1 });
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
  deploymentMarker,
  evidence,
}: {
  readonly foreignHeaderHash: string;
  readonly deploymentMarker: DeploymentMarker;
  readonly evidence: ResolveForeignTipEvidence;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (!/^[0-9a-f]{56}$/u.test(foreignHeaderHash)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Foreign-tip reconciliation key must be exact V1 hex",
          cause: foreignHeaderHash,
        }),
      );
    }
    const requestedResolution = yield* Effect.try({
      try: () => {
        const marker = parseDeploymentMarker(deploymentMarker);
        const candidate = exactRecord(
          evidence,
          evidence.kind === EvidenceKind.VerifiedDa
            ? ["kind", "daIdentity"]
            : ["kind"],
          "ForeignTipReconciliationV1 resolution evidence",
        );
        if (candidate.kind === EvidenceKind.VerifiedEmpty) {
          return {
            deploymentMarker: marker,
            evidence: {
              kind: EvidenceKind.VerifiedEmpty,
            } as const,
          };
        }
        if (candidate.kind !== EvidenceKind.VerifiedDa) {
          throw new Error(
            "resolved evidence must use an exact verified V1 discriminator",
          );
        }
        return {
          deploymentMarker: marker,
          evidence: {
            kind: EvidenceKind.VerifiedDa,
            daIdentity: parseDaIdentity(candidate.daIdentity),
          } as const,
        };
      },
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to resolve with non-canonical ForeignTipReconciliationV1 evidence",
          cause,
        }),
    });
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql.withTransaction(
      Effect.gen(function* () {
        const [raw] = yield* sql<RawEntry>`
          SELECT * FROM ${sql(tableName)}
          WHERE ${sql(Columns.FOREIGN_HEADER_HASH)} = ${Buffer.from(foreignHeaderHash, "hex")}
          FOR UPDATE
        `;
        if (raw === undefined) return [];
        const currentEntry = yield* decodeEntry(raw);
        const current = foreignTipReconciliationFromEntry(currentEntry);
        const canonicalEvidence = yield* Effect.try({
          try: (): ResolvedForeignTipEvidence => {
            assertDeploymentMarkerMatches(
              current.deploymentMarker,
              requestedResolution.deploymentMarker,
              "ForeignTipReconciliationV1 resolution",
            );
            if (
              requestedResolution.evidence.kind === EvidenceKind.VerifiedEmpty
            ) {
              return { kind: EvidenceKind.VerifiedEmpty };
            }
            const authenticated = authenticateForeignTipDaEvidence({
              reconciliation: current,
              deploymentMarker: requestedResolution.deploymentMarker,
              evidence: requestedResolution.evidence.daIdentity,
            });
            return {
              kind: EvidenceKind.VerifiedDa,
              schemaVersion: authenticated.schemaVersion,
              payloadCbor: authenticated.payloadCbor,
              payloadSha256: authenticated.payloadSha256,
            };
          },
          catch: (cause) =>
            new DatabaseError({
              table: tableName,
              message:
                "Foreign-tip resolution evidence is not bound to retained deployment/header/profile identity",
              cause,
            }),
        });
        const next = yield* Effect.try({
          try: () =>
            parseForeignTipReconciliation({
              ...current,
              evidence: canonicalEvidence,
              resolution: { kind: Status.Resolved },
            }),
          catch: (cause) =>
            new DatabaseError({
              table: tableName,
              message:
                "Verified foreign evidence does not match the retained reconciliation",
              cause,
            }),
        });
        if (
          current.evidence.kind !== EvidenceKind.Pending &&
          (current.evidence.kind !== next.evidence.kind ||
            (current.evidence.kind === EvidenceKind.VerifiedDa &&
              next.evidence.kind === EvidenceKind.VerifiedDa &&
              (!current.evidence.payloadCbor.equals(
                next.evidence.payloadCbor,
              ) ||
                !current.evidence.payloadSha256.equals(
                  next.evidence.payloadSha256,
                ))))
        ) {
          return [];
        }
        return yield* sql<{ [Columns.FOREIGN_HEADER_HASH]: Buffer }>`
          UPDATE ${sql(tableName)}
          SET ${sql(Columns.STATUS)} = ${Status.Resolved},
              ${sql(Columns.RESOLVED_AT)} = COALESCE(${sql(Columns.RESOLVED_AT)}, NOW()),
              ${sql(Columns.BLOCKING_REASON)} = NULL,
              ${sql(Columns.EVIDENCE_KIND)} = ${next.evidence.kind},
              ${sql(Columns.VERIFIED_DA_PAYLOAD_CBOR)} = ${
                next.evidence.kind === EvidenceKind.VerifiedDa
                  ? next.evidence.payloadCbor
                  : null
              },
              ${sql(Columns.VERIFIED_DA_SCHEMA_VERSION)} = ${
                next.evidence.kind === EvidenceKind.VerifiedDa
                  ? next.evidence.schemaVersion
                  : null
              },
              ${sql(Columns.VERIFIED_DA_PAYLOAD_SHA256)} = ${
                next.evidence.kind === EvidenceKind.VerifiedDa
                  ? next.evidence.payloadSha256
                  : null
              },
              ${sql(Columns.UPDATED_AT)} = NOW()
          WHERE ${sql(Columns.FOREIGN_HEADER_HASH)} = ${next.foreignHeaderHash}
          RETURNING ${sql(Columns.FOREIGN_HEADER_HASH)}
        `;
      }),
    );
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
    if (
      !/^[0-9a-f]{56}$/u.test(foreignHeaderHash) ||
      typeof reason !== "string" ||
      reason.length === 0
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "ForeignTipReconciliationV1 awaiting update requires an exact key and non-empty reason",
          cause: foreignHeaderHash,
        }),
      );
    }
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
