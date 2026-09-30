import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { parseDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  Columns,
  type Entry,
  EvidenceKind,
  exactBytes,
  exactCount,
  exactDate,
  exactRoot,
  FOREIGN_TIP_RECONCILIATION_VERSION,
  type ForeignTipReconciliation,
  parseEvidence,
  type RawEntry,
  Status,
  tableName,
} from "./foreignTipReconciliations.parse-evidence.js";
import { DatabaseError } from "./utils/common.js";
import { exactRecord } from "./utils/exact-record.js";

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

export const foreignTipReconciliationFromEntry = (
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

export const decodeEntry = (
  entry: RawEntry,
): Effect.Effect<Entry, DatabaseError> =>
  Effect.try({
    try: () => normalizeEntry(entry),
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message: "Refusing to load a non-canonical ForeignTipReconciliationV1",
        cause: String(cause),
      }),
  });

export const decodeForeignHeaderHashKey = (
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
