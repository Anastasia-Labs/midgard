import {
  MIDGARD_CONSENSUS_PROFILE_ID,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import {
  assertDeploymentMarkerMatches,
  type DeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import { sha256 } from "../sha256.js";
import {
  Columns,
  type Entry,
  EvidenceKind,
  exactBytes,
  FOREIGN_TIP_RECONCILIATION_VERSION,
  type ForeignTipDaIdentity,
  type ForeignTipReconciliation,
  type RawEntry,
  Status,
  tableName,
} from "./foreignTipReconciliations.parse-evidence.js";
import {
  decodeEntry,
  decodeForeignHeaderHashKey,
  parseForeignTipReconciliation,
} from "./foreignTipReconciliations.parse-foreign-tip-reconciliation.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import { exactRecord } from "./utils/exact-record.js";

export const parseDaIdentity = (value: unknown): ForeignTipDaIdentity => {
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
