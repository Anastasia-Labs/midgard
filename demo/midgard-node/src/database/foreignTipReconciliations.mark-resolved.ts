import {
  assertDeploymentMarkerMatches,
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import {
  Columns,
  type Entry,
  EvidenceKind,
  type RawEntry,
  type ResolvedForeignTipEvidence,
  type ResolveForeignTipEvidence,
  Status,
  tableName,
} from "./foreignTipReconciliations.parse-evidence.js";
import {
  decodeEntry,
  decodeForeignHeaderHashKey,
  foreignTipReconciliationFromEntry,
  parseForeignTipReconciliation,
} from "./foreignTipReconciliations.parse-foreign-tip-reconciliation.js";
import {
  authenticateForeignTipDaEvidence,
  parseDaIdentity,
} from "./foreignTipReconciliations.record-mismatch.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";
import { exactRecord } from "./utils/exact-record.js";

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

/** The deployment and consensus profile whose retained evidence applies. */
export type EvidenceScope = {
  /** Absent only for a dev bundle without a marker; then no row is excluded. */
  readonly manifestId: string | undefined;
  readonly consensusProfileId: string;
};

/** The stored verdict of a row, read from its columns without decoding. */
export type StoredVerdict = {
  readonly blockingReason: string | null;
  readonly commitments: {
    readonly depositsRoot: string;
    readonly depositCount: bigint;
    readonly forcedTransactionsRoot: string;
    readonly forcedTransactionCount: bigint;
    readonly withdrawalsRoot: string;
    readonly withdrawalCount: bigint;
  };
};

type VerdictColumns = Pick<
  RawEntry,
  | Columns.BLOCKING_REASON
  | Columns.DEPOSITS_ROOT
  | Columns.DEPOSIT_COUNT
  | Columns.FORCED_TRANSACTIONS_ROOT
  | Columns.FORCED_TRANSACTION_COUNT
  | Columns.WITHDRAWALS_ROOT
  | Columns.WITHDRAWAL_COUNT
>;

const storedVerdict = (row: VerdictColumns): StoredVerdict => ({
  blockingReason: row[Columns.BLOCKING_REASON],
  commitments: {
    depositsRoot: row[Columns.DEPOSITS_ROOT],
    depositCount: BigInt(row[Columns.DEPOSIT_COUNT]),
    forcedTransactionsRoot: row[Columns.FORCED_TRANSACTIONS_ROOT],
    forcedTransactionCount: BigInt(row[Columns.FORCED_TRANSACTION_COUNT]),
    withdrawalsRoot: row[Columns.WITHDRAWALS_ROOT],
    withdrawalCount: BigInt(row[Columns.WITHDRAWAL_COUNT]),
  },
});

/** A stored row that no longer decodes; its window is still readable. */
export type UndecodableEvidence = {
  readonly foreignHeaderHash: Buffer;
  readonly blockStartTime: Date;
  readonly blockEndTime: Date;
  readonly verdict: StoredVerdict;
  readonly cause: DatabaseError;
};

/**
 * Evidence of the active deployment and profile only, decoded row by row: a
 * row that no longer decodes is returned beside the others instead of failing
 * the whole history.
 */
export const retrieveEvidenceHistory = (
  scope: EvidenceScope,
): Effect.Effect<
  {
    readonly entries: readonly Entry[];
    readonly undecodable: readonly UndecodableEvidence[];
  },
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows =
      scope.manifestId === undefined
        ? yield* sql<RawEntry>`
            SELECT * FROM ${sql(tableName)}
            WHERE ${sql(Columns.CONSENSUS_PROFILE_ID)} = ${scope.consensusProfileId}
            ORDER BY ${sql(Columns.BLOCK_START_TIME)} ASC,
                     ${sql(Columns.BLOCK_END_TIME)} ASC,
                     ${sql(Columns.CREATED_AT)} ASC
          `
        : yield* sql<RawEntry>`
            SELECT * FROM ${sql(tableName)}
            WHERE ${sql(Columns.CONSENSUS_PROFILE_ID)} = ${scope.consensusProfileId}
              AND ${sql(Columns.DEPLOYMENT_MANIFEST_ID)} = ${scope.manifestId}
            ORDER BY ${sql(Columns.BLOCK_START_TIME)} ASC,
                     ${sql(Columns.BLOCK_END_TIME)} ASC,
                     ${sql(Columns.CREATED_AT)} ASC
          `;
    const entries: Entry[] = [];
    const undecodable: UndecodableEvidence[] = [];
    for (const row of rows) {
      const decoded = yield* Effect.either(decodeEntry(row));
      if (decoded._tag === "Right") {
        entries.push(decoded.right);
      } else {
        undecodable.push({
          foreignHeaderHash: row[Columns.FOREIGN_HEADER_HASH],
          blockStartTime: row[Columns.BLOCK_START_TIME],
          blockEndTime: row[Columns.BLOCK_END_TIME],
          verdict: storedVerdict(row),
          cause: decoded.left,
        });
      }
    }
    return { entries, undecodable };
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve foreign-tip reconciliation evidence history",
    ),
  );

/** A row whose foreign block ended before `cutoff`, read without decoding. */
export type PruneCandidate = {
  readonly foreignHeaderHash: Buffer;
  readonly manifestId: string;
  readonly consensusProfileId: string;
  readonly blockStartTime: Date;
  readonly blockEndTime: Date;
  readonly verdict: StoredVerdict;
};

export const retrievePruneCandidates = (
  cutoff: Date,
): Effect.Effect<readonly PruneCandidate[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<
      VerdictColumns &
        Pick<
          RawEntry,
          | Columns.FOREIGN_HEADER_HASH
          | Columns.DEPLOYMENT_MANIFEST_ID
          | Columns.CONSENSUS_PROFILE_ID
          | Columns.BLOCK_START_TIME
          | Columns.BLOCK_END_TIME
        >
    >`
      SELECT ${sql(Columns.FOREIGN_HEADER_HASH)},
             ${sql(Columns.DEPLOYMENT_MANIFEST_ID)},
             ${sql(Columns.CONSENSUS_PROFILE_ID)},
             ${sql(Columns.BLOCK_START_TIME)},
             ${sql(Columns.BLOCK_END_TIME)},
             ${sql(Columns.BLOCKING_REASON)},
             ${sql(Columns.DEPOSITS_ROOT)},
             ${sql(Columns.DEPOSIT_COUNT)},
             ${sql(Columns.FORCED_TRANSACTIONS_ROOT)},
             ${sql(Columns.FORCED_TRANSACTION_COUNT)},
             ${sql(Columns.WITHDRAWALS_ROOT)},
             ${sql(Columns.WITHDRAWAL_COUNT)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.BLOCK_END_TIME)} < ${cutoff}
      ORDER BY ${sql(Columns.BLOCK_END_TIME)} ASC,
               ${sql(Columns.FOREIGN_HEADER_HASH)} ASC
    `;
    return rows.map((row) => ({
      foreignHeaderHash: row[Columns.FOREIGN_HEADER_HASH],
      manifestId: row[Columns.DEPLOYMENT_MANIFEST_ID],
      consensusProfileId: row[Columns.CONSENSUS_PROFILE_ID],
      blockStartTime: row[Columns.BLOCK_START_TIME],
      blockEndTime: row[Columns.BLOCK_END_TIME],
      verdict: storedVerdict(row),
    }));
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve foreign-tip reconciliations past the challengeability horizon",
    ),
  );

/**
 * Deletes the named rows, re-checking the horizon in the statement itself so a
 * row can only ever leave once its foreign block ended before `cutoff`.
 */
export const deleteSettled = ({
  foreignHeaderHashes,
  cutoff,
}: {
  readonly foreignHeaderHashes: readonly Buffer[];
  readonly cutoff: Date;
}): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (foreignHeaderHashes.length === 0) return 0;
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ [Columns.FOREIGN_HEADER_HASH]: Buffer }>`
      DELETE FROM ${sql(tableName)}
      WHERE ${sql(Columns.FOREIGN_HEADER_HASH)} IN ${sql.in(foreignHeaderHashes)}
        AND ${sql(Columns.BLOCK_END_TIME)} < ${cutoff}
      RETURNING ${sql(Columns.FOREIGN_HEADER_HASH)}
    `;
    return rows.length;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to prune settled foreign-tip reconciliations",
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
