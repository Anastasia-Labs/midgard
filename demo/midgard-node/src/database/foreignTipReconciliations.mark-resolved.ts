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
