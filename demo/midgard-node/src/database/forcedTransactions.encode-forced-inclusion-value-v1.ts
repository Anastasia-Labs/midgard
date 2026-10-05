import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxId,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE_ID,
} from "@al-ft/midgard-core/consensus-profile";
import {
  MidgardForcedTxAdmissionStopped,
  validateMidgardConsensusForcedTxCbor,
} from "@al-ft/midgard-core/consensus-validation";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import { sha256 } from "../sha256.js";
import {
  Columns,
  type Entry,
  type ForcedInclusionValueV1Input,
  projectedEventAdapter,
  projectedEventsTable,
  sameImmutablePayload,
  tableName,
} from "./forcedTransactions.exact-forced-transaction-journal-member.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as ProjectedEvents from "./utils/projected-events.js";

export const encodeForcedInclusionValueV1 = ({
  nativeTxCbor,
  verdict,
  consensusProfile,
}: ForcedInclusionValueV1Input): Effect.Effect<
  {
    readonly txId: Buffer;
    readonly txCompact: Buffer;
    readonly transactionCommitment: Buffer;
    readonly source: ReturnType<typeof deriveMidgardForcedTxProofSource>;
    readonly value: Buffer;
  },
  DatabaseError | MidgardForcedTxAdmissionStopped
> =>
  Effect.gen(function* () {
    if (!isMidgardConsensusProfile(consensusProfile)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to encode a forced transaction under a non-V1 consensus profile",
          cause: "non-v1-profile",
        }),
      );
    }
    const material = yield* Effect.try({
      try: () => {
        const violation = validateMidgardConsensusForcedTxCbor(nativeTxCbor);
        if (violation !== null) {
          throw new MidgardForcedTxAdmissionStopped(violation);
        }
        const nativeTx =
          decodeMidgardForcedTxFullFromCanonicalCbor(nativeTxCbor);
        const txId = computeMidgardNativeTxId(nativeTx.compact);
        const source = deriveMidgardForcedTxProofSource(nativeTx);
        const transactionCommitment =
          computeMidgardForcedTxProofCommitment(source);
        return {
          txId,
          source,
          transactionCommitment,
          txCompact: source.compactCbor,
        };
      },
      catch: (cause) =>
        cause instanceof MidgardForcedTxAdmissionStopped
          ? cause
          : new DatabaseError({
              table: tableName,
              message:
                "Failed to verify the exact canonical V1 forced transaction",
              cause,
            }),
    });
    const forcedInclusionTx: SDK.ForcedInclusionTxV1 = {
      tx_id: material.txId.toString("hex"),
      submitted_source: {
        compact_cbor: material.source.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          material.source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          material.source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict,
    };
    const value = yield* Effect.try({
      try: () =>
        Buffer.from(
          aikenSerialisedPlutusDataCbor(
            LucidData.to(forcedInclusionTx, SDK.ForcedInclusionTxV1),
          ),
          "hex",
        ),
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Failed to encode V1 forced transaction source value",
          cause,
        }),
    });
    return { ...material, value };
  }).pipe(
    Effect.tapError((cause) =>
      cause instanceof MidgardForcedTxAdmissionStopped
        ? Effect.logError(
            "Forced transaction admission stopped; no verdict encoded",
          ).pipe(
            Effect.annotateLogs({
              alarm: cause._tag,
              code: cause.code,
              feature: cause.violation.featureId,
            }),
          )
        : Effect.void,
    ),
  );

export const insertEntries = (
  entries: readonly Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length <= 0) {
      return;
    }
    const incomingById = new Map<string, Entry>();
    for (const incoming of entries) {
      const key = incoming[Columns.TX_ORDER_ID].toString("hex");
      const existingIncoming = incomingById.get(key);
      if (
        existingIncoming !== undefined &&
        !sameImmutablePayload(existingIncoming, incoming)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Refusing to insert forced transactions because the same tx_order_id appears with conflicting payloads in one batch",
            cause: `tx_order_id=${key}`,
          }),
        );
      }
      incomingById.set(key, incoming);
    }

    const sql = yield* SqlClient.SqlClient;
    const normalizedEntries = [...incomingById.values()];
    const rows = yield* sql<{ [Columns.TX_ORDER_ID]: Buffer }>`
      INSERT INTO ${sql(tableName)} ${sql.insert(normalizedEntries)}
      ON CONFLICT (${sql(Columns.TX_ORDER_ID)}) DO UPDATE SET
        ${sql(Columns.TX_ORDER_ID)} = ${sql(tableName)}.${sql(Columns.TX_ORDER_ID)}
      WHERE ${sql(tableName)}.${sql(Columns.TX_ORDER_L1_TX_HASH)} = EXCLUDED.${sql(Columns.TX_ORDER_L1_TX_HASH)}
        AND ${sql(tableName)}.${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)} = EXCLUDED.${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)}
        AND ${sql(tableName)}.${sql(Columns.ASSET_NAME)} = EXCLUDED.${sql(Columns.ASSET_NAME)}
        AND ${sql(tableName)}.${sql(Columns.RAW_DATUM)} = EXCLUDED.${sql(Columns.RAW_DATUM)}
        AND ${sql(tableName)}.${sql(Columns.TX_ID)} = EXCLUDED.${sql(Columns.TX_ID)}
        AND ${sql(tableName)}.${sql(Columns.TX_COMPACT)} = EXCLUDED.${sql(Columns.TX_COMPACT)}
        AND ${sql(tableName)}.${sql(Columns.CONSENSUS_PROFILE_ID)} IS NOT DISTINCT FROM EXCLUDED.${sql(Columns.CONSENSUS_PROFILE_ID)}
        AND ${sql(tableName)}.${sql(Columns.NATIVE_TX_CBOR)} IS NOT DISTINCT FROM EXCLUDED.${sql(Columns.NATIVE_TX_CBOR)}
        AND ${sql(tableName)}.${sql(Columns.TRANSACTION_COMMITMENT)} IS NOT DISTINCT FROM EXCLUDED.${sql(Columns.TRANSACTION_COMMITMENT)}
        AND ${sql(tableName)}.${sql(Columns.INCLUSION_TIME)} = EXCLUDED.${sql(Columns.INCLUSION_TIME)}
      RETURNING ${sql(Columns.TX_ORDER_ID)}
    `;
    if (rows.length !== normalizedEntries.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to upsert forced transaction because the same tx_order_id has conflicting persisted payload",
          cause: `requested=${normalizedEntries.length},upserted=${rows.length}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to insert forced transaction UTxOs",
    ),
  );

export const setProofClassifications = (
  classifications: readonly {
    readonly txOrderId: Buffer;
    readonly verdict: SDK.OperatorVerdict;
    readonly programMaterialSidecarCbor: Buffer;
  }[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (classifications.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.forEach(
        classifications,
        (classification) =>
          Effect.gen(function* () {
            const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
            WHERE ${sql(Columns.TX_ORDER_ID)} = ${classification.txOrderId}
              AND ${sql(Columns.CONSENSUS_PROFILE_ID)} = ${MIDGARD_CONSENSUS_PROFILE_ID}
            FOR UPDATE`;
            const row = rows[0];
            if (row === undefined) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Failed to persist exact V1 forced transaction classification",
                  cause: `tx_order_id=${classification.txOrderId.toString("hex")},updated=0`,
                }),
              );
            }
            // Classification may write only the verdict. Reuse the persisted
            // immutable source so this API cannot substitute a different order payload.
            const value = yield* Effect.try({
              try: () =>
                Buffer.from(
                  aikenSerialisedPlutusDataCbor(
                    LucidData.to(
                      {
                        ...LucidData.from(
                          row[Columns.FORCED_INCLUSION_VALUE].toString("hex"),
                          SDK.ForcedInclusionTxV1,
                        ),
                        verdict: classification.verdict,
                      },
                      SDK.ForcedInclusionTxV1,
                    ),
                  ),
                  "hex",
                ),
              catch: (cause) =>
                new DatabaseError({
                  table: tableName,
                  message: "Failed to encode forced verdict",
                  cause,
                }),
            });
            yield* sql`UPDATE ${sql(tableName)} SET
              ${sql(Columns.FORCED_INCLUSION_VALUE)} = ${value},
              ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)} = ${classification.programMaterialSidecarCbor},
              ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)} = ${sha256(classification.programMaterialSidecarCbor)},
              updated_at = NOW()
            WHERE ${sql(Columns.TX_ORDER_ID)} = ${classification.txOrderId}`;
          }),
        { discard: true },
      ),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to update V1 forced transaction classifications",
    ),
  );

export const retrieveAllEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.TX_ORDER_ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveAllEntries ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve forced transaction UTxOs",
    ),
  );

export const retrieveByTxOrderId = (
  txOrderId: Buffer,
): Effect.Effect<Option.Option<Entry>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.TX_ORDER_ID)} = ${txOrderId}
      LIMIT 1`;
    return rows.length === 0 ? Option.none() : Option.some(rows[0]!);
  }).pipe(
    Effect.withLogSpan(`retrieveByTxOrderId ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve forced transaction by tx_order_id",
    ),
  );

export const retrievePendingHeaderEntriesUpTo = (
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  projectedEventAdapter.retrievePendingHeaderEntriesUpTo(endTime);

export const retrieveByProjectedHeaderHash = (
  projectedHeaderHash: Buffer,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  projectedEventAdapter.retrieveByProjectedHeaderHash(projectedHeaderHash);

export const markAwaitingAsProjected = (
  ids: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  projectedEventAdapter.markAwaitingAsProjected(ids);

export const markProjectedByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  projectedEventAdapter.markProjectedByEventIds(ids, projectedHeaderHash);

export const clearProjectedHeaderAssignmentByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  projectedEventAdapter.clearProjectedHeaderAssignmentByEventIds(
    ids,
    projectedHeaderHash,
  );

export const reopenAfterStateQueueCorrectionByEventIds = (
  ids: readonly Buffer[],
  removedHeaderHash: Buffer,
) =>
  ProjectedEvents.reopenAfterStateQueueCorrectionByEventIds(
    projectedEventsTable,
    ids,
    removedHeaderHash,
  );
