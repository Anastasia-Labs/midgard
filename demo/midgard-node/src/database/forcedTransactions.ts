import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxId,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec";
import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "@al-ft/midgard-core/codec/cbor";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_ID,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusForcedTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
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
import * as ProjectedEvents from "./utils/projected-events.js";

export const tableName = "forced_transaction_utxos";
const PROOF_MAX_CANONICAL_TRANSACTION_BYTES = 295_041;
if (
  PROOF_MAX_CANONICAL_TRANSACTION_BYTES !==
  MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes
) {
  throw new Error("forced-transaction SQL bound does not match canonical V1");
}

export enum Columns {
  TX_ORDER_ID = "tx_order_id",
  TX_ORDER_L1_TX_HASH = "tx_order_l1_tx_hash",
  TX_ORDER_L1_OUTPUT_INDEX = "tx_order_l1_output_index",
  ASSET_NAME = "asset_name",
  RAW_DATUM = "raw_datum",
  TX_ID = "tx_id",
  TX_COMPACT = "tx_compact",
  FORCED_INCLUSION_VALUE = "forced_inclusion_value",
  CONSENSUS_PROFILE_ID = "consensus_profile_id",
  NATIVE_TX_CBOR = "native_tx_cbor",
  TRANSACTION_COMMITMENT = "transaction_commitment",
  CEK_PROGRAM_MATERIAL_SIDECAR_CBOR = "cek_program_material_sidecar_cbor",
  CEK_PROGRAM_MATERIAL_SIDECAR_SHA256 = "cek_program_material_sidecar_sha256",
  INCLUSION_TIME = "inclusion_time",
  PROJECTED_HEADER_HASH = "projected_header_hash",
  STATUS = "status",
}

export const Status = {
  Awaiting: "awaiting",
  Projected: "projected",
  Finalized: "finalized",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export type Entry = {
  [Columns.TX_ORDER_ID]: Buffer;
  [Columns.TX_ORDER_L1_TX_HASH]: Buffer;
  [Columns.TX_ORDER_L1_OUTPUT_INDEX]: number;
  [Columns.ASSET_NAME]: Buffer;
  [Columns.RAW_DATUM]: Buffer;
  [Columns.TX_ID]: Buffer;
  [Columns.TX_COMPACT]: Buffer;
  [Columns.FORCED_INCLUSION_VALUE]: Buffer;
  [Columns.CONSENSUS_PROFILE_ID]: typeof MIDGARD_CONSENSUS_PROFILE_ID;
  [Columns.NATIVE_TX_CBOR]: Buffer;
  [Columns.TRANSACTION_COMMITMENT]: Buffer;
  [Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]: Buffer;
  [Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]: Buffer;
  [Columns.INCLUSION_TIME]: Date;
  [Columns.PROJECTED_HEADER_HASH]: Buffer | null;
  [Columns.STATUS]: Status;
};

export type ForcedInclusionValueV1Input = {
  readonly nativeTxCbor: Buffer;
  readonly verdict: SDK.OperatorVerdict;
  readonly consensusProfile: MidgardConsensusProfile;
};

export const operatorVerdictOfEntry = (entry: Entry): SDK.OperatorVerdict =>
  LucidData.from(
    entry[Columns.FORCED_INCLUSION_VALUE].toString("hex"),
    SDK.ForcedInclusionTxV1,
  ).verdict;

/** Operational classification is derived from the single stored verdict. */
export const operatorValidityOfEntry = (entry: Entry): SDK.MidgardTxValidity =>
  operatorVerdictOfEntry(entry) === "ForcedTxValid"
    ? "TxIsValid"
    : "TxIsInvalid";

export const FORCED_TRANSACTION_JOURNAL_MEMBER_VERSION = 1n;
if (
  Number(FORCED_TRANSACTION_JOURNAL_MEMBER_VERSION) !==
  MIDGARD_CONSENSUS_PROFILE.forcedTransactionJournalVersion
) {
  throw new Error(
    "ForcedTransactionJournalMemberV1 version does not match the compiled consensus profile",
  );
}

export type ForcedTransactionJournalMember = {
  readonly sourceValueCbor: Buffer;
  readonly canonicalTransactionCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
};

const FORCED_TRANSACTION_JOURNAL_MEMBER_FIELDS = [
  "sourceValueCbor",
  "canonicalTransactionCbor",
  "programMaterialSidecarCbor",
] as const;

const exactForcedTransactionJournalMember = (
  value: unknown,
): ForcedTransactionJournalMember => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(
      "ForcedTransactionJournalMemberV1 must be an exact three-field record",
    );
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error("ForcedTransactionJournalMemberV1 must be a plain record");
  }
  const keys = Reflect.ownKeys(value);
  if (
    keys.length !== Object.keys(value).length ||
    keys.length !== FORCED_TRANSACTION_JOURNAL_MEMBER_FIELDS.length ||
    keys.some(
      (key) =>
        typeof key !== "string" ||
        !FORCED_TRANSACTION_JOURNAL_MEMBER_FIELDS.includes(
          key as (typeof FORCED_TRANSACTION_JOURNAL_MEMBER_FIELDS)[number],
        ),
    )
  ) {
    throw new Error(
      "ForcedTransactionJournalMemberV1 must contain exactly sourceValueCbor, canonicalTransactionCbor, and programMaterialSidecarCbor",
    );
  }
  const candidate = value as Record<string, unknown>;
  const exactNonEmptyBytes = (field: string): Buffer => {
    const bytes = candidate[field];
    if (!(bytes instanceof Uint8Array) || bytes.length === 0) {
      throw new Error(
        `ForcedTransactionJournalMemberV1.${field} must be non-empty bytes`,
      );
    }
    return Buffer.from(bytes);
  };
  return {
    sourceValueCbor: exactNonEmptyBytes("sourceValueCbor"),
    canonicalTransactionCbor: exactNonEmptyBytes("canonicalTransactionCbor"),
    programMaterialSidecarCbor: exactNonEmptyBytes(
      "programMaterialSidecarCbor",
    ),
  };
};

const encodeExactForcedTransactionJournalMember = ({
  sourceValueCbor,
  canonicalTransactionCbor,
  programMaterialSidecarCbor,
}: ForcedTransactionJournalMember): Buffer =>
  encodeCbor([
    FORCED_TRANSACTION_JOURNAL_MEMBER_VERSION,
    sourceValueCbor,
    canonicalTransactionCbor,
    programMaterialSidecarCbor,
  ]);

/**
 * Durable V1 journal representation. The committed source and its DA-only
 * canonical preimage remain distinct so the publisher cannot omit
 * either after header construction.
 */
export const encodeForcedTransactionJournalMember = (
  value: ForcedTransactionJournalMember,
): Buffer =>
  encodeExactForcedTransactionJournalMember(
    exactForcedTransactionJournalMember(value),
  );

export const decodeForcedTransactionJournalMember = (
  bytes: Uint8Array,
): ForcedTransactionJournalMember => {
  const fields = asArray(
    decodeSingleCbor(bytes),
    "forced_transaction_journal_member_v1",
  );
  if (
    fields.length !== 4 ||
    asBigInt(fields[0], "forced_transaction_journal_member_v1.version") !==
      FORCED_TRANSACTION_JOURNAL_MEMBER_VERSION
  ) {
    throw new Error(
      "forced_transaction_journal_member_v1 must contain exact version 1 and three byte fields",
    );
  }
  const decoded = exactForcedTransactionJournalMember({
    sourceValueCbor: asBytes(
      fields[1],
      "forced_transaction_journal_member_v1.source_value_cbor",
    ),
    canonicalTransactionCbor: asBytes(
      fields[2],
      "forced_transaction_journal_member_v1.canonical_transaction_cbor",
    ),
    programMaterialSidecarCbor: asBytes(
      fields[3],
      "forced_transaction_journal_member_v1.program_material_sidecar_cbor",
    ),
  });
  if (
    !encodeExactForcedTransactionJournalMember(decoded).equals(
      Buffer.from(bytes),
    )
  ) {
    throw new Error(
      "forced_transaction_journal_member_v1 must use the canonical V1 CBOR encoding",
    );
  }
  return decoded;
};

const projectedEventsTable = {
  tableName,
  idColumn: Columns.TX_ORDER_ID,
  inclusionTimeColumn: Columns.INCLUSION_TIME,
  projectedHeaderHashColumn: Columns.PROJECTED_HEADER_HASH,
  statusColumn: Columns.STATUS,
  awaitingStatus: Status.Awaiting,
  projectedStatus: Status.Projected,
  terminalStatus: Status.Finalized,
  entitySingular: "forced transaction",
  entityPlural: "forced transactions",
  idLabel: "tx_order_id",
  touchUpdatedAt: true,
} as const satisfies ProjectedEvents.ProjectedEventTable;

const projectedEventAdapter = ProjectedEvents.makeProjectedEventAdapter<Entry>({
  config: projectedEventsTable,
  pendingHeaderStatuses: [Status.Awaiting, Status.Projected],
  messages: {
    retrieveByProjectedHeaderHash:
      "Failed to retrieve forced transactions by projected header hash",
    retrievePendingHeaderEntriesUpTo:
      "Failed to retrieve forced transactions pending header assignment",
    retrieveProjectedPendingHeaderEntries:
      "Failed to retrieve projected forced transactions awaiting header assignment",
    markAwaitingAsProjected:
      "Failed to mark awaiting forced transactions as projected",
    markProjectedByEventIds:
      "Failed to mark forced transactions as assigned to the given header",
    clearProjectedHeaderAssignmentByEventIds:
      "Failed to clear projected header assignments for forced transactions",
  },
});

const sameImmutablePayload = (left: Entry, right: Entry): boolean => {
  return (
    left[Columns.TX_ORDER_ID].equals(right[Columns.TX_ORDER_ID]) &&
    left[Columns.TX_ORDER_L1_TX_HASH].equals(
      right[Columns.TX_ORDER_L1_TX_HASH],
    ) &&
    left[Columns.TX_ORDER_L1_OUTPUT_INDEX] ===
      right[Columns.TX_ORDER_L1_OUTPUT_INDEX] &&
    left[Columns.ASSET_NAME].equals(right[Columns.ASSET_NAME]) &&
    left[Columns.RAW_DATUM].equals(right[Columns.RAW_DATUM]) &&
    left[Columns.TX_ID].equals(right[Columns.TX_ID]) &&
    left[Columns.TX_COMPACT].equals(right[Columns.TX_COMPACT]) &&
    left[Columns.CONSENSUS_PROFILE_ID] ===
      right[Columns.CONSENSUS_PROFILE_ID] &&
    left[Columns.NATIVE_TX_CBOR].equals(right[Columns.NATIVE_TX_CBOR]) &&
    left[Columns.TRANSACTION_COMMITMENT].equals(
      right[Columns.TRANSACTION_COMMITMENT],
    ) &&
    left[Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR].equals(
      right[Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR],
    ) &&
    left[Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256].equals(
      right[Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256],
    ) &&
    left[Columns.INCLUSION_TIME].getTime() ===
      right[Columns.INCLUSION_TIME].getTime()
  );
};

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
  DatabaseError
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
          throw new Error(
            `${violation.code} ${violation.featureId}: ${violation.detail}`,
          );
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
        new DatabaseError({
          table: tableName,
          message: "Failed to verify the exact canonical V1 forced transaction",
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
  });

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

export const markFinalizedByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      [Columns.TX_ORDER_ID]: Buffer;
    }>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Finalized},
          ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash},
          updated_at = NOW()
      WHERE ${sql(Columns.TX_ORDER_ID)} IN ${sql.in(ids)}
        AND ${sql(Columns.STATUS)} IN (${Status.Projected}, ${Status.Finalized})
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash}
      RETURNING ${sql(Columns.TX_ORDER_ID)}`;
    if (rows.length !== ids.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to finalize forced transactions because at least one row is missing, unprojected, or assigned to a different header",
          cause: `requested=${ids.length},finalized=${rows.length},header_hash=${projectedHeaderHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markFinalizedByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark forced transactions finalized",
    ),
  );

export const toRootKeyValue = (
  entry: Entry,
): { readonly key: Buffer; readonly value: Buffer } => ({
  key: Buffer.from(entry[Columns.TX_ORDER_ID]),
  value: Buffer.from(entry[Columns.FORCED_INCLUSION_VALUE]),
});

export const clear = clearTable(tableName);
