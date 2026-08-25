import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
} from "@al-ft/midgard-core/codec";
import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "@al-ft/midgard-core/codec/cbor";
import {
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_ID,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { validateMidgardConsensusV1TxCbor } from "@al-ft/midgard-core/consensus-validation-v1";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as ProjectedEvents from "@/database/utils/projected-events.js";
import { Database } from "@/services/database.js";
import { sha256 } from "@/sha256.js";

export const tableName = "forced_transaction_utxos";
const PROOF_MAX_CANONICAL_TRANSACTION_BYTES = 295_041;
if (
  PROOF_MAX_CANONICAL_TRANSACTION_BYTES !==
  MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes
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
  OPERATOR_VALIDITY = "operator_validity",
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
  /**
   * The verdict's two-valued projection — see
   * {@link midgardTxValidityOfVerdictV1}. The verdict itself is carried, arm
   * and coordinates, by {@link Columns.FORCED_INCLUSION_VALUE}.
   */
  [Columns.OPERATOR_VALIDITY]: SDK.MidgardTxValidity;
  [Columns.CONSENSUS_PROFILE_ID]: typeof MIDGARD_CONSENSUS_PROFILE_V1_ID;
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
  readonly verdict: SDK.OperatorVerdictV1;
  readonly consensusProfile: MidgardConsensusProfileV1;
};

/**
 * The two-valued projection of an {@link SDK.OperatorVerdictV1} onto the
 * compact leaf's validity scalar, as the #640 forced-arm authoritativeness
 * predicate binds them: `ForcedTxValid` ⇔ validity code 0 (`TxIsValid`),
 * `ForcedTxInvalid { _ }` ⇔ validity code 1 (`TxIsInvalid`).
 *
 * `operator_validity` persists exactly this bit; the full verdict — arm and
 * subject coordinates — lives in `forced_inclusion_value`, the byte-exact
 * `ForcedInclusionTxV1` leaf the roots commit to.
 */
export const midgardTxValidityOfVerdictV1 = (
  verdict: SDK.OperatorVerdictV1,
): SDK.MidgardTxValidity =>
  verdict === "ForcedTxValid" ? "TxIsValid" : "TxIsInvalid";

export const FORCED_TRANSACTION_JOURNAL_MEMBER_V1_VERSION = 1n;
if (
  Number(FORCED_TRANSACTION_JOURNAL_MEMBER_V1_VERSION) !==
  MIDGARD_CONSENSUS_PROFILE_V1.forcedTransactionJournalVersion
) {
  throw new Error(
    "ForcedTransactionJournalMemberV1 version does not match the compiled consensus profile",
  );
}

export type ForcedTransactionJournalMemberV1 = {
  readonly sourceValueCbor: Buffer;
  readonly canonicalTransactionCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
};

const FORCED_TRANSACTION_JOURNAL_MEMBER_V1_FIELDS = [
  "sourceValueCbor",
  "canonicalTransactionCbor",
  "programMaterialSidecarCbor",
] as const;

const exactForcedTransactionJournalMemberV1 = (
  value: unknown,
): ForcedTransactionJournalMemberV1 => {
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
    keys.length !== FORCED_TRANSACTION_JOURNAL_MEMBER_V1_FIELDS.length ||
    keys.some(
      (key) =>
        typeof key !== "string" ||
        !FORCED_TRANSACTION_JOURNAL_MEMBER_V1_FIELDS.includes(
          key as (typeof FORCED_TRANSACTION_JOURNAL_MEMBER_V1_FIELDS)[number],
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

const encodeExactForcedTransactionJournalMemberV1 = ({
  sourceValueCbor,
  canonicalTransactionCbor,
  programMaterialSidecarCbor,
}: ForcedTransactionJournalMemberV1): Buffer =>
  encodeCbor([
    FORCED_TRANSACTION_JOURNAL_MEMBER_V1_VERSION,
    sourceValueCbor,
    canonicalTransactionCbor,
    programMaterialSidecarCbor,
  ]);

/**
 * Durable V1 journal representation. The committed source and its DA-only
 * canonical preimage remain distinct so the publisher cannot omit
 * either after header construction.
 */
export const encodeForcedTransactionJournalMemberV1 = (
  value: ForcedTransactionJournalMemberV1,
): Buffer =>
  encodeExactForcedTransactionJournalMemberV1(
    exactForcedTransactionJournalMemberV1(value),
  );

export const decodeForcedTransactionJournalMemberV1 = (
  bytes: Uint8Array,
): ForcedTransactionJournalMemberV1 => {
  const fields = asArray(
    decodeSingleCbor(bytes),
    "forced_transaction_journal_member_v1",
  );
  if (
    fields.length !== 4 ||
    asBigInt(fields[0], "forced_transaction_journal_member_v1.version") !==
      FORCED_TRANSACTION_JOURNAL_MEMBER_V1_VERSION
  ) {
    throw new Error(
      "forced_transaction_journal_member_v1 must contain exact version 1 and three byte fields",
    );
  }
  const decoded = exactForcedTransactionJournalMemberV1({
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
    !encodeExactForcedTransactionJournalMemberV1(decoded).equals(
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
    readonly source: ReturnType<typeof deriveMidgardNativeTxProofSourceV1>;
    readonly value: Buffer;
  },
  DatabaseError
> =>
  Effect.gen(function* () {
    if (!isMidgardConsensusProfileV1(consensusProfile)) {
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
        const violation = validateMidgardConsensusV1TxCbor(nativeTxCbor);
        if (violation !== null) {
          throw new Error(
            `${violation.code} ${violation.featureId}: ${violation.detail}`,
          );
        }
        const nativeTx =
          decodeMidgardNativeTxFullV1FromCanonicalCbor(nativeTxCbor);
        const txId = computeMidgardNativeTxIdV1(nativeTx);
        const source = deriveMidgardNativeTxProofSourceV1(nativeTx);
        const transactionCommitment =
          computeMidgardNativeTxProofCommitmentV1(source);
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
      source: {
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

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.TX_ORDER_ID)} BYTEA PRIMARY KEY,
          ${sql(Columns.TX_ORDER_L1_TX_HASH)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.TX_ORDER_L1_TX_HASH)}) = 32),
          ${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)} INTEGER NOT NULL CHECK (${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)} >= 0),
          ${sql(Columns.ASSET_NAME)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.ASSET_NAME)}) BETWEEN 1 AND 32),
          ${sql(Columns.RAW_DATUM)} BYTEA NOT NULL,
          ${sql(Columns.TX_ID)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.TX_ID)}) = 32),
          ${sql(Columns.TX_COMPACT)} BYTEA NOT NULL,
          ${sql(Columns.FORCED_INCLUSION_VALUE)} BYTEA NOT NULL,
	          ${sql(Columns.OPERATOR_VALIDITY)} TEXT NOT NULL CHECK (${sql(Columns.OPERATOR_VALIDITY)} IN (
            'TxIsValid',
            'TxIsInvalid'
	          )),
	          ${sql(Columns.CONSENSUS_PROFILE_ID)} TEXT NOT NULL CHECK (${sql(Columns.CONSENSUS_PROFILE_ID)} = ${MIDGARD_CONSENSUS_PROFILE_V1_ID}),
	          ${sql(Columns.NATIVE_TX_CBOR)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.NATIVE_TX_CBOR)}) <= 295041),
	          ${sql(Columns.TRANSACTION_COMMITMENT)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.TRANSACTION_COMMITMENT)}) = 32),
	          ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)}) > 0),
	          ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)}) = 32),
          ${sql(Columns.INCLUSION_TIME)} TIMESTAMPTZ NOT NULL,
          ${sql(Columns.PROJECTED_HEADER_HASH)} BYTEA,
          ${sql(Columns.STATUS)} TEXT NOT NULL CHECK (${sql(Columns.STATUS)} IN ('awaiting', 'projected', 'finalized')),
          created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
          updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
          UNIQUE (${sql(Columns.TX_ORDER_L1_TX_HASH)}, ${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)}),
	          CHECK (${sql(Columns.STATUS)} <> 'awaiting' OR ${sql(Columns.PROJECTED_HEADER_HASH)} IS NULL)
	        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.STATUS}_${Columns.INCLUSION_TIME}_${Columns.TX_ORDER_ID}`,
        )} ON ${sql(tableName)} (
          ${sql(Columns.STATUS)},
          ${sql(Columns.INCLUSION_TIME)},
          ${sql(Columns.TX_ORDER_ID)}
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.PROJECTED_HEADER_HASH}`,
        )} ON ${sql(tableName)} (${sql(Columns.PROJECTED_HEADER_HASH)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.TX_ID}`,
        )} ON ${sql(tableName)} (${sql(Columns.TX_ID)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to create forced transactions table",
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
    readonly operatorValidity: SDK.MidgardTxValidity;
    readonly forcedInclusionValue: Buffer;
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
          sql`
            UPDATE ${sql(tableName)}
            SET
              ${sql(Columns.OPERATOR_VALIDITY)} = ${classification.operatorValidity},
              ${sql(Columns.FORCED_INCLUSION_VALUE)} = ${classification.forcedInclusionValue},
              ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR)} = ${classification.programMaterialSidecarCbor},
              ${sql(Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256)} = ${sha256(classification.programMaterialSidecarCbor)},
              updated_at = NOW()
            WHERE ${sql(Columns.TX_ORDER_ID)} = ${classification.txOrderId}
              AND ${sql(Columns.CONSENSUS_PROFILE_ID)} = ${MIDGARD_CONSENSUS_PROFILE_V1_ID}
            RETURNING ${sql(Columns.TX_ORDER_ID)}
          `.pipe(
            Effect.flatMap((rows) =>
              rows.length === 1
                ? Effect.void
                : Effect.fail(
                    new DatabaseError({
                      table: tableName,
                      message:
                        "Failed to persist exact V1 forced transaction classification",
                      cause: `tx_order_id=${classification.txOrderId.toString("hex")},updated=${rows.length.toString()}`,
                    }),
                  ),
            ),
          ),
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
