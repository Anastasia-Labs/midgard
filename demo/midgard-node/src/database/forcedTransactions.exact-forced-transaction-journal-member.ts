import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "@al-ft/midgard-core/codec/cbor";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_ID,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

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

export const projectedEventsTable = {
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

export const projectedEventAdapter =
  ProjectedEvents.makeProjectedEventAdapter<Entry>({
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

export const sameImmutablePayload = (left: Entry, right: Entry): boolean => {
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
