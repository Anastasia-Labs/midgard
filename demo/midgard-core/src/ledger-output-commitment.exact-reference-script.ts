import { type MidgardBoundedItem } from "./bounded-item.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT,
  type MidgardValidationMerkleFrontier,
} from "./validation-merkle.js";

export const MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION = 1 as const;

export const MIDGARD_LEDGER_OUTPUT_FIELD_INDEX = 2 as const;

export const MIDGARD_CARDANO_MAX_VALUE_CBOR_BYTES = 5_000 as const;

export const MIDGARD_TX_SIZE_DERIVED_ASSET_COUNT = 16_384 as const;

export const LEDGER_OUTPUT_ASSET_LEAF_DOMAIN = Buffer.from(
  "MidgardLedgerOutputAssetLeafV1",
  "ascii",
);

export type MidgardLedgerOutputDataSummary = {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type MidgardLedgerOutputReferenceScriptLanguage = -1 | 0 | 3 | 128;

/**
 * The compact value stored in a ledger MPF leaf.
 *
 * Complete output bytes remain in DA and persistence. The descriptor binds
 * those bytes to the fixed or independently foldable facts consumed by later
 * proof phases, so no membership proof has to reveal a complete output,
 * Value, datum, or reference script.
 */
export type MidgardLedgerOutputCommitment = {
  readonly version: typeof MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION;
  readonly outputIndex: number;
  readonly totalLength: number;
  readonly itemCommitment: Hash32;
  readonly address: Buffer;
  readonly lovelace: bigint;
  readonly assetCount: number;
  readonly assetFrontierCommitment: Hash32;
  readonly cardanoValueSize: number;
  readonly referenceScriptLanguage: MidgardLedgerOutputReferenceScriptLanguage;
  readonly referenceScriptHash: Buffer;
  readonly referenceScriptTotalLength: number;
  readonly referenceScriptItemCommitment: Buffer;
  readonly cardanoTxOut: MidgardLedgerOutputDataSummary;
  readonly midgardTxOut: MidgardLedgerOutputDataSummary;
  readonly cardanoSpendDatum: MidgardLedgerOutputDataSummary;
};

export type MidgardLedgerOutputCommitmentFacts = Omit<
  MidgardLedgerOutputCommitment,
  "version" | "outputIndex" | "totalLength" | "itemCommitment"
>;

export type MidgardLedgerOutputMaterial = {
  readonly descriptor: MidgardLedgerOutputCommitment;
  readonly descriptorCbor: Buffer;
  readonly item: MidgardBoundedItem;
};

export type MidgardLedgerOutputAsset = {
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly quantity: bigint;
};

export type MidgardLedgerOutputAssetFrontier = {
  readonly count: number;
  readonly leaves: readonly Hash32[];
  readonly frontier: MidgardValidationMerkleFrontier;
  readonly commitment: Hash32;
};

export const exactOutputIndex = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff) {
    throw new Error(
      "V1 ledger output index must be an unsigned 16-bit integer",
    );
  }
  return value;
};

export const exactLength = (value: number, field = "length"): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `V1 ledger output ${field} must be a non-negative safe integer`,
    );
  }
  return value;
};

export const exactAssetCount = (value: number): number => {
  const exact = exactLength(value, "asset count");
  if (
    exact > MIDGARD_TX_SIZE_DERIVED_ASSET_COUNT ||
    exact > MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT
  ) {
    throw new Error(
      "V1 ledger output asset count exceeds the Cardano-size-derived proof envelope",
    );
  }
  return exact;
};

export const exactCardanoValueSize = (value: number): number => {
  const exact = exactLength(value, "Cardano Value size");
  if (exact > MIDGARD_CARDANO_MAX_VALUE_CBOR_BYTES) {
    throw new Error(
      "V1 ledger output Cardano Value exceeds the 5,000-byte mainnet bound",
    );
  }
  return exact;
};

export const exactUint64 = (value: bigint, field: string): bigint => {
  if (value < 0n || value > 0xffff_ffff_ffff_ffffn) {
    throw new Error(`${field} must be an unsigned 64-bit integer`);
  }
  return value;
};

export const decodedSafeInteger = (value: unknown): number => {
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      throw new Error("Decoded V1 ledger output integer is not safe");
    }
    return value;
  }
  if (
    typeof value === "bigint" &&
    value <= BigInt(Number.MAX_SAFE_INTEGER) &&
    value >= BigInt(Number.MIN_SAFE_INTEGER)
  ) {
    return Number(value);
  }
  throw new Error("Decoded V1 ledger output value is not an integer");
};

export const decodedBigInt = (value: unknown, field: string): bigint => {
  if (typeof value === "bigint") return value;
  if (typeof value === "number" && Number.isSafeInteger(value)) {
    return BigInt(value);
  }
  throw new Error(`Decoded ${field} is not an integer`);
};

export const exactBytes = (
  value: unknown,
  length: number,
  field: string,
): Buffer => {
  if (!(value instanceof Uint8Array) || value.length !== length) {
    throw new Error(`${field} must contain exactly ${length.toString()} bytes`);
  }
  return Buffer.from(value);
};

const exactOptionalBytes = (
  value: unknown,
  length: number,
  field: string,
): Buffer => {
  if (
    !(value instanceof Uint8Array) ||
    (value.length !== 0 && value.length !== length)
  ) {
    throw new Error(
      `${field} must be empty or contain exactly ${length.toString()} bytes`,
    );
  }
  return Buffer.from(value);
};

const exactSummary = (
  summary: MidgardLedgerOutputDataSummary,
  field: string,
): MidgardLedgerOutputDataSummary => ({
  root: ensureHash32(summary.root, `${field}.root`),
  cborLength: exactUint64(summary.cborLength, `${field}.cbor_length`),
  memory: exactUint64(summary.memory, `${field}.memory`),
});

export const encodeSummary = (
  summary: MidgardLedgerOutputDataSummary,
  field: string,
): readonly [Hash32, bigint, bigint] => {
  const exact = exactSummary(summary, field);
  return [exact.root, exact.cborLength, exact.memory];
};

export const decodeSummary = (
  value: unknown,
  field: string,
): MidgardLedgerOutputDataSummary => {
  if (!Array.isArray(value) || value.length !== 3) {
    throw new Error(`${field} must be a three-field summary`);
  }
  return exactSummary(
    {
      root: ensureHash32(
        exactBytes(value[0], 32, `${field}.root`),
        `${field}.root`,
      ),
      cborLength: decodedBigInt(value[1], `${field}.cbor_length`),
      memory: decodedBigInt(value[2], `${field}.memory`),
    },
    field,
  );
};

const exactReferenceScriptLanguage = (
  value: number,
): MidgardLedgerOutputReferenceScriptLanguage => {
  if (value !== -1 && value !== 0 && value !== 3 && value !== 128) {
    throw new Error("Invalid V1 ledger output reference-script language");
  }
  return value;
};

export const exactReferenceScript = ({
  language,
  hash,
  totalLength,
  itemCommitment,
}: {
  readonly language: number;
  readonly hash: unknown;
  readonly totalLength: number;
  readonly itemCommitment: unknown;
}): {
  readonly language: MidgardLedgerOutputReferenceScriptLanguage;
  readonly hash: Buffer;
  readonly totalLength: number;
  readonly itemCommitment: Buffer;
} => {
  const exactLanguage = exactReferenceScriptLanguage(language);
  const exactHash = exactOptionalBytes(
    hash,
    28,
    "ledger_output_commitment_v1.reference_script_hash",
  );
  const exactTotalLength = exactLength(totalLength, "reference-script length");
  const exactCommitment = exactOptionalBytes(
    itemCommitment,
    32,
    "ledger_output_commitment_v1.reference_script_item_commitment",
  );
  const absent = exactLanguage === -1;
  if (
    absent !==
    (exactHash.length === 0 &&
      exactTotalLength === 0 &&
      exactCommitment.length === 0)
  ) {
    throw new Error(
      "V1 ledger output reference-script absence fields are inconsistent",
    );
  }
  if (
    !absent &&
    (exactHash.length !== 28 ||
      exactTotalLength === 0 ||
      exactCommitment.length !== 32)
  ) {
    throw new Error(
      "V1 ledger output reference-script descriptor is incomplete",
    );
  }
  return {
    language: exactLanguage,
    hash: exactHash,
    totalLength: exactTotalLength,
    itemCommitment: exactCommitment,
  };
};
