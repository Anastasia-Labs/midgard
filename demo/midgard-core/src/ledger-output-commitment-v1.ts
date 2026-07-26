import { blake2b } from "@noble/hashes/blake2.js";

import {
  buildMidgardBoundedItemV1,
  type MidgardBoundedItemChunkProofV1,
  type MidgardBoundedItemV1,
  verifyMidgardBoundedItemChunkProofV1,
} from "./bounded-item-v1.js";
import { encodeMidgardAddressBytes } from "./codec/address.js";
import {
  decodeSingleCbor,
  encodeCbor,
} from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  buildMidgardValidationMerkleFrontierV1,
  commitMidgardValidationMerkleFrontierV1,
  MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT,
  type MidgardValidationMerkleFrontierV1,
} from "./validation-merkle.js";

export const MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION = 1 as const;
export const MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1 = 2 as const;
export const MIDGARD_CARDANO_MAX_VALUE_CBOR_BYTES_V1 = 5_000 as const;
export const MIDGARD_TX_SIZE_DERIVED_ASSET_COUNT_V1 = 16_384 as const;

const LEDGER_OUTPUT_ASSET_LEAF_DOMAIN = Buffer.from(
  "MidgardLedgerOutputAssetLeafV1",
  "ascii",
);

export type MidgardLedgerOutputDataSummaryV1 = {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type MidgardLedgerOutputReferenceScriptLanguageV1 =
  | -1
  | 0
  | 3
  | 128;

/**
 * The compact value stored in a ledger MPF leaf.
 *
 * Complete output bytes remain in DA and persistence. The descriptor binds
 * those bytes to the fixed or independently foldable facts consumed by later
 * proof phases, so no membership proof has to reveal a complete output,
 * Value, datum, or reference script.
 */
export type MidgardLedgerOutputCommitmentV1 = {
  readonly version: typeof MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION;
  readonly outputIndex: number;
  readonly totalLength: number;
  readonly itemCommitment: Hash32;
  readonly address: Buffer;
  readonly lovelace: bigint;
  readonly assetCount: number;
  readonly assetFrontierCommitment: Hash32;
  readonly cardanoValueSize: number;
  readonly referenceScriptLanguage: MidgardLedgerOutputReferenceScriptLanguageV1;
  readonly referenceScriptHash: Buffer;
  readonly referenceScriptTotalLength: number;
  readonly referenceScriptItemCommitment: Buffer;
  readonly cardanoTxOut: MidgardLedgerOutputDataSummaryV1;
  readonly midgardTxOut: MidgardLedgerOutputDataSummaryV1;
  readonly cardanoSpendDatum: MidgardLedgerOutputDataSummaryV1;
};

export type MidgardLedgerOutputCommitmentFactsV1 = Omit<
  MidgardLedgerOutputCommitmentV1,
  "version" | "outputIndex" | "totalLength" | "itemCommitment"
>;

export type MidgardLedgerOutputMaterialV1 = {
  readonly descriptor: MidgardLedgerOutputCommitmentV1;
  readonly descriptorCbor: Buffer;
  readonly item: MidgardBoundedItemV1;
};

export type MidgardLedgerOutputAssetV1 = {
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly quantity: bigint;
};

export type MidgardLedgerOutputAssetFrontierV1 = {
  readonly count: number;
  readonly leaves: readonly Hash32[];
  readonly frontier: MidgardValidationMerkleFrontierV1;
  readonly commitment: Hash32;
};

const exactOutputIndex = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff) {
    throw new Error(
      "V1 ledger output index must be an unsigned 16-bit integer",
    );
  }
  return value;
};

const exactLength = (value: number, field = "length"): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `V1 ledger output ${field} must be a non-negative safe integer`,
    );
  }
  return value;
};

const exactAssetCount = (value: number): number => {
  const exact = exactLength(value, "asset count");
  if (
    exact > MIDGARD_TX_SIZE_DERIVED_ASSET_COUNT_V1 ||
    exact > MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT
  ) {
    throw new Error(
      "V1 ledger output asset count exceeds the Cardano-size-derived proof envelope",
    );
  }
  return exact;
};

const exactCardanoValueSize = (value: number): number => {
  const exact = exactLength(value, "Cardano Value size");
  if (exact > MIDGARD_CARDANO_MAX_VALUE_CBOR_BYTES_V1) {
    throw new Error(
      "V1 ledger output Cardano Value exceeds the 5,000-byte mainnet bound",
    );
  }
  return exact;
};

const exactUint64 = (value: bigint, field: string): bigint => {
  if (value < 0n || value > 0xffff_ffff_ffff_ffffn) {
    throw new Error(`${field} must be an unsigned 64-bit integer`);
  }
  return value;
};

const decodedSafeInteger = (value: unknown): number => {
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

const decodedBigInt = (value: unknown, field: string): bigint => {
  if (typeof value === "bigint") return value;
  if (typeof value === "number" && Number.isSafeInteger(value)) {
    return BigInt(value);
  }
  throw new Error(`Decoded ${field} is not an integer`);
};

const exactBytes = (
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
  summary: MidgardLedgerOutputDataSummaryV1,
  field: string,
): MidgardLedgerOutputDataSummaryV1 => ({
  root: ensureHash32(summary.root, `${field}.root`),
  cborLength: exactUint64(summary.cborLength, `${field}.cbor_length`),
  memory: exactUint64(summary.memory, `${field}.memory`),
});

const encodeSummary = (
  summary: MidgardLedgerOutputDataSummaryV1,
  field: string,
): readonly [Hash32, bigint, bigint] => {
  const exact = exactSummary(summary, field);
  return [exact.root, exact.cborLength, exact.memory];
};

const decodeSummary = (
  value: unknown,
  field: string,
): MidgardLedgerOutputDataSummaryV1 => {
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
): MidgardLedgerOutputReferenceScriptLanguageV1 => {
  if (value !== -1 && value !== 0 && value !== 3 && value !== 128) {
    throw new Error("Invalid V1 ledger output reference-script language");
  }
  return value;
};

const exactReferenceScript = ({
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
  readonly language: MidgardLedgerOutputReferenceScriptLanguageV1;
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
  const exactTotalLength = exactLength(
    totalLength,
    "reference-script length",
  );
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

export const encodeMidgardLedgerOutputCommitmentV1 = (
  descriptor: MidgardLedgerOutputCommitmentV1,
): Buffer => {
  if (
    descriptor.version !== MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION
  ) {
    throw new Error("Invalid V1 ledger output commitment version");
  }
  const address = encodeMidgardAddressBytes(descriptor.address);
  const referenceScript = exactReferenceScript({
    language: descriptor.referenceScriptLanguage,
    hash: descriptor.referenceScriptHash,
    totalLength: descriptor.referenceScriptTotalLength,
    itemCommitment: descriptor.referenceScriptItemCommitment,
  });
  return encodeCbor([
    BigInt(MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION),
    BigInt(exactOutputIndex(descriptor.outputIndex)),
    BigInt(exactLength(descriptor.totalLength)),
    ensureHash32(
      descriptor.itemCommitment,
      "ledger_output_commitment_v1.item_commitment",
    ),
    address,
    exactUint64(descriptor.lovelace, "ledger_output_commitment_v1.lovelace"),
    BigInt(exactAssetCount(descriptor.assetCount)),
    ensureHash32(
      descriptor.assetFrontierCommitment,
      "ledger_output_commitment_v1.asset_frontier_commitment",
    ),
    BigInt(exactCardanoValueSize(descriptor.cardanoValueSize)),
    BigInt(referenceScript.language),
    referenceScript.hash,
    BigInt(referenceScript.totalLength),
    referenceScript.itemCommitment,
    encodeSummary(descriptor.cardanoTxOut, "cardano_tx_out"),
    encodeSummary(descriptor.midgardTxOut, "midgard_tx_out"),
    encodeSummary(descriptor.cardanoSpendDatum, "cardano_spend_datum"),
  ]);
};

export const decodeMidgardLedgerOutputCommitmentV1 = (
  bytes: Uint8Array,
): MidgardLedgerOutputCommitmentV1 => {
  const value = decodeSingleCbor(bytes);
  if (
    !Array.isArray(value) ||
    value.length !== 16 ||
    !(value[3] instanceof Uint8Array) ||
    !(value[4] instanceof Uint8Array)
  ) {
    throw new Error("Invalid V1 ledger output commitment descriptor");
  }
  const decodedVersion = decodedSafeInteger(value[0]);
  if (decodedVersion !== MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION) {
    throw new Error("Invalid V1 ledger output commitment version");
  }
  const referenceScript = exactReferenceScript({
    language: decodedSafeInteger(value[9]),
    hash: value[10],
    totalLength: decodedSafeInteger(value[11]),
    itemCommitment: value[12],
  });
  const descriptor: MidgardLedgerOutputCommitmentV1 = {
    version: MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION,
    outputIndex: exactOutputIndex(decodedSafeInteger(value[1])),
    totalLength: exactLength(decodedSafeInteger(value[2])),
    itemCommitment: ensureHash32(
      value[3],
      "ledger_output_commitment_v1.item_commitment",
    ),
    address: encodeMidgardAddressBytes(value[4]),
    lovelace: exactUint64(
      decodedBigInt(value[5], "ledger output lovelace"),
      "ledger_output_commitment_v1.lovelace",
    ),
    assetCount: exactAssetCount(decodedSafeInteger(value[6])),
    assetFrontierCommitment: ensureHash32(
      exactBytes(
        value[7],
        32,
        "ledger_output_commitment_v1.asset_frontier_commitment",
      ),
      "ledger_output_commitment_v1.asset_frontier_commitment",
    ),
    cardanoValueSize: exactCardanoValueSize(decodedSafeInteger(value[8])),
    referenceScriptLanguage: referenceScript.language,
    referenceScriptHash: referenceScript.hash,
    referenceScriptTotalLength: referenceScript.totalLength,
    referenceScriptItemCommitment: referenceScript.itemCommitment,
    cardanoTxOut: decodeSummary(value[13], "cardano_tx_out"),
    midgardTxOut: decodeSummary(value[14], "midgard_tx_out"),
    cardanoSpendDatum: decodeSummary(value[15], "cardano_spend_datum"),
  };
  const canonical = encodeMidgardLedgerOutputCommitmentV1(descriptor);
  if (!canonical.equals(Buffer.from(bytes))) {
    throw new Error(
      "V1 ledger output commitment descriptor is not canonical CBOR",
    );
  }
  return descriptor;
};

export const buildMidgardLedgerOutputMaterialV1 = ({
  outputIndex,
  outputCbor,
  facts,
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
  readonly facts: MidgardLedgerOutputCommitmentFactsV1;
}): MidgardLedgerOutputMaterialV1 => {
  const item = buildMidgardBoundedItemV1({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
    itemIndex: exactOutputIndex(outputIndex),
    bytes: outputCbor,
  });
  const descriptor: MidgardLedgerOutputCommitmentV1 = {
    version: MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION,
    outputIndex: item.itemIndex,
    totalLength: item.bytes.length,
    itemCommitment: item.commitment,
    ...facts,
  };
  return {
    descriptor,
    descriptorCbor: encodeMidgardLedgerOutputCommitmentV1(descriptor),
    item,
  };
};

export const hashMidgardLedgerOutputAssetLeafV1 = ({
  policyId,
  assetName,
  quantity,
}: MidgardLedgerOutputAssetV1): Hash32 => {
  if (policyId.length !== 28) {
    throw new Error("V1 ledger output asset policy id must be 28 bytes");
  }
  if (assetName.length > 32) {
    throw new Error(
      "V1 ledger output asset name must contain at most 32 bytes",
    );
  }
  if (quantity <= 0n) {
    throw new Error("V1 ledger output asset quantity must be positive");
  }
  return ensureHash32(
    blake2b(
      Buffer.concat([
        LEDGER_OUTPUT_ASSET_LEAF_DOMAIN,
        encodeCbor([policyId, assetName, quantity]),
      ]),
      { dkLen: 32 },
    ),
    "ledger_output_asset_leaf_v1",
  );
};

export const buildMidgardLedgerOutputAssetFrontierV1 = (
  assets: readonly MidgardLedgerOutputAssetV1[],
): MidgardLedgerOutputAssetFrontierV1 => {
  for (let index = 1; index < assets.length; index += 1) {
    const previous = assets[index - 1]!;
    const current = assets[index]!;
    const policyOrder = Buffer.compare(previous.policyId, current.policyId);
    const assetNameOrder =
      previous.assetName.length - current.assetName.length ||
      Buffer.compare(previous.assetName, current.assetName);
    if (
      policyOrder > 0 ||
      (policyOrder === 0 && assetNameOrder >= 0)
    ) {
      throw new Error(
        "V1 ledger output assets must be in canonical policy/name order",
      );
    }
  }
  const leaves = assets.map(hashMidgardLedgerOutputAssetLeafV1);
  const frontier = buildMidgardValidationMerkleFrontierV1(leaves);
  return {
    count: leaves.length,
    leaves,
    frontier,
    commitment: commitMidgardValidationMerkleFrontierV1(frontier),
  };
};

export const verifyMidgardLedgerOutputChunkV1 = ({
  descriptor,
  proof,
}: {
  readonly descriptor: MidgardLedgerOutputCommitmentV1;
  readonly proof: MidgardBoundedItemChunkProofV1;
}): boolean =>
  descriptor.version === MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION &&
  proof.fieldIndex === MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1 &&
  proof.itemIndex === descriptor.outputIndex &&
  proof.totalLength === descriptor.totalLength &&
  verifyMidgardBoundedItemChunkProofV1({
    expectedCommitment: descriptor.itemCommitment,
    proof,
  });

export const verifyMidgardLedgerOutputReferenceScriptChunkV1 = ({
  descriptor,
  proof,
}: {
  readonly descriptor: MidgardLedgerOutputCommitmentV1;
  readonly proof: MidgardBoundedItemChunkProofV1;
}): boolean =>
  descriptor.version === MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION &&
  descriptor.referenceScriptLanguage !== -1 &&
  descriptor.referenceScriptItemCommitment.length === 32 &&
  proof.fieldIndex === MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1 &&
  proof.itemIndex === descriptor.outputIndex &&
  proof.totalLength === descriptor.referenceScriptTotalLength &&
  verifyMidgardBoundedItemChunkProofV1({
    expectedCommitment: descriptor.referenceScriptItemCommitment,
    proof,
  });
