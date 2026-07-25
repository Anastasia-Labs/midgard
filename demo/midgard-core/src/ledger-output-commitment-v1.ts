import {
  buildMidgardBoundedItemV1,
  type MidgardBoundedItemV1,
  type MidgardBoundedItemChunkProofV1,
  verifyMidgardBoundedItemChunkProofV1,
} from "./bounded-item-v1.js";
import {
  decodeSingleCbor,
  encodeCbor,
} from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

export const MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION = 1 as const;
export const MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1 = 2 as const;

export type MidgardLedgerOutputCommitmentV1 = {
  readonly version: typeof MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION;
  readonly outputIndex: number;
  readonly totalLength: number;
  readonly itemCommitment: Hash32;
};

export type MidgardLedgerOutputMaterialV1 = {
  readonly descriptor: MidgardLedgerOutputCommitmentV1;
  readonly descriptorCbor: Buffer;
  readonly item: MidgardBoundedItemV1;
};

const exactOutputIndex = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff) {
    throw new Error(
      "V1 ledger output index must be an unsigned 16-bit integer",
    );
  }
  return value;
};

const exactLength = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      "V1 ledger output length must be a non-negative safe integer",
    );
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

export const encodeMidgardLedgerOutputCommitmentV1 = (
  descriptor: MidgardLedgerOutputCommitmentV1,
): Buffer =>
  encodeCbor([
    BigInt(MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION),
    BigInt(exactOutputIndex(descriptor.outputIndex)),
    BigInt(exactLength(descriptor.totalLength)),
    ensureHash32(
      descriptor.itemCommitment,
      "ledger_output_commitment_v1.item_commitment",
    ),
  ]);

export const decodeMidgardLedgerOutputCommitmentV1 = (
  bytes: Uint8Array,
): MidgardLedgerOutputCommitmentV1 => {
  const value = decodeSingleCbor(bytes);
  if (
    !Array.isArray(value) ||
    value.length !== 4 ||
    !(value[3] instanceof Uint8Array)
  ) {
    throw new Error("Invalid V1 ledger output commitment descriptor");
  }
  const decodedVersion = decodedSafeInteger(value[0]);
  const decodedOutputIndex = decodedSafeInteger(value[1]);
  const decodedTotalLength = decodedSafeInteger(value[2]);
  if (decodedVersion !== MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION) {
    throw new Error("Invalid V1 ledger output commitment version");
  }
  const descriptor: MidgardLedgerOutputCommitmentV1 = {
    version: MIDGARD_LEDGER_OUTPUT_COMMITMENT_V1_VERSION,
    outputIndex: exactOutputIndex(decodedOutputIndex),
    totalLength: exactLength(decodedTotalLength),
    itemCommitment: ensureHash32(
      value[3],
      "ledger_output_commitment_v1.item_commitment",
    ),
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
}: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
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
  };
  return {
    descriptor,
    descriptorCbor: encodeMidgardLedgerOutputCommitmentV1(descriptor),
    item,
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
