import { blake2b } from "@noble/hashes/blake2.js";

import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  buildMidgardValidationMerkleFrontierV1,
  buildMidgardValidationMerkleMembershipV1,
  commitMidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleFrontierV1,
  verifyMidgardValidationMerkleMembershipV1,
} from "./validation-merkle.js";

export const MIDGARD_BOUNDED_ITEM_V1_VERSION = 1 as const;
export const MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1 = 4_095 as const;
export const MIDGARD_BOUNDED_ITEM_FIELD_COUNT_V1 = 9 as const;

const CHUNK_DOMAIN = Buffer.from("MidgardBoundedItemChunkV1", "ascii");
const COMMITMENT_DOMAIN = Buffer.from(
  "MidgardBoundedItemCommitmentV1",
  "ascii",
);

export type MidgardBoundedItemV1 = {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly bytes: Buffer;
  readonly chunkHashes: readonly Hash32[];
  readonly frontier: MidgardValidationMerkleFrontierV1;
  readonly commitment: Hash32;
};

export type MidgardBoundedItemChunkProofV1 = {
  readonly version: typeof MIDGARD_BOUNDED_ITEM_V1_VERSION;
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly totalLength: number;
  readonly chunkIndex: number;
  readonly chunk: Buffer;
  readonly frontier: MidgardValidationMerkleFrontierV1;
  readonly siblings: readonly Hash32[];
};

const hash32 = (value: Uint8Array): Hash32 =>
  ensureHash32(blake2b(value, { dkLen: 32 }), "bounded_item_v1.hash");

const exactNonNegative = (value: number, name: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${name} must be a non-negative safe integer`);
  }
  return value;
};

const exactFieldIndex = (fieldIndex: number): number => {
  const exact = exactNonNegative(fieldIndex, "fieldIndex");
  if (exact >= MIDGARD_BOUNDED_ITEM_FIELD_COUNT_V1) {
    throw new Error(`unknown V1 bounded-item field index ${fieldIndex}`);
  }
  return exact;
};

export const midgardBoundedItemChunkCountV1 = (totalLength: number): number => {
  const exactLength = exactNonNegative(totalLength, "totalLength");
  return Math.max(
    1,
    Math.ceil(exactLength / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1),
  );
};

export const midgardBoundedItemExpectedChunkLengthV1 = ({
  totalLength,
  chunkIndex,
}: {
  readonly totalLength: number;
  readonly chunkIndex: number;
}): number => {
  const count = midgardBoundedItemChunkCountV1(totalLength);
  const exactChunkIndex = exactNonNegative(chunkIndex, "chunkIndex");
  if (exactChunkIndex >= count) {
    throw new Error("V1 bounded-item chunk index is out of range");
  }
  return exactChunkIndex + 1 < count
    ? MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1
    : totalLength - exactChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
};

export const hashMidgardBoundedItemChunkV1 = ({
  fieldIndex,
  itemIndex,
  chunkIndex,
  chunk,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly chunkIndex: number;
  readonly chunk: Uint8Array;
}): Hash32 => {
  if (chunk.length > MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1) {
    throw new Error("V1 bounded-item chunk exceeds its proof envelope");
  }
  return hash32(
    Buffer.concat([
      CHUNK_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_ITEM_V1_VERSION),
        BigInt(exactFieldIndex(fieldIndex)),
        BigInt(exactNonNegative(itemIndex, "itemIndex")),
        BigInt(exactNonNegative(chunkIndex, "chunkIndex")),
        Buffer.from(chunk),
      ]),
    ]),
  );
};

export const commitMidgardBoundedItemV1 = ({
  fieldIndex,
  itemIndex,
  totalLength,
  frontier,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly totalLength: number;
  readonly frontier: MidgardValidationMerkleFrontierV1;
}): Hash32 => {
  const count = midgardBoundedItemChunkCountV1(totalLength);
  if (frontier.count !== count) {
    throw new Error("V1 bounded-item frontier count does not match its length");
  }
  return hash32(
    Buffer.concat([
      COMMITMENT_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_ITEM_V1_VERSION),
        BigInt(exactFieldIndex(fieldIndex)),
        BigInt(exactNonNegative(itemIndex, "itemIndex")),
        BigInt(exactNonNegative(totalLength, "totalLength")),
        commitMidgardValidationMerkleFrontierV1(frontier),
      ]),
    ]),
  );
};

export const buildMidgardBoundedItemV1 = ({
  fieldIndex,
  itemIndex,
  bytes,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly bytes: Uint8Array;
}): MidgardBoundedItemV1 => {
  const exactField = exactFieldIndex(fieldIndex);
  const exactItem = exactNonNegative(itemIndex, "itemIndex");
  const exactBytes = Buffer.from(bytes);
  const chunks = Array.from(
    { length: midgardBoundedItemChunkCountV1(exactBytes.length) },
    (_, chunkIndex) =>
      exactBytes.subarray(
        chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
        Math.min(
          exactBytes.length,
          (chunkIndex + 1) * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
        ),
      ),
  );
  const chunkHashes = chunks.map((chunk, chunkIndex) =>
    hashMidgardBoundedItemChunkV1({
      fieldIndex: exactField,
      itemIndex: exactItem,
      chunkIndex,
      chunk,
    }),
  );
  const frontier = buildMidgardValidationMerkleFrontierV1(chunkHashes);
  return {
    fieldIndex: exactField,
    itemIndex: exactItem,
    bytes: exactBytes,
    chunkHashes,
    frontier,
    commitment: commitMidgardBoundedItemV1({
      fieldIndex: exactField,
      itemIndex: exactItem,
      totalLength: exactBytes.length,
      frontier,
    }),
  };
};

export const buildMidgardBoundedItemChunkProofV1 = (
  item: MidgardBoundedItemV1,
  chunkIndex: number,
): MidgardBoundedItemChunkProofV1 => {
  const exactChunkIndex = exactNonNegative(chunkIndex, "chunkIndex");
  const membership = buildMidgardValidationMerkleMembershipV1(
    item.chunkHashes,
    exactChunkIndex,
  );
  const offset = exactChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
  return {
    version: MIDGARD_BOUNDED_ITEM_V1_VERSION,
    fieldIndex: item.fieldIndex,
    itemIndex: item.itemIndex,
    totalLength: item.bytes.length,
    chunkIndex: exactChunkIndex,
    chunk: Buffer.from(
      item.bytes.subarray(
        offset,
        offset +
          midgardBoundedItemExpectedChunkLengthV1({
            totalLength: item.bytes.length,
            chunkIndex: exactChunkIndex,
          }),
      ),
    ),
    frontier: membership.frontier,
    siblings: membership.siblings,
  };
};

export const verifyMidgardBoundedItemChunkProofV1 = ({
  expectedCommitment,
  proof,
}: {
  readonly expectedCommitment: Uint8Array;
  readonly proof: MidgardBoundedItemChunkProofV1;
}): boolean => {
  try {
    if (
      proof.version !== MIDGARD_BOUNDED_ITEM_V1_VERSION ||
      proof.totalLength < 0 ||
      proof.chunkIndex < 0 ||
      proof.chunkIndex >= midgardBoundedItemChunkCountV1(proof.totalLength) ||
      proof.chunk.length !== midgardBoundedItemExpectedChunkLengthV1(proof)
    ) {
      return false;
    }
    const leafHash = hashMidgardBoundedItemChunkV1(proof);
    return (
      verifyMidgardValidationMerkleMembershipV1({
        frontier: proof.frontier,
        leafIndex: proof.chunkIndex,
        leafHash,
        siblings: proof.siblings,
      }) &&
      commitMidgardBoundedItemV1({
        fieldIndex: proof.fieldIndex,
        itemIndex: proof.itemIndex,
        totalLength: proof.totalLength,
        frontier: proof.frontier,
      }).equals(ensureHash32(expectedCommitment, "bounded_item_v1.expected"))
    );
  } catch {
    return false;
  }
};
