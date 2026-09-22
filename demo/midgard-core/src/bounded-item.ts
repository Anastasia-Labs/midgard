import { blake2b } from "@noble/hashes/blake2.js";

import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  commitMidgardValidationMerkleFrontier,
  type MidgardValidationMerkleFrontier,
  verifyMidgardValidationMerkleMembership,
} from "./validation-merkle.js";

export const MIDGARD_BOUNDED_ITEM_VERSION = 1 as const;
export const MIDGARD_BOUNDED_ITEM_CHUNK_BYTES = 4_095 as const;
export const MIDGARD_BOUNDED_ITEM_FIELD_COUNT = 9 as const;

const CHUNK_DOMAIN = Buffer.from("MidgardBoundedItemChunkV1", "ascii");
const COMMITMENT_DOMAIN = Buffer.from(
  "MidgardBoundedItemCommitmentV1",
  "ascii",
);

export type MidgardBoundedItem = {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly bytes: Buffer;
  readonly chunkHashes: readonly Hash32[];
  readonly frontier: MidgardValidationMerkleFrontier;
  readonly commitment: Hash32;
};

export type MidgardBoundedItemChunkProof = {
  readonly version: typeof MIDGARD_BOUNDED_ITEM_VERSION;
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly totalLength: number;
  readonly chunkIndex: number;
  readonly chunk: Buffer;
  readonly frontier: MidgardValidationMerkleFrontier;
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
  if (exact >= MIDGARD_BOUNDED_ITEM_FIELD_COUNT) {
    throw new Error(`unknown V1 bounded-item field index ${fieldIndex}`);
  }
  return exact;
};

export const midgardBoundedItemChunkCount = (totalLength: number): number => {
  const exactLength = exactNonNegative(totalLength, "totalLength");
  return Math.max(1, Math.ceil(exactLength / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES));
};

export const midgardBoundedItemExpectedChunkLength = ({
  totalLength,
  chunkIndex,
}: {
  readonly totalLength: number;
  readonly chunkIndex: number;
}): number => {
  const count = midgardBoundedItemChunkCount(totalLength);
  const exactChunkIndex = exactNonNegative(chunkIndex, "chunkIndex");
  if (exactChunkIndex >= count) {
    throw new Error("V1 bounded-item chunk index is out of range");
  }
  return exactChunkIndex + 1 < count
    ? MIDGARD_BOUNDED_ITEM_CHUNK_BYTES
    : totalLength - exactChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
};

export const hashMidgardBoundedItemChunk = ({
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
  if (chunk.length > MIDGARD_BOUNDED_ITEM_CHUNK_BYTES) {
    throw new Error("V1 bounded-item chunk exceeds its proof envelope");
  }
  return hash32(
    Buffer.concat([
      CHUNK_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_ITEM_VERSION),
        BigInt(exactFieldIndex(fieldIndex)),
        BigInt(exactNonNegative(itemIndex, "itemIndex")),
        BigInt(exactNonNegative(chunkIndex, "chunkIndex")),
        Buffer.from(chunk),
      ]),
    ]),
  );
};

export const commitMidgardBoundedItem = ({
  fieldIndex,
  itemIndex,
  totalLength,
  frontier,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly totalLength: number;
  readonly frontier: MidgardValidationMerkleFrontier;
}): Hash32 => {
  const count = midgardBoundedItemChunkCount(totalLength);
  if (frontier.count !== count) {
    throw new Error("V1 bounded-item frontier count does not match its length");
  }
  return hash32(
    Buffer.concat([
      COMMITMENT_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_ITEM_VERSION),
        BigInt(exactFieldIndex(fieldIndex)),
        BigInt(exactNonNegative(itemIndex, "itemIndex")),
        BigInt(exactNonNegative(totalLength, "totalLength")),
        commitMidgardValidationMerkleFrontier(frontier),
      ]),
    ]),
  );
};

export const buildMidgardBoundedItem = ({
  fieldIndex,
  itemIndex,
  bytes,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly bytes: Uint8Array;
}): MidgardBoundedItem => {
  const exactField = exactFieldIndex(fieldIndex);
  const exactItem = exactNonNegative(itemIndex, "itemIndex");
  const exactBytes = Buffer.from(bytes);
  const chunks = Array.from(
    { length: midgardBoundedItemChunkCount(exactBytes.length) },
    (_, chunkIndex) =>
      exactBytes.subarray(
        chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
        Math.min(
          exactBytes.length,
          (chunkIndex + 1) * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
        ),
      ),
  );
  const chunkHashes = chunks.map((chunk, chunkIndex) =>
    hashMidgardBoundedItemChunk({
      fieldIndex: exactField,
      itemIndex: exactItem,
      chunkIndex,
      chunk,
    }),
  );
  const frontier = buildMidgardValidationMerkleFrontier(chunkHashes);
  return {
    fieldIndex: exactField,
    itemIndex: exactItem,
    bytes: exactBytes,
    chunkHashes,
    frontier,
    commitment: commitMidgardBoundedItem({
      fieldIndex: exactField,
      itemIndex: exactItem,
      totalLength: exactBytes.length,
      frontier,
    }),
  };
};

export const buildMidgardBoundedItemChunkProof = (
  item: MidgardBoundedItem,
  chunkIndex: number,
): MidgardBoundedItemChunkProof => {
  const exactChunkIndex = exactNonNegative(chunkIndex, "chunkIndex");
  const membership = buildMidgardValidationMerkleMembership(
    item.chunkHashes,
    exactChunkIndex,
  );
  const offset = exactChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
  return {
    version: MIDGARD_BOUNDED_ITEM_VERSION,
    fieldIndex: item.fieldIndex,
    itemIndex: item.itemIndex,
    totalLength: item.bytes.length,
    chunkIndex: exactChunkIndex,
    chunk: Buffer.from(
      item.bytes.subarray(
        offset,
        offset +
          midgardBoundedItemExpectedChunkLength({
            totalLength: item.bytes.length,
            chunkIndex: exactChunkIndex,
          }),
      ),
    ),
    frontier: membership.frontier,
    siblings: membership.siblings,
  };
};

export const verifyMidgardBoundedItemChunkProof = ({
  expectedCommitment,
  proof,
}: {
  readonly expectedCommitment: Uint8Array;
  readonly proof: MidgardBoundedItemChunkProof;
}): boolean => {
  try {
    if (
      proof.version !== MIDGARD_BOUNDED_ITEM_VERSION ||
      proof.totalLength < 0 ||
      proof.chunkIndex < 0 ||
      proof.chunkIndex >= midgardBoundedItemChunkCount(proof.totalLength) ||
      proof.chunk.length !== midgardBoundedItemExpectedChunkLength(proof)
    ) {
      return false;
    }
    const leafHash = hashMidgardBoundedItemChunk(proof);
    return (
      verifyMidgardValidationMerkleMembership({
        frontier: proof.frontier,
        leafIndex: proof.chunkIndex,
        leafHash,
        siblings: proof.siblings,
      }) &&
      commitMidgardBoundedItem({
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
