import { blake2b } from "@noble/hashes/blake2.js";

import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  commitMidgardValidationMerkleFrontier,
  type MidgardValidationMerkleFrontier,
  type MidgardValidationMerkleMembership,
  verifyMidgardValidationMerkleMembership,
} from "./validation-merkle.js";

export const MIDGARD_BOUNDED_BLOB_VERSION = 1 as const;
export const MIDGARD_BOUNDED_BLOB_CHUNK_BYTES = 4_095 as const;
export const MIDGARD_BOUNDED_BLOB_FIELD_COUNT = 9 as const;

const CHUNK_DOMAIN = Buffer.from("MidgardBoundedBlobChunkV1", "ascii");
const COMMITMENT_DOMAIN = Buffer.from(
  "MidgardBoundedBlobCommitmentV1",
  "ascii",
);

export type MidgardBoundedBlob = {
  readonly fieldIndex: number;
  readonly totalLength: number;
  readonly chunks: readonly Buffer[];
  readonly leafHashes: readonly Hash32[];
  readonly frontier: MidgardValidationMerkleFrontier;
  readonly commitment: Hash32;
};

export type MidgardBoundedBlobChunkProof = {
  readonly version: typeof MIDGARD_BOUNDED_BLOB_VERSION;
  readonly fieldIndex: number;
  readonly totalLength: number;
  readonly chunkIndex: number;
  readonly chunk: Buffer;
  readonly frontier: MidgardValidationMerkleFrontier;
  readonly siblings: readonly Hash32[];
};

const hash32 = (value: Uint8Array): Hash32 =>
  ensureHash32(blake2b(value, { dkLen: 32 }), "bounded_blob_v1.commitment");

const boundedFieldIndex = (fieldIndex: number): number => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_BOUNDED_BLOB_FIELD_COUNT
  ) {
    throw new Error(`unknown V1 bounded-blob field index ${fieldIndex}`);
  }
  return fieldIndex;
};

const boundedLength = (totalLength: number): number => {
  if (!Number.isSafeInteger(totalLength) || totalLength < 0) {
    throw new Error(
      "V1 bounded-blob length must be a non-negative safe integer",
    );
  }
  return totalLength;
};

export const midgardBoundedBlobChunkCount = (totalLength: number): number => {
  const length = boundedLength(totalLength);
  return length === 0
    ? 0
    : Math.ceil(length / MIDGARD_BOUNDED_BLOB_CHUNK_BYTES);
};

export const midgardBoundedBlobChunkLength = ({
  totalLength,
  chunkIndex,
}: {
  readonly totalLength: number;
  readonly chunkIndex: number;
}): number => {
  const count = midgardBoundedBlobChunkCount(totalLength);
  if (
    !Number.isSafeInteger(chunkIndex) ||
    chunkIndex < 0 ||
    chunkIndex >= count
  ) {
    throw new Error("V1 bounded-blob chunk index is out of range");
  }
  return chunkIndex + 1 < count
    ? MIDGARD_BOUNDED_BLOB_CHUNK_BYTES
    : totalLength - chunkIndex * MIDGARD_BOUNDED_BLOB_CHUNK_BYTES;
};

export const hashMidgardBoundedBlobChunk = ({
  fieldIndex,
  chunkIndex,
  chunk,
}: {
  readonly fieldIndex: number;
  readonly chunkIndex: number;
  readonly chunk: Uint8Array;
}): Hash32 => {
  const exactFieldIndex = boundedFieldIndex(fieldIndex);
  if (!Number.isSafeInteger(chunkIndex) || chunkIndex < 0) {
    throw new Error("V1 bounded-blob chunk index must be non-negative");
  }
  const bytes = Buffer.from(chunk);
  if (bytes.length > MIDGARD_BOUNDED_BLOB_CHUNK_BYTES) {
    throw new Error(
      `V1 bounded-blob chunk exceeds ${MIDGARD_BOUNDED_BLOB_CHUNK_BYTES.toString()} bytes`,
    );
  }
  return hash32(
    Buffer.concat([
      CHUNK_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_BLOB_VERSION),
        BigInt(exactFieldIndex),
        BigInt(chunkIndex),
        bytes,
      ]),
    ]),
  );
};

export const commitMidgardBoundedBlob = ({
  fieldIndex,
  totalLength,
  frontier,
}: {
  readonly fieldIndex: number;
  readonly totalLength: number;
  readonly frontier: MidgardValidationMerkleFrontier;
}): Hash32 => {
  const exactFieldIndex = boundedFieldIndex(fieldIndex);
  const exactLength = boundedLength(totalLength);
  const expectedCount = midgardBoundedBlobChunkCount(exactLength);
  if (frontier.count !== expectedCount) {
    throw new Error(
      `V1 bounded-blob frontier count does not match its length: ${frontier.count.toString()} != ${expectedCount.toString()}`,
    );
  }
  return hash32(
    Buffer.concat([
      COMMITMENT_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_BLOB_VERSION),
        BigInt(exactFieldIndex),
        BigInt(exactLength),
        commitMidgardValidationMerkleFrontier(frontier),
      ]),
    ]),
  );
};

export const buildMidgardBoundedBlob = ({
  fieldIndex,
  bytes,
}: {
  readonly fieldIndex: number;
  readonly bytes: Uint8Array;
}): MidgardBoundedBlob => {
  const exactFieldIndex = boundedFieldIndex(fieldIndex);
  const source = Buffer.from(bytes);
  const chunks: Buffer[] = [];
  for (
    let offset = 0;
    offset < source.length;
    offset += MIDGARD_BOUNDED_BLOB_CHUNK_BYTES
  ) {
    chunks.push(
      source.subarray(offset, offset + MIDGARD_BOUNDED_BLOB_CHUNK_BYTES),
    );
  }
  const leafHashes = chunks.map((chunk, chunkIndex) =>
    hashMidgardBoundedBlobChunk({
      fieldIndex: exactFieldIndex,
      chunkIndex,
      chunk,
    }),
  );
  const frontier = buildMidgardValidationMerkleFrontier(leafHashes);
  return {
    fieldIndex: exactFieldIndex,
    totalLength: source.length,
    chunks,
    leafHashes,
    frontier,
    commitment: commitMidgardBoundedBlob({
      fieldIndex: exactFieldIndex,
      totalLength: source.length,
      frontier,
    }),
  };
};

export const buildMidgardBoundedBlobChunkProof = (
  blob: MidgardBoundedBlob,
  chunkIndex: number,
): MidgardBoundedBlobChunkProof => {
  const membership: MidgardValidationMerkleMembership =
    buildMidgardValidationMerkleMembership(blob.leafHashes, chunkIndex);
  return {
    version: MIDGARD_BOUNDED_BLOB_VERSION,
    fieldIndex: blob.fieldIndex,
    totalLength: blob.totalLength,
    chunkIndex,
    chunk: Buffer.from(blob.chunks[chunkIndex]!),
    frontier: membership.frontier,
    siblings: membership.siblings,
  };
};

export const verifyMidgardBoundedBlobChunkProof = ({
  expectedCommitment,
  proof,
}: {
  readonly expectedCommitment: Uint8Array;
  readonly proof: MidgardBoundedBlobChunkProof;
}): boolean => {
  try {
    if (proof.version !== MIDGARD_BOUNDED_BLOB_VERSION) return false;
    const expectedChunkLength = midgardBoundedBlobChunkLength({
      totalLength: proof.totalLength,
      chunkIndex: proof.chunkIndex,
    });
    if (proof.chunk.length !== expectedChunkLength) return false;
    const leafHash = hashMidgardBoundedBlobChunk({
      fieldIndex: proof.fieldIndex,
      chunkIndex: proof.chunkIndex,
      chunk: proof.chunk,
    });
    if (
      !verifyMidgardValidationMerkleMembership({
        frontier: proof.frontier,
        leafIndex: proof.chunkIndex,
        leafHash,
        siblings: proof.siblings,
      })
    ) {
      return false;
    }
    return commitMidgardBoundedBlob({
      fieldIndex: proof.fieldIndex,
      totalLength: proof.totalLength,
      frontier: proof.frontier,
    }).equals(ensureHash32(expectedCommitment, "bounded_blob_v1.expected"));
  } catch {
    return false;
  }
};
