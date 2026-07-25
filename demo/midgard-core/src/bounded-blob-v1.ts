import { blake2b } from "@noble/hashes/blake2.js";

import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  buildMidgardValidationMerkleFrontierV1,
  buildMidgardValidationMerkleMembershipV1,
  commitMidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleMembershipV1,
  verifyMidgardValidationMerkleMembershipV1,
} from "./validation-merkle.js";

export const MIDGARD_BOUNDED_BLOB_V1_VERSION = 1 as const;
export const MIDGARD_BOUNDED_BLOB_CHUNK_BYTES_V1 = 4_095 as const;
export const MIDGARD_BOUNDED_BLOB_FIELD_COUNT_V1 = 9 as const;

const CHUNK_DOMAIN = Buffer.from("MidgardBoundedBlobChunkV1", "ascii");
const COMMITMENT_DOMAIN = Buffer.from(
  "MidgardBoundedBlobCommitmentV1",
  "ascii",
);

export type MidgardBoundedBlobV1 = {
  readonly fieldIndex: number;
  readonly totalLength: number;
  readonly chunks: readonly Buffer[];
  readonly leafHashes: readonly Hash32[];
  readonly frontier: MidgardValidationMerkleFrontierV1;
  readonly commitment: Hash32;
};

export type MidgardBoundedBlobChunkProofV1 = {
  readonly version: typeof MIDGARD_BOUNDED_BLOB_V1_VERSION;
  readonly fieldIndex: number;
  readonly totalLength: number;
  readonly chunkIndex: number;
  readonly chunk: Buffer;
  readonly frontier: MidgardValidationMerkleFrontierV1;
  readonly siblings: readonly Hash32[];
};

const hash32 = (value: Uint8Array): Hash32 =>
  ensureHash32(
    blake2b(value, { dkLen: 32 }),
    "bounded_blob_v1.commitment",
  );

const boundedFieldIndex = (fieldIndex: number): number => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_BOUNDED_BLOB_FIELD_COUNT_V1
  ) {
    throw new Error(`unknown V1 bounded-blob field index ${fieldIndex}`);
  }
  return fieldIndex;
};

const boundedLength = (totalLength: number): number => {
  if (!Number.isSafeInteger(totalLength) || totalLength < 0) {
    throw new Error("V1 bounded-blob length must be a non-negative safe integer");
  }
  return totalLength;
};

export const midgardBoundedBlobChunkCountV1 = (
  totalLength: number,
): number => {
  const length = boundedLength(totalLength);
  return length === 0
    ? 0
    : Math.ceil(length / MIDGARD_BOUNDED_BLOB_CHUNK_BYTES_V1);
};

export const midgardBoundedBlobChunkLengthV1 = ({
  totalLength,
  chunkIndex,
}: {
  readonly totalLength: number;
  readonly chunkIndex: number;
}): number => {
  const count = midgardBoundedBlobChunkCountV1(totalLength);
  if (
    !Number.isSafeInteger(chunkIndex) ||
    chunkIndex < 0 ||
    chunkIndex >= count
  ) {
    throw new Error("V1 bounded-blob chunk index is out of range");
  }
  return chunkIndex + 1 < count
    ? MIDGARD_BOUNDED_BLOB_CHUNK_BYTES_V1
    : totalLength -
        chunkIndex * MIDGARD_BOUNDED_BLOB_CHUNK_BYTES_V1;
};

export const hashMidgardBoundedBlobChunkV1 = ({
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
  if (bytes.length > MIDGARD_BOUNDED_BLOB_CHUNK_BYTES_V1) {
    throw new Error(
      `V1 bounded-blob chunk exceeds ${MIDGARD_BOUNDED_BLOB_CHUNK_BYTES_V1.toString()} bytes`,
    );
  }
  return hash32(
    Buffer.concat([
      CHUNK_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_BLOB_V1_VERSION),
        BigInt(exactFieldIndex),
        BigInt(chunkIndex),
        bytes,
      ]),
    ]),
  );
};

export const commitMidgardBoundedBlobV1 = ({
  fieldIndex,
  totalLength,
  frontier,
}: {
  readonly fieldIndex: number;
  readonly totalLength: number;
  readonly frontier: MidgardValidationMerkleFrontierV1;
}): Hash32 => {
  const exactFieldIndex = boundedFieldIndex(fieldIndex);
  const exactLength = boundedLength(totalLength);
  const expectedCount = midgardBoundedBlobChunkCountV1(exactLength);
  if (frontier.count !== expectedCount) {
    throw new Error(
      `V1 bounded-blob frontier count does not match its length: ${frontier.count.toString()} != ${expectedCount.toString()}`,
    );
  }
  return hash32(
    Buffer.concat([
      COMMITMENT_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_BLOB_V1_VERSION),
        BigInt(exactFieldIndex),
        BigInt(exactLength),
        commitMidgardValidationMerkleFrontierV1(frontier),
      ]),
    ]),
  );
};

export const buildMidgardBoundedBlobV1 = ({
  fieldIndex,
  bytes,
}: {
  readonly fieldIndex: number;
  readonly bytes: Uint8Array;
}): MidgardBoundedBlobV1 => {
  const exactFieldIndex = boundedFieldIndex(fieldIndex);
  const source = Buffer.from(bytes);
  const chunks: Buffer[] = [];
  for (
    let offset = 0;
    offset < source.length;
    offset += MIDGARD_BOUNDED_BLOB_CHUNK_BYTES_V1
  ) {
    chunks.push(
      source.subarray(
        offset,
        offset + MIDGARD_BOUNDED_BLOB_CHUNK_BYTES_V1,
      ),
    );
  }
  const leafHashes = chunks.map((chunk, chunkIndex) =>
    hashMidgardBoundedBlobChunkV1({
      fieldIndex: exactFieldIndex,
      chunkIndex,
      chunk,
    }),
  );
  const frontier = buildMidgardValidationMerkleFrontierV1(leafHashes);
  return {
    fieldIndex: exactFieldIndex,
    totalLength: source.length,
    chunks,
    leafHashes,
    frontier,
    commitment: commitMidgardBoundedBlobV1({
      fieldIndex: exactFieldIndex,
      totalLength: source.length,
      frontier,
    }),
  };
};

export const buildMidgardBoundedBlobChunkProofV1 = (
  blob: MidgardBoundedBlobV1,
  chunkIndex: number,
): MidgardBoundedBlobChunkProofV1 => {
  const membership: MidgardValidationMerkleMembershipV1 =
    buildMidgardValidationMerkleMembershipV1(blob.leafHashes, chunkIndex);
  return {
    version: MIDGARD_BOUNDED_BLOB_V1_VERSION,
    fieldIndex: blob.fieldIndex,
    totalLength: blob.totalLength,
    chunkIndex,
    chunk: Buffer.from(blob.chunks[chunkIndex]!),
    frontier: membership.frontier,
    siblings: membership.siblings,
  };
};

export const verifyMidgardBoundedBlobChunkProofV1 = ({
  expectedCommitment,
  proof,
}: {
  readonly expectedCommitment: Uint8Array;
  readonly proof: MidgardBoundedBlobChunkProofV1;
}): boolean => {
  try {
    if (proof.version !== MIDGARD_BOUNDED_BLOB_V1_VERSION) return false;
    const expectedChunkLength = midgardBoundedBlobChunkLengthV1({
      totalLength: proof.totalLength,
      chunkIndex: proof.chunkIndex,
    });
    if (proof.chunk.length !== expectedChunkLength) return false;
    const leafHash = hashMidgardBoundedBlobChunkV1({
      fieldIndex: proof.fieldIndex,
      chunkIndex: proof.chunkIndex,
      chunk: proof.chunk,
    });
    if (
      !verifyMidgardValidationMerkleMembershipV1({
        frontier: proof.frontier,
        leafIndex: proof.chunkIndex,
        leafHash,
        siblings: proof.siblings,
      })
    ) {
      return false;
    }
    return commitMidgardBoundedBlobV1({
      fieldIndex: proof.fieldIndex,
      totalLength: proof.totalLength,
      frontier: proof.frontier,
    }).equals(ensureHash32(expectedCommitment, "bounded_blob_v1.expected"));
  } catch {
    return false;
  }
};
