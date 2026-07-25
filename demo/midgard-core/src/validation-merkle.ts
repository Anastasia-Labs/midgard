import { blake2b } from "@noble/hashes/blake2.js";

import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

const BRANCH_DOMAIN = Buffer.from(
  "MidgardValidationMerkleBranchV1",
  "ascii",
);
const FRONTIER_DOMAIN = Buffer.from(
  "MidgardValidationMerkleFrontierV1",
  "ascii",
);

export const MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT = 0xffff_ffff;

export type MidgardValidationMerklePeakV1 = {
  readonly height: number;
  readonly hash: Hash32;
};

export type MidgardValidationMerkleFrontierV1 = {
  readonly count: number;
  readonly peaks: readonly MidgardValidationMerklePeakV1[];
};

export type MidgardValidationMerkleMembershipV1 = {
  readonly frontier: MidgardValidationMerkleFrontierV1;
  readonly leafIndex: number;
  readonly leafHash: Hash32;
  readonly siblings: readonly Hash32[];
};

const hash32 = (bytes: Uint8Array): Hash32 =>
  ensureHash32(blake2b(bytes, { dkLen: 32 }), "validation_merkle_hash");

const boundedCount = (count: number, field: string): number => {
  if (
    !Number.isSafeInteger(count) ||
    count < 0 ||
    count > MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT
  ) {
    throw new Error(`${field} is outside the uint32 validation envelope`);
  }
  return count;
};

const bitIsSet = (value: number, height: number): boolean =>
  Math.floor(value / 2 ** height) % 2 === 1;

export const hashMidgardValidationMerkleBranchV1 = (
  left: Uint8Array,
  right: Uint8Array,
): Hash32 =>
  hash32(
    Buffer.concat([
      BRANCH_DOMAIN,
      ensureHash32(left, "validation_merkle.left"),
      ensureHash32(right, "validation_merkle.right"),
    ]),
  );

export const emptyMidgardValidationMerkleFrontierV1 =
  (): MidgardValidationMerkleFrontierV1 => ({ count: 0, peaks: [] });

export const validateMidgardValidationMerkleFrontierV1 = (
  frontier: MidgardValidationMerkleFrontierV1,
): void => {
  const count = boundedCount(frontier.count, "validation_merkle.count");
  let peakIndex = 0;
  let remaining = count;
  let height = 0;
  while (remaining > 0) {
    if (remaining % 2 === 1) {
      const peak = frontier.peaks[peakIndex];
      if (peak === undefined || peak.height !== height) {
        throw new Error(
          `validation Merkle frontier is missing peak height ${height.toString()}`,
        );
      }
      ensureHash32(peak.hash, `validation_merkle.peaks[${peakIndex}].hash`);
      peakIndex += 1;
    }
    remaining = Math.floor(remaining / 2);
    height += 1;
  }
  if (peakIndex !== frontier.peaks.length) {
    throw new Error("validation Merkle frontier contains an unoccupied peak");
  }
};

export const encodeMidgardValidationMerkleFrontierV1 = (
  frontier: MidgardValidationMerkleFrontierV1,
): Buffer => {
  validateMidgardValidationMerkleFrontierV1(frontier);
  return encodeCbor(
    frontier.peaks.map((peak) => [BigInt(peak.height), peak.hash]),
  );
};

export const commitMidgardValidationMerkleFrontierV1 = (
  frontier: MidgardValidationMerkleFrontierV1,
): Hash32 => {
  validateMidgardValidationMerkleFrontierV1(frontier);
  return hash32(
    Buffer.concat([
      FRONTIER_DOMAIN,
      encodeCbor(BigInt(frontier.count)),
      encodeMidgardValidationMerkleFrontierV1(frontier),
    ]),
  );
};

export const appendMidgardValidationMerkleLeafV1 = (
  frontier: MidgardValidationMerkleFrontierV1,
  leafHash: Uint8Array,
): MidgardValidationMerkleFrontierV1 => {
  validateMidgardValidationMerkleFrontierV1(frontier);
  if (frontier.count >= MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT) {
    throw new Error("validation Merkle frontier is full");
  }
  let oldCount = frontier.count;
  let height = 0;
  let carry = ensureHash32(leafHash, "validation_merkle.leaf");
  const peaks = [...frontier.peaks];
  while (oldCount % 2 === 1) {
    const left = peaks.shift();
    if (left === undefined || left.height !== height) {
      throw new Error("validation Merkle frontier carry is malformed");
    }
    carry = hashMidgardValidationMerkleBranchV1(left.hash, carry);
    oldCount = Math.floor(oldCount / 2);
    height += 1;
  }
  const next = {
    count: frontier.count + 1,
    peaks: [{ height, hash: carry }, ...peaks],
  } satisfies MidgardValidationMerkleFrontierV1;
  validateMidgardValidationMerkleFrontierV1(next);
  return next;
};

export const buildMidgardValidationMerkleFrontierV1 = (
  leafHashes: readonly Uint8Array[],
): MidgardValidationMerkleFrontierV1 =>
  leafHashes.reduce(
    (frontier, leafHash) =>
      appendMidgardValidationMerkleLeafV1(frontier, leafHash),
    emptyMidgardValidationMerkleFrontierV1(),
  );

const locatePeak = (
  count: number,
  leafIndex: number,
): { readonly height: number; readonly start: number } => {
  let offset = 0;
  for (
    let height = Math.floor(Math.log2(count));
    height >= 0;
    height -= 1
  ) {
    if (!bitIsSet(count, height)) continue;
    const size = 2 ** height;
    if (leafIndex < offset + size) {
      return { height, start: offset };
    }
    offset += size;
  }
  throw new Error("validation Merkle leaf is outside the frontier");
};

export const buildMidgardValidationMerkleMembershipV1 = (
  leafHashes: readonly Uint8Array[],
  leafIndex: number,
): MidgardValidationMerkleMembershipV1 => {
  if (
    !Number.isSafeInteger(leafIndex) ||
    leafIndex < 0 ||
    leafIndex >= leafHashes.length
  ) {
    throw new Error("validation Merkle membership leaf index is out of range");
  }
  const leaves = leafHashes.map((leaf, index) =>
    ensureHash32(leaf, `validation_merkle.leaves[${index}]`),
  );
  const frontier = buildMidgardValidationMerkleFrontierV1(leaves);
  const location = locatePeak(leaves.length, leafIndex);
  let localIndex = leafIndex - location.start;
  let level = leaves.slice(
    location.start,
    location.start + 2 ** location.height,
  );
  const siblings: Hash32[] = [];
  while (level.length > 1) {
    siblings.push(level[localIndex ^ 1]!);
    const next: Hash32[] = [];
    for (let index = 0; index < level.length; index += 2) {
      next.push(
        hashMidgardValidationMerkleBranchV1(
          level[index]!,
          level[index + 1]!,
        ),
      );
    }
    localIndex = Math.floor(localIndex / 2);
    level = next;
  }
  return {
    frontier,
    leafIndex,
    leafHash: leaves[leafIndex]!,
    siblings,
  };
};

export const verifyMidgardValidationMerkleMembershipV1 = (
  membership: MidgardValidationMerkleMembershipV1,
): boolean => {
  try {
    validateMidgardValidationMerkleFrontierV1(membership.frontier);
    if (
      !Number.isSafeInteger(membership.leafIndex) ||
      membership.leafIndex < 0 ||
      membership.leafIndex >= membership.frontier.count
    ) {
      return false;
    }
    const location = locatePeak(
      membership.frontier.count,
      membership.leafIndex,
    );
    if (membership.siblings.length !== location.height) return false;
    let localIndex = membership.leafIndex - location.start;
    let current = ensureHash32(
      membership.leafHash,
      "validation_merkle.membership.leaf",
    );
    for (const sibling of membership.siblings) {
      const exactSibling = ensureHash32(
        sibling,
        "validation_merkle.membership.sibling",
      );
      current =
        localIndex % 2 === 0
          ? hashMidgardValidationMerkleBranchV1(current, exactSibling)
          : hashMidgardValidationMerkleBranchV1(exactSibling, current);
      localIndex = Math.floor(localIndex / 2);
    }
    const peak = membership.frontier.peaks.find(
      (candidate) => candidate.height === location.height,
    );
    return peak !== undefined && current.equals(peak.hash);
  } catch {
    return false;
  }
};
