import { blake2b } from "@noble/hashes/blake2.js";

import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

const BRANCH_DOMAIN = Buffer.from("MidgardValidationMerkleBranchV1", "ascii");
const FRONTIER_DOMAIN = Buffer.from(
  "MidgardValidationMerkleFrontierV1",
  "ascii",
);

export const MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT = 0xffff_ffff;

export type MidgardValidationMerklePeak = {
  readonly height: number;
  readonly hash: Hash32;
};

export type MidgardValidationMerkleFrontier = {
  readonly count: number;
  readonly peaks: readonly MidgardValidationMerklePeak[];
};

export type MidgardValidationMerkleMembership = {
  readonly frontier: MidgardValidationMerkleFrontier;
  readonly leafIndex: number;
  readonly leafHash: Hash32;
  readonly siblings: readonly Hash32[];
};

export type MidgardValidationMerkleMembershipIndex = {
  readonly frontier: MidgardValidationMerkleFrontier;
  readonly membershipAt: (
    leafIndex: number,
  ) => MidgardValidationMerkleMembership;
};

type PrecomputedMidgardValidationMerklePeak = {
  readonly height: number;
  readonly start: number;
  readonly levels: readonly (readonly Hash32[])[];
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

export const hashMidgardValidationMerkleBranch = (
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

export const emptyMidgardValidationMerkleFrontier =
  (): MidgardValidationMerkleFrontier => ({ count: 0, peaks: [] });

export const validateMidgardValidationMerkleFrontier = (
  frontier: MidgardValidationMerkleFrontier,
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

export const encodeMidgardValidationMerkleFrontier = (
  frontier: MidgardValidationMerkleFrontier,
): Buffer => {
  validateMidgardValidationMerkleFrontier(frontier);
  return encodeCbor(
    frontier.peaks.map((peak) => [BigInt(peak.height), peak.hash]),
  );
};

export const commitMidgardValidationMerkleFrontier = (
  frontier: MidgardValidationMerkleFrontier,
): Hash32 => {
  validateMidgardValidationMerkleFrontier(frontier);
  return hash32(
    Buffer.concat([
      FRONTIER_DOMAIN,
      encodeCbor(BigInt(frontier.count)),
      encodeMidgardValidationMerkleFrontier(frontier),
    ]),
  );
};

export const appendMidgardValidationMerkleLeaf = (
  frontier: MidgardValidationMerkleFrontier,
  leafHash: Uint8Array,
): MidgardValidationMerkleFrontier => {
  validateMidgardValidationMerkleFrontier(frontier);
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
    carry = hashMidgardValidationMerkleBranch(left.hash, carry);
    oldCount = Math.floor(oldCount / 2);
    height += 1;
  }
  const next = {
    count: frontier.count + 1,
    peaks: [{ height, hash: carry }, ...peaks],
  } satisfies MidgardValidationMerkleFrontier;
  validateMidgardValidationMerkleFrontier(next);
  return next;
};

export const buildMidgardValidationMerkleFrontier = (
  leafHashes: readonly Uint8Array[],
): MidgardValidationMerkleFrontier =>
  leafHashes.reduce(
    (frontier, leafHash) =>
      appendMidgardValidationMerkleLeaf(frontier, leafHash),
    emptyMidgardValidationMerkleFrontier(),
  );

const locatePeak = (
  count: number,
  leafIndex: number,
): { readonly height: number; readonly start: number } => {
  let offset = 0;
  for (let height = Math.floor(Math.log2(count)); height >= 0; height -= 1) {
    if (!bitIsSet(count, height)) continue;
    const size = 2 ** height;
    if (leafIndex < offset + size) {
      return { height, start: offset };
    }
    offset += size;
  }
  throw new Error("validation Merkle leaf is outside the frontier");
};

const cloneMidgardValidationMerkleFrontier = (
  frontier: MidgardValidationMerkleFrontier,
): MidgardValidationMerkleFrontier => ({
  count: frontier.count,
  peaks: frontier.peaks.map((peak) => ({
    height: peak.height,
    hash: Buffer.from(peak.hash),
  })),
});

export const buildMidgardValidationMerkleMembershipIndex = (
  leafHashes: readonly Uint8Array[],
): MidgardValidationMerkleMembershipIndex => {
  boundedCount(leafHashes.length, "validation_merkle.leaves.length");
  const leaves = leafHashes.map((leaf, index) =>
    ensureHash32(leaf, `validation_merkle.leaves[${index}]`),
  );
  const peaks: PrecomputedMidgardValidationMerklePeak[] = [];
  let start = 0;
  for (
    let height =
      leaves.length === 0 ? -1 : Math.floor(Math.log2(leaves.length));
    height >= 0;
    height -= 1
  ) {
    if (!bitIsSet(leaves.length, height)) continue;
    const levels: Hash32[][] = [leaves.slice(start, start + 2 ** height)];
    while (levels.at(-1)!.length > 1) {
      const level = levels.at(-1)!;
      const next: Hash32[] = [];
      for (let index = 0; index < level.length; index += 2) {
        next.push(
          hashMidgardValidationMerkleBranch(level[index]!, level[index + 1]!),
        );
      }
      levels.push(next);
    }
    peaks.push({ height, start, levels });
    start += 2 ** height;
  }
  const cachedFrontier: MidgardValidationMerkleFrontier = {
    count: leaves.length,
    peaks: peaks
      .map((peak) => ({
        height: peak.height,
        hash: peak.levels.at(-1)![0]!,
      }))
      .reverse(),
  };
  validateMidgardValidationMerkleFrontier(cachedFrontier);

  return {
    frontier: cloneMidgardValidationMerkleFrontier(cachedFrontier),
    membershipAt: (leafIndex: number): MidgardValidationMerkleMembership => {
      if (
        !Number.isSafeInteger(leafIndex) ||
        leafIndex < 0 ||
        leafIndex >= leaves.length
      ) {
        throw new Error(
          "validation Merkle membership leaf index is out of range",
        );
      }
      const location = locatePeak(leaves.length, leafIndex);
      const peak = peaks.find(
        (candidate) =>
          candidate.height === location.height &&
          candidate.start === location.start,
      );
      if (peak === undefined) {
        throw new Error(
          "validation Merkle membership peak was not precomputed",
        );
      }
      let localIndex = leafIndex - peak.start;
      const siblings: Hash32[] = [];
      for (
        let levelIndex = 0;
        levelIndex < peak.levels.length - 1;
        levelIndex += 1
      ) {
        siblings.push(Buffer.from(peak.levels[levelIndex]![localIndex ^ 1]!));
        localIndex = Math.floor(localIndex / 2);
      }
      return {
        frontier: cloneMidgardValidationMerkleFrontier(cachedFrontier),
        leafIndex,
        leafHash: Buffer.from(leaves[leafIndex]!),
        siblings,
      };
    },
  };
};

export const buildMidgardValidationMerkleMembership = (
  leafHashes: readonly Uint8Array[],
  leafIndex: number,
): MidgardValidationMerkleMembership => {
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
  const frontier = buildMidgardValidationMerkleFrontier(leaves);
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
        hashMidgardValidationMerkleBranch(level[index]!, level[index + 1]!),
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

export const verifyMidgardValidationMerkleMembership = (
  membership: MidgardValidationMerkleMembership,
): boolean => {
  try {
    validateMidgardValidationMerkleFrontier(membership.frontier);
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
          ? hashMidgardValidationMerkleBranch(current, exactSibling)
          : hashMidgardValidationMerkleBranch(exactSibling, current);
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
