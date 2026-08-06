/**
 * Synthetic deep MPF membership proofs for the published-chunk carriage
 * fixtures (issue #545).
 *
 * ## Why these are synthetic
 *
 * `adversarialMembershipSiblingKeys` builds a genuinely grinded trie, and that
 * is what bounds the *cost* of the attack: forcing branch level `i` is a
 * fixed-target search costing about 16^i candidate hashings. The measured
 * envelope ceiling is level 21..23, which is 2^84..2^92 work — real for a
 * funded adversary, unreachable for a test fixture, which is why the grinded
 * fixture stops at five levels.
 *
 * Carriage, though, does not care how the depth was produced. What the step
 * validator checks is that the proof it is handed reconstructs the root the
 * challenged header commits. So the depth is synthesised here — a deterministic
 * all-`branch` ladder, MPF's widest step shape, one per forced level — and the
 * header is then made to commit exactly the root that proof proves. The proof
 * is a real MPF proof of a real trie shape, verified by the library's own
 * `verify`, and it is a proof of a trie that a 2^128 adversary can construct.
 * Only the *cost of constructing it* is elided.
 *
 * This mirrors, byte for byte, how the on-chain selectors in
 * `midgard/fraud_proofs/chunked_inclusion_v1.test` and the four families' step
 * modules reach the same depth.
 */

import { Proof } from "@aiken-lang/merkle-patricia-forestry";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";

/** Digest width of every MPF neighbour hash. */
const DIGEST_BYTES = 32;

/** An MPF path is 64 nibbles, so no proof can carry more than 64 steps. */
export const MAXIMUM_PROOF_STEP_COUNT = 64;

/**
 * Four deterministic neighbour digests for one forced branch level, chained so
 * that the ladder is reproducible from the level index alone.
 */
const branchNeighbors = (level: number): string => {
  let digest = computeHash32(Buffer.from([level & 0xff, (level >> 8) & 0xff]));
  const parts: Buffer[] = [];
  for (let index = 0; index < 4; index += 1) {
    parts.push(digest);
    digest = computeHash32(digest);
  }
  return Buffer.concat(parts).toString("hex");
};

export type SyntheticDeepProofV1 = {
  /** The raw MPF root this proof proves, hex. */
  readonly transactionsPhasRoot: string;
  /** The proof, in the canonical on-chain CBOR the step redeemer would carry. */
  readonly proofCbor: string;
  readonly branchLevels: number;
  readonly proofCborBytes: number;
};

/**
 * A membership proof of `(key, value)` at exactly `branchLevels` forced branch
 * levels, together with the root it proves.
 *
 * Every step is a `branch` — the largest step shape MPF has, and the shape a
 * sibling-grinding adversary forces — so `branchLevels` is also the proof's
 * step count.
 */
export const syntheticDeepMembershipProofV1 = ({
  key,
  value,
  branchLevels,
}: {
  readonly key: Buffer;
  readonly value: Buffer;
  readonly branchLevels: number;
}): SyntheticDeepProofV1 => {
  if (
    !Number.isInteger(branchLevels) ||
    branchLevels < 1 ||
    branchLevels > MAXIMUM_PROOF_STEP_COUNT
  ) {
    throw new Error(
      `branchLevels must be an integer in 1..${String(MAXIMUM_PROOF_STEP_COUNT)}, got ${String(branchLevels)}.`,
    );
  }
  const steps = Array.from({ length: branchLevels }, (_unused, level) => ({
    type: "branch",
    skip: 0,
    neighbors: branchNeighbors(level),
  }));
  const proof = Proof.fromJSON(key, value, steps);
  const root = proof.verify(true);
  if (root === null || root.length !== DIGEST_BYTES) {
    throw new Error(
      "Synthetic deep membership proof did not reconstruct a 32-byte root.",
    );
  }
  const proofCbor = proof.toCBOR();
  return {
    transactionsPhasRoot: root.toString("hex"),
    proofCbor: proofCbor.toString("hex"),
    branchLevels,
    proofCborBytes: proofCbor.length,
  };
};

// ---------------------------------------------------------------------------
// Several deep openings of ONE trie (issue #549)
//
// Q12 and Q14 open the challenged block's transactions trie exactly once, so a
// standalone ladder per proof is all #545 needed. Q10 and Q11 do not: the
// double-spend family opens the SAME root twice (tx1 at step-01, tx2 at
// step-02) and the no-input family opens it twice as well (the challenged
// transaction's membership at step-01, the phantom input's producing
// transaction's ABSENCE at step-04). Two independent ladders cannot be used for
// that: each ladder determines its own root, and the step validators
// re-authenticate every opening against the one root the challenged header
// commits.
//
// The construction below is the honest one. Two keys whose hashed paths diverge
// at nibble `i` really do live under one common branch node in a real trie,
// each below its own sub-trie. So the fixture builds exactly that: one root
// branch node whose child slots hold the sub-trie digests of the individual
// ladders, and then reads each opening's top-level neighbours OUT of that
// shared node. Every proof is a real MPF proof of a real trie shape, all of
// them reconstruct the one root, and the library's own `verify` is what says so
// — the helper computes nothing it does not then hand back to `Proof.verify`
// for confirmation.
//
// As in the single-key case only the COST of grinding such a trie is elided;
// see the module header.
// ---------------------------------------------------------------------------

/** MPF's empty-child sentinel. */
const NULL_HASH = Buffer.alloc(DIGEST_BYTES);

/** The 16-ary node's binary merkle is four levels deep. */
const BRANCH_MERKLE_LEVELS = 4;
const BRANCH_CHILD_COUNT = 16;

const hashPair = (left: Buffer, right: Buffer): Buffer =>
  computeHash32(Buffer.concat([left, right]));

/** `blake2b_256(key)` as 64 nibbles, which is the key's path through the trie. */
const mpfPath = (key: Buffer): string => computeHash32(key).toString("hex");

const nibbleAt = (path: string, index: number): number =>
  Number.parseInt(path[index]!, 16);

/** A nibble sequence as one byte per nibble, MPF's prefix encoding. */
const nibbleBytes = (nibbles: string): Buffer =>
  Buffer.from(Array.from(nibbles, (digit) => Number.parseInt(digit, 16)));

const merkleRootOf = (nodes: readonly Buffer[]): Buffer => {
  let level = [...nodes];
  while (level.length > 1) {
    const next: Buffer[] = [];
    for (let index = 0; index < level.length; index += 2) {
      next.push(hashPair(level[index]!, level[index + 1]!));
    }
    level = next;
  }
  const root = level[0];
  if (root === undefined) {
    throw new Error("Cannot take the merkle root of an empty node list.");
  }
  return root;
};

/**
 * The four sibling digests on the path from child slot `child` to the node's
 * merkle root, widest first — the exact order an MPF `branch` step records.
 */
const branchMerkleProof = (
  children: readonly Buffer[],
  child: number,
): readonly Buffer[] => {
  const neighbors: Buffer[] = [];
  let pivot = BRANCH_CHILD_COUNT / 2;
  let width = BRANCH_CHILD_COUNT / 2;
  while (width >= 1) {
    if (child < pivot) {
      neighbors.push(merkleRootOf(children.slice(pivot, pivot + width)));
      pivot -= width >> 1;
    } else {
      neighbors.push(merkleRootOf(children.slice(pivot - width, pivot)));
      pivot += width >> 1;
    }
    width = width >> 1;
  }
  return neighbors;
};

/** The node merkle root a `branch` step reconstructs from its own neighbours. */
const branchMerkleFromNeighbors = ({
  child,
  below,
  neighbors,
}: {
  readonly child: number;
  readonly below: Buffer;
  readonly neighbors: readonly Buffer[];
}): Buffer => {
  let node = below;
  for (let level = BRANCH_MERKLE_LEVELS - 1; level >= 0; level -= 1) {
    const neighbor = neighbors[level]!;
    const rightHandSide =
      ((child >> (BRANCH_MERKLE_LEVELS - 1 - level)) & 1) === 1;
    node = rightHandSide ? hashPair(neighbor, node) : hashPair(node, neighbor);
  }
  return node;
};

/** `Leaf.computeHash`: MPF's leaf node digest over a nibble suffix. */
const leafNodeHash = (suffix: string, valueDigest: Buffer): Buffer => {
  const isOdd = suffix.length % 2 > 0;
  const head = isOdd
    ? Buffer.concat([Buffer.from([0]), nibbleBytes(suffix.slice(0, 1))])
    : Buffer.from([255]);
  const tail = Buffer.from(isOdd ? suffix.slice(1) : suffix, "hex");
  return computeHash32(Buffer.concat([head, tail, valueDigest]));
};

/** `Branch.computeHash`: MPF's branch node digest over a nibble prefix. */
const branchNodeHash = (prefix: string, merkle: Buffer): Buffer =>
  computeHash32(Buffer.concat([nibbleBytes(prefix), merkle]));

/** Ladder neighbours below the shared root, distinct per claim. */
const ladderNeighbors = (domain: number, level: number): readonly Buffer[] => {
  let digest = computeHash32(
    Buffer.from([
      domain & 0xff,
      (domain >> 8) & 0xff,
      level & 0xff,
      (level >> 8) & 0xff,
    ]),
  );
  const parts: Buffer[] = [];
  for (let index = 0; index < BRANCH_MERKLE_LEVELS; index += 1) {
    parts.push(digest);
    digest = computeHash32(digest);
  }
  return parts;
};

/**
 * One opening the fixture wants of the shared trie. A `value` makes it a
 * membership claim; its absence makes it an absence claim, whose deepest branch
 * node simply has no child in the key's slot.
 */
export type SyntheticSharedRootClaimV1 = {
  readonly key: Buffer;
  readonly value?: Buffer;
};

export type SyntheticSharedRootOpeningV1 = {
  readonly key: Buffer;
  readonly isMembership: boolean;
  readonly proofCbor: string;
  readonly proofCborBytes: number;
};

export type SyntheticDeepSharedRootV1 = {
  /** The one raw MPF root every returned opening reconstructs, hex. */
  readonly root: string;
  readonly openings: readonly SyntheticSharedRootOpeningV1[];
  readonly branchLevels: number;
};

/**
 * Several openings of one synthesised trie, each at exactly `branchLevels`
 * forced branch levels, all reconstructing the same root.
 *
 * The claims' hashed paths must diverge at their first nibble, which the four
 * families' fixture keys do. That is asserted rather than worked around: a
 * fixture that silently fell back to a shallower shared prefix would be
 * measuring a different depth than the one it reports.
 */
export const syntheticDeepSharedRootProofsV1 = ({
  claims,
  branchLevels,
}: {
  readonly claims: readonly SyntheticSharedRootClaimV1[];
  readonly branchLevels: number;
}): SyntheticDeepSharedRootV1 => {
  if (
    !Number.isInteger(branchLevels) ||
    branchLevels < 2 ||
    branchLevels > MAXIMUM_PROOF_STEP_COUNT
  ) {
    throw new Error(
      `branchLevels must be an integer in 2..${String(MAXIMUM_PROOF_STEP_COUNT)}, got ${String(branchLevels)}.`,
    );
  }
  if (claims.length < 2 || claims.length > BRANCH_CHILD_COUNT) {
    throw new Error(
      `A shared-root fixture needs 2..${String(BRANCH_CHILD_COUNT)} claims, got ${String(claims.length)}.`,
    );
  }

  const paths = claims.map((claim) => mpfPath(claim.key));
  const rootSlots = paths.map((path) => nibbleAt(path, 0));
  if (new Set(rootSlots).size !== rootSlots.length) {
    throw new Error(
      "Shared-root claims must diverge at their first path nibble; two of these keys do not.",
    );
  }

  // Each claim's sub-trie below the shared root node, folded up from its own
  // leaf (or from the empty slot an absence claim terminates in).
  const children = Array.from<Buffer>({ length: BRANCH_CHILD_COUNT }).fill(
    NULL_HASH,
  );
  claims.forEach((claim, index) => {
    const path = paths[index]!;
    let node =
      claim.value === undefined
        ? NULL_HASH
        : leafNodeHash(path.slice(branchLevels), computeHash32(claim.value));
    for (let level = branchLevels - 1; level >= 1; level -= 1) {
      node = branchNodeHash(
        "",
        branchMerkleFromNeighbors({
          child: nibbleAt(path, level),
          below: node,
          neighbors: ladderNeighbors(index, level),
        }),
      );
    }
    children[rootSlots[index]!] = node;
  });

  const root = branchNodeHash("", merkleRootOf(children));
  const openings = claims.map((claim, index) => {
    const steps = [
      {
        type: "branch",
        skip: 0,
        neighbors: Buffer.concat([
          ...branchMerkleProof(children, rootSlots[index]!),
        ]).toString("hex"),
      },
      ...Array.from({ length: branchLevels - 1 }, (_unused, offset) => ({
        type: "branch",
        skip: 0,
        neighbors: Buffer.concat([
          ...ladderNeighbors(index, offset + 1),
        ]).toString("hex"),
      })),
    ];
    const isMembership = claim.value !== undefined;
    const proof = Proof.fromJSON(
      claim.key,
      claim.value ?? Buffer.alloc(0),
      steps,
    );
    // The library, not this module, decides whether the construction is right.
    const reconstructed = proof.verify(isMembership);
    if (
      reconstructed === null ||
      reconstructed.toString("hex") !== root.toString("hex")
    ) {
      throw new Error(
        `Synthetic shared-root opening ${String(index)} reconstructs ${String(reconstructed?.toString("hex"))}, not the shared root ${root.toString("hex")}.`,
      );
    }
    const proofCbor = proof.toCBOR();
    return {
      key: claim.key,
      isMembership,
      proofCbor: proofCbor.toString("hex"),
      proofCborBytes: proofCbor.length,
    };
  });

  return { root: root.toString("hex"), openings, branchLevels };
};
