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
