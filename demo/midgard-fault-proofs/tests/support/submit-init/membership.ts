import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";

/**
 * The deepest branch level at which the L1 envelope still admits the proof,
 * derived from a MEASURED transaction at a measured branch depth plus the
 * measured constant marginal cost of one further level.
 */
export const membershipProofBranchLevelByteCeiling = ({
  measuredTransactionBytes,
  measuredBranchLevels,
  l1MaxTxSize,
}: {
  readonly measuredTransactionBytes: number;
  readonly measuredBranchLevels: number;
  readonly l1MaxTxSize: number;
}): number =>
  measuredBranchLevels +
  Math.floor(
    (l1MaxTxSize - measuredTransactionBytes) /
      PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
  );

/**
 * The deepest branch level reachable by an adversary willing to spend `2^n`
 * digest evaluations. Forcing a branch at level `i` means finding a key whose
 * hashed path agrees with the challenged transaction's path on `i` chosen
 * nibbles, which is a fixed-target search costing ~16^i = 2^(4i).
 */
export const membershipProofBranchLevelsReachableWithWork = (
  log2Work: number,
): number => Math.floor(log2Work / 4);

export type MembershipProofShape = {
  readonly branchLevels: number;
  readonly siblingCount: number;
  readonly proofSteps: number;
  readonly proofCborBytes: number;
};

/**
 * Insert the grinded siblings of every named target into `trie` and return how
 * many keys were added. A zero `branchLevels` leaves the trie untouched, which
 * is exactly the minimal fixture the four families already measured.
 */
export const insertAdversarialMembershipSiblings = async ({
  trie,
  targets,
  branchLevels,
}: {
  readonly trie: Trie;
  readonly targets: readonly {
    readonly key: Buffer;
    readonly domain: number;
  }[];
  readonly branchLevels: number;
}): Promise<number> => {
  if (branchLevels === 0) {
    return 0;
  }
  const reserved = new Set(targets.map(({ key }) => key.toString("hex")));
  let inserted = 0;
  for (const target of targets) {
    for (const key of adversarialMembershipSiblingKeys({
      targetKey: target.key,
      branchLevels,
      domain: target.domain,
    })) {
      const label = key.toString("hex");
      if (reserved.has(label)) {
        throw new Error(
          `Grinded adversarial sibling ${label} collides with an existing trie key.`,
        );
      }
      reserved.add(label);
      await trie.insert(key, ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE);
      inserted += 1;
    }
  }
  return inserted;
};

export const membershipProofShape = async ({
  trie,
  key,
  branchLevels,
  siblingCount,
}: {
  readonly trie: Trie;
  readonly key: Buffer;
  readonly branchLevels: number;
  readonly siblingCount: number;
}): Promise<MembershipProofShape> => {
  const proof = await trie.prove(key);
  const steps = proof.toJSON() as readonly { readonly type: string }[];
  if (branchLevels > 0 && steps.length < branchLevels) {
    throw new Error(
      `Adversarial fixture asked for ${branchLevels.toString()} branch levels but the membership proof carries only ${steps.length.toString()} steps.`,
    );
  }
  return {
    branchLevels,
    siblingCount,
    proofSteps: steps.length,
    proofCborBytes: Buffer.from(proof.toCBOR()).length,
  };
};

// ---------------------------------------------------------------------------
// Adversarial MPF membership-proof depth (GOAL_SPEC.md 9.1 output 5, axis 1)
// ---------------------------------------------------------------------------
//
// Every family's step-01 redeemer carries `tx_membership_proof`, and that proof
// is the only part of a fault proof whose size an adversary controls by
// choosing what the challenged block contains. The proof serializes one CBOR
// step per level at which the trie branches along the challenged
// transaction's hashed path, so the lever is not "more transactions" — a
// larger random block only grows the path logarithmically — but
// "transactions whose hashed paths branch at consecutive nibbles of the
// challenged transaction's own path".
//
// Two siblings per level force the on-path node to hold at least three
// children, which is what makes each step serialize as the largest shape the
// MPF proof encoding has (`branch`, carrying four 32-byte neighbour hashes)
// rather than the smaller `fork` or `leaf`. That is the worst case, not a
// typical one.
//
// Grinding is deterministic: candidate keys are a counter written into a fixed
// buffer, so the same fixture is reproduced byte for byte on any machine, in
// the same spirit as the deterministic emulator wallets.
//
// Forcing a branch at level `i` costs ~16^i digest evaluations, so this axis is
// bounded by adversary WORK, not by protocol structure. See
// `membershipProofBranchLevelsReachableWithWork`.
export const ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS = 5;

// Marginal CBOR cost of one additional `branch` proof step. Measured, and
// re-measured by `tests/max-proof-fit-membership-depth.test.ts` rather than
// assumed: the MPF proof encoding is a definite list of fixed-shape steps, so
// the cost per level is exactly constant.
export const MPF_BRANCH_PROOF_STEP_CBOR_BYTES = 139;

// Marginal cost of one additional forced branch level in the COMPLETE SIGNED
// step-01 transaction, which is what the L1 envelope actually measures. It is
// not the MPF CBOR figure above: the proof reaches the chain as Plutus data in
// the step redeemer, and that representation is roughly twice the size of the
// library's own compact CBOR. Measured end to end by
// `submit-init-emulator-max-proof-fit.test.ts` at two real depths, not derived.
export const PROOF_TRANSACTION_BRANCH_LEVEL_BYTES = 276;

const MPF_PATH_NIBBLES = 64;

const mpfPathDigest = (key: Buffer): Buffer =>
  Buffer.from(computeHash32(Uint8Array.from(key)));

const sharedNibbleCount = (left: Buffer, right: Buffer): number => {
  const a = left.toString("hex");
  const b = right.toString("hex");
  let shared = 0;
  while (shared < a.length && a[shared] === b[shared]) {
    shared += 1;
  }
  return shared;
};

/**
 * Deterministically grind trie keys whose hashed path diverges from
 * `targetKey`'s hashed path at exactly level 0, 1, ... `branchLevels - 1`, two
 * per level. `domain` separates one family's grind from another's so two
 * fixtures in the same block cannot reuse each other's keys.
 */
export const adversarialMembershipSiblingKeys = ({
  targetKey,
  branchLevels,
  domain,
}: {
  readonly targetKey: Buffer;
  readonly branchLevels: number;
  readonly domain: number;
}): readonly Buffer[] => {
  if (branchLevels < 0 || branchLevels > MPF_PATH_NIBBLES) {
    throw new Error(
      `Adversarial branch level count ${branchLevels.toString()} is outside the 0..${MPF_PATH_NIBBLES.toString()} nibbles of an MPF path.`,
    );
  }
  const targetPath = mpfPathDigest(targetKey);
  const candidate = Buffer.alloc(32, 0x00);
  candidate.writeUInt32BE(domain >>> 0, 8);
  const keys: Buffer[] = [];
  let counter = 0;
  for (let level = 0; level < branchLevels; level += 1) {
    // Two siblings that diverge at this level are not enough on their own: if
    // they diverge into the SAME nibble slot the on-path node still holds only
    // two children and serializes as the cheaper `fork`. Requiring distinct
    // divergence nibbles makes every on-path node a three-child node, which is
    // what forces the largest `branch` step shape at every level.
    const takenNibbles = new Set<string>();
    while (takenNibbles.size < 2) {
      candidate.writeUInt32BE(counter >>> 0, 0);
      counter += 1;
      const path = mpfPathDigest(candidate);
      if (sharedNibbleCount(path, targetPath) !== level) {
        continue;
      }
      const nibble = path.toString("hex")[level]!;
      if (takenNibbles.has(nibble)) {
        continue;
      }
      takenNibbles.add(nibble);
      keys.push(Buffer.from(candidate));
    }
  }
  return keys;
};

/** Every grinded sibling carries the same one-byte filler value. */
export const ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE = Buffer.from("ad", "hex");
