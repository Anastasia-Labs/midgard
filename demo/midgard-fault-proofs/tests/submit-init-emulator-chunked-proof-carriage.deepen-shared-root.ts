import {
  type CompleteSignedTransactionMeasurement,
  printProofFit,
} from "./support/submit-init-emulator-shared.js";
import {
  syntheticDeepMembershipProof,
  syntheticDeepSharedRootProofs,
} from "./support/synthetic-deep-proof.js";

/**
 * The depth this file drives. 22 is past the envelope-exhaustion level of every
 * family on the direct route (21 for Q12, 22 for Q10/Q11, 23 for Q14), so no
 * proof of this depth can reach L1 through a step redeemer.
 */
export const CARRIAGE_BRANCH_LEVELS = 22;

/** The depth a 2^128 adversary reaches: 128 / 4 bits of forced prefix. */
export const ADVERSARY_BRANCH_LEVELS = 32;

/** Both depths need this many publication UTxOs at 16 steps per chunk. */
export const EXPECTED_CHUNK_COUNT = 2;

export const printCarriageFit = (
  label: string,
  proofFit: Record<string, CompleteSignedTransactionMeasurement>,
  extra: Record<string, unknown>,
): void =>
  printProofFit({
    headline: `${label} chunked carriage fit`,
    stages: proofFit,
    extra,
    includeReferenceInputs: true,
  });

/**
 * Re-commits a fixture's inclusion evidence at `branchLevels` forced branch
 * levels: the same challenged transaction, a synthesised deep proof, and the
 * raw root that proof proves. The header the setup transaction commits is built
 * from the returned root, so the step's counted-root authentication is genuine.
 */
export const deepenInclusion = ({
  inclusion,
  branchLevels,
}: {
  readonly inclusion: unknown;
  readonly branchLevels: number;
}) => {
  const parsed = inclusion as {
    readonly nativeTxId: string;
    readonly l2TransactionSourceCbor: string;
  };
  // The transactions trie commits `Data(L2TransactionSourceV1)` per tx id, not
  // the bare compact encoding, so the synthesised proof must prove that value —
  // it is the value the step's inclusion claim names.
  const deep = syntheticDeepMembershipProof({
    key: Buffer.from(parsed.nativeTxId, "hex"),
    value: Buffer.from(parsed.l2TransactionSourceCbor, "hex"),
    branchLevels,
  });
  return {
    deep,
    inclusion: {
      ...(inclusion as Record<string, unknown>),
      transactionsPhasRoot: deep.transactionsPhasRoot,
      txMembershipProofCbor: deep.proofCbor,
    },
  };
};

/** A fixture's inclusion record, whose declared type is deliberately opaque. */
export const inclusionRecord = (
  inclusion: unknown,
): Record<string, unknown> & {
  readonly nativeTxId: string;
  readonly l2TransactionSourceCbor: string;
} =>
  inclusion as Record<string, unknown> & {
    readonly nativeTxId: string;
    readonly l2TransactionSourceCbor: string;
  };

/**
 * The same re-commitment for a family that opens the challenged block's
 * transactions trie more than once (issue #549). Q10 opens it twice for
 * membership (tx1 at step-01, tx2 at step-02); Q11 opens it once for membership
 * and once for ABSENCE (the phantom input's producing transaction, at step-04).
 * All openings must reconstruct one root, because the step validators
 * re-authenticate every one of them against the root the challenged header
 * commits.
 */
export const deepenSharedRoot = ({
  members,
  absentKeys = [],
  branchLevels,
}: {
  readonly members: readonly {
    readonly nativeTxId: string;
    readonly l2TransactionSourceCbor: string;
  }[];
  readonly absentKeys?: readonly string[];
  readonly branchLevels: number;
}) => {
  // Same committed value as `deepenInclusion`: the trie's per-tx-id value is
  // `Data(L2TransactionSourceV1)`.
  const shared = syntheticDeepSharedRootProofs({
    claims: [
      ...members.map((member) => ({
        key: Buffer.from(member.nativeTxId, "hex"),
        value: Buffer.from(member.l2TransactionSourceCbor, "hex"),
      })),
      ...absentKeys.map((key) => ({ key: Buffer.from(key, "hex") })),
    ],
    branchLevels,
  });
  const openings = shared.openings;
  return {
    root: shared.root,
    membershipProofCbors: openings
      .slice(0, members.length)
      .map((opening) => opening.proofCbor),
    absenceProofCbors: openings
      .slice(members.length)
      .map((opening) => opening.proofCbor),
    proofCborBytes: openings[0]!.proofCborBytes,
  };
};
