import { type Hash32 } from "./codec/hash.js";
import {
  combine,
  hash32,
  type MidgardMpfProofStep,
  nibbleAt,
  NULL_HASH,
} from "./mpf-proof-fold.parse-midgard-mpf-proof-json.js";

// A deletion whose terminal proof step is a Branch keeps that branch only if
// at least two other children remain; with one it collapses into that child,
// and the Branch step would stand in for a node the honest trie no longer
// holds. Its four neighbour groups (n8, n4, n2, n1) show two remaining
// children directly when two of them are non-empty. When exactly one group of
// two or more slots is non-empty, the prover opens it down to the level where
// both halves are non-empty. This is the twin of
// `terminal_branch_keeps_two_children` in `mpf-proof-v1.ak`.
//
// The opening is `side_0 ‖ … ‖ side_{d-1} ‖ left ‖ right`: one side byte per
// level walked down from the group root (0 = left, anything else = right),
// then the two 32-byte halves at the split. The empty siblings passed on the
// way down are null subtrees and are not sent.

const DIGEST_BYTES = 32;

export const NULL_HASH_2: Hash32 = combine(NULL_HASH, NULL_HASH);
export const NULL_HASH_4: Hash32 = combine(NULL_HASH_2, NULL_HASH_2);
export const NULL_HASH_8: Hash32 = combine(NULL_HASH_4, NULL_HASH_4);

// log2 of a group's slot count, in neighbour order n8, n4, n2, n1.
const GROUP_LOGS = [3, 2, 1, 0] as const;

const groupStart = (me: number, size: number): number =>
  (Math.floor(me / size) ^ 1) * size;

const merkleOf = (hashes: readonly Uint8Array[]): Uint8Array => {
  let level = hashes;
  while (level.length > 1) {
    const next: Uint8Array[] = [];
    for (let index = 0; index < level.length; index += 2) {
      next.push(combine(level[index]!, level[index + 1]!));
    }
    level = next;
  }
  return level[0]!;
};

// The null hash of a subtree of 2^(splitLog - 1) slots: one half of a node
// covering 2^splitLog slots.
const halfNull = (splitLog: number): Uint8Array =>
  splitLog === 1 ? NULL_HASH : splitLog === 2 ? NULL_HASH_2 : NULL_HASH_4;

const climb = (
  hash: Uint8Array,
  log: number,
  opening: Uint8Array,
  remaining: number,
): Uint8Array => {
  if (remaining === 0) {
    return hash;
  }
  const empty = halfNull(log + 1);
  const parent =
    opening[remaining - 1] === 0 ? combine(hash, empty) : combine(empty, hash);
  return climb(parent, log + 1, opening, remaining - 1);
};

const openedGroupSplits = (
  group: Uint8Array,
  groupLog: number,
  opening: Uint8Array,
): boolean => {
  const depth = opening.length - 2 * DIGEST_BYTES;
  const splitLog = groupLog - depth;
  if (depth < 0 || splitLog < 1) {
    return false;
  }
  const left = opening.subarray(depth, depth + DIGEST_BYTES);
  const right = opening.subarray(depth + DIGEST_BYTES);
  const empty = halfNull(splitLog);
  return (
    !Buffer.from(left).equals(empty) &&
    !Buffer.from(right).equals(empty) &&
    Buffer.from(climb(combine(left, right), splitLog, opening, depth)).equals(
      group,
    )
  );
};

/** Whether a deletion's terminal Branch `neighbors` (n8 ‖ n4 ‖ n2 ‖ n1) keep
 * two other children, reading `opening` only when one group is non-empty. */
export const midgardMpfTerminalBranchKeepsTwoChildren = (
  neighbors: Uint8Array,
  opening: Uint8Array,
): boolean => {
  if (neighbors.length !== 4 * DIGEST_BYTES) {
    return false;
  }
  const bytes = Buffer.from(neighbors);
  const n8 = bytes.subarray(0, 32);
  const n4 = bytes.subarray(32, 64);
  const n2 = bytes.subarray(64, 96);
  const n1 = bytes.subarray(96, 128);
  if (!n8.equals(NULL_HASH_8)) {
    return (
      !n4.equals(NULL_HASH_4) ||
      !n2.equals(NULL_HASH_2) ||
      !n1.equals(NULL_HASH) ||
      openedGroupSplits(n8, 3, opening)
    );
  }
  if (!n4.equals(NULL_HASH_4)) {
    return (
      !n2.equals(NULL_HASH_2) ||
      !n1.equals(NULL_HASH) ||
      openedGroupSplits(n4, 2, opening)
    );
  }
  if (!n2.equals(NULL_HASH_2)) {
    return !n1.equals(NULL_HASH) || openedGroupSplits(n2, 1, opening);
  }
  return false;
};

const openGroup = (
  children: readonly (Uint8Array | undefined)[],
  start: number,
  size: number,
  sides: readonly number[],
): Buffer => {
  const half = size / 2;
  const hashesOf = (from: number): Uint8Array[] =>
    children.slice(from, from + half).map((child) => child ?? NULL_HASH);
  const occupied = (from: number): boolean =>
    children.slice(from, from + half).some((child) => child !== undefined);
  const left = occupied(start);
  const right = occupied(start + half);
  if (left && right) {
    return Buffer.concat([
      Buffer.from(sides),
      merkleOf(hashesOf(start)),
      merkleOf(hashesOf(start + half)),
    ]);
  }
  if (half === 1) {
    return Buffer.alloc(0);
  }
  return left
    ? openGroup(children, start, half, [...sides, 0])
    : openGroup(children, start + half, half, [...sides, 1]);
};

/** The opening an honest prover sends with a deletion's terminal Branch for
 * child `me` of a node whose sixteen child hashes are `children` (undefined
 * where empty): empty unless exactly one neighbour group of two or more slots
 * is non-empty, then that group opened down to its first split. */
export const midgardMpfDeletionOpening = (
  children: readonly (Uint8Array | undefined)[],
  me: number,
): Buffer => {
  if (children.length !== 16 || !Number.isInteger(me) || me < 0 || me > 15) {
    throw new Error("MPF deletion opening needs 16 children and a slot");
  }
  const occupied = GROUP_LOGS.map((log) => ({
    log,
    start: groupStart(me, 2 ** log),
  })).filter(({ log, start }) =>
    children
      .slice(start, start + 2 ** log)
      .some((child) => child !== undefined),
  );
  if (occupied.length !== 1 || occupied[0]!.log === 0) {
    return Buffer.alloc(0);
  }
  return openGroup(children, occupied[0]!.start, 2 ** occupied[0]!.log, []);
};

/** A trie node as the off-chain MPF library exposes it: a branch carries its
 * sixteen children, each a node or a stored hash reference. */
export type MidgardMpfBranchLike = {
  readonly children?: readonly ({ readonly hash: Uint8Array } | undefined)[];
};

/** The part of the off-chain MPF trie the opening reads. */
export type MidgardMpfTrieLike = {
  childAt(path: string): Promise<unknown>;
};

/** The opening for deleting `key` with `steps`, read from the pre-delete
 * `trie`. It is empty unless the terminal step is a Branch; the result is
 * checked against that step before it is returned. */
export const buildMidgardMpfDeletionOpening = async (
  trie: MidgardMpfTrieLike,
  key: Uint8Array,
  steps: readonly MidgardMpfProofStep[],
): Promise<Buffer> => {
  const terminal = steps.at(-1);
  if (terminal === undefined || terminal.kind !== "branch") {
    return Buffer.alloc(0);
  }
  const cursor = steps
    .slice(0, -1)
    .reduce((sum, step) => sum + 1 + step.skip, 0);
  const path = hash32(key);
  const node = (await trie.childAt(
    Buffer.from(path).toString("hex").slice(0, cursor),
  )) as MidgardMpfBranchLike | undefined;
  const children = node?.children;
  if (children === undefined || children.length !== 16) {
    throw new Error("MPF deletion opening: terminal node is not a branch");
  }
  const opening = midgardMpfDeletionOpening(
    children.map((child) => child?.hash),
    nibbleAt(path, cursor + terminal.skip),
  );
  if (!midgardMpfTerminalBranchKeepsTwoChildren(terminal.neighbors, opening)) {
    throw new Error(
      "MPF deletion opening does not show two remaining children",
    );
  }
  return opening;
};
