import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfDeletionOpening,
  buildMidgardMpfProofFoldTrace,
  midgardMpfDeletionOpening,
  type MidgardMpfProofStep,
  midgardMpfTerminalBranchKeepsTwoChildren,
  NULL_HASH_2,
  NULL_HASH_4,
  NULL_HASH_8,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

// A deletion's terminal Branch must keep two other children
// (`terminal_branch_keeps_two_children` in mpf-proof-v1.ak). These tests pin
// the TypeScript twin, the honest opening and the fold-trace refusal.

const NULL = Buffer.alloc(32);
const hash = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));
const combine = (left: Uint8Array, right: Uint8Array): Buffer =>
  hash(Buffer.concat([left, right]));
const merkle = (hashes: readonly Buffer[]): Buffer => {
  if (hashes.length === 1) return hashes[0]!;
  const next: Buffer[] = [];
  for (let index = 0; index < hashes.length; index += 2) {
    next.push(combine(hashes[index]!, hashes[index + 1]!));
  }
  return merkle(next);
};
type Children = readonly (Buffer | undefined)[];
const group = (children: Children, start: number, size: number): Buffer =>
  merkle(children.slice(start, start + size).map((child) => child ?? NULL));
// n8 ‖ n4 ‖ n2 ‖ n1 of child `me`.
const neighborsOf = (children: Children, me: number): Buffer =>
  Buffer.concat(
    [8, 4, 2, 1].map((size) =>
      group(children, (Math.floor(me / size) ^ 1) * size, size),
    ),
  );
const childrenAt = (slots: readonly number[], tag = 0): Children =>
  Array.from({ length: 16 }, (_, slot) =>
    slots.includes(slot) ? hash(Buffer.from([slot, tag])) : undefined,
  );

// The same slot keys as `mpf_branch_opening_fixture.slot_key`: a one-byte key
// whose path starts with nibble `slot`.
const SLOT_KEYS = [
  "00", "42", "19", "0f", "0b", "0a", "04", "1f",
  "07", "0e", "13", "02", "0d", "16", "01", "05",
].map((hex) => Buffer.from(hex, "hex")); // prettier-ignore

const newTrie = async (): Promise<Trie> => {
  const store = new Store(undefined);
  await store.ready();
  return new Trie(store);
};
const exactRoot = (trie: Trie): Buffer =>
  trie.hash == null ? Buffer.alloc(32) : Buffer.from(trie.hash);
const slotTrie = async (slots: readonly number[]): Promise<Trie> => {
  const trie = await newTrie();
  for (const slot of slots)
    await trie.insert(SLOT_KEYS[slot]!, SLOT_KEYS[slot]!);
  return trie;
};
const proveSteps = async (
  trie: Trie,
  key: Buffer,
): Promise<readonly MidgardMpfProofStep[]> =>
  parseMidgardMpfProofJson((await trie.prove(key)).toJSON());

// Computed independently from the Aiken fixture
// (`mpf_terminal_branch_opening_cross_language_vector`).
const CROSS_LANGUAGE_OPENING =
  "000124ad99ebbc63b632ebc3667337d6acf7309b9129dd2991caa9422f307381c91d9b61e4dff12c06adfb269d41e9cc73eb8592229e3f7bba464971d09ef9387a92";

const subsets = (items: readonly number[], size: number): number[][] =>
  size === 0
    ? [[]]
    : items.flatMap((item, index) =>
        subsets(items.slice(index + 1), size - 1).map((rest) => [
          item,
          ...rest,
        ]),
      );

describe("MPF deletion terminal-Branch rule", () => {
  it("pins the null subtree hashes", () => {
    expect(NULL_HASH_2).toEqual(combine(NULL, NULL));
    expect(NULL_HASH_4).toEqual(combine(NULL_HASH_2, NULL_HASH_2));
    expect(NULL_HASH_8).toEqual(combine(NULL_HASH_4, NULL_HASH_4));
  });

  it("builds the Aiken cross-language opening from a real trie", async () => {
    const trie = await slotTrie([5, 10, 11]);
    const key = SLOT_KEYS[5]!;
    const steps = await proveSteps(trie, key);
    expect(steps).toHaveLength(1);
    expect(steps[0]!.kind).toBe("branch");
    const opening = await buildMidgardMpfDeletionOpening(trie, key, steps);
    expect(opening.toString("hex")).toBe(CROSS_LANGUAGE_OPENING);
    const step = steps[0] as Extract<MidgardMpfProofStep, { kind: "branch" }>;
    expect(
      midgardMpfTerminalBranchKeepsTwoChildren(step.neighbors, opening),
    ).toBe(true);
    expect(
      midgardMpfTerminalBranchKeepsTwoChildren(step.neighbors, Buffer.alloc(0)),
    ).toBe(false);
  });

  it("accepts exactly the honest shapes with two or more other children", () => {
    let checked = 0;
    for (let me = 0; me < 16; me += 1) {
      const others = Array.from({ length: 16 }, (_, slot) => slot).filter(
        (slot) => slot !== me,
      );
      for (let size = 0; size <= 4; size += 1) {
        for (const chosen of subsets(others, size)) {
          const children = childrenAt([me, ...chosen]);
          const opening = midgardMpfDeletionOpening(children, me);
          expect(
            midgardMpfTerminalBranchKeepsTwoChildren(
              neighborsOf(children, me),
              opening,
            ),
          ).toBe(chosen.length >= 2);
          expect(opening.length <= 66).toBe(true);
          checked += 1;
        }
      }
    }
    expect(checked).toBe(16 * (1 + 15 + 105 + 455 + 1365));
  });

  it("refuses a Branch standing in for a node with one other child, whatever the opening", () => {
    for (let me = 0; me < 16; me += 1) {
      for (let other = 0; other < 16; other += 1) {
        if (other === me) continue;
        const children = childrenAt([me, other]);
        const neighbors = neighborsOf(children, me);
        const lone = children[other]!;
        const openings = [
          Buffer.alloc(0),
          Buffer.concat([lone, NULL]),
          Buffer.concat([NULL, lone]),
          Buffer.concat([Buffer.from([0]), lone, NULL]),
          Buffer.concat([Buffer.from([1]), NULL, lone]),
          Buffer.concat([Buffer.from([0, 0]), lone, NULL]),
          Buffer.concat([Buffer.from([1, 1]), NULL, lone]),
          Buffer.concat([lone, lone]),
        ];
        for (const opening of openings) {
          expect(
            midgardMpfTerminalBranchKeepsTwoChildren(neighbors, opening),
          ).toBe(false);
        }
      }
    }
  });

  it("refuses a tampered opening of a lone group", () => {
    const children = childrenAt([5, 10, 11]);
    const neighbors = neighborsOf(children, 5);
    const honest = midgardMpfDeletionOpening(children, 5);
    expect(honest).toHaveLength(66);
    expect(midgardMpfTerminalBranchKeepsTwoChildren(neighbors, honest)).toBe(
      true,
    );
    const sides = honest.subarray(0, 2);
    const left = honest.subarray(2, 34);
    const right = honest.subarray(34);
    const tampered = [
      Buffer.concat([Buffer.from([1, 1]), left, right]),
      Buffer.concat([Buffer.from([0, 0]), left, right]),
      Buffer.concat([sides, right, left]),
      Buffer.concat([sides, left, left]),
      Buffer.concat([sides, NULL, right]),
      Buffer.concat([sides, left, NULL]),
      Buffer.concat([sides.subarray(1), left, right]),
      Buffer.concat([Buffer.from([0]), sides, left, right]),
      honest.subarray(0, 65),
      Buffer.concat([honest, Buffer.from([0])]),
      Buffer.alloc(0),
    ];
    for (const opening of tampered) {
      expect(midgardMpfTerminalBranchKeepsTwoChildren(neighbors, opening)).toBe(
        false,
      );
    }
    expect(
      midgardMpfTerminalBranchKeepsTwoChildren(neighbors.subarray(1), honest),
    ).toBe(false);
  });

  // Mirrors `mpf_terminal_branch_refuses_an_opening_below_a_single_slot`: a
  // lone child whose hash is the combine of two 32-byte values hashes back to
  // its group when read as a split below its own slot, but is one child.
  it("refuses an opening that splits below a single slot", () => {
    const left = hash(Buffer.from([1]));
    const right = hash(Buffer.from([2]));
    const halves = Buffer.concat([left, right]);
    for (const [slot, depth] of [
      [6, 1],
      [0, 2],
      [8, 3],
    ] as const) {
      const children: (Buffer | undefined)[] = childrenAt([]).slice();
      children[5] = left;
      children[slot] = combine(left, right);
      expect(
        midgardMpfTerminalBranchKeepsTwoChildren(
          neighborsOf(children, 5),
          Buffer.concat([Buffer.alloc(depth), halves]),
        ),
      ).toBe(false);
    }
  });

  it("refuses the Branch masquerade in the deletion fold trace and its opening builder", async () => {
    const trie = await slotTrie([5, 10]);
    const key = SLOT_KEYS[5]!;
    const honestSteps = await proveSteps(trie, key);
    expect(honestSteps.map(({ kind }) => kind)).toEqual(["leaf"]);
    const leafTen = (await trie.childAt("a")) as { hash: Buffer };
    const children: (Buffer | undefined)[] = childrenAt([]).slice();
    children[10] = Buffer.from(leafTen.hash);
    const masquerade: readonly MidgardMpfProofStep[] = [
      { kind: "branch", skip: 0, neighbors: neighborsOf(children, 5) },
    ];
    const including = buildMidgardMpfProofFoldTrace({
      key,
      value: key,
      steps: masquerade,
    });
    expect(including.terminal.includingRoot).toEqual(exactRoot(trie));
    for (const deletionOpening of [
      Buffer.alloc(0),
      Buffer.concat([Buffer.from([1]), children[10], NULL]),
    ]) {
      expect(() =>
        buildMidgardMpfProofFoldTrace({
          key,
          value: key,
          steps: masquerade,
          deletionOpening,
        }),
      ).toThrow("MPF terminal branch of a deletion does not keep two children");
    }
    await expect(
      buildMidgardMpfDeletionOpening(trie, key, masquerade),
    ).rejects.toThrow(
      "MPF deletion opening does not show two remaining children",
    );
  });

  it("gives every real deletion an opening its fold accepts", async () => {
    const trie = await newTrie();
    const entries = Array.from({ length: 400 }, (_, index) => ({
      key: hash(Buffer.from(`deletion-opening-${index}`)),
      value: Buffer.from(`value-${index}`),
    }));
    for (const { key, value } of entries) await trie.insert(key, value);
    let opened = 0;
    let branches = 0;
    for (const { key, value } of entries.reverse()) {
      const steps = await proveSteps(trie, key);
      const opening = await buildMidgardMpfDeletionOpening(trie, key, steps);
      const trace = buildMidgardMpfProofFoldTrace({
        key,
        value,
        steps,
        deletionOpening: opening,
      });
      await trie.delete(key);
      expect(trace.terminal.excludingRoot).toEqual(exactRoot(trie));
      if (steps.at(-1)?.kind === "branch") branches += 1;
      else expect(opening).toHaveLength(0);
      if (opening.length > 0) opened += 1;
    }
    expect(branches).toBeGreaterThan(0);
    expect(opened).toBeGreaterThan(0);
  });
});
