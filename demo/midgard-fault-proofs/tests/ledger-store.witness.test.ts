import { midgardBlake2b } from "@al-ft/midgard-core/codec/hash";
import { describe, expect, it } from "vitest";

import {
  ledgerEntriesRoot,
  type LedgerEntry,
  ledgerEntryLeaf,
} from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  ledgerFromWitness,
  type LedgerMutation,
  LedgerNodeMissing,
  type LedgerWitness,
  LedgerWitnessRefused,
  recordWitness,
  type StoredNode,
  type WitnessBranch,
  type WitnessNode,
} from "../src/ledger-store/index.js";
import {
  event,
  testLedgerEntries,
  testLedgerEntry,
  testOutRef,
  UNDECODABLE_ENTRY,
} from "./support/ledger-store-conformance.js";

/** One replay: mutations, then reads and proofs of `keys` on the result. */
type Replay = {
  readonly mutations: readonly LedgerMutation[][];
  readonly keys: readonly Buffer[];
};

type Observed = {
  readonly roots: string[];
  readonly reads: [string, string | undefined, string][];
};

const run = async (ledger: Ledger, replay: Replay): Promise<Observed> => {
  const roots: string[] = [];
  let current = ledger;
  for (const mutations of replay.mutations) {
    current = await current.apply(mutations);
    roots.push(current.root);
  }
  const reads = await current.read(replay.keys);
  const observed: [string, string | undefined, string][] = [];
  for (const [outRef, read] of reads)
    observed.push([
      outRef,
      read.output?.toString("hex"),
      (await read.proof()).toString("hex"),
    ]);
  return { roots, reads: observed };
};

const persisted = async (entries: readonly LedgerEntry[]) => {
  const storage = createMemoryLedgerStorage();
  const store = createLedgerStore(storage);
  const ledger = await store.admit(entries, await ledgerEntriesRoot(entries));
  return { storage, store, ledger: await store.at(ledger.root) };
};

/**
 * Runs `replay` on the full store, records it, rebuilds the ledger from the
 * witness alone and runs it again: roots and proof bytes must be equal.
 */
const expectWitnessReplays = async (
  entries: readonly LedgerEntry[],
  replay: Replay,
) => {
  const { ledger } = await persisted(entries);
  const expected = await run(ledger, replay);
  const recorder = recordWitness(ledger);
  expect(await run(recorder.ledger, replay)).toEqual(expected);
  const witness = recorder.witness();
  expect(await run(ledgerFromWitness(ledger.root, witness), replay)).toEqual(
    expected,
  );
  return { ledger, witness, expected };
};

const digest = (...parts: readonly Uint8Array[]) =>
  Buffer.from(midgardBlake2b(Buffer.concat(parts), { dkLen: 32 }));

/** A node's hash, derived from the trie's hashing rules for a hand-built
 * malformed trie. */
const hashOf = (node: WitnessNode): string => {
  if (node.__kind === "Leaf") {
    const odd = node.prefix.length % 2 === 1;
    const head = odd
      ? Buffer.from([0x10, Number.parseInt(node.prefix[0]!, 16)])
      : Buffer.from([0xff]);
    const tail = Buffer.from(odd ? node.prefix.slice(1) : node.prefix, "hex");
    return digest(head, tail, digest(Buffer.from(node.value, "hex"))).toString(
      "hex",
    );
  }
  let layer = node.children.map((child) =>
    child === null ? Buffer.alloc(32) : Buffer.from(child, "hex"),
  );
  while (layer.length > 1)
    layer = Array.from({ length: layer.length / 2 }, (_, index) =>
      digest(layer[index * 2]!, layer[index * 2 + 1]!),
    );
  const prefix = Buffer.from(
    [...node.prefix].map((nibble) => Number.parseInt(nibble, 16)),
  );
  return digest(prefix, layer[0]!).toString("hex");
};

const keyPath = (entry: LedgerEntry) => digest(entry.outRef).toString("hex");

describe("ledger witnesses replay exactly as the full store", () => {
  it("deletes down to the empty ledger", async () => {
    const entries = testLedgerEntries(0, 6);
    const { expected } = await expectWitnessReplays(entries, {
      mutations: [event([0, 1, 2], []), event([3, 4, 5], [])],
      keys: [testOutRef(0), testOutRef(5)],
    });
    expect(expected.roots.at(-1)).toBe(EMPTY_LEDGER_ROOT);
  });

  it("collapses a two-leaf branch to a leaf", async () => {
    const entries = testLedgerEntries(0, 2);
    const { ledger } = await persisted(entries);
    const { expected } = await expectWitnessReplays(entries, {
      mutations: [event([0], [])],
      keys: [testOutRef(0), testOutRef(1)],
    });
    expect(expected.roots[0]).toBe(
      await ledgerEntriesRoot([testLedgerEntry(1)]),
    );
    expect(ledger.root).not.toBe(expected.roots[0]);
  });

  it("collapses onto a sibling branch read only for the collapse", async () => {
    const entries = testLedgerEntries(0, 400);
    const { storage, ledger } = await persisted(entries);
    const node = async (hash: string): Promise<StoredNode> =>
      (await storage.readNodes([hash]))[0]!;
    // A branch whose only children are one leaf and one branch.
    let found: { leaf: string; sibling: string } | undefined;
    const pending = [ledger.root];
    while (found === undefined && pending.length > 0) {
      const current = await node(pending.pop()!);
      if (current.__kind === "Leaf") continue;
      const children = current.children.filter((child) => child !== null);
      pending.push(...children);
      if (children.length !== 2) continue;
      const [first, second] = await Promise.all(children.map(node));
      if (first!.__kind === "Leaf" && second!.__kind === "Branch")
        found = { leaf: first!.key, sibling: children[1]! };
      if (first!.__kind === "Branch" && second!.__kind === "Leaf")
        found = { leaf: second!.key, sibling: children[0]! };
    }
    expect(found).toBeDefined();
    const deleted = Buffer.from(found!.leaf, "hex");
    const { witness } = await expectWitnessReplays(entries, {
      mutations: [[{ type: "delete", outRef: deleted }]],
      keys: [],
    });
    expect(witness.nodes.map(hashOf)).toContain(found!.sibling);
    // The sibling is not on the deleted key's path: a lookup alone does not
    // read it.
    const lookup = recordWitness(ledger);
    await lookup.ledger.read([deleted]);
    expect(lookup.witness().nodes.map(hashOf)).not.toContain(found!.sibling);
  });

  it("proves non-membership and membership", async () => {
    const entries = testLedgerEntries(0, 40);
    await expectWitnessReplays(entries, {
      mutations: [],
      keys: [testOutRef(3), testOutRef(700), testOutRef(701), testOutRef(39)],
    });
  });

  it("replays a block of events with inserts, deletes and proofs", async () => {
    const entries = [
      ...testLedgerEntries(0, 60),
      testLedgerEntry(UNDECODABLE_ENTRY),
    ];
    await expectWitnessReplays(entries, {
      mutations: [
        event([0, 1, 2], [100, 101]),
        event([100, 3], [102, 103, 104]),
        event([UNDECODABLE_ENTRY, 4], [1]),
      ],
      keys: [testOutRef(1), testOutRef(2), testOutRef(103), testOutRef(5)],
    });
  });
});

describe("ledgerFromWitness refuses what does not open the root", () => {
  const witnessed = async () => {
    const entries = testLedgerEntries(0, 30);
    const { ledger, witness } = await expectWitnessReplays(entries, {
      mutations: [event([0, 1], [200])],
      keys: [testOutRef(2), testOutRef(800)],
    });
    return { root: ledger.root, witness };
  };

  it("fails replay with LedgerNodeMissing when a read node is left out", async () => {
    const { root, witness } = await witnessed();
    const nodes = [...witness.nodes];
    // The leaf of out-ref 0, which the replay deletes: it must read it, and
    // no witnessed output rests on it. Chosen by key, not by position: nodes
    // are listed by hash, and leaf hashes vary from run to run with the test
    // address.
    const deleted = testOutRef(0).toString("hex");
    const leaf = nodes.findIndex(
      (node) => node.__kind === "Leaf" && node.key === deleted,
    );
    expect(leaf).toBeGreaterThanOrEqual(0);
    const [left] = nodes.splice(leaf, 1);
    const partial = ledgerFromWitness(root, { ...witness, nodes });
    const refusal = await run(partial, {
      mutations: [event([0, 1], [200])],
      keys: [testOutRef(2), testOutRef(800)],
    }).catch((error: unknown) => error);
    expect(refusal).toBeInstanceOf(LedgerNodeMissing);
    expect((refusal as LedgerNodeMissing).hash).toBe(hashOf(left!));
  });

  it("refuses a node not reachable from the root", async () => {
    const { root, witness } = await witnessed();
    const { ledger: foreign } = await persisted(testLedgerEntries(500, 3));
    const recorder = recordWitness(foreign);
    await recorder.ledger.read([testOutRef(500)]);
    expect(() =>
      ledgerFromWitness(root, {
        ...witness,
        nodes: [...witness.nodes, ...recorder.witness().nodes],
      }),
    ).toThrow(/not reachable from the root/);
  });

  it("refuses an output that is not its leaf's value, or has no leaf", async () => {
    const { root, witness } = await witnessed();
    const [first, ...rest] = witness.outputs;
    const tampered: LedgerWitness = {
      ...witness,
      outputs: [
        { outRef: first!.outRef, output: testLedgerEntry(9).output },
        ...rest,
      ],
    };
    expect(() => ledgerFromWitness(root, tampered)).toThrow(
      /is not its leaf's value/,
    );
    expect(() =>
      ledgerFromWitness(root, {
        ...witness,
        outputs: [...witness.outputs, testLedgerEntry(900)],
      }),
    ).toThrow(/has no witnessed leaf/);
  });

  /** The two-entry ledger's root branch, built by hand: its two leaves sit
   * at nibbles `a` and `b`; `spare` is an empty slot. */
  const twoLeafRoot = async () => {
    const [left, right] = testLedgerEntries(0, 2).sort((a, b) =>
      keyPath(a) < keyPath(b) ? -1 : 1,
    );
    const leaf = (entry: LedgerEntry): WitnessNode => ({
      __kind: "Leaf",
      prefix: keyPath(entry).slice(1),
      key: entry.outRef.toString("hex"),
      value: ledgerEntryLeaf(entry, "test").leaf.toString("hex"),
    });
    const [l, r] = [leaf(left!), leaf(right!)];
    const branch = (slots: Record<number, WitnessNode>): WitnessBranch => ({
      __kind: "Branch",
      prefix: "",
      children: Array.from({ length: 16 }, (_, nibble) =>
        slots[nibble] === undefined ? null : hashOf(slots[nibble]),
      ),
    });
    const a = Number.parseInt(keyPath(left!)[0]!, 16);
    const b = Number.parseInt(keyPath(right!)[0]!, 16);
    const honest = branch({ [a]: l, [b]: r });
    // The two-entry root opens with the honest shape.
    expect(hashOf(honest)).toBe(await ledgerEntriesRoot([left!, right!]));
    const spare = [...Array(16).keys()].find((n) => n !== a && n !== b)!;
    return { left: left!, right: right!, l, r, a, b, spare, honest, branch };
  };

  it("refuses a branch child equal to the empty slot's hash", async () => {
    const { left, right, l, r, honest, spare } = await twoLeafRoot();
    const children = [...honest.children];
    children[spare] = "00".repeat(32);
    const forged: WitnessBranch = { ...honest, children };
    // The trie hashes an all-zero child exactly as an empty slot, so the
    // forged branch opens the honest root.
    expect(hashOf(forged)).toBe(hashOf(honest));
    const opened = (() => {
      try {
        return ledgerFromWitness(hashOf(honest), {
          nodes: [forged, l, r],
          outputs: [],
        });
      } catch (error) {
        return error;
      }
    })();
    expect(opened).toBeInstanceOf(LedgerWitnessRefused);
    expect((opened as Error).message).toMatch(/empty slot/);
    // The honest witness collapses to the one remaining leaf on delete.
    const after = await ledgerFromWitness(hashOf(honest), {
      nodes: [honest, l, r],
      outputs: [],
    }).apply([{ type: "delete", outRef: left.outRef }]);
    expect(after.root).toBe(await ledgerEntriesRoot([right]));
  });

  it("refuses a leaf away from its key's path and a node at two paths", async () => {
    const { l, r, a, b, spare, branch } = await twoLeafRoot();
    const swapped = branch({ [spare]: l, [b]: r });
    expect(() =>
      ledgerFromWitness(hashOf(swapped), {
        nodes: [swapped, l, r],
        outputs: [],
      }),
    ).toThrow(/not at its key's path/);
    // `r` again at a slot visited after its own: reached at two paths.
    const earlier = [...Array(b).keys()].find((n) => n !== a);
    expect(earlier).toBeDefined();
    const twice = branch({ [a]: l, [b]: r, [earlier!]: r });
    expect(() =>
      ledgerFromWitness(hashOf(twice), { nodes: [twice, l, r], outputs: [] }),
    ).toThrow(/reached at two paths/);
    const lonely = branch({ [b]: r });
    expect(() =>
      ledgerFromWitness(hashOf(lonely), { nodes: [lonely, r], outputs: [] }),
    ).toThrow(/at least two children/);
  });

  it("never persists a recording or witness ledger", async () => {
    const { store, ledger } = await persisted(testLedgerEntries(0, 5));
    const recorder = recordWitness(ledger);
    const derived = await recorder.ledger.apply(event([0], []));
    const rebuilt = ledgerFromWitness(ledger.root, recorder.witness());
    for (const foreign of [recorder.ledger, derived, rebuilt])
      await expect(store.persist(foreign)).rejects.toThrow(
        /does not belong to this store/,
      );
  });
});
