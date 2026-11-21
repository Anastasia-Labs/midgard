/**
 * Conformance cases on what a sweep leaves behind, run by every adapter
 * through `describeLedgerStoreConformance`. The kit imports this module, so
 * nothing here reads the kit's exports while the module loads.
 */
import { midgardBlake2b } from "@al-ft/midgard-core/codec/hash";
import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot, type LedgerEntry } from "../../src/ledger-at.js";
import {
  createLedgerStore,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  LedgerNotRetained,
  LedgerOutputMissing,
  type LedgerStorage,
  type LedgerStore,
  recordWitness,
  type StoredNode,
} from "../../src/ledger-store/index.js";
import {
  appliedEntries,
  event,
  expectLedgerMatches,
  type OpenLedgerStorage,
  testLedgerEntries,
  testLedgerEntry,
  testOutRef,
  UNDECODABLE_ENTRY,
} from "./ledger-store-conformance.js";
import {
  gatedStorage,
  holedStorage,
  interruptedStorage,
} from "./ledger-store-sweep-faults.js";

/** The entry numbers of the base ledger. */
const baseNumbers = () => [
  ...Array.from({ length: 40 }, (_, n) => n),
  UNDECODABLE_ENTRY,
];
const base = () => baseNumbers().map((n) => testLedgerEntry(n));

/** Out-ref `n`'s path in the trie: the hex digest of its bytes. */
const pathOf = (n: number): string =>
  Buffer.from(midgardBlake2b(testOutRef(n), { dkLen: 32 })).toString("hex");

const sharedNibbles = (left: string, right: string): number => {
  let shared = 0;
  while (shared < left.length && left[shared] === right[shared]) shared += 1;
  return shared;
};

/**
 * An entry `y` of `numbers` and a new entry `z` whose insertion moves `y`'s
 * leaf: `z`'s path shares more nibbles with `y`'s than any other entry's does,
 * so the two end up under a new branch. `y`'s parent branch has at least two
 * other children, so deleting `y` collapses nothing.
 */
const movingPair = (numbers: readonly number[]) => {
  for (const y of numbers) {
    const path = pathOf(y);
    const others = numbers.filter((n) => n !== y).map(pathOf);
    const depth = Math.max(
      ...others.map((other) => sharedNibbles(other, path)),
    );
    const siblings = new Set(
      others
        .filter((other) => sharedNibbles(other, path) === depth)
        .map((other) => other[depth]),
    );
    if (siblings.size < 2) continue;
    for (let z = 5_000; z < 500_000; z += 1)
      if (sharedNibbles(pathOf(z), path) > depth) return { y, z };
  }
  throw new Error("no entry's leaf can be moved without a collapse");
};

/** The prefix of the leaf that holds `n` in `ledger`, read through a
 * witness. */
const leafPrefix = async (ledger: Ledger, n: number) => {
  const recorder = recordWitness(ledger);
  await recorder.ledger.read([testOutRef(n)]);
  const key = testOutRef(n).toString("hex");
  const leaf = recorder
    .witness()
    .nodes.find((node) => node.__kind === "Leaf" && node.key === key);
  return leaf?.prefix;
};

/**
 * A persisted base ledger `x`, its child `s` without `y` (persisted), and its
 * child `moved` with `z` inserted (not persisted), whose leaf for `y` is a new
 * node carrying no output: the trie moved it under a branch with `z`.
 */
const movedLeafScene = async (store: LedgerStore) => {
  const entries = base();
  const admitted = await store.admit(entries, await ledgerEntriesRoot(entries));
  await store.persist(admitted);
  const x = await store.at(admitted.root);
  const { y, z } = movingPair(baseNumbers());
  const s = await x.apply(event([y], []));
  await store.persist(s);
  const moved = await x.apply(event([], [z]));
  const before = await leafPrefix(x, y);
  const after = await leafPrefix(moved, y);
  expect(before).toBeDefined();
  expect(after).toBeDefined();
  expect(after!.length).toBeLessThan(before!.length);
  return { s, moved, movedEntries: appliedEntries(entries, event([], [z])) };
};

/**
 * `persist(ledger)` either refused with `LedgerOutputMissing` and left the
 * root unretained, or retained a ledger every read of which succeeds.
 */
const expectRefusedOrComplete = async (
  store: LedgerStore,
  ledger: Ledger,
  entries: Parameters<typeof expectLedgerMatches>[1],
  persisted: unknown,
) => {
  if (persisted === undefined) {
    await expectLedgerMatches(await store.at(ledger.root), entries);
    return;
  }
  expect(persisted).toBeInstanceOf(LedgerOutputMissing);
  await expect(store.at(ledger.root)).rejects.toBeInstanceOf(LedgerNotRetained);
};

const outcome = (persisting: Promise<void>): Promise<unknown> =>
  persisting.then(
    () => undefined,
    (error: unknown) => error,
  );

/**
 * New entries `z1` and `z2` that end up alone under a new branch, whose
 * parent is a branch of the base ledger at depth one or more with at least
 * two other children; and an entry `w` under another root nibble.
 */
const holePair = (numbers: readonly number[]) => {
  const paths = numbers.map(pathOf);
  const seen: [number, string][] = [];
  for (let n = 5_000; n < 500_000; n += 1) {
    const path = pathOf(n);
    for (const [m, other] of seen) {
      const shared = path.slice(0, sharedNibbles(path, other));
      const depth = Math.max(...paths.map((p) => sharedNibbles(p, shared)));
      if (depth < 1 || depth >= shared.length) continue;
      const siblings = new Set(
        paths
          .filter((p) => sharedNibbles(p, shared) === depth)
          .map((p) => p[depth]),
      );
      if (siblings.size < 2) continue;
      let w = n + 1;
      while (pathOf(w)[0] === path[0]) w += 1;
      return { z1: m, z2: n, w };
    }
    seen.push([n, path]);
  }
  throw new Error("no pair of entries shares a new branch");
};

/** Every node `root` reaches, by hash, with its parent's hash. */
const reachable = async (storage: LedgerStorage, root: string) => {
  const nodes = new Map<string, { node: StoredNode; parent?: string }>();
  const pending: [string, string | undefined][] = [[root, undefined]];
  while (pending.length > 0) {
    const [hash, parent] = pending.pop()!;
    const [node] = await storage.readNodes([hash]);
    if (node === undefined) throw new Error(`node ${hash} is not held`);
    nodes.set(hash, parent === undefined ? { node } : { node, parent });
    if (node.__kind === "Branch")
      for (const child of node.children)
        if (child !== null) pending.push([child, hash]);
  }
  return nodes;
};

/**
 * Persisted ledgers: `sound` (the base), `holed` (the base with `z1` and
 * `z2`), and `sharing` (`holed` with `w`). `hole` is the branch holding `z1`
 * and `z2`, which `holed` and `sharing` reach through a shared parent that
 * `sound` does not reach; that parent's other children `sound` reaches.
 */
const holedScene = async (store: LedgerStore, storage: LedgerStorage) => {
  const entries = base();
  const sound = await store.admit(entries, await ledgerEntriesRoot(entries));
  await store.persist(sound);
  const { z1, z2, w } = holePair(baseNumbers());
  const holed = await sound.apply(event([], [z1, z2]));
  await store.persist(holed);
  const sharing = await holed.apply(event([], [w]));
  await store.persist(sharing);
  const [soundNodes, holedNodes, sharingNodes] = await Promise.all(
    [sound, holed, sharing].map(({ root }) => reachable(storage, root)),
  );
  const keys = new Set([z1, z2].map((n) => testOutRef(n).toString("hex")));
  const isLeafOf = (hash: string | null) => {
    const node = hash === null ? undefined : holedNodes.get(hash)?.node;
    return node?.__kind === "Leaf" && keys.has(node.key);
  };
  const [hole, { parent }] = [...holedNodes].find(
    ([, { node }]) =>
      node.__kind === "Branch" &&
      node.children.filter((child) => child !== null).length === 2 &&
      node.children.filter(isLeafOf).length === 2,
  )!;
  expect(parent).toBeDefined();
  expect(parent).not.toBe(holed.root);
  expect(sharingNodes.has(parent!)).toBe(true);
  expect(soundNodes.has(parent!)).toBe(false);
  const shared = holedNodes.get(parent!)!.node;
  expect(
    shared.__kind === "Branch" &&
      shared.children.some((child) => child !== null && soundNodes.has(child)),
  ).toBe(true);
  return { entries, sound, holed, sharing, hole };
};

/** The value a promise resolves to, or the name of what it rejects with. */
const settled = <T>(promise: Promise<T>) =>
  promise.then(
    (value) => ({ value }),
    (error: unknown) => ({
      error: error instanceof Error ? error.name : String(error),
    }),
  );

/** Registers the sweep cases for one adapter. */
export const describeLedgerStoreSweepConformance = (
  adapter: string,
  open: OpenLedgerStorage,
): void => {
  /** Record counts of a fresh store holding only `ledgers`. */
  const countsOfOnly = async (ledgers: readonly (readonly LedgerEntry[])[]) => {
    const opened = await open();
    try {
      const store = createLedgerStore(opened.storage);
      for (const entries of ledgers)
        await store.persist(
          await store.admit(entries, await ledgerEntriesRoot(entries)),
        );
      return await opened.storage.counts();
    } finally {
      await opened.close();
    }
  };

  describe(`ledger store sweeps: ${adapter}`, () => {
    it.each([
      ["the holed root", ["holed", "sharing", "sound"]],
      ["the root sharing its path", ["sharing", "holed", "sound"]],
    ] as const)(
      "drops each retained root that reaches a missing node and completes the sweep, %s marked first",
      async (_first, order) => {
        const opened = await open();
        try {
          const faulty = holedStorage(opened.storage);
          const store = createLedgerStore(faulty.storage);
          const scene = await holedScene(store, opened.storage);
          const { entries, sound, holed, sharing } = scene;
          const mutations = event([0], [700]);
          const next = await sound.apply(mutations);
          faulty.holeAt(scene.hole);
          const roots = order.map((name) => scene[name].root);
          const swept = await settled(store.retain(new Set(roots)));
          const persisted = await settled(store.persist(next));
          const again = await settled(
            store.retain(new Set([sound.root, next.root])),
          );
          expect({ swept, persisted, again }).toEqual({
            swept: {
              value: { notRetained: new Set([holed.root, sharing.root]) },
            },
            persisted: { value: undefined },
            again: { value: { notRetained: new Set() } },
          });
          for (const dropped of [holed, sharing])
            await expect(store.at(dropped.root)).rejects.toBeInstanceOf(
              LedgerNotRetained,
            );
          await expectLedgerMatches(await store.at(sound.root), entries);
          const nextEntries = appliedEntries(entries, mutations);
          await expectLedgerMatches(await store.at(next.root), nextEntries);
          expect(await opened.storage.counts()).toEqual(
            await countsOfOnly([entries, nextEntries]),
          );
        } finally {
          await opened.close();
        }
      },
    );

    it("finishes a sweep that died before deleting anything before a new store persists", async () => {
      let opened = await open();
      try {
        const first = createLedgerStore(
          interruptedStorage(opened.storage, "nothing"),
        );
        const entries = base();
        const a = await first.admit(entries, await ledgerEntriesRoot(entries));
        await first.persist(a);
        const dropped = await a.apply(event([0], [600, 601]));
        await first.persist(dropped);
        await expect(first.retain(new Set([a.root]))).rejects.toThrow(
          "the process died mid-sweep",
        );
        opened = await opened.reopen();
        const store = createLedgerStore(opened.storage);
        const mutations = event([1], [602]);
        await store.persist(await (await store.at(a.root)).apply(mutations));
        // The finished sweep deleted the dropped ledger's records.
        expect(await opened.storage.counts()).toEqual(
          await countsOfOnly([entries, appliedEntries(entries, mutations)]),
        );
        await expect(store.at(dropped.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
      } finally {
        await opened.close();
      }
    });

    it("sweeps every page of records, not only the first", async () => {
      const opened = await open();
      try {
        const store = createLedgerStore(opened.storage);
        const range = (from: number, count: number) =>
          Array.from({ length: count }, (_, index) => from + index);
        // More nodes, and more outputs, than one sweep page lists.
        const a = await (
          await store.at(EMPTY_LEDGER_ROOT)
        ).apply(event([], range(0, 4_500)));
        await store.persist(a);
        const mutations = event(range(0, 1_500), range(10_000, 100));
        const b = await a.apply(mutations);
        await store.persist(b);
        expect(await store.retain(new Set([b.root]))).toEqual({
          notRetained: new Set(),
        });
        await expect(store.at(a.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        const bEntries = appliedEntries(testLedgerEntries(0, 4_500), mutations);
        const listed: string[] = [];
        for await (const { outRef } of (await store.at(b.root)).entries())
          listed.push(outRef.toString("hex"));
        expect(listed.sort()).toEqual(
          bEntries.map(({ outRef }) => outRef.toString("hex")).sort(),
        );
        const only = await open();
        try {
          const fresh = createLedgerStore(only.storage);
          await fresh.persist(await fresh.admit(bEntries, b.root));
          expect(await opened.storage.counts()).toEqual(
            await only.storage.counts(),
          );
        } finally {
          await only.close();
        }
      } finally {
        await opened.close();
      }
    });

    it("never retains a ledger whose moved leaf's output a finished sweep deleted", async () => {
      const opened = await open();
      try {
        const store = createLedgerStore(opened.storage);
        const { s, moved, movedEntries } = await movedLeafScene(store);
        await store.retain(new Set([s.root]));
        await expectRefusedOrComplete(
          store,
          moved,
          movedEntries,
          await outcome(store.persist(moved)),
        );
      } finally {
        await opened.close();
      }
    });

    it("never retains a ledger persisted while a sweep deletes outputs without its moved leaf's output", async () => {
      const opened = await open();
      try {
        const gate = gatedStorage(opened.storage, "outputs");
        const store = createLedgerStore(gate.storage);
        const { s, moved, movedEntries } = await movedLeafScene(store);
        const sweeping = store.retain(new Set([s.root]));
        await gate.atGate;
        const persisted = await outcome(store.persist(moved));
        gate.release();
        await sweeping;
        await expectRefusedOrComplete(store, moved, movedEntries, persisted);
      } finally {
        await opened.close();
      }
    });
  });
};
