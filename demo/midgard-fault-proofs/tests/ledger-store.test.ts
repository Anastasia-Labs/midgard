import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot, type LedgerEntry } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  LedgerNodeMissing,
  LedgerNotRetained,
  LedgerOutputMissing,
  type LedgerStorage,
  type LedgerStorageChange,
} from "../src/ledger-store/index.js";
import {
  appliedEntries,
  describeLedgerStoreConformance,
  event,
  expectLedgerMatches,
  type OpenedLedgerStorage,
  storedNodesOf,
  testLedgerEntries,
  testOutRef,
} from "./support/ledger-store-conformance.js";

const openedOver = (storage: LedgerStorage): OpenedLedgerStorage => ({
  storage,
  close: async () => undefined,
  // A new storage object over the same records.
  reopen: async () => openedOver({ ...storage }),
});

describeLedgerStoreConformance("memory", async () =>
  openedOver(createMemoryLedgerStorage()),
);

describe("ledger store over storage that never waits", () => {
  it("lets timers run between the batches of a deletion", async () => {
    const memory = createMemoryLedgerStorage();
    let batches = 0;
    const storage: LedgerStorage = {
      ...memory,
      write: async (change) => {
        await memory.write(change);
        if (change.addRoots.length === 0 && change.removeRoots.length === 0)
          batches += 1;
      },
    };
    const store = createLedgerStore(storage);
    const entries = testLedgerEntries(0, 40);
    await store.persist(
      await store.admit(entries, await ledgerEntriesRoot(entries)),
    );
    let batchesBeforeTimer: number | undefined;
    setTimeout(() => {
      batchesBeforeTimer = batches;
    }, 0);
    // The timer is due before the retain starts, so the first turn of the
    // event loop the retain gives up runs it.
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 5);
    await store.retain(new Set());
    expect(batches).toBeGreaterThan(1);
    expect(batchesBeforeTimer).toBeDefined();
    expect(batchesBeforeTimer).toBeLessThan(batches);
  });
});

describe("ledger store adopt", () => {
  const base = testLedgerEntries(0, 40);
  const bMutations = event([0, 1], [100, 101]);
  const bEntries = appliedEntries(base, bMutations);
  const cMutations = event([2], [102]);

  const outputsOf = (entries: readonly LedgerEntry[]) =>
    new Map(
      entries.map(({ outRef, output }) => [outRef.toString("hex"), output]),
    );

  /**
   * Ledgers `a` (`base`), and `b` and `c` each built on `a`, persisted in
   * their own memory storage: the caller's copy of the nodes adopt is handed.
   */
  const held = async () => {
    const storage = createMemoryLedgerStorage();
    const store = createLedgerStore(storage);
    const a = await store.admit(base, await ledgerEntriesRoot(base));
    await store.persist(a);
    const b = await a.apply(bMutations);
    await store.persist(b);
    const c = await a.apply(cMutations);
    await store.persist(c);
    const nodesOf = (root: string) => storedNodesOf(storage, root);
    return {
      a: {
        root: a.root,
        input: { nodes: await nodesOf(a.root), outputs: outputsOf(base) },
      },
      b: {
        root: b.root,
        // The nodes of every ledger the caller holds, `c`'s included, and the
        // outputs `b` inserts.
        input: {
          nodes: new Map([
            ...(await nodesOf(b.root)),
            ...(await nodesOf(c.root)),
          ]),
          outputs: outputsOf(testLedgerEntries(100, 2)),
        },
      },
      c: {
        root: c.root,
        input: {
          nodes: await nodesOf(c.root),
          outputs: outputsOf(testLedgerEntries(102, 1)),
        },
      },
    };
  };

  /** Memory storage that records each change it is handed, and fails the
   * next write, writing nothing, once `failNext` is set. */
  const recorded = () => {
    const memory = createMemoryLedgerStorage();
    const changes: LedgerStorageChange[] = [];
    const control = { failNext: false };
    const storage: LedgerStorage = {
      ...memory,
      write: async (change) => {
        if (control.failNext) {
          control.failNext = false;
          throw new Error("the storage write failed");
        }
        changes.push(change);
        await memory.write(change);
      },
    };
    return { storage, changes, control, store: createLedgerStore(storage) };
  };

  /** The counts of a memory store that persisted `base`, then `b` on it. */
  const persistedCounts = async () => {
    const storage = createMemoryLedgerStorage();
    const store = createLedgerStore(storage);
    const a = await store.admit(base, await ledgerEntriesRoot(base));
    await store.persist(a);
    await store.persist(await a.apply(bMutations));
    return await storage.counts();
  };

  const markerOnly = (root: string): LedgerStorageChange => ({
    putNodes: new Map(),
    nodeRefs: new Map(),
    deleteNodes: [],
    putOutputs: new Map(),
    outputRefs: new Map(),
    deleteOutputs: [],
    addRoots: [],
    removeRoots: [],
    queueDead: [],
    unqueueDead: [],
    currentRoot: root,
  });

  it("gives the counts persist gives, each in one write that moves the marker", async () => {
    const { a, b } = await held();
    const { storage, changes, store } = recorded();
    await store.adopt(a.root, a.input);
    await store.adopt(b.root, b.input);
    expect(
      changes.map(({ addRoots, currentRoot }) => ({ addRoots, currentRoot })),
    ).toEqual([
      { addRoots: [a.root], currentRoot: a.root },
      { addRoots: [b.root], currentRoot: b.root },
    ]);
    expect(await storage.counts()).toEqual(await persistedCounts());
    await expectLedgerMatches(await store.at(a.root), base);
    await expectLedgerMatches(await store.at(b.root), bEntries);
  });

  it("writes only the marker for a retained root and for the empty one", async () => {
    const { a, b } = await held();
    const { storage, changes, store } = recorded();
    await store.adopt(a.root, a.input);
    await store.adopt(b.root, b.input);
    const before = await storage.counts();
    await store.adopt(a.root, a.input);
    await store.adopt(EMPTY_LEDGER_ROOT, { nodes: new Map() });
    expect(changes.slice(2)).toEqual([
      markerOnly(a.root),
      markerOnly(EMPTY_LEDGER_ROOT),
    ]);
    expect(await storage.counts()).toEqual(before);
  });

  it("refuses a node or an output neither held nor supplied, and writes nothing", async () => {
    const { a, b } = await held();
    const { storage, changes, store } = recorded();
    await store.adopt(a.root, a.input);
    const before = await storage.counts();
    // The leaf `b` inserts, below nodes that are supplied.
    const leafKey = testOutRef(100).toString("hex");
    const [leaf] = [...b.input.nodes].find(
      ([, node]) => node.__kind === "Leaf" && node.key === leafKey,
    )!;
    const nodes = new Map(b.input.nodes);
    nodes.delete(leaf);
    const missingNode = await store
      .adopt(b.root, { ...b.input, nodes })
      .catch((error: unknown) => error);
    expect(missingNode).toBeInstanceOf(LedgerNodeMissing);
    expect(missingNode).toMatchObject({ hash: leaf });
    const missingOutput = await store
      .adopt(b.root, { nodes: b.input.nodes })
      .catch((error: unknown) => error);
    expect(missingOutput).toBeInstanceOf(LedgerOutputMissing);
    expect(changes).toHaveLength(1);
    expect(await storage.counts()).toEqual(before);
    await expect(store.at(b.root)).rejects.toBeInstanceOf(LedgerNotRetained);
  });

  it("leaves counts and the retained set as they were when the write fails, and adopts on the next call", async () => {
    const { a, b } = await held();
    const { storage, control, store } = recorded();
    await store.adopt(a.root, a.input);
    const before = await storage.counts();
    control.failNext = true;
    await expect(store.adopt(b.root, b.input)).rejects.toThrow(
      "the storage write failed",
    );
    expect(await storage.counts()).toEqual(before);
    await expect(store.at(b.root)).rejects.toBeInstanceOf(LedgerNotRetained);
    await store.adopt(b.root, b.input);
    await expectLedgerMatches(await store.at(b.root), bEntries);
    expect(await storage.counts()).toEqual(await persistedCounts());
  });

  it("deletes an adopted root's nodes once a retain drops it", async () => {
    const { a, b, c } = await held();
    const { storage, store } = recorded();
    for (const { root, input } of [a, b, c]) await store.adopt(root, input);
    await store.retain(new Set([a.root, b.root]));
    await expect(store.at(c.root)).rejects.toBeInstanceOf(LedgerNotRetained);
    expect(await storage.counts()).toEqual(await persistedCounts());
    await expectLedgerMatches(await store.at(b.root), bEntries);
  });
});
