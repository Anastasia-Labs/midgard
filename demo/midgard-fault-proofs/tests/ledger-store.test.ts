import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import { type LedgerEntry } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type LedgerMutation,
  LedgerNodeMissing,
  LedgerNotRetained,
  LedgerOutputMissing,
  type LedgerStorage,
  type LedgerStorageChange,
} from "../src/ledger-store/index.js";
import { oracleLedgerRoot } from "./support/ledger-root-oracle.js";
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
    await store.admit(entries, await oracleLedgerRoot(entries));
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

/**
 * The `Map` lookups `run` makes. A ledger's in-memory records are reached
 * only through maps, so this counts the work a read does wherever the
 * records sit, without naming how they are laid out.
 */
const mapLookups = async (run: () => Promise<unknown>): Promise<number> => {
  const get = Map.prototype.get;
  let lookups = 0;
  Map.prototype.get = function (this: Map<unknown, unknown>, key: unknown) {
    lookups += 1;
    return get.call(this, key);
  };
  try {
    await run();
  } finally {
    Map.prototype.get = get;
  }
  return lookups;
};

describe("ledgers derived by many applies", () => {
  it.each([
    ["one-entry applies", (step: number) => event([], [10_000 + step])],
    // A step that spends and creates nothing, as a rejected event's.
    ["applies that change nothing", (): LedgerMutation[] => []],
  ])(
    "reads after 1,000 %s with at most three times the lookups of a read after 10",
    async (_, mutationsOf) => {
      const store = createLedgerStore(createMemoryLedgerStorage());
      const base = testLedgerEntries(0, 200);
      let ledger = await store.admit(base, await oracleLedgerRoot(base));
      // One apply first, so the reads below also pass a record in memory.
      ledger = await ledger.apply(event([0], []));
      let entries: readonly LedgerEntry[] = appliedEntries(
        base,
        event([0], []),
      );
      const after = new Map<number, typeof ledger>();
      for (let step = 1; step <= 1_000; step += 1) {
        const mutations = mutationsOf(step);
        ledger = await ledger.apply(mutations);
        entries = appliedEntries(entries, mutations);
        after.set(step, ledger);
      }
      // An entry of the admitted base: a lookup misses every in-memory
      // record before it reaches the store.
      const key = testOutRef(7);
      const lookupsAfter = async (step: number) =>
        await mapLookups(async () => await after.get(step)!.read([key]));
      expect(await lookupsAfter(1_000)).toBeLessThanOrEqual(
        3 * (await lookupsAfter(10)),
      );
      await expectLedgerMatches(ledger, entries);
    },
  );

  it("reads the output an out-ref was inserted with last, over earlier ones the ledger deleted", async () => {
    const store = createLedgerStore(createMemoryLedgerStorage());
    const base = testLedgerEntries(0, 40);
    const outRef = testOutRef(500);
    const first = { outRef, output: makeOutput(7_000_000n) };
    const second = { outRef, output: makeOutput(9_000_000n) };
    let ledger = await store.admit(base, await oracleLedgerRoot(base));
    const steps = [
      [{ type: "insert" as const, ...first }],
      [{ type: "delete" as const, outRef }],
      [{ type: "insert" as const, ...second }],
      ...[1, 2, 3, 4, 5, 6].map((n) => event([n], [])),
    ];
    let entries: readonly LedgerEntry[] = base;
    const ledgers = [];
    for (const mutations of steps) {
      ledger = await ledger.apply(mutations);
      entries = appliedEntries(entries, mutations);
      ledgers.push({ ledger, entries });
    }
    for (const { ledger: each, entries: held } of ledgers)
      await expectLedgerMatches(each, held, [testOutRef(900_001)]);
    await store.persist(ledger);
    await expectLedgerMatches(await store.at(ledger.root), entries);
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
    const a = await store.admit(base, await oracleLedgerRoot(base));
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
    const a = await store.admit(base, await oracleLedgerRoot(base));
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
