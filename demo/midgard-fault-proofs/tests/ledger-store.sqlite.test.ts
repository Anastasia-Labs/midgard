import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setFlagsFromString } from "node:v8";
import { runInNewContext } from "node:vm";

import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { afterEach, describe, expect, it } from "vitest";

import { ledgerEntriesRoot, ledgerEntryLeaf } from "../src/ledger-at.js";
import { LedgerTrieStore } from "../src/ledger-store/engine.js";
import {
  createLedgerStore,
  LedgerNotRetained,
  type LedgerStorage,
} from "../src/ledger-store/index.js";
import { openSqliteLedgerStorage } from "../src/ledger-store/sqlite.js";
import {
  appliedEntries,
  describeLedgerStoreConformance,
  event,
  expectLedgerMatches,
  type OpenedLedgerStorage,
  testLedgerEntries,
  testLedgerEntry,
} from "./support/ledger-store-conformance.js";

const directories: string[] = [];
const freshPath = async () => {
  const directory = await mkdtemp(join(tmpdir(), "ledger-store-sqlite-"));
  directories.push(directory);
  return join(directory, "ledger.sqlite");
};

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

const openedAt = (path: string): OpenedLedgerStorage => {
  const storage = openSqliteLedgerStorage(path);
  return {
    storage,
    close: async () => storage.close(),
    reopen: async () => {
      storage.close();
      return openedAt(path);
    },
  };
};

describeLedgerStoreConformance("sqlite", async () =>
  openedAt(await freshPath()),
);

describe("sqlite ledger storage across reopen", () => {
  const base = testLedgerEntries(0, 30);

  it("serves persisted ledgers, and keeps their counts, across a reopen", async () => {
    const path = await freshPath();
    const first = openSqliteLedgerStorage(path);
    const store = createLedgerStore(first);
    const a = await store.admit(base, await ledgerEntriesRoot(base));
    const b = await a.apply(event([0, 1], [100, 101]));
    await store.persist(b);
    const c = await b.apply(event([2], [102]));
    await store.persist(c);
    const unpersisted = await c.apply(event([3], [103]));
    first.close();

    const second = openSqliteLedgerStorage(path);
    try {
      const reopened = createLedgerStore(second);
      const bEntries = appliedEntries(base, event([0, 1], [100, 101]));
      await expectLedgerMatches(await reopened.at(a.root), base);
      await expectLedgerMatches(await reopened.at(b.root), bEntries);
      await expectLedgerMatches(
        await reopened.at(c.root),
        appliedEntries(bEntries, event([2], [102])),
      );
      await expect(reopened.at(unpersisted.root)).rejects.toBeInstanceOf(
        LedgerNotRetained,
      );
      // The counts written before the reopen decide what this retain
      // deletes.
      await reopened.retain(new Set([a.root]));
      await expectLedgerMatches(await reopened.at(a.root), base);
      for (const dropped of [b, c])
        await expect(reopened.at(dropped.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
      const only = openSqliteLedgerStorage(await freshPath());
      try {
        const fresh = createLedgerStore(only);
        await fresh.admit(base, a.root);
        expect(await second.counts()).toEqual(await only.counts());
      } finally {
        only.close();
      }
    } finally {
      second.close();
    }
  });

  it("keeps the verified head across a reopen, and has none before the first write", async () => {
    const path = await freshPath();
    const first = openSqliteLedgerStorage(path);
    expect(await first.verifiedHead.read()).toBeUndefined();
    await first.verifiedHead.write("aa".repeat(28));
    await first.verifiedHead.write("bb".repeat(28));
    first.close();

    const second = openSqliteLedgerStorage(path);
    try {
      expect(await second.verifiedHead.read()).toBe("bb".repeat(28));
    } finally {
      second.close();
    }
  });

  it("refuses a second opener while the writer lease is held, and frees it on close", async () => {
    const path = await freshPath();
    const first = openSqliteLedgerStorage(path);
    expect(() => openSqliteLedgerStorage(path)).toThrow(
      /open in another process/,
    );
    first.close();
    openSqliteLedgerStorage(path).close();
  });

  it("applies a change all or nothing", async () => {
    const path = await freshPath();
    const storage = openSqliteLedgerStorage(path);
    try {
      const store = createLedgerStore(storage);
      const a = await store.admit(base, await ledgerEntriesRoot(base));
      const before = await storage.counts();
      await expect(
        storage.write({
          putNodes: new Map([
            [
              "ef".repeat(32),
              {
                node: { __kind: "Leaf", prefix: "", key: "00", value: "00" },
                refs: 1,
              },
            ],
          ]),
          nodeRefs: new Map([[a.root, 2]]),
          deleteNodes: [],
          // Not bindable: the write fails after the node rows changed.
          putOutputs: new Map([
            [
              "00",
              { output: Symbol("unbindable") as unknown as Buffer, refs: 1 },
            ],
          ]),
          outputRefs: new Map(),
          deleteOutputs: [],
          addRoots: ["cd".repeat(32)],
          removeRoots: [],
          queueDead: [],
          unqueueDead: [],
        }),
      ).rejects.toThrow();
      expect(await storage.counts()).toEqual(before);
    } finally {
      storage.close();
    }
  });
});

describe("sqlite admit of a large snapshot", () => {
  const SIZE = 50_000;
  const EARLY = 10_000;
  const SAMPLE_EVERY = 2_500;
  const MB = 2 ** 20;
  /** The heap the last half's peak may exceed the early peak by. A path
   * holding the snapshot grows by more than half the whole-snapshot path's
   * footprint over the same stretch. */
  const BOUND_MB = 16;

  setFlagsFromString("--expose-gc");
  const gc = runInNewContext("gc") as () => void;
  /** The heap in use after a collection, with the array buffers held
   * outside it. */
  const heapInUse = () => {
    gc();
    const { heapUsed, arrayBuffers } = process.memoryUsage();
    return heapUsed + arrayBuffers;
  };

  /**
   * What the whole-snapshot path left in memory: every entry's output, and
   * every node of the trie the library builds from the whole entry set.
   */
  const wholeSnapshot = async () => {
    const outputs = new Map<string, Buffer>();
    const pairs: { key: Buffer; value: Buffer }[] = [];
    for (let n = 0; n < SIZE; n += 1) {
      const { entry, leaf } = ledgerEntryLeaf(testLedgerEntry(n), "reference");
      outputs.set(entry.outRef.toString("hex"), entry.output);
      pairs.push({ key: entry.outRef, value: leaf });
    }
    const store = new LedgerTrieStore(async () => undefined);
    await Trie.fromList(pairs, store);
    return { outputs, nodes: store.writes };
  };

  it(`admits ${SIZE.toString()} streamed entries in a heap that does not grow with them`, async () => {
    const root = await ledgerEntriesRoot(testLedgerEntries(0, SIZE));

    let baseline = heapInUse();
    let started = performance.now();
    let whole: Awaited<ReturnType<typeof wholeSnapshot>> | undefined =
      await wholeSnapshot();
    const wholeHeap = heapInUse() - baseline;
    const wholeMs = performance.now() - started;
    expect(whole.nodes.size).toBeGreaterThan(SIZE);
    whole = undefined;

    const sqlite = openSqliteLedgerStorage(await freshPath());
    try {
      baseline = heapInUse();
      let read = 0;
      let early = baseline;
      let late = baseline;
      const sample = () => {
        const used = heapInUse();
        if (read <= EARLY) early = Math.max(early, used);
        else if (read > SIZE / 2) late = Math.max(late, used);
      };
      // Sampled at each write, while the batch it writes is in memory, and
      // while the stream is read.
      const storage: LedgerStorage = {
        ...sqlite,
        write: async (change) => {
          sample();
          await sqlite.write(change);
        },
      };
      async function* streamed() {
        for (; read < SIZE; read += 1) {
          if (read % SAMPLE_EVERY === 0) sample();
          yield testLedgerEntry(read);
        }
      }
      started = performance.now();
      const admitted = await createLedgerStore(storage).admit(streamed(), root);
      const admitMs = performance.now() - started;
      expect(admitted.root).toBe(root);
      const peak = Math.max(early, late) - baseline;
      console.log(
        `${SIZE.toString()} entries: the whole-snapshot path held ${(wholeHeap / MB).toFixed(1)} MB (${wholeMs.toFixed(0)} ms, in memory); streamed admit peaked ${(peak / MB).toFixed(1)} MB above its baseline, ${((early - baseline) / MB).toFixed(1)} MB over the first ${EARLY.toString()} entries and ${((late - baseline) / MB).toFixed(1)} MB over the last half (${admitMs.toFixed(0)} ms into SQLite, with a collection at each sample)`,
      );
      expect(late - early).toBeLessThan(BOUND_MB * MB);
      expect(peak).toBeLessThan(wholeHeap);
    } finally {
      sqlite.close();
    }
  }, 600_000);
});
