import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { ledgerEntriesRoot } from "../src/ledger-at.js";
import {
  createLedgerStore,
  LedgerNotRetained,
} from "../src/ledger-store/index.js";
import { openSqliteLedgerStorage } from "../src/ledger-store/sqlite.js";
import {
  appliedEntries,
  describeLedgerStoreConformance,
  event,
  expectLedgerMatches,
  type OpenedLedgerStorage,
  testLedgerEntries,
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
    await store.persist(a);
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
        await fresh.persist(await fresh.admit(base, a.root));
        expect(await second.counts()).toEqual(await only.counts());
      } finally {
        only.close();
      }
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
      await store.persist(a);
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
