/**
 * Conformance cases on storage writes that fail: a `retain` whose process
 * dies partway, and a store used again after one of its writes failed. Run
 * by every adapter through `describeLedgerStoreConformance`. The kit imports
 * this module, so nothing here reads the kit's exports while the module
 * loads.
 */
import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot } from "../../src/ledger-at.js";
import {
  createLedgerStore,
  LedgerNotRetained,
  type LedgerStorage,
  type LedgerStore,
} from "../../src/ledger-store/index.js";
import {
  appliedEntries,
  countsOfOnly,
  event,
  expectLedgerMatches,
  type OpenLedgerStorage,
  testLedgerEntries,
} from "./ledger-store-conformance.js";
import { LARGE_SNAPSHOT } from "./ledger-store-conformance-admit.js";

const FAILED = "the storage write failed";

/**
 * A storage whose `write` fails, writing nothing, on its `n`-th call after
 * `arm`, as a process killed before that write would leave it. Every other
 * write goes through.
 */
const failingAt = (storage: LedgerStorage, n: number) => {
  let calls: number | undefined;
  return {
    arm: () => {
      calls = 0;
    },
    storage: {
      ...storage,
      write: async (change) => {
        if (calls !== undefined && ++calls === n) throw new Error(FAILED);
        await storage.write(change);
      },
    } satisfies LedgerStorage,
  };
};

/**
 * Persists a ledger `a` and a chain `b`, `c` built on it. A `retain` of `a`
 * alone then makes five writes: the retained set's replacement, then four
 * deletion batches.
 */
const persistChain = async (store: LedgerStore) => {
  const entries = testLedgerEntries(0, 40);
  const a = await store.admit(entries, await ledgerEntriesRoot(entries));
  const bMutations = event([0, 1], [440, 441]);
  const b = await a.apply(bMutations);
  await store.persist(b);
  const cMutations = event([2, 440], [442]);
  const c = await b.apply(cMutations);
  await store.persist(c);
  const bEntries = appliedEntries(entries, bMutations);
  return {
    entries,
    a,
    b,
    c,
    bMutations,
    bEntries,
    cEntries: appliedEntries(bEntries, cMutations),
  };
};

/** `retain` failed with the injected write failure. */
const expectFailed = async (pending: Promise<unknown>) => {
  const failed = await pending.then(
    () => undefined,
    (error: unknown) => error,
  );
  expect(failed).toBeInstanceOf(Error);
  expect((failed as Error).message).toBe(FAILED);
};

/** Registers the failed-write cases for one adapter. */
export const describeLedgerStoreCrashConformance = (
  adapter: string,
  open: OpenLedgerStorage,
): void => {
  describe(`ledger store interrupted retain: ${adapter}`, () => {
    it.each([1, 2, 3, 4, 5])(
      "recovers from a retain that died at its write %i",
      async (n) => {
        let opened = await open();
        try {
          const failing = failingAt(opened.storage, n);
          const first = createLedgerStore(failing.storage);
          const { entries, a, b, c, bMutations, bEntries, cEntries } =
            await persistChain(first);
          failing.arm();
          await expectFailed(first.retain(new Set([a.root])));

          opened = await opened.reopen();
          const store = createLedgerStore(opened.storage);
          // Re-creating the dropped ledger makes nodes the retain queued for
          // deletion, or already deleted, recur.
          const again = await (await store.at(a.root)).apply(bMutations);
          expect(again.root).toBe(b.root);
          await store.persist(again);
          await expectLedgerMatches(await store.at(a.root), entries);
          await expectLedgerMatches(await store.at(b.root), bEntries);
          const cHeld = await store.at(c.root).then(
            (ledger) => ledger,
            () => undefined,
          );
          if (cHeld !== undefined) await expectLedgerMatches(cHeld, cEntries);
          await store.retain(new Set([a.root, b.root]));
          await expectLedgerMatches(await store.at(b.root), bEntries);
          expect(await opened.storage.counts()).toEqual(
            await countsOfOnly(open, [entries, bEntries]),
          );
        } finally {
          await opened.close();
        }
      },
    );
  });

  describe(`ledger store after a failed write: ${adapter}`, () => {
    const withFailingStore = async (
      n: number,
      run: (
        store: LedgerStore,
        failing: ReturnType<typeof failingAt>,
        storage: LedgerStorage,
      ) => Promise<void>,
    ) => {
      const opened = await open();
      try {
        const failing = failingAt(opened.storage, n);
        await run(createLedgerStore(failing.storage), failing, opened.storage);
      } finally {
        await opened.close();
      }
    };

    it("leaves a ledger whose persist failed unretained, and persists it again", async () => {
      await withFailingStore(1, async (store, failing, storage) => {
        const entries = testLedgerEntries(0, 40);
        const a = await store.admit(entries, await ledgerEntriesRoot(entries));
        const bMutations = event([0, 1], [440, 441]);
        const b = await a.apply(bMutations);
        failing.arm();
        await expectFailed(store.persist(b));
        await expect(store.at(b.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        expect(await storage.counts()).toEqual(
          await countsOfOnly(open, [entries]),
        );
        await store.persist(b);
        const bEntries = appliedEntries(entries, bMutations);
        await expectLedgerMatches(await store.at(b.root), bEntries);
        expect(await storage.counts()).toEqual(
          await countsOfOnly(open, [entries, bEntries]),
        );
      });
    });

    it.each([1, 2, 3])(
      "admits a large snapshot again after its write %i failed, and the next retain deletes what the failed admit left",
      async (n) =>
        await withFailingStore(n, async (store, failing, storage) => {
          const entries = testLedgerEntries(0, LARGE_SNAPSHOT);
          const root = await ledgerEntriesRoot(entries);
          failing.arm();
          await expectFailed(store.admit(entries, root));
          await expect(store.at(root)).rejects.toBeInstanceOf(
            LedgerNotRetained,
          );
          expect((await store.admit(entries, root)).root).toBe(root);
          await expectLedgerMatches(await store.at(root), entries);
          await store.retain(new Set([root]));
          expect(await storage.counts()).toEqual(
            await countsOfOnly(open, [entries]),
          );
        }),
      120_000,
    );

    it("keeps serving every retained root after a retain whose replacement failed", async () => {
      await withFailingStore(1, async (store, failing, storage) => {
        const { entries, a, b, c, bEntries, cEntries } =
          await persistChain(store);
        failing.arm();
        await expectFailed(store.retain(new Set([a.root])));
        await expectLedgerMatches(await store.at(a.root), entries);
        await expectLedgerMatches(await store.at(b.root), bEntries);
        await expectLedgerMatches(await store.at(c.root), cEntries);
        expect(await store.retain(new Set([a.root]))).toEqual({
          notRetained: new Set(),
        });
        for (const dropped of [b, c])
          await expect(store.at(dropped.root)).rejects.toBeInstanceOf(
            LedgerNotRetained,
          );
        expect(await storage.counts()).toEqual(
          await countsOfOnly(open, [entries]),
        );
      });
    });

    it.each([2, 3, 4, 5])(
      "carries on after a retain whose write %i, a deletion batch, failed",
      async (n) =>
        await withFailingStore(n, async (store, failing, storage) => {
          const { entries, a, b, c, bMutations, bEntries } =
            await persistChain(store);
          failing.arm();
          await expectFailed(store.retain(new Set([a.root])));
          for (const dropped of [b, c])
            await expect(store.at(dropped.root)).rejects.toBeInstanceOf(
              LedgerNotRetained,
            );
          await store.persist(await (await store.at(a.root)).apply(bMutations));
          await expectLedgerMatches(await store.at(b.root), bEntries);
          expect(await store.retain(new Set([a.root, b.root]))).toEqual({
            notRetained: new Set(),
          });
          await expectLedgerMatches(await store.at(a.root), entries);
          await expectLedgerMatches(await store.at(b.root), bEntries);
          await expect(store.at(c.root)).rejects.toBeInstanceOf(
            LedgerNotRetained,
          );
          expect(await storage.counts()).toEqual(
            await countsOfOnly(open, [entries, bEntries]),
          );
        }),
    );
  });
};
