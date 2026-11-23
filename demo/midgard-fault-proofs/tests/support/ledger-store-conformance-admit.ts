/**
 * Conformance cases for `admit` on snapshots large enough to be written in
 * several batches. Expected roots come from the test-support oracle
 * (`oracleLedgerRoot`), which builds one library trie from the whole entry
 * set and never reads the store. Run by every adapter through
 * `describeLedgerStoreConformance`. The kit imports this module, so nothing
 * here reads the kit's exports while the module loads.
 */
import { describe, expect, it } from "vitest";

import {
  type LedgerEntry,
  LedgerRootMismatchError,
} from "../../src/ledger-at.js";
import {
  createLedgerStore,
  EMPTY_LEDGER_ROOT,
  LedgerNotRetained,
  LedgerSnapshotRefused,
  type LedgerStorage,
  type LedgerStore,
} from "../../src/ledger-store/index.js";
import { oracleLedgerRoot } from "./ledger-root-oracle.js";
import {
  expectLedgerMatches,
  type OpenLedgerStorage,
  testLedgerEntries,
  testLedgerEntry,
} from "./ledger-store-conformance.js";

/** Entries in a snapshot `admit` writes in more than two batches. */
export const LARGE_SNAPSHOT = 2_100;

const NOTHING_HELD = {
  nodes: 0,
  outputs: 0,
  roots: 0,
  nodeRefs: 0,
  outputRefs: 0,
  dead: 0,
};

/** `entries` reordered by a fixed linear congruential sequence. */
const shuffled = (entries: readonly LedgerEntry[]): LedgerEntry[] => {
  const result = [...entries];
  let seed = 0x5eed;
  for (let index = result.length - 1; index > 0; index -= 1) {
    seed = (seed * 1_103_515_245 + 12_345) % 2 ** 31;
    const other = seed % (index + 1);
    [result[index], result[other]] = [result[other]!, result[index]!];
  }
  return result;
};

/** Registers the large-snapshot `admit` cases for one adapter. */
export const describeLedgerStoreAdmitConformance = (
  adapter: string,
  open: OpenLedgerStorage,
): void => {
  const withStore = async (
    run: (store: LedgerStore, storage: LedgerStorage) => Promise<void>,
  ) => {
    const opened = await open();
    try {
      await run(createLedgerStore(opened.storage), opened.storage);
    } finally {
      await opened.close();
    }
  };

  /** The store retains no root and, after a `retain`, holds nothing. */
  const expectNothingLeft = async (
    store: LedgerStore,
    storage: LedgerStorage,
    root: string,
  ) => {
    await expect(store.at(root)).rejects.toBeInstanceOf(LedgerNotRetained);
    expect(await storage.retainedRoots()).toEqual([]);
    await store.retain(new Set());
    expect(await storage.counts()).toEqual(NOTHING_HELD);
  };

  describe(`ledger store admit of a large snapshot: ${adapter}`, () => {
    const entries = testLedgerEntries(0, LARGE_SNAPSHOT);

    it("gives the oracle's root for the entries in any order", async () => {
      const root = await oracleLedgerRoot(entries);
      for (const order of [entries, [...entries].reverse(), shuffled(entries)])
        await withStore(async (store) => {
          expect((await store.admit(order, root)).root).toBe(root);
        });
      await withStore(async (store) => {
        async function* streamed() {
          yield* shuffled(entries);
        }
        await store.admit(streamed(), root);
        await expectLedgerMatches(await store.at(root), entries);
      });
    }, 120_000);

    it("refuses a root other than the expected one, retaining nothing, and the next retain deletes what it wrote", async () => {
      await withStore(async (store, storage) => {
        const actual = await oracleLedgerRoot(entries);
        const expected = await oracleLedgerRoot(entries.slice(1));
        const refusal = await store
          .admit(entries, expected)
          .catch((error: unknown) => error);
        expect(refusal).toBeInstanceOf(LedgerRootMismatchError);
        expect(refusal).toMatchObject({
          expectedRoot: expected,
          actualRoot: actual,
        });
        // The batches before the last were written, and are left to delete.
        expect((await storage.counts()).nodes).toBeGreaterThan(0);
        await expectNothingLeft(store, storage, actual);
      });
    }, 120_000);

    it.each([
      ["the same batch", 10],
      ["a later batch", LARGE_SNAPSHOT - 1],
    ])(
      "refuses an out-ref listed twice, in %s, as a typed value, retaining nothing",
      async (_where, at) =>
        await withStore(async (store, storage) => {
          const twice = [...entries];
          twice.splice(at, 0, testLedgerEntry(3));
          const refusal = await store
            .admit(twice, await oracleLedgerRoot(entries))
            .catch((error: unknown) => error);
          expect(refusal).toBeInstanceOf(LedgerSnapshotRefused);
          expect((refusal as LedgerSnapshotRefused).outRef).toEqual(
            testLedgerEntry(3).outRef,
          );
          await expectNothingLeft(
            store,
            storage,
            await oracleLedgerRoot(entries),
          );
        }),
      120_000,
    );

    it("admits an empty snapshot as the empty ledger", async () => {
      await withStore(async (store, storage) => {
        expect((await store.admit([], EMPTY_LEDGER_ROOT)).root).toBe(
          EMPTY_LEDGER_ROOT,
        );
        await expect(
          store.admit([], await oracleLedgerRoot(entries.slice(0, 1))),
        ).rejects.toBeInstanceOf(LedgerRootMismatchError);
        expect(await storage.counts()).toEqual(NOTHING_HELD);
      });
    });
  });
};
