/**
 * Conformance cases on a `retain` whose process dies partway, run by every
 * adapter through `describeLedgerStoreConformance`. The kit imports this
 * module, so nothing here reads the kit's exports while the module loads.
 */
import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot } from "../../src/ledger-at.js";
import {
  createLedgerStore,
  type LedgerStorage,
} from "../../src/ledger-store/index.js";
import {
  appliedEntries,
  countsOfOnly,
  event,
  expectLedgerMatches,
  type OpenLedgerStorage,
  testLedgerEntries,
} from "./ledger-store-conformance.js";

/**
 * A storage whose `write` fails, writing nothing, on its `n`-th call after
 * `arm`, as a process killed before that write would leave it. Every write
 * before it goes through.
 */
const dyingAt = (storage: LedgerStorage, n: number) => {
  let calls: number | undefined;
  return {
    arm: () => {
      calls = 0;
    },
    storage: {
      ...storage,
      write: async (change) => {
        if (calls !== undefined && ++calls === n)
          throw new Error("the process died mid-retain");
        await storage.write(change);
      },
    } satisfies LedgerStorage,
  };
};

/** Registers the interrupted-retain cases for one adapter. */
export const describeLedgerStoreCrashConformance = (
  adapter: string,
  open: OpenLedgerStorage,
): void => {
  describe(`ledger store interrupted retain: ${adapter}`, () => {
    // The retain makes five writes: the retained set's replacement, then
    // four deletion batches.
    it.each([1, 2, 3, 4, 5])(
      "recovers from a retain that died at its write %i",
      async (n) => {
        let opened = await open();
        try {
          const dying = dyingAt(opened.storage, n);
          const first = createLedgerStore(dying.storage);
          const entries = testLedgerEntries(0, 40);
          const a = await first.admit(
            entries,
            await ledgerEntriesRoot(entries),
          );
          await first.persist(a);
          const bMutations = event([0, 1], [440, 441]);
          const b = await a.apply(bMutations);
          await first.persist(b);
          const cMutations = event([2, 440], [442]);
          const c = await b.apply(cMutations);
          await first.persist(c);
          dying.arm();
          const died = await first.retain(new Set([a.root])).then(
            () => undefined,
            (error: unknown) => error,
          );
          expect(died).toBeInstanceOf(Error);
          expect((died as Error).message).toBe("the process died mid-retain");

          opened = await opened.reopen();
          const store = createLedgerStore(opened.storage);
          // Re-creating the dropped ledger makes nodes the retain queued for
          // deletion, or already deleted, recur.
          const again = await (await store.at(a.root)).apply(bMutations);
          expect(again.root).toBe(b.root);
          await store.persist(again);
          const bEntries = appliedEntries(entries, bMutations);
          await expectLedgerMatches(await store.at(a.root), entries);
          await expectLedgerMatches(await store.at(b.root), bEntries);
          const cHeld = await store.at(c.root).then(
            (ledger) => ledger,
            () => undefined,
          );
          if (cHeld !== undefined)
            await expectLedgerMatches(
              cHeld,
              appliedEntries(bEntries, cMutations),
            );
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
};
