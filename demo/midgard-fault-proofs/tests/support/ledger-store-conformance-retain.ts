/**
 * Conformance cases on `retain` calls overlapping persists and each other,
 * run by every adapter through `describeLedgerStoreConformance`. The kit
 * imports this module, so nothing here reads the kit's exports while the
 * module loads.
 */
import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot } from "../../src/ledger-at.js";
import {
  createLedgerStore,
  LedgerNotRetained,
} from "../../src/ledger-store/index.js";
import {
  appliedEntries,
  event,
  expectLedgerMatches,
  type OpenLedgerStorage,
  testLedgerEntries,
} from "./ledger-store-conformance.js";
import { gatedStorage } from "./ledger-store-sweep-faults.js";

const base = () => testLedgerEntries(0, 40);

/** Registers the overlapping-retain cases for one adapter. */
export const describeLedgerStoreRetainConformance = (
  adapter: string,
  open: OpenLedgerStorage,
): void => {
  describe(`ledger store overlapping retains: ${adapter}`, () => {
    it("keeps a root persisted right after a retain is called", async () => {
      const opened = await open();
      try {
        const store = createLedgerStore(opened.storage);
        const entries = base();
        const a = await store.admit(entries, await ledgerEntriesRoot(entries));
        await store.persist(a);
        const mutations = event([0, 1], [700]);
        const next = await (await store.at(a.root)).apply(mutations);
        const retaining = store.retain(new Set([a.root]));
        const persisting = store.persist(next);
        const [retained] = await Promise.all([retaining, persisting]);
        expect(retained).toEqual({ notRetained: new Set() });
        await expectLedgerMatches(
          await store.at(next.root),
          appliedEntries(entries, mutations),
        );
        await expectLedgerMatches(await store.at(a.root), entries);
      } finally {
        await opened.close();
      }
    });

    it("replaces a retain still waiting to start with the next one, and both resolve with its result", async () => {
      const opened = await open();
      try {
        const gate = gatedStorage(opened.storage);
        const store = createLedgerStore(gate.storage);
        const entries = base();
        const a = await store.admit(entries, await ledgerEntriesRoot(entries));
        await store.persist(a);
        const [bMutations, cMutations] = [event([0], [701]), event([1], [702])];
        const b = await a.apply(bMutations);
        await store.persist(b);
        const c = await a.apply(cMutations);
        await store.persist(c);
        const unknownRoot = "cd".repeat(32);
        const sweeping = store.retain(new Set([a.root, b.root, c.root]));
        await gate.atGate;
        // Run on its own, this one would drop `c`.
        const middle = store.retain(new Set([a.root, b.root]));
        const last = store.retain(new Set([a.root, c.root, unknownRoot]));
        gate.release();
        const results = await Promise.all([sweeping, middle, last]);
        expect(results).toEqual([
          { notRetained: new Set() },
          { notRetained: new Set([unknownRoot]) },
          { notRetained: new Set([unknownRoot]) },
        ]);
        await expect(store.at(b.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        await expectLedgerMatches(await store.at(a.root), entries);
        await expectLedgerMatches(
          await store.at(c.root),
          appliedEntries(entries, cMutations),
        );
      } finally {
        await opened.close();
      }
    });
  });
};
