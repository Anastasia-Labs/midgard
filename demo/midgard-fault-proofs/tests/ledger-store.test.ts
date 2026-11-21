import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type LedgerStorage,
} from "../src/ledger-store/index.js";
import {
  describeLedgerStoreConformance,
  type OpenedLedgerStorage,
  testLedgerEntries,
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
