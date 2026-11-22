/**
 * The replayed ledger's holds report a root lost only when this process saw
 * it held and the store no longer holds it, also across a `retain` that a
 * persist overtook.
 */
import { describe, expect, it } from "vitest";

import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type LedgerStore,
} from "../src/ledger-store/index.js";
import { createLedgerHolds } from "../src/replayed-ledger.holds.js";
import { replayedLedgerChain } from "./helpers/replayed-ledger-chain.js";

describe("the replayed ledger's holds", { timeout: 120_000 }, () => {
  it("still reports a root lost when a retain that dropped it saw it persisted again while it ran", async () => {
    const { base, baseRoot } = await replayedLedgerChain(1);
    const inner = createLedgerStore(createMemoryLedgerStorage());
    // A retain whose deletion the test holds open: the store replaces its
    // retained set when `retain` is called, and resolves on `finish`.
    let finish!: () => void;
    let holdRetain = false;
    const store: LedgerStore = {
      ...inner,
      retain: (roots) => {
        const done = inner.retain(roots);
        if (!holdRetain) return done;
        return new Promise<void>((resolve) => {
          finish = resolve;
        }).then(() => done);
      },
    };
    const holds = createLedgerHolds(store);
    const persist = async () => {
      const release = holds.reserve(baseRoot);
      try {
        await holds.persist(await store.admit(base, baseRoot));
      } finally {
        release();
      }
    };
    await persist();

    holdRetain = true;
    const dropping = holds.retain([]);
    // Persisted after that retain was called: the store keeps it.
    await persist();
    finish();
    expect(await dropping).toEqual(new Set());
    holdRetain = false;
    await expect(store.at(baseRoot)).resolves.toBeDefined();

    // The store loses the root behind the holds' back.
    await inner.retain(new Set());
    expect(await holds.retain([baseRoot])).toEqual(new Set([baseRoot]));
  });
});
