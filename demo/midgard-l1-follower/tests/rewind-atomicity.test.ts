import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import type { BlockSummary, FactStore } from "../src/index.js";
import { rewindFaults } from "../src/store/fact-store.js";
import {
  dumpStore,
  FACT_QUERIES,
  type StoreDump,
} from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";
import { resetAdapters } from "./support/reset-stores.js";
import { chain, fill, ORIGIN } from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-rewind-atomicity-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

/**
 * Every row a rewind may touch, the generation and the rollback log
 * included, plus the fixture D-t tables (`dumpStore` adds registered
 * tables). A failed rewind must leave all of it exactly as it was.
 */
const EVERYTHING: Readonly<Record<string, string>> = {
  ...FACT_QUERIES,
  l1_follower_cursor: "SELECT * FROM l1_follower_cursor",
  l1_rollbacks: "SELECT * FROM l1_rollbacks",
};

const snapshot = (store: FactStore): Promise<StoreDump> =>
  dumpStore(store, EVERYTHING);

const [b1, , b3] = chain() as [BlockSummary, BlockSummary, BlockSummary];

describe.each(resetAdapters(databases, scratch))(
  "rewind atomicity ($name)",
  (adapter) => {
    /** origin + three blocks with fixture D-t rows, an event key and a protocol-init fact. */
    const followed = async (): Promise<FactStore> => {
      const store = (await adapter.create()).store();
      store.watchProtocolInit({ txHash: fill(0x01), index: 0 });
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(await store.initialize(ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      for (const block of chain())
        expect(await store.applyBlock(block)).toMatchObject({
          kind: "applied",
        });
      return store;
    };

    it("a rewind changes rows (the snapshot sees a rewind)", async () => {
      const store = await followed();
      try {
        const before = await snapshot(store);
        expect(await store.rewind(b1.point)).toMatchObject({
          kind: "rewound",
          generation: 1,
        });
        expect(await snapshot(store)).not.toEqual(before);
      } finally {
        await store.close();
      }
    });

    it("a rewind whose post-rewind check fails (R5) rolls back every row", async () => {
      const store = await followed();
      try {
        const before = await snapshot(store);
        rewindFaults.set(store, "skip_temporal_truncation");
        expect(await store.rewind(b1.point)).toMatchObject({
          kind: "intervention",
          reason: "store_integrity",
        });
        expect(await snapshot(store)).toEqual(before);
        expect((await store.cursor())?.point.hash.equals(b3.point.hash)).toBe(
          true,
        );
      } finally {
        await store.close();
      }
    });

    it("a rewind that fails after moving the cursor rolls back every row, and the next rewind succeeds", async () => {
      const store = await followed();
      try {
        const before = await snapshot(store);
        rewindFaults.set(store, "fail_after_cursor_update");
        expect(await store.rewind(b1.point)).toMatchObject({ kind: "error" });
        expect(await snapshot(store)).toEqual(before);
        expect((await store.checkInvariants()).ok).toBe(true);
        rewindFaults.delete(store);
        expect(await store.rewind(b1.point)).toMatchObject({
          kind: "rewound",
          generation: 1,
          depth: 2,
        });
        expect((await store.checkInvariants()).ok).toBe(true);
      } finally {
        await store.close();
      }
    });
  },
);
