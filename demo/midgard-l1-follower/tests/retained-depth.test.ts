import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  type BlockSummary,
  createFactStore,
  type SqlBackend,
} from "../src/index.js";
import { testDatabases } from "./support/postgres.js";
import {
  chain,
  fill,
  options,
  point,
  started,
  storeAdapters,
  TX1,
} from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-retained-depth-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const adapters = storeAdapters(databases, scratch);

// The retained depth (how deep prune keeps facts) is separate from k (the
// deepest rollback the store rewinds): a role whose reads sit below k keeps
// those facts, and a rollback deeper than k is still refused.
it("refuses a retained depth below k or not an integer", () => {
  // Refused before the backend is touched.
  for (const retainedDepth of [1, 2.5, Number.NaN])
    expect(() =>
      createFactStore({} as SqlBackend, { ...options(2), retainedDepth }),
    ).toThrow(
      /retainedDepth must be an integer no smaller than securityParameter/u,
    );
});

describe.each(adapters)("retained depth ($name)", (adapter) => {
  it("keeps facts down to the retained depth while refusing rollbacks deeper than k", async () => {
    const [b1, b2, b3] = chain();
    // An empty b4 (height 54): with k = 1 and two blocks more retained, the
    // block k + 2 below it is b1 (height 51), and b2 is k + 1 below it.
    const b4: BlockSummary = {
      point: { slot: 107, hash: fill(0xc4) },
      height: 54,
      parentHash: point(b3).hash,
      txs: [],
    };
    const { store } = await started(adapter, 1, 3, { retainedDepth: 3 });
    const { store: plain } = await started(adapter, 1);
    try {
      for (const each of [store, plain])
        expect(await each.applyBlock(b4)).toMatchObject({ kind: "applied" });
      // Retained: the boundary is b1, so TX1#0 (spent at 103) and b1 stay.
      expect(await store.prune()).toMatchObject({
        done: true,
        prunedThroughSlot: 101,
      });
      expect(await store.blockByHash(point(b1).hash)).not.toBeNull();
      expect(await store.output({ txHash: TX1, index: 0 })).not.toBeNull();
      expect(await store.spenderOf({ txHash: TX1, index: 0 })).toMatchObject({
        kind: "spent",
      });
      // Without it the boundary is k below the cursor (b3): both go.
      expect(await plain.prune()).toMatchObject({
        done: true,
        prunedThroughSlot: 105,
      });
      expect(await plain.blockByHash(point(b1).hash)).toBeNull();
      expect(await plain.output({ txHash: TX1, index: 0 })).toBeNull();
      // A rollback of k + 1 to the retained b2 is refused for its depth.
      const before = (await store.cursor())!;
      expect(await store.rewind(point(b2))).toMatchObject({
        kind: "intervention",
        reason: "rollback_beyond_k",
        detail: expect.stringMatching(
          /^rollback of 2 blocks .* exceeds k = 1 /u,
        ) as unknown,
      });
      expect(await store.cursor()).toEqual(before);
      // A rollback of exactly k is taken.
      expect(await store.rewind(point(b3))).toMatchObject({
        kind: "rewound",
        depth: 1,
      });
      expect((await store.checkInvariants()).ok).toBe(true);
    } finally {
      await store.close();
      await plain.close();
    }
  });
});
