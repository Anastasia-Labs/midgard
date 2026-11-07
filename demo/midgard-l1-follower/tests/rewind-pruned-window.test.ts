import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import { testDatabases } from "./support/postgres.js";
import {
  chain,
  ORIGIN,
  point,
  started,
  storeAdapters,
} from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-rewind-pruned-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const adapters = storeAdapters(databases, scratch);

// The rewind guard refuses a target the prune step has already cut below,
// even when the target is within k of the cursor: the rows the rewind would
// need are gone.
describe.each(adapters)("rewind below the pruned window ($name)", (adapter) => {
  it("refuses a rewind within k to a kept row below the pruned window, and takes one inside it", async () => {
    const { store } = await started(adapter, 2);
    const [b1, b2] = chain();
    try {
      // k = 2 at b3 (height 53): pruned through b1 (slot 101).
      expect(await store.prune()).toMatchObject({
        done: true,
        prunedThroughSlot: 101,
      });
      // Back to b2 (height 52): the origin's kept row (slot 100) is now
      // within k (depth 2), yet below the pruned window.
      expect(await store.rewind(point(b2))).toMatchObject({
        kind: "rewound",
        depth: 1,
      });
      const before = (await store.cursor())!;
      expect(before).toMatchObject({ height: 52, prunedThroughSlot: 101 });
      expect(await store.blockByHash(ORIGIN.point.hash)).not.toBeNull();
      expect(before.height - ORIGIN.height).toBeLessThanOrEqual(2);
      expect(await store.rewind(ORIGIN.point)).toMatchObject({
        kind: "intervention",
        reason: "rollback_beyond_k",
        detail: expect.stringMatching(/retained window/u) as unknown,
      });
      expect(await store.cursor()).toEqual(before);
      // At the window's edge (b1, slot 101): taken.
      expect(await store.rewind(point(b1))).toMatchObject({
        kind: "rewound",
        depth: 1,
      });
      expect(await store.cursor()).toMatchObject({
        height: 51,
        point: point(b1),
      });
      expect((await store.checkInvariants()).ok).toBe(true);
    } finally {
      await store.close();
    }
  });
});
