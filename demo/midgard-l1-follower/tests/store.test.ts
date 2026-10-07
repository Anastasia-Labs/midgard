import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import pg from "pg";
import { afterAll, describe, expect, it } from "vitest";

import { type BlockSummary, listenForGenerations } from "../src/index.js";
import { matching } from "./support/matchers.js";
import { testDatabases } from "./support/postgres.js";
import {
  chain,
  CRED,
  fill,
  ORIGIN,
  output,
  point,
  POLICY,
  started,
  storeAdapters,
  TRACKED,
  tx,
  TX1,
  TX2,
  TX3,
} from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-store-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const adapters = storeAdapters(databases, scratch);

describe.each(adapters)("fact store ($name)", (adapter) => {
  it("refuses writes before start, before initialize, and off the cursor", async () => {
    const { store } = await adapter.open(2);
    try {
      expect(await store.applyBlock(chain()[0] as BlockSummary)).toMatchObject({
        kind: "error",
      });
      await store.start();
      expect(await store.applyBlock(chain()[0] as BlockSummary)).toMatchObject({
        kind: "rejected",
        reason: "not_initialized",
      });
      expect(await store.initialize(ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      expect(await store.initialize(ORIGIN)).toMatchObject({
        kind: "already_initialized",
      });
      expect(
        await store.initialize({
          ...ORIGIN,
          point: { slot: 100, hash: fill(0xa1) },
        }),
      ).toMatchObject({ kind: "origin_mismatch" });
      expect(await store.applyBlock(chain()[1] as BlockSummary)).toMatchObject({
        kind: "rejected",
        reason: "not_on_cursor",
      });
      expect((await store.checkInvariants()).ok).toBe(true);
    } finally {
      await store.close();
    }
  });

  it("qualifies, stores and answers the read API", async () => {
    const { store } = await started(adapter);
    const [b1, b2, b3] = chain();
    try {
      expect(store.liveOutRefCount()).toBe(1);
      expect(store.isTrackedLive({ txHash: TX3, index: 1 })).toBe(true);
      const atTip = await store.liveUtxos({ by: "address", address: TRACKED });
      expect(atTip).toMatchObject({
        kind: "ok",
        utxos: [
          {
            outRef: { txHash: TX3, index: 1 },
            created: { slot: 105, txIndex: 0 },
          },
        ],
      });
      const atB1 = await store.liveUtxos(
        { by: "address", address: TRACKED },
        point(b1),
      );
      expect(atB1).toMatchObject({
        kind: "ok",
        utxos: [
          {
            outRef: { txHash: TX1, index: 0 },
            spent: { slot: 103, txHash: TX2 },
          },
        ],
      });
      if (atB1.kind === "ok")
        expect(
          atB1.utxos[0]?.output.assets.get(POLICY.toString("hex"))?.get("aa"),
        ).toBe(3n);
      expect(
        await store.liveUtxos({ by: "unit", policyId: POLICY }, point(b1)),
      ).toMatchObject({ kind: "ok", utxos: [{}] });
      expect(
        await store.liveUtxos(
          { by: "unit", policyId: POLICY, assetName: Buffer.from("bb", "hex") },
          point(b1),
        ),
      ).toMatchObject({ kind: "ok", utxos: [] });
      expect(
        await store.liveUtxos(
          { by: "payment_credential", hash: CRED },
          point(b2),
        ),
      ).toMatchObject({ kind: "ok", utxos: [{ outRef: { txHash: TX2 } }] });
      expect(
        await store.liveUtxos({
          by: "outref",
          outRefs: [
            { txHash: TX2, index: 0 },
            { txHash: TX3, index: 1 },
          ],
        }),
      ).toMatchObject({ kind: "ok", utxos: [{ outRef: { txHash: TX3 } }] });
      expect(await store.liveUtxos({ by: "outref", outRefs: [] })).toEqual({
        kind: "ok",
        utxos: [],
      });
      expect(
        await store.liveUtxos(
          { by: "address", address: TRACKED },
          { slot: 103, hash: fill(0xee) },
        ),
      ).toMatchObject({ kind: "point_not_canonical" });
      // The untracked output of TX1 was never stored.
      expect(await store.output({ txHash: TX1, index: 1 })).toBeNull();
      expect(await store.spenderOf({ txHash: TX1, index: 1 })).toEqual({
        kind: "unknown",
      });
      expect(await store.spenderOf({ txHash: TX2, index: 0 })).toEqual({
        kind: "spent",
        txHash: TX3,
        slot: 105,
      });
      expect(await store.spenderOf({ txHash: TX3, index: 1 })).toEqual({
        kind: "unspent",
      });
      expect(await store.txByHash(TX3)).toMatchObject({
        isValid: false,
        blockSlot: 105,
        outputCount: 1,
        hasCollateralReturn: true,
        collaterals: [{ txHash: TX2, index: 0 }],
      });
      expect(await store.isCanonical(point(b2).hash)).toBe(true);
      expect(await store.blockAtHeight(52)).toMatchObject({
        slot: 103,
        qualifyingTxCount: 1,
      });
      expect(await store.blockAtOrBeforeSlot(104)).toMatchObject({ slot: 103 });
      expect(await store.blockByHash(point(b3).hash)).toMatchObject({
        height: 53,
        parentHash: point(b2).hash,
      });
      expect(await store.pointStatus(point(b1))).toEqual({
        kind: "canonical",
        height: 51,
        depth: 2,
      });
      expect(await store.cursor()).toMatchObject({
        point: { slot: 105 },
        height: 53,
        generation: 0,
        prunedThroughSlot: 100,
      });
    } finally {
      await store.close();
    }
  });

  it("rewinds with typed noop, R1 and R2 results and patches the cache", async () => {
    const { store } = await started(adapter);
    const [b1, b2, b3] = chain();
    try {
      expect(await store.rewind(point(b3))).toMatchObject({ kind: "noop" });
      expect(await store.rewind({ slot: 104, hash: fill(0xee) })).toMatchObject(
        { kind: "intervention", reason: "intersection_outside_history" },
      );
      expect(
        await store.rewind({ slot: 102, hash: point(b1).hash }),
      ).toMatchObject({
        kind: "intervention",
        reason: "intersection_outside_history",
      });
      expect(await store.rewind({ slot: 99, hash: fill(0xee) })).toMatchObject({
        kind: "intervention",
        reason: "rollback_beyond_k",
      });
      expect(await store.rewind(ORIGIN.point)).toMatchObject({
        kind: "intervention",
        reason: "rollback_beyond_k",
      });
      const events: number[] = [];
      store.onGeneration(({ generation }) => events.push(generation));
      const rewound = await store.rewind(point(b1));
      expect(rewound).toMatchObject({
        kind: "rewound",
        generation: 1,
        depth: 2,
        unspent: [{ txHash: TX1, index: 0 }],
      });
      if (rewound.kind === "rewound")
        expect(
          rewound.deleted.map((outRef) => outRef.txHash.toString("hex")).sort(),
        ).toEqual([TX2, TX3].map((hash) => hash.toString("hex")));
      expect(events).toEqual([1]);
      expect(store.liveOutRefCount()).toBe(1);
      expect(store.isTrackedLive({ txHash: TX1, index: 0 })).toBe(true);
      expect(await store.isCanonical(point(b2).hash)).toBe(false);
      expect(await store.txByHash(TX2)).toBeNull();
      const rollbacks = await store.transaction("read", (sql) =>
        sql.query(
          "SELECT generation, from_slot, to_slot, depth_blocks FROM l1_rollbacks",
        ),
      );
      expect(
        rollbacks.map((row) => [
          Number(row.generation),
          Number(row.from_slot),
          Number(row.to_slot),
          Number(row.depth_blocks),
        ]),
      ).toEqual([[1, 105, 101, 2]]);
      // The same blocks re-apply on the rewound cursor.
      expect(await store.applyBlock(chain()[1] as BlockSummary)).toMatchObject({
        kind: "applied",
        cursor: { generation: 1 },
      });
      expect((await store.checkInvariants()).ok).toBe(true);
    } finally {
      await store.close();
    }
  });

  it("invalidates a view only when a rewind dropped its point (§8.1)", async () => {
    const { store } = await started(adapter, 3);
    const [b1, b2] = chain();
    try {
      const atB3 = await store.currentView();
      if (atB3 === null) throw new Error("no view");
      expect(await store.viewValid(atB3)).toBe(true);
      expect(await store.rewind(point(b2))).toMatchObject({
        kind: "rewound",
        generation: 1,
      });
      expect(await store.viewValid(atB3)).toBe(false);
      const atB2 = await store.currentView();
      if (atB2 === null) throw new Error("no view");
      expect(await store.rewind(point(b1))).toMatchObject({
        kind: "rewound",
        generation: 2,
      });
      expect(await store.viewValid(atB2)).toBe(false);
      const stale = { generation: 0, point: point(b1), height: 51 };
      expect(await store.viewValid(stale)).toBe(true);
      expect(
        await store.viewValid({ ...stale, generation: 2, point: point(b2) }),
      ).toBe(true);
    } finally {
      await store.close();
    }
  });

  it("stores seed outputs once and never for a stored creating tx (INV6)", async () => {
    const { store } = await started(adapter, 2, 1);
    try {
      const seed = {
        outRef: { txHash: fill(0x5e), index: 2 },
        output: output(TRACKED, 7n, true),
      };
      const first = await store.insertSeedOutputs(99, [
        seed,
        seed,
        { outRef: { txHash: TX1, index: 7 }, output: output(TRACKED, 1n) },
      ]);
      expect(first).toMatchObject({ kind: "seeded", inserted: [{ index: 2 }] });
      expect(
        first !== null && "skipped" in first ? first.skipped.length : -1,
      ).toBe(2);
      expect(await store.insertSeedOutputs(99, [seed])).toMatchObject({
        inserted: [],
        skipped: [{ index: 2 }],
      });
      expect(await store.output(seed.outRef)).toMatchObject({
        created: null,
        seedSlot: 99,
        spent: null,
      });
      expect(store.isTrackedLive(seed.outRef)).toBe(true);
      expect(
        await store.liveUtxos(
          { by: "address", address: TRACKED },
          ORIGIN.point,
        ),
      ).toMatchObject({ kind: "ok", utxos: [{ seedSlot: 99 }] });
      expect((await store.checkInvariants()).ok).toBe(true);
    } finally {
      await store.close();
    }
  });

  it("reloads the tracked-outref cache on start and returns R5 on a broken store", async () => {
    const { store, reopen } = await started(adapter);
    await store.close();
    const again = reopen();
    try {
      expect(await again.start()).toMatchObject({
        kind: "ready",
        liveOutRefs: 1,
        migrated: [],
        cursor: { height: 53 },
      });
      expect(again.isTrackedLive({ txHash: TX3, index: 1 })).toBe(true);
      // A tracked row the cache believes live disappears: the next spend is R5, and R5 is sticky.
      await again.transaction("write", (sql) =>
        sql.query("DELETE FROM l1_outputs WHERE tx_hash = ?", [TX3]),
      );
      const spend: BlockSummary = {
        point: { slot: 107, hash: fill(0xc4) },
        height: 54,
        parentHash: fill(0xc3),
        txs: [tx(fill(0xb4), { inputs: [{ txHash: TX3, index: 1 }] })],
      };
      expect(await again.applyBlock(spend)).toMatchObject({
        kind: "intervention",
        reason: "store_integrity",
      });
      expect(await again.rewind(point(chain()[0]))).toMatchObject({
        kind: "intervention",
        reason: "store_integrity",
      });
      // A block row above the cursor is an INV5 violation at start.
      await again.transaction("write", (sql) =>
        sql.query(
          "INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count) VALUES (?, ?, ?, ?, 0)",
          [999, fill(0xdd), 99, fill(0xc3)],
        ),
      );
    } finally {
      await again.close();
    }
    const broken = reopen();
    try {
      expect(await broken.start()).toMatchObject({
        kind: "intervention",
        reason: "store_integrity",
        detail: matching(/INV5/u),
      });
    } finally {
      await broken.close();
    }
  });

  it("prunes in budgeted steps and refuses reads and rewinds below the window", async () => {
    const { store } = await started(adapter, 1);
    const [b1, b2, b3] = chain();
    try {
      const results = [];
      for (let i = 0; i < 10; i += 1) {
        const result = await store.prune(1);
        if ("kind" in result)
          throw new Error(`prune failed: ${JSON.stringify(result)}`);
        results.push(result);
        if (result.done) break;
      }
      expect(results.length).toBeGreaterThan(1);
      expect(results.at(-1)).toMatchObject({
        done: true,
        prunedThroughSlot: 103,
      });
      // TX1#0 was spent at slot 103 (the boundary): pruned. TX2#0 was spent above it: kept.
      expect(await store.output({ txHash: TX1, index: 0 })).toBeNull();
      expect(await store.output({ txHash: TX2, index: 0 })).not.toBeNull();
      expect(await store.blockByHash(point(b1).hash)).toBeNull();
      expect(await store.blockByHash(ORIGIN.point.hash)).not.toBeNull();
      expect(await store.pointStatus(point(b1))).toMatchObject({
        kind: "point_beyond_retention",
      });
      expect(
        await store.liveUtxos({ by: "address", address: TRACKED }, point(b1)),
      ).toMatchObject({ kind: "point_beyond_retention" });
      expect(await store.rewind(point(b1))).toMatchObject({
        kind: "intervention",
        reason: "rollback_beyond_k",
      });
      expect(await store.rewind(point(b2))).toMatchObject({
        kind: "rewound",
        depth: 1,
      });
      expect(await store.applyBlock(b3 as BlockSummary)).toMatchObject({
        kind: "applied",
      });
      expect((await store.checkInvariants()).ok).toBe(true);
    } finally {
      await store.close();
    }
  });

  if (adapter.name === "postgres")
    it("notifies other processes of a committed rewind", async () => {
      const { store, url } = await started(adapter);
      const pool = new pg.Pool({ connectionString: url, max: 1 });
      const seen: number[] = [];
      const stop = await listenForGenerations(pool, (generation) =>
        seen.push(generation),
      );
      try {
        expect(await store.rewind(point(chain()[1]))).toMatchObject({
          kind: "rewound",
          generation: 1,
        });
        // A refused rewind rolls back and notifies nothing.
        expect(
          await store.rewind({ slot: 50, hash: fill(0xee) }),
        ).toMatchObject({ kind: "intervention" });
        expect(await store.rewind(point(chain()[0]))).toMatchObject({
          kind: "rewound",
          generation: 2,
        });
        for (let i = 0; i < 50 && seen.length < 2; i += 1)
          await new Promise((resolve) => setTimeout(resolve, 20));
        expect(seen).toEqual([1, 2]);
      } finally {
        await stop();
        await pool.end();
        await store.close();
      }
    });
});
