import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  type BlockSummary,
  createWalletSeeder,
  seedWallets,
} from "../src/index.js";
import type { SimUtxo } from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";
import {
  chain,
  ORIGIN,
  point,
  started,
  storeAdapters,
  TRACKED,
  TX1,
  TX2,
  UNTRACKED,
} from "./support/small-chain.js";
import {
  atCursor,
  fakeLedger,
  key,
  preOrigin,
  seedRows,
  WALLET_B,
} from "./support/wallet-seed-ledger.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-wallet-seed-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const sleep = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

describe.each(storeAdapters(databases, scratch))(
  "LSQ wallet seed ($name)",
  (adapter) => {
    it("writes pre-origin wallet UTxOs as seed rows at the cursor and skips stored ones", async () => {
      const { store } = await started(adapter, 4, 2);
      try {
        const cursor = point(chain()[1]);
        const a1 = preOrigin(0xd1, TRACKED);
        const a2 = preOrigin(0xd2, TRACKED, 3);
        // TX2#0 is the post-origin wallet output the follower stored itself.
        const stored: SimUtxo = {
          outRef: { txHash: TX2, index: 0 },
          output: { address: TRACKED, lovelace: 2_000_000n },
        };
        const isCursor = await atCursor(store)();
        const ledger = fakeLedger({
          utxos: () => [a1, a2, stored],
          acquirable: isCursor,
        });
        const result = await seedWallets(store, ledger, [TRACKED]);
        expect(result).toMatchObject({
          kind: "seeded",
          at: cursor,
          generation: 0,
        });
        expect(
          result.kind === "seeded" ? result.inserted.map(String) : [],
        ).toHaveLength(2);
        expect(result.kind === "seeded" ? result.skipped : []).toEqual([
          stored.outRef,
        ]);
        expect(ledger.acquired).toEqual([String(cursor.slot)]);
        expect(await seedRows(store)).toEqual([
          `${key(a1)}@${cursor.slot}`,
          `${key(a2)}@${cursor.slot}`,
        ]);
        expect(await store.output(a2.outRef)).toMatchObject({
          created: null,
          seedSlot: cursor.slot,
          spent: null,
          output: { address: TRACKED, lovelace: 0xd2n * 1_000_000n },
        });
        expect(store.isTrackedLive(a1.outRef)).toBe(true);
        expect((await store.checkInvariants()).ok).toBe(true);
        // Idempotent: the same seed again writes nothing.
        expect(await seedWallets(store, ledger, [TRACKED])).toMatchObject({
          kind: "seeded",
          inserted: [],
        });
        expect(await seedRows(store)).toHaveLength(2);
      } finally {
        await store.close();
      }
    });

    it("waits while the cursor is not acquirable, writing nothing", async () => {
      const { store } = await started(adapter, 4, 2);
      try {
        const ledger = fakeLedger({
          utxos: () => [preOrigin(0xd1, TRACKED)],
          acquirable: () => false,
        });
        expect(await seedWallets(store, ledger, [TRACKED])).toMatchObject({
          kind: "pending",
          reason: "cursor_not_acquirable",
        });
        expect(await seedRows(store)).toEqual([]);
      } finally {
        await store.close();
      }
    });

    it("a slow seed read while a block arrives succeeds on its first read, and the block's spend of a seeded output lands after it", async () => {
      const { store } = await started(adapter, 4, 2);
      try {
        // b3's failed tx consumes the pre-origin wallet UTxO a1 as
        // collateral: a seed written after b3 without seeing that spend
        // would leave a1 live.
        const a1 = preOrigin(0xd1, TRACKED);
        const b3 = chain()[2] as BlockSummary;
        const spendA1: BlockSummary = {
          ...b3,
          txs: [
            {
              ...(b3.txs[0] as BlockSummary["txs"][number]),
              collaterals: [a1.outRef],
            },
          ],
        };
        let landing: Promise<unknown> | undefined;
        const ledger = fakeLedger({
          utxos: () => [a1],
          acquirable: () => true,
          duringQuery: async () => {
            if (landing !== undefined) return;
            // The block arrives while the node is still scanning; the scan
            // outlasts the block's write.
            landing = store.applyBlock(spendA1);
            await Promise.race([landing, sleep(300)]);
          },
        });
        const result = await seedWallets(store, ledger, [TRACKED]);
        expect(result).toMatchObject({
          kind: "seeded",
          at: point(chain()[1]),
        });
        expect(ledger.queries).toHaveLength(1);
        // The scan is sent with no request deadline.
        expect(ledger.deadlines).toEqual(["none"]);
        expect(await landing).toMatchObject({ kind: "applied" });
        expect(await store.output(a1.outRef)).toMatchObject({
          seedSlot: point(chain()[1]).slot,
          spent: { slot: point(spendA1).slot },
        });
        expect(store.isTrackedLive(a1.outRef)).toBe(false);
        expect((await store.checkInvariants()).ok).toBe(true);
      } finally {
        await store.close();
      }
    });

    it("refuses an answer that holds an output at another address", async () => {
      const { store } = await started(adapter, 4, 2);
      try {
        const ledger = fakeLedger({
          utxos: () => [preOrigin(0xd1, TRACKED)],
          acquirable: () => true,
          tamper: (utxos) => [...utxos, preOrigin(0xd9, WALLET_B)],
        });
        expect(await seedWallets(store, ledger, [TRACKED])).toMatchObject({
          kind: "pending",
          reason: "ledger_answer_invalid",
        });
        expect(await seedRows(store)).toEqual([]);
      } finally {
        await store.close();
      }
    });

    it("reports a lost writer lease as store_locked, writing nothing", async () => {
      const { store } = await started(adapter, 4, 2);
      try {
        // What a newer lease holder's start does.
        await store.transaction("write", (tx) =>
          tx.query(
            "UPDATE l1_follower_writer SET writer_epoch = writer_epoch + 1",
          ),
        );
        const ledger = fakeLedger({
          utxos: () => [preOrigin(0xd1, TRACKED)],
          acquirable: () => true,
        });
        expect(await seedWallets(store, ledger, [TRACKED])).toMatchObject({
          kind: "pending",
          reason: "store_locked",
        });
        expect(await seedRows(store)).toEqual([]);
      } finally {
        await store.close();
      }
    });

    it("adding a wallet tracks it and seeds that wallet only, once", async () => {
      const { store } = await started(adapter, 4, 2);
      const a1 = preOrigin(0xd1, TRACKED);
      const b1 = preOrigin(0xe1, WALLET_B);
      const b2 = preOrigin(0xe2, WALLET_B, 1);
      const ledger = fakeLedger({
        utxos: () => [a1, b1, b2],
        acquirable: () => true,
      });
      const seeder = createWalletSeeder({ store, ledger, wallets: [TRACKED] });
      try {
        expect(seeder.ready()).toBe(false);
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(ledger.queries).toEqual([[TRACKED.toString("hex")]]);
        expect(await seedRows(store)).toEqual([
          `${key(a1)}@${point(chain()[1]).slot}`,
        ]);

        seeder.addWallets([WALLET_B]);
        expect(store.trackedSet().addresses.has(WALLET_B.toString("hex"))).toBe(
          true,
        );
        expect(seeder.owed()).toEqual([WALLET_B]);
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(ledger.queries.at(-1)).toEqual([WALLET_B.toString("hex")]);
        const slot = point(chain()[1]).slot;
        expect(await seedRows(store)).toEqual([
          `${key(a1)}@${slot}`,
          `${key(b1)}@${slot}`,
          `${key(b2)}@${slot}`,
        ]);

        // Adding it again: its stored seed point still covers it, so the
        // step settles it with no read and writes nothing new.
        seeder.addWallets([WALLET_B]);
        expect(seeder.owed()).toEqual([WALLET_B]);
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(ledger.queries).toHaveLength(2);
        expect(await seedRows(store)).toHaveLength(3);
        expect((await store.checkInvariants()).ok).toBe(true);
      } finally {
        seeder.close();
        await store.close();
      }
    });

    it("deletes the seed on a rewind below its point, owes it again and re-seeds", async () => {
      const { store } = await started(adapter, 4, 3);
      const a1 = preOrigin(0xd1, TRACKED);
      const ledger = fakeLedger({ utxos: () => [a1], acquirable: () => true });
      const seeder = createWalletSeeder({ store, ledger, wallets: [TRACKED] });
      try {
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(await seedRows(store)).toEqual([
          `${key(a1)}@${point(chain()[2]).slot}`,
        ]);
        // The rewind to b2 lies below the seed point (b3): the seed row goes.
        expect(await store.rewind(point(chain()[1]))).toMatchObject({
          kind: "rewound",
        });
        expect(await seedRows(store)).toEqual([]);
        expect(store.isTrackedLive(a1.outRef)).toBe(false);
        expect(seeder.owed()).toEqual([TRACKED]);
        expect(await seeder.step()).toEqual({ kind: "ready" });
        const b2 = point(chain()[1]).slot;
        expect(await seedRows(store)).toEqual([`${key(a1)}@${b2}`]);
        // A rewind to the seed point itself keeps the row and owes nothing.
        expect(
          await store.applyBlock(chain()[2] as BlockSummary),
        ).toMatchObject({ kind: "applied" });
        expect(await store.rewind(point(chain()[1]))).toMatchObject({
          kind: "rewound",
        });
        expect(seeder.ready()).toBe(true);
        expect(await seedRows(store)).toEqual([`${key(a1)}@${b2}`]);
        expect((await store.checkInvariants()).ok).toBe(true);
      } finally {
        seeder.close();
        await store.close();
      }
    });

    it("a restarted seeder over a seeded store issues no utxo_by_address query", async () => {
      const opened = await started(adapter, 4, 2);
      const a1 = preOrigin(0xd1, TRACKED);
      const ledger = fakeLedger({ utxos: () => [a1], acquirable: () => true });
      const first = createWalletSeeder({
        store: opened.store,
        ledger,
        wallets: [TRACKED],
      });
      expect(await first.step()).toEqual({ kind: "ready" });
      first.close();
      await opened.store.close();
      expect(ledger.queries).toEqual([[TRACKED.toString("hex")]]);
      const store = opened.reopen();
      expect(await store.start()).toMatchObject({ kind: "ready" });
      const again = createWalletSeeder({ store, ledger, wallets: [TRACKED] });
      try {
        expect(await again.step()).toEqual({ kind: "ready" });
        expect(again.ready()).toBe(true);
        expect(ledger.queries).toHaveLength(1);
        expect(await seedRows(store)).toEqual([
          `${key(a1)}@${point(chain()[1]).slot}`,
        ]);
      } finally {
        again.close();
        await store.close();
      }
    });

    it("owes the seed again for a wallet a restart did not track, so an output paid to it meanwhile is seeded", async () => {
      const opened = await started(adapter, 4, 2);
      const b1 = preOrigin(0xe1, WALLET_B);
      // Paid to WALLET_B while no start tracked it: no block row holds it.
      const paidUntracked = preOrigin(0xe2, WALLET_B);
      const utxos: SimUtxo[] = [b1];
      const ledger = fakeLedger({ utxos: () => utxos, acquirable: () => true });
      const first = createWalletSeeder({
        store: opened.store,
        ledger,
        wallets: [TRACKED],
      });
      first.addWallets([WALLET_B]);
      expect(await first.step()).toEqual({ kind: "ready" });
      first.close();
      await opened.store.close();
      // A start whose own wallets leave WALLET_B out.
      const untracked = opened.reopen();
      expect(await untracked.start()).toMatchObject({ kind: "ready" });
      await untracked.close();
      utxos.push(paidUntracked);
      const store = opened.reopen();
      expect(await store.start()).toMatchObject({ kind: "ready" });
      const again = createWalletSeeder({ store, ledger, wallets: [TRACKED] });
      try {
        again.addWallets([WALLET_B]);
        expect(await again.step()).toEqual({ kind: "ready" });
        expect(ledger.queries.at(-1)).toEqual([WALLET_B.toString("hex")]);
        expect(store.isTrackedLive(paidUntracked.outRef)).toBe(true);
        expect((await store.checkInvariants()).ok).toBe(true);
      } finally {
        again.close();
        await store.close();
      }
    });

    it("records a wallet with no outputs as seeded and does not query it again after a restart", async () => {
      const opened = await started(adapter, 4, 2, { wallets: [WALLET_B] });
      const ledger = fakeLedger({ utxos: () => [], acquirable: () => true });
      const first = createWalletSeeder({
        store: opened.store,
        ledger,
        wallets: [WALLET_B],
      });
      expect(await first.step()).toEqual({ kind: "ready" });
      first.close();
      await opened.store.close();
      expect(ledger.queries).toEqual([[WALLET_B.toString("hex")]]);
      const store = opened.reopen();
      expect(await store.start()).toMatchObject({ kind: "ready" });
      const again = createWalletSeeder({ store, ledger, wallets: [WALLET_B] });
      try {
        expect(await again.step()).toEqual({ kind: "ready" });
        expect(ledger.queries).toHaveLength(1);
        expect(await seedRows(store)).toEqual([]);
      } finally {
        again.close();
        await store.close();
      }
    });

    it("owes the seed after a restart only when a committed rewind went below its point", async () => {
      // Seeded at b1; the store then follows to b3.
      const opened = await started(adapter, 4, 1);
      const a1 = preOrigin(0xd1, TRACKED);
      const ledger = fakeLedger({ utxos: () => [a1], acquirable: () => true });
      const b1 = point(chain()[0]).slot;
      let store = opened.store;
      const restarted = async () => {
        await store.close();
        store = opened.reopen();
        expect(await store.start()).toMatchObject({ kind: "ready" });
        return createWalletSeeder({ store, ledger, wallets: [TRACKED] });
      };
      let seeder = createWalletSeeder({ store, ledger, wallets: [TRACKED] });
      try {
        expect(await seeder.step()).toEqual({ kind: "ready" });
        for (const block of chain().slice(1))
          expect(await store.applyBlock(block)).toMatchObject({
            kind: "applied",
          });
        // A rewind to b2 lies above the seed point (b1): still seeded.
        expect(await store.rewind(point(chain()[1]))).toMatchObject({
          kind: "rewound",
        });
        expect(seeder.ready()).toBe(true);
        seeder.close();
        seeder = await restarted();
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(ledger.queries).toHaveLength(1);
        // A rewind to the seed point itself keeps it too.
        expect(await store.rewind(point(chain()[0]))).toMatchObject({
          kind: "rewound",
        });
        seeder.close();
        seeder = await restarted();
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(ledger.queries).toHaveLength(1);
        expect(await seedRows(store)).toEqual([`${key(a1)}@${b1}`]);
        // A rewind to the origin lies below it: owed again, after a restart too.
        expect(await store.rewind(ORIGIN.point)).toMatchObject({
          kind: "rewound",
        });
        expect(seeder.ready()).toBe(false);
        seeder.close();
        seeder = await restarted();
        expect(seeder.ready()).toBe(false);
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(ledger.queries).toHaveLength(2);
        expect(await seedRows(store)).toEqual([
          `${key(a1)}@${ORIGIN.point.slot}`,
        ]);
        expect((await store.checkInvariants()).ok).toBe(true);
      } finally {
        seeder.close();
        await store.close();
      }
    });

    it("seeds the output a stored tx paid to an added wallet, and drops it when a fork orphans the tx", async () => {
      // TX1 (b1, slot 101) is stored for its tracked output #0; its output
      // #1 pays UNTRACKED, which got no row. The role then adds UNTRACKED.
      const { store } = await started(adapter, 4, 2);
      const paid: SimUtxo = {
        outRef: { txHash: TX1, index: 1 },
        output: { address: UNTRACKED, lovelace: 1n },
      };
      // The node's chain: TX1 exists while the cursor is at b1 or above.
      let tipSlot = point(chain()[1]).slot;
      const ledger = fakeLedger({
        utxos: () => (tipSlot >= point(chain()[0]).slot ? [paid] : []),
        acquirable: () => true,
      });
      const seeder = createWalletSeeder({ store, ledger, wallets: [TRACKED] });
      try {
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(await store.output(paid.outRef)).toBeNull();
        seeder.addWallets([UNTRACKED]);
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(await seedRows(store)).toEqual([`${key(paid)}@${tipSlot}`]);
        expect(await store.txByHash(TX1)).not.toBeNull();
        expect((await store.checkInvariants()).ok).toBe(true);
        // A fork back to the origin orphans TX1: the seed row goes, and the
        // re-seed on the new chain finds nothing (no phantom).
        tipSlot = ORIGIN.point.slot;
        expect(await store.rewind(ORIGIN.point)).toMatchObject({
          kind: "rewound",
        });
        expect(await store.output(paid.outRef)).toBeNull();
        expect(seeder.owed()).toEqual(
          expect.arrayContaining([TRACKED, UNTRACKED]),
        );
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(await seedRows(store)).toEqual([]);
        expect(store.isTrackedLive(paid.outRef)).toBe(false);
        expect((await store.checkInvariants()).ok).toBe(true);
      } finally {
        seeder.close();
        await store.close();
      }
    });
  },
);
