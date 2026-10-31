import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type ChainPoint,
  type LedgerQuery,
  TransportRequestError,
} from "@al-ft/l1-node-transport";
import { afterAll, describe, expect, it } from "vitest";

import {
  type BlockSummary,
  createWalletSeeder,
  type FactStore,
  seedWallets,
  type WalletLedger,
} from "../src/index.js";
import { encodeUtxoAnswer, type SimUtxo } from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";
import {
  chain,
  fill,
  ORIGIN,
  point,
  started,
  storeAdapters,
  TRACKED,
  TX1,
  TX2,
  UNTRACKED,
} from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-wallet-seed-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

/** A second own wallet (enterprise key address) the store does not track yet. */
const WALLET_B = Buffer.concat([Buffer.of(0x60), fill(0x44, 28)]);

const preOrigin = (byte: number, address: Buffer, index = 0): SimUtxo => ({
  outRef: { txHash: fill(byte), index },
  output: { address, lovelace: BigInt(byte) * 1_000_000n },
});

const key = (utxo: SimUtxo): string =>
  `${utxo.outRef.txHash.toString("hex")}#${utxo.outRef.index}`;

/**
 * A node's ledger for the seed: the UTxO set it answers `utxo_by_address`
 * from, acquirable only at the points `acquirable` admits (the volatile
 * window). Records every acquisition and query.
 */
const fakeLedger = (
  input: Readonly<{
    utxos: () => readonly SimUtxo[];
    acquirable: (at: ChainPoint | "tip") => boolean;
    duringQuery?: () => Promise<void>;
    tamper?: (utxos: SimUtxo[]) => SimUtxo[];
  }>,
): WalletLedger & { queries: string[][]; acquired: string[] } => {
  const queries: string[][] = [];
  const acquired: string[] = [];
  return {
    queries,
    acquired,
    withLedgerState: async (at, use) => {
      if (!input.acquirable(at))
        throw new TransportRequestError(
          "acquire_point_too_old",
          "the point is not in the volatile window",
        );
      acquired.push(
        at === "tip" ? "tip" : at.kind === "point" ? `${at.slot}` : "origin",
      );
      return await use({
        query: async (query: LedgerQuery) => {
          if (query.query !== "utxo_by_address")
            throw new Error(`unexpected query ${query.query}`);
          const wanted = query.addresses.map((a) =>
            Buffer.from(a).toString("hex"),
          );
          queries.push(wanted);
          await input.duringQuery?.();
          const matching = input
            .utxos()
            .filter((u) => wanted.includes(u.output.address.toString("hex")));
          return encodeUtxoAnswer(input.tamper?.(matching) ?? matching);
        },
      });
    },
  };
};

const atCursor =
  (store: FactStore) =>
  async (): Promise<(at: ChainPoint | "tip") => boolean> => {
    const cursor = await store.cursor();
    return (at) =>
      at !== "tip" &&
      at.kind === "point" &&
      cursor !== null &&
      at.slot === BigInt(cursor.point.slot) &&
      at.hash === cursor.point.hash.toString("hex");
  };

const seedRows = async (store: FactStore) =>
  await store.transaction("read", async (tx) =>
    (
      await tx.query(
        "SELECT tx_hash, output_index, seed_slot, created_slot FROM l1_outputs WHERE created_slot IS NULL ORDER BY tx_hash, output_index",
      )
    ).map(
      (row) =>
        `${Buffer.from(row.tx_hash as Uint8Array).toString("hex")}#${Number(row.output_index)}@${Number(row.seed_slot)}`,
    ),
  );

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

    it("rereads at the new cursor when a block lands between the read and the write", async () => {
      const { store } = await started(adapter, 4, 2);
      try {
        // A pre-origin wallet UTxO that b3's failed tx consumes as collateral
        // would be missed by a seed written after b3; here a1 is spent in b3.
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
        let spent = false;
        let landed = false;
        const ledger = fakeLedger({
          utxos: () => (spent ? [] : [a1]),
          acquirable: () => true,
          duringQuery: async () => {
            if (landed) return;
            landed = true;
            expect(await store.applyBlock(spendA1)).toMatchObject({
              kind: "applied",
            });
            spent = true;
          },
        });
        const result = await seedWallets(store, ledger, [TRACKED]);
        expect(result).toMatchObject({
          kind: "seeded",
          at: point(spendA1),
          inserted: [],
        });
        expect(ledger.acquired).toEqual([
          String(point(chain()[1]).slot),
          String(point(spendA1).slot),
        ]);
        expect(await store.output(a1.outRef)).toBeNull();
        expect(store.isTrackedLive(a1.outRef)).toBe(false);
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

    it("adding a wallet tracks it and re-seeds that wallet only, idempotently", async () => {
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

        // Adding it again re-seeds it alone and writes nothing new.
        seeder.addWallets([WALLET_B]);
        expect(await seeder.step()).toEqual({ kind: "ready" });
        expect(ledger.queries.at(-1)).toEqual([WALLET_B.toString("hex")]);
        expect(ledger.queries).toHaveLength(3);
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
