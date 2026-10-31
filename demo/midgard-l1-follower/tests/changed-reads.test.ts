import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  applyChainSyncEvent,
  changedUtxosIn,
  type FactStore,
  liveUnitBeforeIn,
  openPostgresFactStore,
  openSqliteFactStore,
  type OutRef,
} from "../src/index.js";
import {
  SIM_ORIGIN,
  SimChain,
  type SimOutput,
  simStoreOptions,
  simUniverse,
} from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-changed-reads-"));
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const universe = simUniverse();
const policy = universe.trackedPolicy;
const PREFIX = Buffer.from("MRET");

const node = (name: Buffer): SimOutput => ({
  address: universe.trackedAddress,
  lovelace: 2_000_000n,
  assets: new Map([[policy, new Map([[name.toString("hex"), 1n]])]]),
});
const keyed = (byte: number): Buffer =>
  Buffer.concat([PREFIX, Buffer.alloc(28, byte)]);

let nonces = 0;
const nonce = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xc7);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: 0 };
};

describe.each(["sqlite", "postgres"] as const)(
  "changed and predecessor reads (%s)",
  (dialect) => {
    const open = async (k: number) => {
      const options = {
        ...simStoreOptions([], k, dialect),
        trackedSet: universe.tracked,
      };
      const store =
        dialect === "sqlite"
          ? openSqliteFactStore({ ...options, path: ":memory:" })
          : openPostgresFactStore({
              ...options,
              connection: { connectionString: (await databases.create()).url },
            });
      opened.push(store);
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(await store.initialize(SIM_ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      const chain = new SimChain(universe, SIM_ORIGIN, universe.tracked);
      const forward = async (
        outputs: readonly SimOutput[],
        spend: readonly OutRef[] = [],
      ) => {
        const { event, encoded } = chain.forward([
          { inputs: [nonce(), ...spend], outputs, nonce: nonces },
        ]);
        const step = await applyChainSyncEvent(store, event);
        expect(step.result.kind).toBe("applied");
        return { txHash: encoded.txHashes[0]!, slot: chain.tip.point.slot };
      };
      return { store, chain, forward };
    };

    it("returns only the rows created or spent after a slot, and refuses a slot below the retained window", async () => {
      const { store, forward } = await open(100);
      const filter = {
        by: "unit",
        policyId: Buffer.from(policy, "hex"),
      } as const;
      const first = await forward([node(keyed(1)), node(keyed(2))]);
      const second = await forward([node(keyed(3))]);
      const third = await forward([], [{ txHash: first.txHash, index: 0 }]);
      const read = (since: number | null) =>
        store.transaction("read", (tx) =>
          changedUtxosIn(tx, store.dialect, filter, since),
        );
      const sinceFirst = await read(first.slot);
      if (sinceFirst.kind !== "ok") throw new Error(sinceFirst.kind);
      // The row the second block created, and the first block's row the
      // third spent (spent rows included); never the untouched one.
      const rows = sinceFirst.utxos.map((u) => ({
        tx: u.outRef.txHash.toString("hex"),
        index: u.outRef.index,
        spentSlot: u.spent?.slot ?? null,
      }));
      expect(rows).toHaveLength(2);
      expect(rows).toEqual(
        expect.arrayContaining([
          { tx: first.txHash.toString("hex"), index: 0, spentSlot: third.slot },
          { tx: second.txHash.toString("hex"), index: 0, spentSlot: null },
        ]),
      );
      expect(await read(third.slot)).toEqual({ kind: "ok", utxos: [] });
      // Null reads the filter's whole retained history, spent rows included.
      const all = await read(null);
      if (all.kind !== "ok") throw new Error(all.kind);
      expect(all.utxos).toHaveLength(3);
      expect(await read(SIM_ORIGIN.point.slot - 1)).toMatchObject({
        kind: "point_beyond_retention",
      });
    });

    it("finds the live predecessor of a key by asset name, never a spent or larger one", async () => {
      const { store, forward } = await open(100);
      const policyId = Buffer.from(policy, "hex");
      // A name above every key under the prefix is never a predecessor.
      const above = Buffer.from("MRETzz", "utf8");
      const made = await forward([node(keyed(0x10)), node(keyed(0x30))]);
      await forward([node(keyed(0x20)), node(above)]);
      const before = (byte: number) =>
        store.transaction("read", (tx) =>
          liveUnitBeforeIn(tx, store.dialect, {
            policyId,
            from: PREFIX,
            below: keyed(byte),
          }),
        );
      const nameOf = (found: Awaited<ReturnType<typeof before>>) =>
        found === null
          ? null
          : [...found.output.assets.get(policy)!.keys()][0]!;
      expect(nameOf(await before(0x25))).toBe(keyed(0x20).toString("hex"));
      expect(nameOf(await before(0x40))).toBe(keyed(0x30).toString("hex"));
      expect(await before(0x05)).toBeNull();
      // Spending 0x10 leaves no predecessor of 0x15.
      await forward([], [{ txHash: made.txHash, index: 0 }]);
      expect(await before(0x15)).toBeNull();
    });
  },
);
