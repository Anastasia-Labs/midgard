import { performance } from "node:perf_hooks";

import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { addressCredentials } from "../src/decode/output.js";
import {
  type FactStore,
  livePlainOutputsByDatumIn,
  livePlainOutputsExceptDatumsIn,
  openPostgresFactStore,
  openSqliteFactStore,
  type OutputSummary,
  type OutRef,
  type StoredOutput,
} from "../src/index.js";
import {
  addressText,
  L1FollowerProvider,
  L1ProviderScopeError,
} from "../src/provider/index.js";
import { insertOutputs, type OutputPlacement } from "../src/store/rows.js";
import { SIM_ORIGIN, simUniverse } from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const universe = simUniverse();
const ADDRESS = universe.trackedAddress;
const OTHER = universe.untrackedAddress;
const POLICY = universe.trackedPolicy;

/** A 32-byte Plutus bytestring datum: `58 20 <32 x byte>`. */
const datumOf = (byte: number): Buffer =>
  Buffer.concat([Buffer.of(0x58, 0x20), Buffer.alloc(32, byte)]);
const A = datumOf(0xa1);
const B = datumOf(0xb2);
const ABSENT = datumOf(0xc3);

const outRefAt = (n: number): OutRef => {
  const txHash = Buffer.alloc(32, 0x31);
  txHash.writeUInt32BE(n, 28);
  return { txHash, index: n % 3 };
};
const label = ({ outRef }: StoredOutput): string =>
  `${outRef.txHash.readUInt32BE(28).toString()}#${outRef.index.toString()}`;

const SEED: OutputPlacement = { kind: "seed", seedSlot: SIM_ORIGIN.point.slot };
const SCRIPT = {
  hash: Buffer.alloc(28, 0x77),
  type: "plutus_v3",
  bytes: Buffer.from("4e4d01000033222220051200120011", "hex"),
} as const;

const output = (
  shape: Partial<OutputSummary> & { address?: Buffer },
): OutputSummary => {
  const address = shape.address ?? ADDRESS;
  const { payment, stake } = addressCredentials(address);
  return {
    address,
    paymentCredential: payment,
    stakeCredential: stake,
    lovelace: 1_000_000n,
    assets: new Map(),
    datumHash: null,
    datum: null,
    scriptRef: null,
    ...shape,
  };
};

/** A dust datum byte: never one of the looked-up datums. */
const dustByte = (n: number): number =>
  [0xa1, 0xb2, 0xc3].includes(n % 251) ? 0x01 : n % 251;

/** Dust at the address: 40,000 outputs no lookup should return. */
const DUST = 40_000;
const dustAt = (n: number): OutputSummary => {
  switch (n % 8) {
    case 0:
    case 1:
      return output({});
    case 2:
    case 3:
      return output({ datum: datumOf(dustByte(n)) });
    case 4:
      // The looked-up bytes, but carrying a token.
      return output({
        datum: A,
        assets: new Map([[POLICY, new Map([["01", 1n]])]]),
      });
    case 5:
      // The looked-up bytes, with a reference script.
      return output({ datum: B, scriptRef: SCRIPT });
    case 6:
      return output({ datumHash: Buffer.alloc(32, 0xa1) });
    default:
      // The looked-up bytes at another address.
      return output({ address: OTHER, datum: A });
  }
};
/** The plain copies, at out-refs spread through the dust. */
const COPIES_OF_A = [31_001, 7_003, 39_999];
const COPY_OF_B = 20_007;

/** Each read of the 40,000-row address must finish within this. */
const READ_BUDGET_MS = 250;

const timed = async <T>(read: () => Promise<T>) => {
  const started = performance.now();
  const value = await read();
  return { value, ms: performance.now() - started };
};

describe.each(["sqlite", "postgres"] as const)(
  "plain-output reads over dust (%s)",
  (dialect) => {
    const open = async () => {
      const options = {
        securityParameter: 4,
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
      const rows = Array.from({ length: DUST }, (_, n) => ({
        outRef: outRefAt(n),
        output: COPIES_OF_A.includes(n)
          ? output({ datum: A })
          : n === COPY_OF_B
            ? output({ datum: B })
            : dustAt(n),
        placement: SEED,
      }));
      await store.transaction("write", (tx) =>
        insertOutputs(tx, store.dialect, rows),
      );
      return store;
    };

    it("returns only the first plain copies of each datum, within the budget", async () => {
      const store = await open();
      const read = (excluded: readonly OutRef[] = []) =>
        store.transaction("read", (tx) =>
          livePlainOutputsByDatumIn(tx, store.dialect, {
            address: ADDRESS,
            requests: [
              { datum: A, limit: 2 },
              { datum: B, limit: 1 },
              { datum: ABSENT, limit: 1 },
              { datum: A, limit: 1 },
            ],
            excluded,
          }),
        );
      await read();
      const { value, ms } = await timed(() => read());
      expect(ms).toBeLessThan(READ_BUDGET_MS);
      expect(value.kind).toBe("ok");
      if (value.kind !== "ok") return;
      expect(value.cursor.point.slot).toBe(SIM_ORIGIN.point.slot);
      expect(value.utxos.map(label)).toEqual(["7003#1", "20007#0", "31001#2"]);
      for (const utxo of value.utxos) {
        expect(utxo.output.assets.size).toBe(0);
        expect(utxo.output.scriptRef).toBeNull();
        expect(utxo.output.address).toEqual(ADDRESS);
      }
      const skipping = await read([outRefAt(7_003)]);
      expect(skipping.kind === "ok" && skipping.utxos.map(label)).toEqual([
        "20007#0",
        "31001#2",
        "39999#0",
      ]);
    });

    it("returns at most a page of plain outputs needed by no datum, within the budget", async () => {
      const store = await open();
      const read = (excluded: readonly OutRef[] = []) =>
        store.transaction("read", (tx) =>
          livePlainOutputsExceptDatumsIn(tx, store.dialect, {
            address: ADDRESS,
            datums: [A, B, A],
            excluded,
            limit: 100,
          }),
        );
      await read();
      const { value, ms } = await timed(() => read());
      expect(ms).toBeLessThan(READ_BUDGET_MS);
      expect(value.kind).toBe("ok");
      if (value.kind !== "ok") return;
      expect(value.utxos).toHaveLength(100);
      for (const utxo of value.utxos) {
        expect(utxo.output.assets.size).toBe(0);
        expect(utxo.output.scriptRef).toBeNull();
        expect(utxo.output.datumHash).toBeNull();
        expect(utxo.output.address).toEqual(ADDRESS);
        expect([A.toString("hex"), B.toString("hex")]).not.toContain(
          utxo.output.datum?.toString("hex"),
        );
      }
      const numbers = value.utxos.map(({ outRef }) =>
        outRef.txHash.readUInt32BE(28),
      );
      // Out-ref order: dust kinds 0-3 of each eight, from the first.
      expect(numbers.slice(0, 6)).toEqual([0, 1, 2, 3, 8, 9]);
      const skipping = await read([outRefAt(0), outRefAt(2)]);
      expect(
        skipping.kind === "ok" &&
          skipping.utxos
            .slice(0, 4)
            .map(({ outRef }) => outRef.txHash.readUInt32BE(28)),
      ).toEqual([1, 3, 8, 9]);
    });

    it("answers through the provider with the snapshot's point, tracked addresses only", async () => {
      const store = await open();
      const provider = new L1FollowerProvider({
        store,
        transport: fakeL1NodeTransport(),
      });
      const address = addressText(ADDRESS);
      const found = await provider.plainOutputsByDatum({
        address,
        requests: [{ datum: A.toString("hex"), limit: 1 }],
        excluded: [
          { txHash: outRefAt(7_003).txHash.toString("hex"), outputIndex: 1 },
        ],
      });
      expect(found.point).toBe(
        `0:${SIM_ORIGIN.point.slot.toString()}:${SIM_ORIGIN.point.hash.toString("hex")}`,
      );
      expect(await provider.factsPoint()).toBe(found.point);
      expect(found.utxos.map((utxo) => utxo.outputIndex)).toEqual([2]);
      expect(found.utxos[0]).toMatchObject({
        address,
        assets: { lovelace: 1_000_000n },
        datum: A.toString("hex"),
      });
      const unneeded = await provider.plainOutputsExceptDatums({
        address,
        datums: [A.toString("hex")],
        limit: 3,
      });
      expect(unneeded.utxos).toHaveLength(3);
      await expect(
        provider.plainOutputsByDatum({
          address: addressText(OTHER),
          requests: [{ datum: A.toString("hex"), limit: 1 }],
        }),
      ).rejects.toBeInstanceOf(L1ProviderScopeError);
    });
  },
);
