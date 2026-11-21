import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  credentialToAddress,
  type OutRef as LucidOutRef,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import type { FactStore, OutRef } from "../../src/index.js";
import {
  decodePendingTransactions,
  type PendingTransaction,
} from "../../src/mempool/overlay.js";
import {
  L1FollowerProvider,
  pendingOutsideFacts,
} from "../../src/provider/index.js";
import { encodeSimTx, type SimTx, simTxHash } from "../../src/testing/index.js";
import { testDatabases } from "../support/postgres.js";
import {
  built,
  type BuiltOutput,
  DEVNET_LEDGER,
  fakeLedgerTransport,
  ledgerEntry,
  type OutputSpec,
  providerAdapters,
} from "../support/provider.js";
import { fill, ORIGIN, tx } from "../support/small-chain.js";

/**
 * The follower provider with a pending source: every UTxO answer is the
 * facts with the node's mempool transactions applied in mempool order, and
 * without a source the answers are the facts alone.
 */
const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-provider-pending-"));

const keyAddress = (byte: string): string =>
  credentialToAddress("Custom", { type: "Key", hash: byte.repeat(28) });
const wallet = keyAddress("ab");
const untracked = keyAddress("ee");
const POLICY = "cc".repeat(28);
const UNIT = `${POLICY}01`;
const DATUM = "d8799f4401020304ff";
const OTHER_DATUM = "d87a9f01ff";
const ada = (n: number) => ({ lovelace: BigInt(n) * 1_000_000n });

const at = (byte: number, index = 0): OutRef => ({
  txHash: fill(byte),
  index,
});
const out = (outRef: OutRef, spec: OutputSpec): BuiltOutput =>
  built(outRef, spec);

/** In the facts at the wallet: two coins, a token, three datum copies, one other datum. */
const COIN_0 = out(at(0x10), { address: wallet, assets: ada(5) });
const COIN_1 = out(at(0x11), { address: wallet, assets: ada(6) });
const TOKEN = out(at(0x12), {
  address: wallet,
  assets: { ...ada(2), [UNIT]: 1n },
});
const COPY_0 = out(at(0x20), {
  address: wallet,
  assets: ada(2),
  inlineDatum: DATUM,
});
const COPY_1 = out(at(0x21), {
  address: wallet,
  assets: ada(2),
  inlineDatum: DATUM,
});
const COPY_2 = out(at(0x22), {
  address: wallet,
  assets: ada(2),
  inlineDatum: DATUM,
});
const OTHER = out(at(0x23), {
  address: wallet,
  assets: ada(2),
  inlineDatum: OTHER_DATUM,
});
const STORED = [COIN_0, COIN_1, TOKEN, COPY_0, COPY_1, COPY_2, OTHER];

/** On the ledger only, outside the tracked scope. */
const LEDGER_KEPT = out(at(0x71), { address: untracked, assets: ada(4) });
const LEDGER_SPENT = out(at(0x72), { address: untracked, assets: ada(4) });

const raw = (address: string): Buffer =>
  built(at(0), { address, assets: ada(1) }).summary.address;
const WALLET_RAW = raw(wallet);
const UNTRACKED_RAW = raw(untracked);
const DATUM_RAW = Buffer.from(DATUM, "hex");

/**
 * P1 spends COIN_0, TOKEN and COPY_0, and creates a coin at the wallet, a
 * coin elsewhere, the token at the wallet, a plain copy of the datum at the
 * wallet and a copy that carries a token. P2 chains on P1's wallet coin and
 * spends the ledger-only LEDGER_SPENT.
 */
const P1: SimTx = {
  inputs: [COIN_0.outRef, TOKEN.outRef, COPY_0.outRef],
  outputs: [
    { address: WALLET_RAW, lovelace: 3_000_000n },
    { address: UNTRACKED_RAW, lovelace: 1_000_000n },
    {
      address: WALLET_RAW,
      lovelace: 2_000_000n,
      assets: new Map([[POLICY, new Map([["01", 1n]])]]),
    },
    { address: WALLET_RAW, lovelace: 2_000_000n, datum: DATUM_RAW },
    {
      address: WALLET_RAW,
      lovelace: 2_000_000n,
      datum: DATUM_RAW,
      assets: new Map([[POLICY, new Map([["02", 1n]])]]),
    },
  ],
  nonce: 1,
};
const P1_HASH = simTxHash(P1).toString("hex");
const P2: SimTx = {
  inputs: [{ txHash: simTxHash(P1), index: 0 }, LEDGER_SPENT.outRef],
  outputs: [{ address: WALLET_RAW, lovelace: 2_500_000n }],
  nonce: 2,
};
const P2_HASH = simTxHash(P2).toString("hex");
const MEMPOOL = decodePendingTransactions([encodeSimTx(P1), encodeSimTx(P2)]);

const label = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;
const labels = (utxos: readonly UTxO[]): string[] =>
  utxos.map((utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`);
const lucidRef = (outRef: OutRef): LucidOutRef => ({
  txHash: outRef.txHash.toString("hex"),
  outputIndex: outRef.index,
});
const sorted = (values: readonly string[]): string[] =>
  [...values].sort((left, right) => (left < right ? -1 : 1));

let transport: L1NodeTransport;
beforeAll(async () => {
  transport = await fakeLedgerTransport(scratch, {
    answers: DEVNET_LEDGER.lsq,
    utxos: [LEDGER_KEPT, LEDGER_SPENT].map(ledgerEntry),
  });
});
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await transport?.close();
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

describe.each(providerAdapters(databases, scratch))(
  "L1 follower provider with a pending source ($name)",
  (adapter) => {
    const openStore = async (): Promise<FactStore> => {
      const store = await adapter.open({
        addresses: new Set([WALLET_RAW.toString("hex")]),
        paymentCredentials: new Set(),
        policies: new Set([POLICY]),
      });
      opened.push(store);
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(await store.initialize(ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      expect(
        await store.insertSeedOutputs(
          ORIGIN.point,
          STORED.map(({ outRef, summary }) => ({ outRef, output: summary })),
        ),
      ).toMatchObject({ kind: "seeded" });
      return store;
    };
    const open = async (
      pending?: readonly PendingTransaction[],
    ): Promise<L1FollowerProvider> => {
      const store = await openStore();
      return new L1FollowerProvider({
        store,
        transport,
        ...(pending === undefined ? {} : { pending: async () => pending }),
      });
    };

    it("never lists a coin the mempool spends and lists the mempool's outputs at the address", async () => {
      const provider = await open(MEMPOOL);
      expect(sorted(labels(await provider.getUtxos(wallet)))).toEqual(
        sorted([
          label(COIN_1.outRef),
          label(COPY_1.outRef),
          label(COPY_2.outRef),
          label(OTHER.outRef),
          `${P1_HASH}#2`,
          `${P1_HASH}#3`,
          `${P1_HASH}#4`,
          `${P2_HASH}#0`,
        ]),
      );
      expect(labels(await provider.getUtxosWithUnit(wallet, UNIT))).toEqual([
        `${P1_HASH}#2`,
      ]);
      expect(
        sorted(labels(await provider.getUtxosWithPolicy(wallet, POLICY))),
      ).toEqual(sorted([`${P1_HASH}#2`, `${P1_HASH}#4`]));
      expect(labels([await provider.getUtxoByUnit(UNIT)])).toEqual([
        `${P1_HASH}#2`,
      ]);
    });

    it("answers out-refs from the facts, the ledger and the mempool, a chained pair included", async () => {
      const provider = await open(MEMPOOL);
      const request = [
        COIN_0.outRef,
        `${P2_HASH}#0`,
        COIN_1.outRef,
        `${P1_HASH}#0`,
        LEDGER_SPENT.outRef,
        LEDGER_KEPT.outRef,
        `${P1_HASH}#1`,
        at(0x99),
      ].map((outRef) =>
        typeof outRef === "string"
          ? {
              txHash: outRef.slice(0, 64),
              outputIndex: Number(outRef.slice(65)),
            }
          : lucidRef(outRef),
      );
      const found = await provider.getUtxosByOutRef(request);
      expect(labels(found)).toEqual([
        `${P2_HASH}#0`,
        label(COIN_1.outRef),
        label(LEDGER_KEPT.outRef),
        `${P1_HASH}#1`,
      ]);
      expect(found[0]).toMatchObject({
        address: wallet,
        assets: { lovelace: 2_500_000n },
      });
    });

    it("skips plain outputs the mempool spends and includes the plain ones it creates", async () => {
      const provider = await open(MEMPOOL);
      const copies = sorted([
        label(COPY_1.outRef),
        label(COPY_2.outRef),
        `${P1_HASH}#3`,
      ]);
      const byDatum = await provider.plainOutputsByDatum({
        address: wallet,
        requests: [{ datum: DATUM, limit: 2 }],
      });
      expect(labels(byDatum.utxos)).toEqual(copies.slice(0, 2));
      const all = await provider.plainOutputsByDatum({
        address: wallet,
        requests: [{ datum: DATUM, limit: 10 }],
        excluded: [lucidRef(COPY_2.outRef)],
      });
      expect(labels(all.utxos)).toEqual(
        copies.filter((copy) => copy !== label(COPY_2.outRef)),
      );
      const except = await provider.plainOutputsExceptDatums({
        address: wallet,
        datums: [DATUM],
        limit: 10,
      });
      expect(labels(except.utxos)).toEqual(
        sorted([label(COIN_1.outRef), label(OTHER.outRef), `${P2_HASH}#0`]),
      );
      const limited = await provider.plainOutputsExceptDatums({
        address: wallet,
        datums: [DATUM, OTHER_DATUM],
        limit: 1,
      });
      expect(labels(limited.utxos)).toEqual(
        sorted([label(COIN_1.outRef), `${P2_HASH}#0`]).slice(0, 1),
      );
      // The point names the mempool listing the answer applied.
      expect(byDatum.point).toMatch(/:mempool:[0-9a-f]{64}$/u);
      expect(await provider.factsPoint()).toBe(byDatum.point);
      expect(except.point).toBe(byDatum.point);
    });

    it("answers exactly as without a source when the source is empty or absent", async () => {
      const bare = await open();
      const empty = await open([]);
      for (const provider of [bare, empty]) {
        expect(sorted(labels(await provider.getUtxos(wallet)))).toEqual(
          sorted(STORED.map(({ outRef }) => label(outRef))),
        );
        expect(labels([await provider.getUtxoByUnit(UNIT)])).toEqual([
          label(TOKEN.outRef),
        ]);
        expect(
          labels(
            await provider.getUtxosByOutRef(
              [COIN_0, LEDGER_SPENT, LEDGER_KEPT].map(({ outRef }) =>
                lucidRef(outRef),
              ),
            ),
          ),
        ).toEqual([
          label(COIN_0.outRef),
          label(LEDGER_SPENT.outRef),
          label(LEDGER_KEPT.outRef),
        ]);
        const byDatum = await provider.plainOutputsByDatum({
          address: wallet,
          requests: [{ datum: DATUM, limit: 2 }],
        });
        expect(labels(byDatum.utxos)).toEqual([
          label(COPY_0.outRef),
          label(COPY_1.outRef),
        ]);
        expect(byDatum.point).not.toContain("mempool");
        expect(await provider.factsPoint()).toBe(byDatum.point);
      }
    });

    it("drops a listed transaction the facts hold or whose input they show spent", async () => {
      const store = await openStore();
      // The follower applied P1 and a transaction spending COIN_1.
      const spender = fill(0xd1);
      expect(
        await store.applyBlock({
          point: { slot: 101, hash: fill(0xc1) },
          height: ORIGIN.height + 1,
          parentHash: ORIGIN.point.hash,
          txs: [
            tx(simTxHash(P1), { inputs: P1.inputs }),
            tx(spender, { index: 1, inputs: [COIN_1.outRef] }),
          ],
        }),
      ).toMatchObject({ kind: "applied" });
      const conflicting: SimTx = {
        inputs: [COIN_1.outRef],
        outputs: [{ address: WALLET_RAW, lovelace: 1_000_000n }],
        nonce: 3,
      };
      const unrelated: SimTx = {
        inputs: [COPY_1.outRef],
        outputs: [{ address: WALLET_RAW, lovelace: 1_000_000n }],
        nonce: 4,
      };
      const kept = await pendingOutsideFacts(
        store,
        decodePendingTransactions(
          [P1, conflicting, P2, unrelated].map(encodeSimTx),
        ),
      );
      expect(kept.map(({ txHash }) => txHash)).toEqual([
        P2_HASH,
        simTxHash(unrelated).toString("hex"),
      ]);
    });
  },
);
