import {
  type LedgerQuery,
  TransportRequestError,
} from "@al-ft/l1-node-transport";

import type { WalletLedger } from "../follow/wallet-seed.js";
import type { FactStore } from "../store/fact-store.js";
import type { SeedOutput } from "../store/seed.js";
import type { BlockSummary, OutputSummary, Point } from "../types.js";
import { encodeUtxoAnswer, type SimOutput } from "./block-cbor.js";
import { outRefHex, type SimUtxo } from "./sim-chain.js";

const simOutput = (output: OutputSummary): SimOutput => ({
  address: output.address,
  lovelace: output.lovelace,
  ...(output.assets.size === 0 ? {} : { assets: output.assets }),
  ...(output.datum === null ? {} : { datum: output.datum }),
});

/** The UTxO set of a chain: the pre-origin set, then every block, by outref hex. */
export const simUtxoSet = (
  preOrigin: readonly SimUtxo[],
  blocks: readonly BlockSummary[],
): Map<string, SimUtxo> => {
  const utxos = new Map<string, SimUtxo>(
    preOrigin.map((utxo) => [outRefHex(utxo.outRef), utxo]),
  );
  for (const block of blocks)
    for (const tx of block.txs) {
      for (const outRef of tx.isValid ? tx.inputs : tx.collaterals)
        utxos.delete(outRefHex(outRef));
      const created = tx.isValid
        ? tx.outputs.map((output, index) => ({ output, index }))
        : tx.collateralReturn === null
          ? []
          : [{ output: tx.collateralReturn, index: tx.outputs.length }];
      for (const { output, index } of created)
        utxos.set(outRefHex({ txHash: tx.hash, index }), {
          outRef: { txHash: tx.hash, index },
          output: simOutput(output),
        });
    }
  return utxos;
};

/** The outref hex keys of `utxos` at any of `addresses`, sorted. */
export const keysAt = (
  utxos: ReadonlyMap<string, SimUtxo>,
  addresses: readonly Buffer[],
): string[] => {
  const wanted = new Set(addresses.map((a) => a.toString("hex")));
  return [...utxos]
    .flatMap(([key, utxo]) =>
      wanted.has(utxo.output.address.toString("hex")) ? [key] : [],
    )
    .sort();
};

/**
 * The simulated node's LocalStateQuery for the wallet seed: the UTxO set of
 * the current chain, acquirable only at its tip. The runner feeds events in
 * lockstep, so the tip is the cursor.
 */
export const simLedger = (
  origin: Point,
  preOrigin: readonly SimUtxo[],
  canonical: () => readonly BlockSummary[],
): WalletLedger => ({
  withLedgerState: async (at, use) => {
    const blocks = canonical();
    const tip = blocks[blocks.length - 1]?.point ?? origin;
    if (
      at === "tip" ||
      at.kind !== "point" ||
      at.slot !== BigInt(tip.slot) ||
      at.hash !== tip.hash.toString("hex")
    )
      throw new TransportRequestError(
        "acquire_point_not_on_chain",
        "the simulated node acquires only its tip",
      );
    const utxos = simUtxoSet(preOrigin, blocks);
    return await use({
      query: async (query: LedgerQuery) => {
        if (query.query !== "utxo_by_address")
          throw new Error(`the simulated node does not answer ${query.query}`);
        const keys = new Set(
          keysAt(
            utxos,
            query.addresses.map((a) => Buffer.from(a)),
          ),
        );
        return Promise.resolve(
          encodeUtxoAnswer(
            [...utxos].flatMap(([key, utxo]) => (keys.has(key) ? [utxo] : [])),
          ),
        );
      },
    });
  },
});

/**
 * Whether the store's live rows at the wallets are exactly the ledger's
 * UTxOs there (no phantom, none missing); the first difference, or null.
 */
export const walletViewDiffer = async (
  store: FactStore,
  utxos: ReadonlyMap<string, SimUtxo>,
  wallets: readonly Buffer[],
): Promise<string | null> => {
  const expected = keysAt(utxos, wallets);
  const stored = (
    await store.transaction("read", (tx) =>
      tx.query(
        "SELECT tx_hash, output_index, address FROM l1_outputs WHERE spent_slot IS NULL",
      ),
    )
  )
    .filter((row) =>
      wallets.some((w) => w.equals(Buffer.from(row.address as Uint8Array))),
    )
    .map((row) =>
      outRefHex({
        txHash: Buffer.from(row.tx_hash as Uint8Array),
        index: Number(row.output_index),
      }),
    )
    .sort();
  const want = new Set(expected);
  const have = new Set(stored);
  const phantom = stored.find((key) => !want.has(key));
  if (phantom !== undefined)
    return `wallet output ${phantom} is live in the store but not in the ledger`;
  const missing = expected.find((key) => !have.has(key));
  return missing === undefined
    ? null
    : `wallet output ${missing} is in the ledger but not live in the store`;
};

export type SeedRow = Readonly<{ seed: SeedOutput; seedSlot: number }>;

/** The store's seed rows (`created_slot IS NULL`), keyed by outref hex. */
export const readSeedRows = async (
  store: FactStore,
): Promise<Map<string, SeedRow>> => {
  const rows = await store.transaction("read", (tx) =>
    tx.query(
      "SELECT tx_hash, output_index, seed_slot FROM l1_outputs WHERE created_slot IS NULL",
    ),
  );
  const seeds = new Map<string, SeedRow>();
  for (const row of rows) {
    const outRef = {
      txHash: Buffer.from(row.tx_hash as Uint8Array),
      index: Number(row.output_index),
    };
    const stored = await store.output(outRef);
    if (stored === null) throw new Error("a seed row vanished mid-read");
    seeds.set(outRefHex(outRef), {
      seed: { outRef, output: stored.output },
      seedSlot: Number(row.seed_slot),
    });
  }
  return seeds;
};

/** The first difference between the seeded rows and the store's, or null. */
export const seedRowsDiffer = (
  expected: ReadonlyMap<string, SeedRow>,
  actual: ReadonlyMap<string, SeedRow>,
): string | null => {
  for (const [key, row] of expected) {
    const now = actual.get(key);
    if (now === undefined) return `seed row ${key} is gone`;
    if (now.seedSlot !== row.seedSlot)
      return `seed row ${key} moved from seed slot ${row.seedSlot} to ${now.seedSlot}`;
  }
  for (const key of actual.keys())
    if (!expected.has(key)) return `seed row ${key} appeared unseeded`;
  return null;
};
