/** The wallet inputs a transaction of ours is completed from: the plain
 * coins of the selected wallet alone, and no preset at all for a wallet
 * that offers nothing else, so its own collateral survives. */
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { plainWalletInputs } from "../src/funding/plain-funding-output.js";

const utxo = (index: number, extra: Partial<UTxO> = {}): UTxO => ({
  txHash: "ab".repeat(32),
  outputIndex: index,
  address: "addr_test1vz",
  assets: { lovelace: 10_000_000n },
  ...extra,
});

const walletOf = (utxos: readonly UTxO[]) =>
  ({
    wallet: () => ({ getUtxos: async () => [...utxos] }),
  }) as unknown as LucidEvolution;

describe("plainWalletInputs", () => {
  it("presets the plain coins alone when the wallet also holds an output with a datum, a datum hash, a reference script or a token", async () => {
    const plain = [utxo(0), utxo(1)];
    const others = [
      utxo(2, { datum: "d87980" }),
      utxo(3, { datumHash: "cd".repeat(32) }),
      utxo(4, {
        scriptRef: { type: "PlutusV3", script: "4e4d01000033222220051" },
      }),
      utxo(5, { assets: { lovelace: 2_000_000n, ["ef".repeat(28)]: 1n } }),
    ];
    for (const other of others)
      expect(
        await plainWalletInputs(walletOf([plain[0]!, other, plain[1]!])),
      ).toStrictEqual({ presetWalletInputs: plain });
  });

  it("presets nothing for a wallet that offers plain coins only", async () => {
    expect(await plainWalletInputs(walletOf([utxo(0), utxo(1)]))).toStrictEqual(
      {},
    );
  });
});
