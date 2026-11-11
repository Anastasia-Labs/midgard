/** The wallet inputs a transaction of ours is completed from: the plain
 * coins of the selected wallet outside what it excludes, and no preset at
 * all when that removes nothing, so the wallet's own collateral survives;
 * a wallet with nothing left to select is refused. */
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  plainWalletInputs,
  stepWalletFunding,
} from "../src/funding/plain-funding-output.js";

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

describe("plainWalletInputs with exclusions", () => {
  it("presets the wallet without an excluded coin it holds", async () => {
    const [kept, excluded] = [utxo(0), utxo(1)];
    expect(
      await plainWalletInputs(walletOf([kept!, excluded!]), [excluded!]),
    ).toStrictEqual({ presetWalletInputs: [kept] });
  });

  it("drops a non-plain coin as well as an excluded one", async () => {
    const token = utxo(2, {
      assets: { lovelace: 2_000_000n, ["ef".repeat(28)]: 1n },
    });
    expect(
      await plainWalletInputs(walletOf([utxo(0), utxo(1), token]), [utxo(1)]),
    ).toStrictEqual({ presetWalletInputs: [utxo(0)] });
  });

  it("presets nothing when no excluded coin is in the wallet", async () => {
    expect(
      await plainWalletInputs(walletOf([utxo(0), utxo(1)]), [
        { txHash: "cd".repeat(32), outputIndex: 0 },
        { ...utxo(7) },
      ]),
    ).toStrictEqual({});
    expect(await plainWalletInputs(walletOf([utxo(0)]))).toStrictEqual({});
  });

  it("refuses a wallet whose every coin is excluded or not plain, since an empty preset would bring them all back", async () => {
    const referenceScript = utxo(1, {
      scriptRef: { type: "PlutusV3", script: "4e4d01000033222220051" },
    });
    await expect(
      plainWalletInputs(walletOf([utxo(0), referenceScript]), [utxo(0)]),
    ).rejects.toThrow(/must contain a pure-ADA UTxO outside/);
  });
});

describe("stepWalletFunding", () => {
  it("takes the fee input from the coins coin selection may spend", async () => {
    const excludedLargest = utxo(0, { assets: { lovelace: 90_000_000n } });
    const tokenLarger = utxo(1, {
      assets: { lovelace: 80_000_000n, ["ef".repeat(28)]: 1n },
    });
    const fee = utxo(2, { assets: { lovelace: 20_000_000n } });
    const funding = await stepWalletFunding(
      walletOf([excludedLargest, tokenLarger, fee, utxo(3)]),
      { excluding: [excludedLargest] },
    );
    expect(funding).toStrictEqual({
      feeInput: fee,
      walletInputs: { presetWalletInputs: [fee, utxo(3)] },
    });
  });

  it("presets nothing and picks the largest plain coin from a plain wallet", async () => {
    const fee = utxo(1, { assets: { lovelace: 20_000_000n } });
    expect(await stepWalletFunding(walletOf([utxo(0), fee]))).toStrictEqual({
      feeInput: fee,
      walletInputs: {},
    });
  });
});
