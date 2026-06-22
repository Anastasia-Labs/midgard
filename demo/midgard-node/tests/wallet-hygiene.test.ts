import "./utils.js";

import type { Assets, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  classifyWalletUtxos,
  formatWalletHygieneError,
  isPlainAdaOnlyUtxo,
  selectCollateralCandidate,
  selectPlainAdaFundingUtxos,
} from "@/transactions/wallet-hygiene.js";

const ADDRESS = "addr_test1wallet";

const txHashFixture = (value: string): string => value.padStart(64, "0");

const mkUtxo = ({
  txHash,
  outputIndex = 0,
  assets,
  datum,
  datumHash,
  scriptRef = false,
}: {
  readonly txHash: string;
  readonly outputIndex?: number;
  readonly assets: Assets;
  readonly datum?: string;
  readonly datumHash?: string;
  readonly scriptRef?: boolean;
}): UTxO => ({
  txHash: txHashFixture(txHash),
  outputIndex,
  address: ADDRESS,
  assets,
  ...(datum === undefined ? {} : { datum }),
  ...(datumHash === undefined ? {} : { datumHash }),
  ...(scriptRef
    ? {
        scriptRef: {
          type: "Native" as const,
          script: "8200",
        },
      }
    : {}),
});

describe("wallet hygiene classification", () => {
  it("treats only no-datum/no-script/no-token lovelace outputs as plain ADA-only", () => {
    const plain = mkUtxo({
      txHash: "1",
      assets: { lovelace: 6_000_000n },
    });
    const datum = mkUtxo({
      txHash: "2",
      assets: { lovelace: 4_000_000n },
      datum: "d87980",
    });
    const datumHash = mkUtxo({
      txHash: "3",
      assets: { lovelace: 4_000_000n },
      datumHash: "ab".repeat(32),
    });
    const scriptRef = mkUtxo({
      txHash: "4",
      assets: { lovelace: 4_000_000n },
      scriptRef: true,
    });
    const tokenBearing = mkUtxo({
      txHash: "5",
      assets: { lovelace: 2_000_000n, [`${"a".repeat(56)}01`]: 1n },
    });

    expect(isPlainAdaOnlyUtxo(plain)).toEqual(true);
    expect(isPlainAdaOnlyUtxo(datum)).toEqual(false);
    expect(isPlainAdaOnlyUtxo(datumHash)).toEqual(false);
    expect(isPlainAdaOnlyUtxo(scriptRef)).toEqual(false);
    expect(isPlainAdaOnlyUtxo(tokenBearing)).toEqual(false);

    const report = classifyWalletUtxos({
      role: "operator-main",
      address: ADDRESS,
      utxos: [tokenBearing, scriptRef, datumHash, datum, plain],
      requirements: {
        minPlainAdaLovelace: 10_000_000n,
        minCollateralLovelace: 5_000_000n,
        minPlainAdaUtxoCount: 2,
      },
    });

    expect(report.totalLovelace).toEqual(20_000_000n);
    expect(report.plainAdaOnlyLovelace).toEqual(6_000_000n);
    expect(report.plainAdaOnlyUtxoCount).toEqual(1);
    expect(report.collateralCandidates).toEqual([
      { outRef: `${plain.txHash}#0`, lovelace: 6_000_000n },
    ]);
    expect(report.missing).toEqual({
      plainAdaLovelace: 4_000_000n,
      collateralLovelace: 0n,
      plainAdaUtxoCount: 1,
    });
    expect(report.status).toEqual("needs_funding");
    expect(report.ignoredOutRefs.map(({ reasons }) => reasons)).toEqual([
      ["has_datum"],
      ["has_datum_hash"],
      ["has_script_ref"],
      ["has_non_lovelace_assets"],
    ]);
    expect(formatWalletHygieneError(report)).toContain(
      "missing_plain_lovelace=4000000",
    );
  });

  it("marks stale and reserved plain outrefs as ignored instead of funding candidates", () => {
    const stale = mkUtxo({
      txHash: "10",
      assets: { lovelace: 7_000_000n },
    });
    const hubNonce = mkUtxo({
      txHash: "11",
      assets: { lovelace: 8_000_000n },
    });
    const spendable = mkUtxo({
      txHash: "12",
      assets: { lovelace: 9_000_000n },
    });

    const report = classifyWalletUtxos({
      role: "operator-main",
      address: ADDRESS,
      utxos: [spendable, hubNonce, stale],
      staleOutRefs: new Set([`${stale.txHash}#0`]),
      reservedHubOracleNonceOutRefs: new Set([`${hubNonce.txHash}#0`]),
      requirements: {
        minPlainAdaLovelace: 9_000_000n,
        minCollateralLovelace: 5_000_000n,
        minPlainAdaUtxoCount: 1,
      },
    });

    expect(report.plainAdaOnlyLovelace).toEqual(9_000_000n);
    expect(report.selectedFundingOutRefs).toEqual([`${spendable.txHash}#0`]);
    expect(report.status).toEqual("ready");
    expect(report.ignoredOutRefs).toEqual([
      {
        outRef: `${stale.txHash}#0`,
        lovelace: 7_000_000n,
        reasons: ["stale_out_ref"],
      },
      {
        outRef: `${hubNonce.txHash}#0`,
        lovelace: 8_000_000n,
        reasons: ["reserved_hub_oracle_nonce"],
      },
    ]);
  });

  it("selects only plain ADA funding inputs and the smallest collateral candidate above the floor", () => {
    const small = mkUtxo({
      txHash: "20",
      assets: { lovelace: 3_000_000n },
    });
    const medium = mkUtxo({
      txHash: "21",
      assets: { lovelace: 6_000_000n },
    });
    const large = mkUtxo({
      txHash: "22",
      assets: { lovelace: 10_000_000n },
    });
    const polluted = mkUtxo({
      txHash: "23",
      assets: { lovelace: 100_000_000n, [`${"b".repeat(56)}02`]: 1n },
    });

    expect(
      selectPlainAdaFundingUtxos(
        [small, medium, polluted, large],
        12_000_000n,
      ).map((utxo) => `${utxo.txHash}#0`),
    ).toEqual([`${large.txHash}#0`, `${medium.txHash}#0`]);
    expect(
      selectCollateralCandidate([large, small, medium, polluted], 5_000_000n)
        ?.txHash,
    ).toEqual(medium.txHash);
  });
});
