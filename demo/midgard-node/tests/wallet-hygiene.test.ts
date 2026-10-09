import "./utils.js";

import type { Assets, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isPlainAdaOnlyUtxo } from "../src/transactions/wallet-hygiene.js";

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
  });
});
