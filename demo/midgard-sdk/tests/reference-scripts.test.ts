import type { Assets, LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createReferenceScriptAuthPolicy,
  hasReferenceScriptAuthRole,
  referenceScriptAuthPolicyDeploymentInfo,
  referenceScriptAuthPolicyFromDeploymentInfo,
  referenceScriptAuthTokenName,
  referenceScriptAuthUnit,
  referenceScriptPublicationFundingTarget,
  referenceScriptRoleAssets,
  SCRIPT_REF_OUTPUT_LOVELACE,
  SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE,
  selectReferenceScriptFundingUtxos,
} from "../src/reference-scripts.js";

const address = "addr_test1reference";

const utxo = ({
  txHash,
  assets,
  outputIndex = 0,
  scriptRef,
  datum,
}: {
  readonly txHash: string;
  readonly assets: Assets;
  readonly outputIndex?: number;
  readonly scriptRef?: UTxO["scriptRef"];
  readonly datum?: string;
}): UTxO => ({
  txHash: txHash.padStart(64, "0"),
  outputIndex,
  address,
  assets,
  ...(scriptRef === undefined ? {} : { scriptRef }),
  ...(datum === undefined ? {} : { datum }),
});

describe("reference-script SDK boundary", () => {
  it("creates restorable native auth-policy deployment info", () => {
    const lucid = {
      unixTimeToSlot: (time: number) => Math.floor(time / 1000),
    } as unknown as LucidEvolution;
    const policy = createReferenceScriptAuthPolicy(lucid, 1_000, 10_000);
    const info = referenceScriptAuthPolicyDeploymentInfo(policy);
    const restored = referenceScriptAuthPolicyFromDeploymentInfo(info);

    expect(restored).toMatchObject({
      policyId: policy.policyId,
      mintingScriptCBOR: policy.mintingScriptCBOR,
      expiresAtUnixTime: 11_000,
      timelockDurationMs: 10_000,
    });
  });

  it("derives role-token assets for publication outputs", () => {
    const target = {
      name: "hub-oracle minting",
      script: { type: "Native" as const, script: "8200" },
    };
    const assets = referenceScriptRoleAssets(target, {
      policyId: "aa".repeat(28),
    });

    expect(assets.lovelace).toBe(SCRIPT_REF_OUTPUT_LOVELACE);
    expect(
      assets[referenceScriptAuthUnit("aa".repeat(28), "hub-oracle minting")],
    ).toBe(1n);
    expect(referenceScriptAuthTokenName("hub-oracle minting")).toMatch(
      /^[0-9a-f]+$/,
    );
    expect(
      hasReferenceScriptAuthRole(utxo({ txHash: "1", assets }), target, {
        policyId: "aa".repeat(28),
      }),
    ).toBe(true);
  });

  it("selects only plain ADA funding inputs in deterministic largest-first order", () => {
    const selected = selectReferenceScriptFundingUtxos(
      [
        utxo({
          txHash: "1",
          assets: { lovelace: 20_000_000n, ["bb".repeat(28)]: 1n },
        }),
        utxo({ txHash: "2", assets: { lovelace: 4_000_000n }, datum: "00" }),
        utxo({ txHash: "3", assets: { lovelace: 5_000_000n } }),
        utxo({ txHash: "4", assets: { lovelace: 15_000_000n } }),
      ],
      16_000_000n,
    );

    expect(selected.map(({ txHash }) => txHash.slice(-1))).toEqual(["4", "3"]);
  });

  it("computes publication funding target from output count and buffer", () => {
    expect(referenceScriptPublicationFundingTarget(2)).toBe(
      SCRIPT_REF_OUTPUT_LOVELACE * 3n +
        SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE,
    );
  });
});
