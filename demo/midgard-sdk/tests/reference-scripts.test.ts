import type { Assets, LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertReferenceScriptRawBodiesFitL1EnvelopeV1,
  createReferenceScriptAuthPolicy,
  hasReferenceScriptAuthRole,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
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
  it("rejects a reference script whose raw body alone cannot fit the L1 transaction envelope", () => {
    expect(() =>
      assertReferenceScriptRawBodiesFitL1EnvelopeV1([
        {
          name: "availability-challenge minting",
          script: {
            type: "PlutusV3",
            script: "00".repeat(20_017),
          },
        },
      ]),
    ).toThrow(
      /availability-challenge minting raw script is 20017 bytes, exceeding the 16384-byte L1 transaction envelope by at least 3633 bytes/u,
    );
  });

  it("admits only the raw-body lower bound and leaves complete signed fit to the publisher", () => {
    expect(() =>
      assertReferenceScriptRawBodiesFitL1EnvelopeV1([
        {
          name: "boundary",
          script: { type: "PlutusV3", script: "00".repeat(16_383) },
        },
      ]),
    ).not.toThrow();
    expect(() =>
      assertReferenceScriptRawBodiesFitL1EnvelopeV1([
        {
          name: "exact-envelope",
          script: { type: "PlutusV3", script: "00".repeat(16_384) },
        },
      ]),
    ).toThrow(/exact-envelope raw script is 16384 bytes/u);
  });

  it("assigns unique <=32-byte auth tokens to every registered fraud-proof role", () => {
    const fraudProofEntries = Object.entries(
      REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    ).filter(([role]) => role.startsWith("V1 fraud-proof "));
    const tokenNames = fraudProofEntries.map(([, tokenName]) => tokenName);

    expect(fraudProofEntries).toHaveLength(116);
    expect(new Set(tokenNames).size).toBe(tokenNames.length);
    expect(tokenNames.every((name) => Buffer.byteLength(name) <= 32)).toBe(
      true,
    );
    expect(REFERENCE_SCRIPT_AUTH_TOKEN_NAMES).toMatchObject({
      "V1 fraud-proof transition-trace route": "V1FpTransitionTraceRoute",
      "V1 fraud-proof transition-trace final-0": "V1FpTransitionTraceFinal0",
      "V1 fraud-proof transition-trace final-7": "V1FpTransitionTraceFinal7",
      "V1 fraud-proof missing-native-script-tx step-06":
        "V1FpMissingNativeScriptTxS06",
      "V1 fraud-proof missing-native-script-tx step-07":
        "V1FpMissingNativeScriptTxS07",
      "V1 fraud-proof missing-native-script-tx step-08":
        "V1FpMissingNativeScriptTxS08",
      "V1 fraud-proof withdrawn-input step-03": "V1FpWithdrawnInputS03",
      "V1 fraud-proof value-not-preserved step-04": "V1FpValueNotPreservedS04",
      "V1 fraud-proof input-set-uniqueness step-02":
        "V1FpInputSetUniquenessS02",
      "V1 fraud-proof mint-authorization step-05": "V1FpMintAuthorizationS05",
    });
  });

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
