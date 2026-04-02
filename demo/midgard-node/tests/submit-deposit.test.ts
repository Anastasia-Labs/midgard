import { describe, expect, it } from "vitest";
import {
  getProtocolParameters,
  resolveEventInclusionTime,
} from "@al-ft/midgard-sdk";
import {
  parseLovelace,
  parseAdditionalAssetSpec,
  parseAdditionalAssetSpecs,
  parseSubmitDepositConfig,
} from "@/transactions/submit-deposit.js";

const VALID_L2_ADDRESS =
  "addr_test1qrtuk9vwzuyj2ly4fp2e0fdc36xzk7j2n34jeygl9n38yce7qqpmjursw62tm3acwl6z2kw38cxau355ukc9cxqyhk0sjy7d2s";

describe("submit deposit parsing", () => {
  it("keeps the offchain event wait duration aligned with the onchain environment", () => {
    expect(getProtocolParameters("Preprod").event_wait_duration).toEqual(60_000);
    expect(getProtocolParameters("Mainnet").event_wait_duration).toEqual(60_000);
    expect(resolveEventInclusionTime(1_000, "Preprod")).toEqual(60_999);
  });

  it("parses lovelace amounts", () => {
    expect(parseLovelace("1")).toEqual(1n);
    expect(parseLovelace("1500000")).toEqual(1_500_000n);
  });

  it("rejects malformed lovelace amounts", () => {
    expect(() => parseLovelace("0")).toThrow(
      "Deposit lovelace amount must be greater than zero.",
    );
    expect(() => parseLovelace("1.0000001")).toThrow(
      'Invalid lovelace amount "1.0000001".',
    );
  });

  it("parses additional asset specs", () => {
    expect(
      parseAdditionalAssetSpec(
        `${"ab".repeat(28)}.${"cd".repeat(4)}:42`,
      ),
    ).toEqual({
      unit: `${"ab".repeat(28)}${"cd".repeat(4)}`,
      amount: 42n,
    });
  });

  it("rejects duplicate additional assets", () => {
    const duplicateUnitSpec = `${"ab".repeat(28)}.${"cd".repeat(4)}:1`;
    expect(() =>
      parseAdditionalAssetSpecs([duplicateUnitSpec, duplicateUnitSpec]),
    ).toThrow("Duplicate additional asset");
  });

  it("parses full submit-deposit command config", () => {
    expect(
      parseSubmitDepositConfig({
        l2Address: VALID_L2_ADDRESS,
        l2Datum: "deadbeef",
        lovelace: "12345678",
        assetSpecs: [`${"11".repeat(28)}.${"22".repeat(2)}:7`],
      }),
    ).toEqual({
      l2Address: VALID_L2_ADDRESS,
      l2Datum: "deadbeef",
      lovelace: 12_345_678n,
      additionalAssets: {
        [`${"11".repeat(28)}${"22".repeat(2)}`]: 7n,
      },
    });
  });
});
