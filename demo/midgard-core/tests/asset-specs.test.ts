import {
  parseAdditionalAssetSpec,
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "@al-ft/midgard-core/asset-specs";
import { describe, expect, it } from "vitest";

describe("asset and lovelace specs", () => {
  it("parses lovelace amounts", () => {
    expect(
      parseLovelaceAmount(
        "1",
        "Deposit lovelace amount must be greater than zero.",
      ),
    ).toEqual(1n);
    expect(
      parseLovelaceAmount(
        "1500000",
        "Deposit lovelace amount must be greater than zero.",
      ),
    ).toEqual(1_500_000n);
  });

  it("rejects malformed lovelace amounts", () => {
    expect(() =>
      parseLovelaceAmount(
        "0",
        "Deposit lovelace amount must be greater than zero.",
      ),
    ).toThrow("Deposit lovelace amount must be greater than zero.");
    expect(() =>
      parseLovelaceAmount(
        "1.0000001",
        "Deposit lovelace amount must be greater than zero.",
      ),
    ).toThrow(
      'Invalid lovelace amount "1.0000001". Use a positive integer number of lovelace.',
    );
  });

  it("parses additional asset specs", () => {
    expect(
      parseAdditionalAssetSpec(`${"ab".repeat(28)}.${"cd".repeat(4)}:42`),
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
});
