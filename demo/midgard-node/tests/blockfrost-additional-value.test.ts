import { describe, expect, it } from "vitest";
import { toBlockfrostAdditionalValue } from "@/services/lucid.js";

describe("toBlockfrostAdditionalValue", () => {
  it("serializes non-ADA tokens using Ogmios-style policy maps", () => {
    const policyA =
      "565e23bcb46916d008be00384a247c1f06e98ed8b51307605012ec2e";
    const policyB =
      "0123456789abcdef0123456789abcdef0123456789abcdef01234567";
    const assetA = "4e6f6465";
    const assetB = "4e6f646532";

    const value = toBlockfrostAdditionalValue({
      lovelace: 2_000_000n,
      [`${policyA}${assetA}`]: 1n,
      [`${policyA}${assetB}`]: 3n,
      [`${policyB}`]: 7n,
    });

    expect(value).toEqual({
      coins: 2_000_000,
      [policyA]: {
        [assetA]: 1,
        [assetB]: 3,
      },
      [policyB]: {
        "": 7,
      },
    });
    expect("assets" in value).toBe(false);
  });
});
