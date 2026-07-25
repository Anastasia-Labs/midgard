import type { MidgardValue } from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  isZeroValueDelta,
  mintToValueDelta,
  valuePreservationDelta,
} from "../src/value-accounting.js";

const policyId = "11".repeat(28);
const assetName = "abcd";

const value = (lovelace: bigint, quantity: bigint): MidgardValue => ({
  lovelace,
  assets:
    quantity === 0n
      ? new Map()
      : new Map([[policyId, new Map([[assetName, quantity]])]]),
});

describe("mint-aware value accounting", () => {
  it("preserves positive mint quantities in the dormant profile-independent equation", () => {
    const mintDelta = mintToValueDelta({
      assets: [
        {
          policyId: Buffer.from(policyId, "hex"),
          assetName: Buffer.from(assetName, "hex"),
          quantity: 3n,
        },
      ],
    });

    expect(
      isZeroValueDelta(
        valuePreservationDelta(value(5n, 2n), 1n, mintDelta, value(4n, 5n)),
      ),
    ).toBe(true);
  });

  it("preserves negative burn quantities in the dormant profile-independent equation", () => {
    const mintDelta = mintToValueDelta({
      assets: [
        {
          policyId: Buffer.from(policyId, "hex"),
          assetName: Buffer.from(assetName, "hex"),
          quantity: -2n,
        },
      ],
    });

    expect(
      isZeroValueDelta(
        valuePreservationDelta(value(5n, 5n), 1n, mintDelta, value(4n, 3n)),
      ),
    ).toBe(true);
  });
});
