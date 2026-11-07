import { describe, expect, it } from "vitest";

import { outputDatumCborMatches } from "../src/tx-output-utils.js";

describe("output datum matching", () => {
  it("distinguishes ordered Plutus maps used by native-asset payouts", () => {
    const alphaThenBeta = "a245616c70686105446265746107";
    const betaThenAlpha = "a244626574610745616c70686105";
    expect(
      outputDatumCborMatches({ datum: alphaThenBeta }, betaThenAlpha),
    ).toBe(false);
    expect(
      outputDatumCborMatches({ datum: alphaThenBeta }, alphaThenBeta),
    ).toBe(true);
  });

  it("accepts equivalent CBOR encodings while preserving nested map order", () => {
    const definite = "d87981a245616c70686105446265746107";
    const indefinite = "d8799fbf45616c7068611805446265746107ffff";
    expect(outputDatumCborMatches({ datum: definite }, indefinite)).toBe(true);
    expect(outputDatumCborMatches({}, definite)).toBe(false);
  });
});
