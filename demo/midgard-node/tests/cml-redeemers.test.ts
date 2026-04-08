import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import { getTxInfoRedeemerIndexes } from "@/cml-redeemers.js";

describe("cml redeemer pointer ordering", () => {
  it("maps context-order redeemers into tx-info order by purpose then pointer index", () => {
    const pointers = [
      { tag: CML.RedeemerTag.Mint, index: 1n },
      { tag: CML.RedeemerTag.Spend, index: 2n },
      { tag: CML.RedeemerTag.Cert, index: 0n },
      { tag: CML.RedeemerTag.Spend, index: 0n },
      { tag: CML.RedeemerTag.Mint, index: 0n },
    ];

    expect(getTxInfoRedeemerIndexes(pointers)).toEqual([3, 1, 4, 0, 2]);
  });

  it("preserves context order when pointers share the same tag and index", () => {
    const pointers = [
      { tag: CML.RedeemerTag.Spend, index: 0n },
      { tag: CML.RedeemerTag.Spend, index: 0n },
      { tag: CML.RedeemerTag.Mint, index: 0n },
    ];

    expect(getTxInfoRedeemerIndexes(pointers)).toEqual([0, 1, 2]);
  });
});
