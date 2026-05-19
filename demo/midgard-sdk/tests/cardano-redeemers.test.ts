import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  getTxInfoRedeemerIndexes,
  resolveMintPolicyContextIndex,
  resolveMintPolicyTxInfoRedeemerIndexFromPolicySet,
  resolveRedeemerTxInfoIndex,
} from "../src/index.js";

describe("Cardano redeemer ordering helpers", () => {
  it("maps context-order redeemers into Aiken tx.redeemers order by purpose then pointer index", () => {
    const pointers = [
      { tag: CML.RedeemerTag.Mint, index: 1n },
      { tag: CML.RedeemerTag.Spend, index: 2n },
      { tag: CML.RedeemerTag.Cert, index: 0n },
      { tag: CML.RedeemerTag.Spend, index: 0n },
      { tag: CML.RedeemerTag.Mint, index: 0n },
    ];

    expect(getTxInfoRedeemerIndexes(pointers)).toEqual([3, 1, 4, 0, 2]);
    expect(
      resolveRedeemerTxInfoIndex({
        pointers,
        target: { tag: CML.RedeemerTag.Mint, index: 1n },
      }),
    ).toBe(3n);
  });

  it("resolves mint policy context and tx-info indexes from a policy set", () => {
    const policyA = "22".repeat(28);
    const policyB = "00".repeat(28);
    const policyC = "11".repeat(28);

    expect(
      resolveMintPolicyContextIndex({
        policyIds: [policyA, policyB, policyC],
        targetPolicyId: policyC,
      }),
    ).toBe(1n);
    expect(
      resolveMintPolicyTxInfoRedeemerIndexFromPolicySet({
        policyIds: [policyA, policyB, policyC],
        targetPolicyId: policyA,
      }),
    ).toBe(2n);
    expect(
      resolveMintPolicyTxInfoRedeemerIndexFromPolicySet({
        policyIds: [policyA, policyB, policyC],
        targetPolicyId: policyA,
        precedingSpendRedeemerCount: 3,
      }),
    ).toBe(5n);
  });
});
