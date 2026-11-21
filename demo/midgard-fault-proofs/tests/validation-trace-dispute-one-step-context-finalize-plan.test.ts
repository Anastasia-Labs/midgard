/** The finalization our runner plans for a CEK context step of the finalize
 * authenticate, from its evidence and the context control: the finalize the
 * authenticate routes the context to, a MidgardV1 execution's to the
 * finalize midgard and a PlutusV3 one's by its purpose, with the redeemers
 * control's items and current redeemer the pending finalization carries;
 * no finalization for a PlutusV3 receive, which no honest trace reaches;
 * and its refusal of evidence of the other purpose's shape. */
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { planValidationTraceContextFinalization } from "../src/validation-trace-dispute/one-step-context-finalize.js";

const items = new Constr<Data>(0, ["aa".repeat(32), 2n, 40n, 7n]);
const redeemer = new Constr<Data>(0, ["bb".repeat(32), 9n, 3n]);
const control = new Constr<Data>(0, [
  2n,
  items,
  "",
  "",
  new Constr(0, ["", 0n, 0n]),
  redeemer,
  2n,
]);
const spendEvidence = new Constr<Data>(20, [
  control,
  0n,
  "cc".repeat(34),
  "dd",
  [],
]);
const finalizeEvidence = new Constr<Data>(19, [control]);

const plan = (evidence: Data, languageTag: bigint, purposeKind: bigint) =>
  planValidationTraceContextFinalization({
    auxiliaryCbor: Data.to(evidence),
    context: new Constr(0, [10n, languageTag, "", purposeKind]),
  });

describe("validation-trace one-step CEK context finalization plan", () => {
  it("routes a Cardano spend to the finalize spend, with the redeemer items and current redeemer of its redeemers control", () => {
    expect(plan(spendEvidence, 3n, 0n)).toStrictEqual({
      holder: "contextFinalizeSpend",
      redeemerItemsCbor: Data.to(items),
      redeemerCbor: Data.to(redeemer),
    });
  });

  it("routes a PlutusV3 mint and required observer to the finalize mint and withdraw, and every MidgardV1 purpose to the finalize midgard", () => {
    const routes: readonly (readonly [bigint, bigint, string])[] = [
      [3n, 1n, "contextFinalizeMint"],
      [3n, 2n, "contextFinalizeWithdraw"],
      [128n, 0n, "contextFinalizeMidgard"],
      [128n, 1n, "contextFinalizeMidgard"],
      [128n, 2n, "contextFinalizeMidgard"],
      [128n, 3n, "contextFinalizeMidgard"],
    ];
    for (const [languageTag, purposeKind, holder] of routes)
      expect(plan(finalizeEvidence, languageTag, purposeKind)).toStrictEqual({
        holder,
        redeemerItemsCbor: Data.to(items),
        redeemerCbor: Data.to(redeemer),
      });
  });

  it("plans no finalization for a PlutusV3 receive, which no honest trace reaches", () => {
    expect(plan(finalizeEvidence, 3n, 3n)).toBeUndefined();
  });

  it("refuses a Cardano spend's evidence without its spent input, and another purpose's with one", () => {
    expect(() => plan(finalizeEvidence, 3n, 0n)).toThrow(/constructor 20/u);
    expect(() => plan(spendEvidence, 3n, 1n)).toThrow(/constructor 19/u);
    expect(() => plan(spendEvidence, 128n, 0n)).toThrow(/constructor 19/u);
  });
});
