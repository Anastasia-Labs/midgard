import { describe, expect, it } from "vitest";

import { transitionOutputIsWhole } from "../src/transition-trace/output-phases.js";
import { registerTransitionTraceFinalCases } from "./support/transition-trace-final-cases.js";

const enterprise = `581d60${"aa".repeat(28)}`;
const base = `583900${"ab".repeat(56)}`;
const policy = `581c${"44".repeat(28)}`;

// Each output is inserted at the deepest MPF path a ledger allows, so its
// replay pays the dearest insert. The runner holds every transaction to the
// per-transaction budget, and a step that takes the other path than the
// yield's choice is refused, so each case proves its path and its fit.
const OUTPUTS = [
  {
    label: "the longest output replayed whole",
    outputCbor: `a200${base}01821bffffffffffffffffa0`,
    bytes: 73,
    whole: true,
  },
  {
    label: "one asset at the bound",
    outputCbor: `a200${enterprise}01821818a1${policy}a1410118a0`,
    bytes: 73,
    whole: false,
  },
  {
    label: "one asset past the bound",
    outputCbor: `a200${enterprise}01821a001e8480a1${policy}a14001`,
    bytes: 74,
    whole: false,
  },
  {
    label: "an inline datum at the bound",
    outputCbor: `a300${enterprise}018200a0025821581f${"cd".repeat(31)}`,
    bytes: 73,
    whole: false,
  },
  {
    label: "an inline datum past the bound",
    outputCbor: `a300${enterprise}018200a00258225820${"cd".repeat(32)}`,
    bytes: 74,
    whole: false,
  },
  {
    label: "a short output whose inline datum lists 79 empty byte strings",
    outputCbor: `a300${enterprise}01821a004c4b40a0025851${`9f${"40".repeat(79)}ff`}`,
    bytes: 125,
    whole: false,
  },
] as const;

describe("accepted-transaction output step routing", () => {
  it.each(OUTPUTS)("routes $label", ({ outputCbor, bytes, whole }) => {
    expect(outputCbor.length / 2).toBe(bytes);
    expect(transitionOutputIsWhole(outputCbor)).toBe(whole);
  });
});

registerTransitionTraceFinalCases(
  OUTPUTS.map(({ outputCbor }) => ({
    assetCount: 0,
    outputCount: 1,
    ledgerDepth: 64,
    outputCbor,
  })),
  "output-step",
);
