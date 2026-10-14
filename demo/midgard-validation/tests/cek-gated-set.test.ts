import { describe, expect, it } from "vitest";

import { MIDGARD_CEK_GATED_SET_V1 } from "../src/index.js";
import { readCoverageReport } from "./fuzz/cek-step-fuzz.corpus.js";

/**
 * The admission screen's gated set never names more than the step
 * differential has shown the two machines to agree on: every builtin in it
 * has an agreed executing arm in the checked-in coverage report, every arm
 * in it is an agreed arm, and verifySchnorr stays out until its on-chain
 * equivalence lands.
 */

const VERIFY_SCHNORR = 53;

/** The arms that execute a builtin to a result. */
const EXECUTING_ARMS = new Set([
  "executeBuiltinDirect",
  "executeBuiltinSemantic",
  "startBuiltinMapConversion",
  "executeBuiltinBlsFinal",
]);

describe("CEK gated set v1", () => {
  const { coverage } = readCoverageReport();

  it("names only builtins the coverage report executes", () => {
    const executed = Object.entries(coverage.builtins)
      .filter(([, arms]) => arms.some((arm) => EXECUTING_ARMS.has(arm)))
      .map(([tag]) => Number(tag));
    expect(
      MIDGARD_CEK_GATED_SET_V1.builtins.filter(
        (tag) => !executed.includes(tag),
      ),
    ).toStrictEqual([]);
  });

  it("names only arms the coverage report agrees on", () => {
    expect(
      MIDGARD_CEK_GATED_SET_V1.arms.filter(
        (arm) => !coverage.arms.includes(arm),
      ),
    ).toStrictEqual([]);
  });

  it("leaves verifySchnorr out", () => {
    expect(MIDGARD_CEK_GATED_SET_V1.builtins).not.toContain(VERIFY_SCHNORR);
  });

  it("is not empty", () => {
    expect(MIDGARD_CEK_GATED_SET_V1.builtins.length).toBeGreaterThan(0);
    expect(MIDGARD_CEK_GATED_SET_V1.arms.length).toBeGreaterThan(0);
  });
});
