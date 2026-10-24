import { describe, expect, it } from "vitest";

import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { type Data, DataConstr, DataI, DataList } from "../src/uplc-data.js";
import { type UPLCTerm } from "../src/uplc-term.js";
import {
  app,
  data,
  discard,
  lam,
  prepareMidgardCekProgram,
  runMidgardCekProgram,
  v,
} from "./helpers/cek-uplc-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * A source constant and the script context share a value root when their
 * values are equal. Each step names that one root: the constant's
 * `computeConstant` and the context's `computeContextConstant`.
 */

const RECORD: Data = new DataConstr(0n, [
  new DataI(7n),
  new DataList([new DataI(1n), new DataI(2n)]),
]);

const runBothModes = (script: UPLCTerm, context: Data) => {
  const prepared = prepareMidgardCekProgram(script, context);
  return {
    proof: runMidgardCekProgram(prepared, "proof"),
    verdict: runMidgardCekProgram(prepared, "verdict"),
  };
};

describe("CEK constant witnesses of equal values", () => {
  it("runs a source constant equal to the script context", () => {
    const { proof, verdict } = runBothModes(
      lam(discard(app(builtin("equalsData"), v(0), data(RECORD)))),
      RECORD,
    );
    expect(proof.terminalState.mode).toBe("haltSuccess");
    expect(verdict.terminalState.mode).toBe("haltSuccess");
    expect(verdict.stepCount).toBe(proof.steps.length);
    const contextRoots = proof.steps.flatMap((step) =>
      step.witness.kind === "computeContextConstant"
        ? [Buffer.from(step.witness.valueRoot)]
        : [],
    );
    expect(contextRoots).toHaveLength(1);
    // The script's own Unit result is the other source constant.
    const constantSteps = proof.steps.filter(
      (step) =>
        step.witness.kind === "computeConstant" &&
        contextRoots[0]!.equals(step.witness.valueRoot),
    );
    expect(constantSteps).toHaveLength(1);
    expect(
      proof.steps.every((step) =>
        verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });
});
