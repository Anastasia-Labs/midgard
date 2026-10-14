import {
  type Data,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";
import { Builtin, type UPLCTerm } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import {
  app,
  data,
  lam,
  prepareMidgardCekProgram,
  runMidgardCekProgram,
  v,
} from "./helpers/cek-uplc-terms.js";

/**
 * A source constant and the script context share a value root when their
 * values are equal. The constant's step still records its own direct witness,
 * and the context keeps its semantic one.
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
      lam(app(Builtin.equalsData, v(0), data(RECORD))),
      RECORD,
    );
    expect(proof.terminalState.mode).toBe("haltSuccess");
    expect(verdict.terminalState.mode).toBe("haltSuccess");
    expect(verdict.stepCount).toBe(proof.steps.length);
    const constantSteps = proof.steps.filter(
      (step) => step.witness.kind === "computeConstant",
    );
    expect(constantSteps).toHaveLength(1);
    expect(
      proof.steps.every((step) =>
        verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });
});
