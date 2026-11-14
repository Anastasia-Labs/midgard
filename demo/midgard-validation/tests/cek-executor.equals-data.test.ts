import { describe, expect, it } from "vitest";

import {
  decodeMidgardCekConstantWitness,
  midgardCekConstantWitnessFromUplc,
} from "../src/cek-constant.js";
import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { type Data, DataConstr, DataI } from "../src/uplc-data.js";
import {
  Application,
  Lambda,
  UPLCProgram,
  type UPLCTerm,
} from "../src/uplc-term.js";
import { builtin } from "./helpers/uplc-builtins.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

const compile = (term: UPLCTerm): Buffer =>
  encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], term));

// equalsData compares Data by value. Each row runs
// `[(lam _ (con unit ())) [[equalsData (con data left)] (con data right)]]`
// and reads the builtin step. The wrapper returns unit, so the script halts
// successfully, as a PlutusV3 script must.
const equalsDataStep = (left: Data, right: Data) => {
  const program = buildCekProgramFromRawFlat(
    compile(
      new Application(
        new Lambda(UPLCConst.unit),
        new Application(
          new Application(builtin("equalsData"), UPLCConst.data(left)),
          UPLCConst.data(right),
        ),
      ),
    ),
  );
  const execution = executeMidgardCekStructuralProgram({
    root: program.termRoot,
    material: program.material.values(),
    constantWitnesses: program.constantWitnesses,
    maxSteps: 32,
  });
  expect(execution.terminalState.mode).toBe("haltSuccess");
  expect(
    execution.steps.every((step) =>
      verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
    ),
  ).toBe(true);
  const step = execution.steps.find(
    (candidate) =>
      candidate.witness.kind === "executeBuiltinDirect" ||
      candidate.witness.kind === "executeBuiltinSemantic",
  );
  if (
    step === undefined ||
    (step.witness.kind !== "executeBuiltinDirect" &&
      step.witness.kind !== "executeBuiltinSemantic") ||
    step.witness.tag !== 47n ||
    step.witness.result.kind !== "constant"
  ) {
    throw new Error("equalsData did not return a constant");
  }
  const payload = decodeMidgardCekConstantWitness(
    step.witness.result.witness,
  ).payload;
  if (!(payload instanceof DataConstr)) {
    throw new Error("equalsData did not return a boolean");
  }
  return {
    step,
    equal: payload.constr === 1n,
    cpu: step.post.cpu - step.pre.cpu,
    memory: step.post.memory - step.pre.memory,
  };
};

describe("equalsData by value", () => {
  it("matches an integer with itself and separates -6 from -5 at the same cost", () => {
    const same = equalsDataStep(new DataI(-6n), new DataI(-6n));
    const different = equalsDataStep(new DataI(-6n), new DataI(-5n));
    expect(same.equal).toBe(true);
    expect(different.equal).toBe(false);
    expect(different.cpu).toBe(same.cpu);
    expect(different.memory).toBe(same.memory);
  });

  it("separates -2^64 from -2^64 - 1", () => {
    const boundary = -(1n << 64n);
    expect(
      equalsDataStep(new DataI(boundary), new DataI(boundary - 1n)).equal,
    ).toBe(false);
    expect(equalsDataStep(new DataI(boundary), new DataI(boundary)).equal).toBe(
      true,
    );
  });

  it("refuses a step that claims the opposite verdict", () => {
    for (const [left, right] of [
      [new DataI(-6n), new DataI(-6n)],
      [new DataI(-6n), new DataI(-5n)],
    ] as const) {
      const { step, equal } = equalsDataStep(left, right);
      const flipped = {
        ...step.witness,
        result: {
          kind: "constant" as const,
          witness: midgardCekConstantWitnessFromUplc(UPLCConst.bool(!equal)),
        },
      } as typeof step.witness;
      let accepted: boolean;
      try {
        accepted = verifyMidgardCekCoreStep(step.pre, step.post, flipped);
      } catch {
        accepted = false;
      }
      expect(accepted).toBe(false);
    }
  });
});
