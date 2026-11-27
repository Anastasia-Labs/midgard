import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekDirectBuiltin,
  type MidgardCekDirectValueWitness,
} from "../src/cek-builtin.js";
import { midgardCekConstantWitnessFromUplc } from "../src/cek-constant.js";
import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { constT, UPLCConst } from "../src/uplc-constant.js";
import {
  Application,
  Lambda,
  UPLCProgram,
  type UPLCTerm,
} from "../src/uplc-term.js";
import { builtin, type BuiltinName } from "./helpers/uplc-builtins.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

// A V3 script returns Unit, so the builtin's value is discarded for Unit.
const run = (term: UPLCTerm) => {
  const program = buildCekProgramFromRawFlat(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram(
        [1, 1, 0],
        new Application(new Lambda(UPLCConst.unit), term),
      ),
    ),
  );
  return executeMidgardCekStructuralProgram({
    root: program.termRoot,
    material: program.material.values(),
    constantWitnesses: program.constantWitnesses,
    maxSteps: 64,
  });
};

const apply = (name: BuiltinName, ...arguments_: UPLCConst[]): UPLCTerm =>
  arguments_.reduce<UPLCTerm>(
    (term, argument) => new Application(term, argument),
    builtin(name),
  );

const direct = (constant: UPLCConst): MidgardCekDirectValueWitness => ({
  kind: "constant",
  witness: midgardCekConstantWitnessFromUplc(constant),
});

// The direct failure arm proves only the failures in L1's known set; any
// other failure of a well-typed application is another arm's.
describe("V1 direct builtin failure arm", () => {
  it("still proves a failure in the known set", () => {
    const execution = run(
      apply("divideInteger", UPLCConst.int(1), UPLCConst.int(0)),
    );
    const step = execution.steps.at(-1)!;
    expect(execution.terminalState.mode).toBe("haltError");
    expect(step.witness.kind).toBe("executeBuiltinFailure");
    expect(verifyMidgardCekCoreStep(step.pre, step.post, step.witness)).toBe(
      true,
    );
  });

  it.each([-1n, 1n << 64n, 1n << 70n])(
    "refuses a constrData failure outside the known set (index %s)",
    (index) => {
      const list = UPLCConst.listOf(constT.data)([]);
      const execution = run(apply("constrData", UPLCConst.int(index), list));
      const step = execution.steps.at(-1)!;
      expect(execution.terminalState.mode).toBe("haltError");
      // The honest step proves it through the semantic failure arm.
      expect(step.witness.kind).toBe("executeBuiltinSemanticFailure");
      expect(verifyMidgardCekCoreStep(step.pre, step.post, step.witness)).toBe(
        true,
      );

      // The same application presented to the direct failure arm, with the
      // successor that arm would charge: nothing, as the semantic arm did.
      const arguments_ = [direct(UPLCConst.int(index)), direct(list)];
      const evaluated = evaluateMidgardCekDirectBuiltin(37n, arguments_);
      expect(evaluated.kind).toBe("failure");
      expect(evaluated.budget).toEqual({ cpu: 0n, memory: 0n });
      expect(step.post.cpu).toBe(step.pre.cpu);
      expect(step.post.memory).toBe(step.pre.memory);
      expect(
        verifyMidgardCekCoreStep(step.pre, step.post, {
          kind: "executeBuiltinFailure",
          tag: 37n,
          arguments: arguments_,
        }),
      ).toBe(false);
    },
  );
});
