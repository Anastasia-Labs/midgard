import { describe, expect, it } from "vitest";

import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "../src/cek-executor.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { UPLCConst } from "../src/uplc-constant.js";
import {
  Case,
  Constr,
  ErrorUPLC,
  Lambda,
  UPLCProgram,
  type UPLCTerm,
} from "../src/uplc-term.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

/** `\ctx -> case (constr tag []) [error, .., unit, .., error]`. */
const caseOnConstructor = (tag: number, branches: number): Buffer => {
  const terms: UPLCTerm[] = Array.from({ length: branches }, (_, index) =>
    index === tag ? UPLCConst.unit : new ErrorUPLC(),
  );
  return encodeMidgardCekCardanoFlatProgram(
    new UPLCProgram(
      [1, 1, 0],
      new Lambda(new Case(new Constr(BigInt(tag), []), terms)),
    ),
  );
};

describe("V1 CEK case branch selection", () => {
  it.each([
    { tag: 0, branches: 1 },
    { tag: 0, branches: 2 },
    { tag: 1, branches: 3 },
    { tag: 0, branches: 3 },
  ])(
    "selects constructor $tag of $branches branches and proves every step",
    ({ tag, branches }) => {
      const program = buildCekProgramFromRawFlat(
        caseOnConstructor(tag, branches),
      );
      const graph = buildMidgardCekExecutionGraph(
        program.termRoot,
        program.material.values(),
        Buffer.from("d87980", "hex"),
      );
      const execution = executeMidgardCekStructuralProgram({
        root: graph.root,
        material: graph.material.values(),
        constantWitnesses: graph.constantWitnesses,
        maxSteps: 64,
      });
      expect(execution.terminalState.mode).toBe("haltSuccess");
      expect(
        execution.steps.every((step) =>
          verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
        ),
      ).toBe(true);
    },
  );
});
