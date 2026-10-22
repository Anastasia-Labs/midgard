import { DataI } from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  constT,
  Force,
  Lambda,
  UPLCConst,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  hashMidgardCekDirectValueWitness,
  type MidgardCekDirectValueWitness,
} from "../src/cek-builtin.js";
import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "../src/cek-executor.js";
import {
  type MidgardCekCoreStepWitness,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

type BuiltinWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinDirect" | "executeBuiltinSemantic" }
>;

// A branch too wide to be revealed twice within the direct payload bound.
const wide = UPLCConst.byteString(new Uint8Array(5_000).fill(7));
const narrow = (byte: number) => UPLCConst.byteString(new Uint8Array([byte]));

const apply = (head: UPLCTerm, ...arguments_: UPLCTerm[]): UPLCTerm =>
  arguments_.reduce<UPLCTerm>(
    (term, argument) => new Application(term, argument),
    head,
  );

const builtinStep = (term: UPLCTerm, tag: bigint) => {
  const program = buildCekProgramFromRawFlat(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram(
        [1, 1, 0],
        new Lambda(new Application(new Lambda(UPLCConst.unit), term)),
      ),
    ),
  );
  const graph = buildMidgardCekExecutionGraph(
    program.termRoot,
    program.material.values(),
    encodeMidgardCekPlutusData(new DataI(0n)),
  );
  const trace = executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: 64,
  });
  expect(
    trace.steps.every((step) =>
      verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
    ),
  ).toBe(true);
  const step = trace.steps.find(
    (candidate) =>
      (candidate.witness.kind === "executeBuiltinDirect" ||
        candidate.witness.kind === "executeBuiltinSemantic") &&
      candidate.witness.tag === tag,
  );
  if (
    step === undefined ||
    (step.witness.kind !== "executeBuiltinDirect" &&
      step.witness.kind !== "executeBuiltinSemantic")
  ) {
    throw new Error("the program has no step for the builtin");
  }
  return { pre: step.pre, post: step.post, witness: step.witness };
};

const refuses = (
  step: ReturnType<typeof builtinStep>,
  witness: BuiltinWitness,
): boolean => {
  try {
    return !verifyMidgardCekCoreStep(step.pre, step.post, witness);
  } catch {
    return true;
  }
};

const forged = (
  value: MidgardCekDirectValueWitness,
): MidgardCekDirectValueWitness => {
  const root = Buffer.from(hashMidgardCekDirectValueWitness(value));
  root[0] = root[0]! ^ 1;
  return { kind: "opaque", root };
};

const opaqueOf = (
  value: MidgardCekDirectValueWitness,
): MidgardCekDirectValueWitness => ({
  kind: "opaque",
  root: hashMidgardCekDirectValueWitness(value),
});

const cases = [
  {
    name: "ifThenElse",
    tag: 26n,
    term: apply(
      new Force(Builtin.ifThenElse),
      UPLCConst.bool(true),
      wide,
      narrow(1),
    ),
    selected: 1,
  },
  {
    name: "chooseUnit",
    tag: 27n,
    term: apply(new Force(Builtin.chooseUnit), UPLCConst.unit, wide),
    selected: 1,
  },
  {
    name: "trace",
    tag: 28n,
    term: apply(new Force(Builtin.trace), UPLCConst.str("m"), wide),
    selected: 1,
  },
  {
    name: "chooseList",
    tag: 31n,
    term: apply(
      new Force(new Force(Builtin.chooseList)),
      UPLCConst.listOf(constT.int)([]),
      wide,
      narrow(1),
    ),
    selected: 1,
  },
  {
    name: "chooseData",
    tag: 36n,
    term: apply(
      new Force(Builtin.chooseData),
      UPLCConst.data(new DataI(1n)),
      narrow(1),
      narrow(2),
      narrow(3),
      wide,
      narrow(5),
    ),
    selected: 4,
  },
] as const;

describe("CEK builtins that pass a branch through", () => {
  for (const { name, tag, term, selected } of cases) {
    it(`${name} presents every branch and its result by value root`, () => {
      const step = builtinStep(term, tag);
      const { arguments: arguments_, result } = step.witness;
      expect(arguments_[0]!.kind).not.toBe("opaque");
      expect(arguments_.slice(1).map((argument) => argument.kind)).toEqual(
        arguments_.slice(1).map(() => "opaque"),
      );
      expect(result).toEqual(arguments_[selected]);
    });

    it(`${name} refuses a forged branch root and an unselected result`, () => {
      const step = builtinStep(term, tag);
      const { arguments: arguments_ } = step.witness;
      for (let index = 1; index < arguments_.length; index += 1) {
        const changed = arguments_.map((argument, at) =>
          at === index ? forged(argument) : argument,
        );
        expect(refuses(step, { ...step.witness, arguments: changed })).toBe(
          true,
        );
      }
      expect(
        refuses(step, { ...step.witness, result: forged(step.witness.result) }),
      ).toBe(true);
      for (let index = 0; index < arguments_.length; index += 1) {
        if (index === selected) continue;
        expect(
          refuses(step, {
            ...step.witness,
            result: opaqueOf(arguments_[index]!),
          }),
        ).toBe(true);
      }
    });
  }
});
