import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekDirectBuiltin,
  type MidgardCekDirectValueWitness,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.js";
import { type MidgardCekConstantType } from "../src/cek-constant.js";
import { encodeMidgardCekCoreStepDataCbor } from "../src/cek-data.js";
import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import { midgardCekListConstructIsSemantic } from "../src/cek-executor.semantic-presentation.js";
import {
  type MidgardCekCoreStepWitness,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { constT, type ConstType, UPLCConst } from "../src/uplc-constant.js";
import { DataB, DataI } from "../src/uplc-data.js";
import { Application, Force, Lambda, UPLCProgram } from "../src/uplc-term.js";
import { listConstructSemanticWitness } from "./cek-machine.list-construct-witness.js";
import VECTORS from "./fixtures/cek-builtin-step-evidence-vectors.json" with { type: "json" };
import { builtin } from "./helpers/uplc-builtins.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

// mkCons (32) is proved as a semantic step only over a list whose element
// type the split list hop can read. The split roots hop compacts the source
// and the item, so the list hop reads the source's memory from its list node
// and binds the item's summary memory to its memory; it can do both for a
// Data, integer, bytestring, unit, boolean or `pair<data, data>` element. Over
// any other element type the executor emits the direct list-construct witness
// on the revealed values (`midgardCekListConstructIsSemantic`).

const DATA = { kind: "data" } as const;
const INTEGER = { kind: "integer" } as const;

/** The element types of the pinned matrix the Aiken module proves. */
const ROWS: readonly {
  readonly pin: string;
  readonly element: MidgardCekConstantType;
  readonly semantic: boolean;
}[] = [
  { pin: "data", element: DATA, semantic: true },
  { pin: "integer", element: INTEGER, semantic: true },
  { pin: "bytes", element: { kind: "bytes" }, semantic: true },
  { pin: "unit", element: { kind: "unit" }, semantic: true },
  { pin: "bool", element: { kind: "boolean" }, semantic: true },
  { pin: "string", element: { kind: "string" }, semantic: false },
  {
    pin: "data_pair",
    element: { kind: "pair", first: DATA, second: DATA },
    semantic: true,
  },
  {
    pin: "integer_pair",
    element: { kind: "pair", first: INTEGER, second: INTEGER },
    semantic: false,
  },
  {
    pin: "integer_list",
    element: { kind: "list", element: INTEGER },
    semantic: false,
  },
  {
    pin: "data_list_list",
    element: { kind: "list", element: { kind: "list", element: DATA } },
    semantic: false,
  },
];

const describeType = (type: MidgardCekConstantType): string => {
  switch (type.kind) {
    case "bytes":
      return "bytestring";
    case "list":
      return `list<${describeType(type.element)}>`;
    case "pair":
      return `pair<${describeType(type.first)}, ${describeType(type.second)}>`;
    case "integer":
    case "string":
    case "unit":
    case "boolean":
    case "data":
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
      return type.kind;
    case "array":
      return `array<${describeType(type.element)}>`;
    case "value":
      return "value";
  }
};

const constTypeOf = (type: MidgardCekConstantType): ConstType => {
  switch (type.kind) {
    case "integer":
      return constT.int;
    case "bytes":
      return constT.byteStr;
    case "string":
      return constT.str;
    case "unit":
      return constT.unit;
    case "boolean":
      return constT.bool;
    case "data":
      return constT.data;
    case "list":
      return constT.listOf(constTypeOf(type.element));
    case "pair":
      return constT.pairOf(constTypeOf(type.first), constTypeOf(type.second));
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
    case "array":
    case "value":
      throw new Error(`no constant of type ${type.kind}`);
  }
};

/** A value of the type, distinct for each seed. */
const valueOf = (type: MidgardCekConstantType, seed: number): unknown => {
  switch (type.kind) {
    case "integer":
      return BigInt(seed);
    case "bytes":
      return new DataB(Buffer.from([seed])).bytes;
    case "string":
      return `s${seed.toString()}`;
    case "unit":
      return UPLCConst.unit.value;
    case "boolean":
      return seed % 2 === 0;
    case "data":
      return new DataI(BigInt(seed));
    case "list":
      return [valueOf(type.element, seed + 1)];
    case "pair":
      return {
        fst: valueOf(type.first, seed + 1),
        snd: valueOf(type.second, seed + 2),
      };
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
    case "array":
    case "value":
      throw new Error(`no constant of type ${type.kind}`);
  }
};

/** `mkCons item list` over a one-item list, applied under a lambda. */
const mkConsStep = (element: MidgardCekConstantType) => {
  const item = new UPLCConst(
    constTypeOf(element),
    valueOf(element, 1) as never,
  );
  const list = new UPLCConst(constTypeOf({ kind: "list", element }), [
    valueOf(element, 4),
  ] as never);
  const program = buildCekProgramFromRawFlat(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram(
        [1, 1, 0],
        new Application(
          new Lambda(UPLCConst.unit),
          new Application(
            new Application(new Force(builtin("mkCons")), item),
            list,
          ),
        ),
      ),
    ),
  );
  const execution = executeMidgardCekStructuralProgram({
    root: program.termRoot,
    material: program.material.values(),
    constantWitnesses: program.constantWitnesses,
    maxSteps: 64,
  });
  const step = execution.steps.find(
    (candidate) =>
      candidate.witness.kind === "executeBuiltinSemantic" ||
      candidate.witness.kind === "executeBuiltinDirect",
  )!;
  return {
    step,
    allVerify: execution.steps.every((candidate) =>
      verifyMidgardCekCoreStep(
        candidate.pre,
        candidate.post,
        candidate.witness,
      ),
    ),
    halted: execution.terminalState.mode,
    constants: [...program.constantWitnesses.values()],
    evidence: Buffer.from(encodeMidgardCekCoreStepDataCbor(step)).toString(
      "hex",
    ),
  };
};

type BuiltinStep = ReturnType<typeof mkConsStep>["step"];

const compacted = (value: MidgardCekDirectValueWitness) =>
  value.kind === "constant"
    ? semanticizeMidgardCekDirectConstant(value)
    : value;

/**
 * The step's semantic witness as the split chain sees it: every argument and
 * the result compacted to its summary. The machine states are those of the
 * emitted step.
 */
const compactedSemanticWitness = (
  step: BuiltinStep,
): MidgardCekCoreStepWitness => {
  const witness = listConstructSemanticWitness(step.witness);
  return {
    ...witness,
    arguments: witness.arguments.map(compacted),
    result: compacted(witness.result),
  };
};

const splitChainAccepts = (step: BuiltinStep): boolean =>
  verifyMidgardCekCoreStep(step.pre, step.post, compactedSemanticWitness(step));

/**
 * The direct witness of a semantic mkCons step: each summarised argument
 * revealed as the source constant it summarises, and the result the direct
 * evaluator computes from them.
 */
const directWitnessOf = (
  built: ReturnType<typeof mkConsStep>,
): MidgardCekCoreStepWitness => {
  const { witness } = built.step;
  if (witness.kind !== "executeBuiltinSemantic") {
    throw new Error("the step is not a semantic witness");
  }
  const reveal = (
    value: MidgardCekDirectValueWitness,
  ): MidgardCekDirectValueWitness => {
    if (value.kind !== "semanticConstant") return value;
    const key = (summary: MidgardCekDirectValueWitness): string =>
      summary.kind === "semanticConstant"
        ? `${Buffer.from(summary.witness.typeCbor).toString("hex")}:${Buffer.from(summary.witness.payload.root).toString("hex")}`
        : "";
    const source = built.constants.find(
      (constant) =>
        constant.kind === "constant" &&
        key(semanticizeMidgardCekDirectConstant(constant)) === key(value),
    );
    if (source?.kind !== "constant") {
      throw new Error("no source constant has this summary");
    }
    return { kind: "constant", witness: source.witness };
  };
  const arguments_ = witness.arguments.map(reveal);
  const evaluated = evaluateMidgardCekDirectBuiltin(32n, arguments_);
  if (evaluated.kind !== "success") throw new Error("mkCons failed");
  return {
    kind: "executeBuiltinDirect",
    tag: 32n,
    arguments: arguments_,
    result: evaluated.result,
  };
};

/**
 * The pinned mkCons evidence, by label. The vector file is the one source:
 * the Aiken module `cek-direct-list-construct.test.ak` imports the same bytes
 * from the module generated from it, and proves each step.
 */
const pinnedEvidence = (): ReadonlyMap<string, string> =>
  new Map(
    VECTORS.directListConstruct.map(({ name, evidence }) => [name, evidence]),
  );

/**
 * Every element type a list constant can have, to one level of nesting. A
 * BLS value has no constant form, so no list of BLS values exists.
 */
const ELEMENT_TYPES: readonly MidgardCekConstantType[] = (() => {
  const leaves: MidgardCekConstantType[] = [
    INTEGER,
    { kind: "bytes" },
    { kind: "string" },
    { kind: "unit" },
    { kind: "boolean" },
    DATA,
  ];
  return [
    ...leaves,
    ...leaves.map((element) => ({ kind: "list" as const, element })),
    ...leaves.flatMap((first) =>
      leaves.map((second) => ({ kind: "pair" as const, first, second })),
    ),
    { kind: "list", element: { kind: "list", element: DATA } },
  ];
})();

const MATRIX = ROWS.map((row) => ({ ...row, name: describeType(row.element) }));

describe("mkCons emission by element type", () => {
  it.each(MATRIX)(
    "mkCons over a $name list emits its witness and every step verifies",
    ({ element, semantic }) => {
      const built = mkConsStep(element);
      expect(built.step.witness.kind).toBe(
        semantic ? "executeBuiltinSemantic" : "executeBuiltinDirect",
      );
      expect(built.allVerify).toBe(true);
      expect(built.halted).toBe("haltSuccess");
    },
  );

  it.each(MATRIX)(
    "the split chain accepts the compacted semantic witness over a $name list exactly when it is emitted",
    ({ element, semantic }) => {
      expect(splitChainAccepts(mkConsStep(element).step)).toBe(semantic);
    },
  );

  it("each row's evidence is pinned in the vector file", () => {
    const pinned = pinnedEvidence();
    for (const { pin, element, semantic } of ROWS) {
      const { step, evidence } = mkConsStep(element);
      expect(pinned.get(`mk_cons_${pin}`)).toBe(evidence);
      if (!semantic) {
        const compactedEvidence = Buffer.from(
          encodeMidgardCekCoreStepDataCbor({
            ...step,
            witness: compactedSemanticWitness(step),
          }),
        ).toString("hex");
        expect(pinned.get(`mk_cons_${pin}_compacted`)).toBe(compactedEvidence);
      }
    }
  });

  it.each(
    ELEMENT_TYPES.map((element) => ({ element, name: describeType(element) })),
  )(
    "over a $name list the emitted arm is the one the split chain accepts",
    ({ element }) => {
      const built = mkConsStep(element);
      const semantic = midgardCekListConstructIsSemantic(element);
      expect(built.step.witness.kind).toBe(
        semantic ? "executeBuiltinSemantic" : "executeBuiltinDirect",
      );
      expect(built.allVerify).toBe(true);
      expect(splitChainAccepts(built.step)).toBe(semantic);
    },
  );
});

// The step verifier accepts either witness kind for the same mkCons step:
// which witness the executor emits does not narrow what is accepted.
const SEMANTIC_ROWS = MATRIX.filter((row) => row.semantic);

describe("mkCons step acceptance by witness kind", () => {
  it.each(SEMANTIC_ROWS)(
    "over a $name list the semantic and the direct witness of the same step are both accepted",
    ({ element }) => {
      const built = mkConsStep(element);
      const { pre, post, witness } = built.step;
      expect(witness.kind).toBe("executeBuiltinSemantic");
      expect(verifyMidgardCekCoreStep(pre, post, witness)).toBe(true);
      expect(verifyMidgardCekCoreStep(pre, post, directWitnessOf(built))).toBe(
        true,
      );
    },
  );

  it.each(SEMANTIC_ROWS)(
    "over a $name list a semantic witness with another post-state is refused",
    ({ element }) => {
      const { pre, post, witness } = mkConsStep(element).step;
      expect(
        verifyMidgardCekCoreStep(pre, { ...post, cpu: post.cpu + 1n }, witness),
      ).toBe(false);
    },
  );

  it("the integer row's direct witness is pinned in the vector file", () => {
    const built = mkConsStep(INTEGER);
    expect(pinnedEvidence().get("mk_cons_integer_direct")).toBe(
      Buffer.from(
        encodeMidgardCekCoreStepDataCbor({
          ...built.step,
          witness: directWitnessOf(built),
        }),
      ).toString("hex"),
    );
  });
});
