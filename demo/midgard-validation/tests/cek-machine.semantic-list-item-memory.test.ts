import { DataB, DataI } from "@harmoniclabs/plutus-data";
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
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.js";
import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import {
  type MidgardCekCoreStepWitness,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";
import { listSequenceFromNode } from "../src/cek-machine.verify-case-select.js";
import {
  listDataSummary,
  prependListSequence,
} from "../src/cek-machine.verify-semantic-builtin-control.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";

type SemanticWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinSemantic" }
>;

const D = constT.data;

/** `mkCons item items`, applied under a lambda so the step is reached. */
const mkConsStep = (item: UPLCConst, items: UPLCConst) => {
  const term: UPLCTerm = new Application(
    new Application(new Force(Builtin.mkCons), item),
    items,
  );
  const program = buildMidgardCanonicalCekProgram(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram(
        [1, 1, 0],
        new Application(new Lambda(UPLCConst.unit), term),
      ),
    ),
  );
  const execution = executeMidgardCekStructuralProgram({
    root: program.envelope.termRoot,
    material: program.material.values(),
    constantWitnesses: program.constantWitnesses,
    maxSteps: 64,
  });
  const step = execution.steps.find(
    (candidate) => candidate.witness.kind === "executeBuiltinSemantic",
  )!;
  return { step, witness: step.witness as SemanticWitness };
};

const semanticParts = (value: MidgardCekDirectValueWitness) => {
  if (value.kind !== "semanticConstant") {
    throw new Error("expected a semantic constant");
  }
  return value.witness;
};

/**
 * The step with its list item presented by summary, the summary memory moved
 * by `delta`, and the result and post-state derived from that item.
 */
const withPresentedItem = (
  { step, witness }: ReturnType<typeof mkConsStep>,
  delta: bigint,
  source: MidgardCekDirectValueWitness = witness.arguments[1]!,
) => {
  const presented = semanticizeMidgardCekDirectConstant(witness.arguments[0]!);
  const parts = semanticParts(presented);
  const item: MidgardCekDirectValueWitness = {
    ...presented,
    witness: {
      ...parts,
      payload: { ...parts.payload, memory: parts.payload.memory + delta },
    },
  } as MidgardCekDirectValueWitness;
  const resultParts = semanticParts(
    semanticizeMidgardCekDirectConstant(witness.result),
  );
  const sequence = listSequenceFromNode(witness.material.dataNodes[0]!)!;
  const result = {
    kind: "semanticConstant",
    witness: {
      ...resultParts,
      payload: listDataSummary(
        prependListSequence(semanticParts(item).payload, sequence),
      ),
    },
  } as MidgardCekDirectValueWitness;
  const post = {
    ...step.post,
    focusRoot: hashMidgardCekDirectValueWitness(result),
  };
  return {
    accepted: verifyMidgardCekCoreStep(step.pre, post, {
      ...witness,
      arguments: [item, source],
      result,
    }),
    item: parts,
  };
};

/** Element types whose presented item memory is bound, with the words its payload adds. */
const BOUND = [
  {
    element: "data",
    words: 0n,
    step: () =>
      mkConsStep(
        UPLCConst.data(new DataI(0n)),
        UPLCConst.listOf(D)([new DataI(1n), new DataI(2n)]),
      ),
  },
  {
    element: "integer",
    words: 4n,
    step: () =>
      mkConsStep(
        UPLCConst.int(70000n),
        UPLCConst.listOf(constT.int)([1n, 2n] as never),
      ),
  },
  {
    element: "bytes",
    words: 4n,
    step: () =>
      mkConsStep(
        UPLCConst.byteString(Uint8Array.from(Buffer.alloc(9, 0x2a))),
        UPLCConst.listOf(constT.byteStr)([
          new DataB(Buffer.from("01", "hex")).bytes,
        ] as never),
      ),
  },
  {
    element: "pair<data,data>",
    words: 4n,
    step: () =>
      mkConsStep(
        UPLCConst.pairOf(D, D)(
          new DataI(1n),
          new DataB(Buffer.from("02", "hex")),
        ),
        UPLCConst.listOf(constT.pairOf(D, D))([
          { fst: new DataI(3n), snd: new DataI(4n) },
        ] as never),
      ),
  },
  {
    element: "unit",
    words: 3n,
    step: () =>
      mkConsStep(
        UPLCConst.unit,
        UPLCConst.listOf(constT.unit)([UPLCConst.unit.value] as never),
      ),
  },
  {
    element: "boolean",
    words: 3n,
    step: () =>
      mkConsStep(
        UPLCConst.bool(true),
        UPLCConst.listOf(constT.bool)([false] as never),
      ),
  },
] as const;

/** Element types with no such relation: their item must be revealed. */
const REVEALED_ONLY = [
  {
    element: "string",
    step: () =>
      mkConsStep(
        UPLCConst.str("ab"),
        UPLCConst.listOf(constT.str)(["c"] as never),
      ),
  },
  {
    element: "list<integer>",
    step: () =>
      mkConsStep(
        UPLCConst.listOf(constT.int)([1n] as never),
        UPLCConst.listOf(constT.listOf(constT.int))([[2n]] as never),
      ),
  },
  {
    element: "pair<integer,integer>",
    step: () =>
      mkConsStep(
        UPLCConst.pairOf(constT.int, constT.int)(1n, 2n),
        UPLCConst.listOf(constT.pairOf(constT.int, constT.int))([
          { fst: 3n, snd: 4n },
        ] as never),
      ),
  },
] as const;

const ALL = [...BOUND, ...REVEALED_ONLY];

describe("mkCons list item summary memory", () => {
  it.each(BOUND)(
    "the honest $element item's summary memory is its memory plus $words words",
    ({ step, words }) => {
      const { item } = withPresentedItem(step(), 0n);
      expect(item.payload.memory).toBe(item.memory + words);
    },
  );

  it.each(BOUND)(
    "mkCons accepts a presented $element item whose summary memory matches its memory",
    ({ step }) => {
      expect(withPresentedItem(step(), 0n).accepted).toBe(true);
    },
  );

  it.each(
    BOUND.flatMap((row) =>
      [1n, -1n].map((delta) => ({ ...row, delta, shift: Number(delta) })),
    ),
  )(
    "mkCons refuses a presented $element item whose summary memory is off by $shift",
    ({ step, delta }) => {
      expect(withPresentedItem(step(), delta).accepted).toBe(false);
    },
  );

  it("mkCons refuses an item whose summary memory differs from its memory", () => {
    const step = BOUND[0].step();
    expect(withPresentedItem(step, 0n).accepted).toBe(true);
    for (const delta of [1n, -1n, 100n]) {
      expect(withPresentedItem(step, delta).accepted).toBe(false);
    }
  });

  it.each(REVEALED_ONLY)(
    "mkCons refuses a presented $element item at any summary memory",
    ({ step }) => {
      const built = step();
      const { item } = withPresentedItem(built, 0n);
      const words = item.payload.memory - item.memory;
      for (const delta of [0n, -words, 3n - words, 4n - words]) {
        expect(withPresentedItem(built, delta).accepted).toBe(false);
      }
    },
  );

  it.each(ALL)("mkCons accepts a revealed $element item", ({ step }) => {
    const { step: core, witness } = step();
    expect(witness.arguments[0]!.kind).toBe("constant");
    expect(verifyMidgardCekCoreStep(core.pre, core.post, witness)).toBe(true);
  });

  it.each(BOUND)(
    "mkCons over a $element list is proved with its item and list presented by summary",
    ({ step }) => {
      const built = step();
      const source = semanticizeMidgardCekDirectConstant(
        built.witness.arguments[1]!,
      );
      expect(withPresentedItem(built, 0n, source).accepted).toBe(true);
    },
  );
});
