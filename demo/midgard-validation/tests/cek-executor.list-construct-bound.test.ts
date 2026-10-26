import { createHash } from "node:crypto";

import { hashMidgardCekMachineState } from "@al-ft/midgard-core";
import { constT, type ConstType, UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { midgardCekDirectBuiltinFits } from "../src/cek-builtin.evaluate-direct-builtin.js";
import { type MidgardCekDirectValueWitness } from "../src/cek-builtin.js";
import {
  encodeMidgardCekCanonicalConstant,
  encodeMidgardCekCanonicalDataConstant,
} from "../src/cek-constant.js";
import {
  type MidgardCekExecutionMode,
  type MidgardCekStructuralExecution,
} from "../src/cek-executor.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import {
  DIRECT_BOUND,
  I,
  L,
  program,
} from "./helpers/cek-differential-builders.js";
import { MIDGARD_CEK_DIFFERENTIAL_CORPUS } from "./helpers/cek-differential-corpus.js";
import { mkCons, runMidgardCekProgram } from "./helpers/cek-uplc-terms.js";

// mkCons over a list whose element type the split list hop cannot read (here
// `list<int>`) is proved by the direct list-construct witness while that
// witness fits the aggregate direct payload bound with its result, and as a
// semantic step past it. Both execution modes route the step the same way, so
// a list grown past the bound halts in either mode.

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

/** The pre and post state hashes of every step, folded into one digest. */
const stateDigest = (run: MidgardCekStructuralExecution): string =>
  createHash("sha256")
    .update(
      JSON.stringify(
        run.steps.map((step) => [
          hex(hashMidgardCekMachineState(step.pre)),
          hex(hashMidgardCekMachineState(step.post)),
        ]),
      ),
    )
    .digest("hex");

const GROWN = MIDGARD_CEK_DIFFERENTIAL_CORPUS.find(
  (entry) =>
    entry.label === "list of integer lists grown past the bound at runtime",
)!;

/**
 * `mkCons [7] source` over a source of `count` lists of twenty integers. At
 * 209 lists the direct witness carries 9,206 payload bytes; at 210 it would
 * carry 9,250, past the 9,215-byte bound.
 */
// One proof-mode run of the grown list, shared by the tests that read it.
let grownProofRun: ReturnType<typeof runMidgardCekProgram> | undefined;
const grownRun = (mode: MidgardCekExecutionMode) =>
  mode === "proof"
    ? (grownProofRun ??= runMidgardCekProgram(GROWN.prepare(), "proof"))
    : runMidgardCekProgram(GROWN.prepare(), mode);

const integerListCons = (count: number) =>
  program(
    `mkCons over ${count.toString()} integer lists`,
    mkCons(
      UPLCConst.listOf(constT.int)([7n]),
      UPLCConst.listOf(constT.listOf(constT.int))(
        Array.from({ length: count }, (_, row) =>
          Array.from({ length: 20 }, (_, column) =>
            BigInt((row + column) % 20),
          ),
        ),
      ),
    ),
  );

const consStep = (count: number, mode: MidgardCekExecutionMode) => {
  const run = runMidgardCekProgram(integerListCons(count).prepare(), mode);
  const steps = "steps" in run ? run.steps : [];
  return {
    run,
    step: steps.find(
      (step) =>
        step.witness.kind === "executeBuiltinDirect" ||
        step.witness.kind === "executeBuiltinSemantic",
    ),
  };
};

describe("mkCons over integer lists at the direct payload bound", () => {
  it.each(["proof", "verdict"] as const)(
    "a list of integer lists grown past the bound halts in %s mode",
    (mode) => {
      const run = grownRun(mode);
      expect(run.stopReason).toBe("halted");
      expect("steps" in run ? run.steps.length : run.stepCount).toBe(7885);
      expect(hex(hashMidgardCekMachineState(run.initialState))).toBe(
        "5621d68251aea5651d35a1aa60c513c000e063a1e6863c3dc4bade8cf2b709aa",
      );
      expect(hex(hashMidgardCekMachineState(run.terminalState))).toBe(
        "fc1e398125c5c229af9f07f05caa25a6c491d86a68b03ebdcd655e5d17ad6481",
      );
    },
  );

  it("the grown list's states are the semantic route's states at every step", () => {
    const run = grownRun("proof");
    expect("steps" in run).toBe(true);
    if (!("steps" in run)) return;
    expect(stateDigest(run)).toBe(
      "1ae16efe5f5aa7f766fbd46c38c9141e7f949e741f124f9a27356ab716608064",
    );
    expect(
      run.steps.every((step) =>
        verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });

  it.each([
    { count: 100, kind: "executeBuiltinDirect" },
    { count: 209, kind: "executeBuiltinDirect" },
    { count: 210, kind: "executeBuiltinSemantic" },
    { count: 300, kind: "executeBuiltinSemantic" },
  ])(
    "mkCons over $count integer lists is proved by $kind",
    ({ count, kind }) => {
      const { run, step } = consStep(count, "proof");
      expect(step?.witness.kind).toBe(kind);
      expect(run.stopReason).toBe("halted");
      expect(
        "steps" in run &&
          run.steps.every((each) =>
            verifyMidgardCekCoreStep(each.pre, each.post, each.witness),
          ),
      ).toBe(true);
    },
  );

  it.each([100, 209, 210, 300])(
    "mkCons over %i integer lists ends in the same state in both modes",
    (count) => {
      const proof = consStep(count, "proof").run;
      const verdict = consStep(count, "verdict").run;
      expect(verdict.stopReason).toBe("halted");
      expect(hashMidgardCekMachineState(verdict.terminalState)).toEqual(
        hashMidgardCekMachineState(proof.terminalState),
      );
    },
  );
});

// The direct witness of mkCons carries the head, the source and the result,
// so over a non-empty source its payload is twice the arguments' and always
// even. Over the empty source it is odd, which reaches the bound exactly.
// Lists of integer lists are revealed at any size, so each side of the bound
// is reached with revealed arguments.

/** `lengths.length` lists of zeros, one payload byte per zero. */
const zeroLists = (lengths: readonly number[]): bigint[][] =>
  lengths.map((length) => Array.from({ length }, () => 0n));

/** 209 lists of twenty zeros, then one of `last` zeros: `4,602 + last` bytes. */
const grownLists = (last: number): bigint[][] =>
  zeroLists([...Array.from({ length: 209 }, () => 20), last]);

const INTEGER_LIST = constT.listOf(constT.int);

type ConsShape = {
  readonly headType: ConstType;
  readonly head: unknown;
  readonly source: readonly unknown[];
};

const consConstants = ({ headType, head, source }: ConsShape) => ({
  head: UPLCConst.listOf(headType)(head as never),
  source: UPLCConst.listOf(constT.listOf(headType))(source as never),
});

/** The payload bytes of the direct witness of `mkCons head source`. */
const consPayloadBytes = (shape: ConsShape): number => {
  const { head, source } = consConstants(shape);
  const result = UPLCConst.listOf(constT.listOf(shape.headType))([
    shape.head,
    ...shape.source,
  ] as never);
  return [head, source, result].reduce(
    (total, constant) =>
      total + encodeMidgardCekCanonicalConstant(constant).payloadCbor.length,
    0,
  );
};

const consOnto = (shape: ConsShape, mode: MidgardCekExecutionMode) => {
  const { head, source } = consConstants(shape);
  const run = runMidgardCekProgram(
    program(
      "mkCons at the direct payload bound",
      mkCons(head, source),
    ).prepare(),
    mode,
  );
  const steps = "steps" in run ? run.steps : [];
  return {
    run,
    step: steps.find(
      (step) =>
        step.witness.kind === "executeBuiltinDirect" ||
        step.witness.kind === "executeBuiltinSemantic",
    ),
  };
};

describe("mkCons whose direct witness is at the payload bound", () => {
  it.each([
    {
      at: "one byte under",
      shape: { headType: constT.int, head: [7n], source: grownLists(2) },
      bytes: DIRECT_BOUND - 1,
      kind: "executeBuiltinDirect",
    },
    {
      at: "exactly",
      shape: { headType: INTEGER_LIST, head: grownLists(4), source: [] },
      bytes: DIRECT_BOUND,
      kind: "executeBuiltinDirect",
    },
    {
      at: "one byte over",
      shape: { headType: constT.int, head: [7n], source: grownLists(3) },
      bytes: DIRECT_BOUND + 1,
      kind: "executeBuiltinSemantic",
    },
  ])(
    "a witness $at the bound is proved by $kind in both modes",
    ({ shape, bytes, kind }) => {
      expect(consPayloadBytes(shape)).toBe(bytes);
      const proof = consOnto(shape, "proof");
      expect(proof.step?.witness.kind).toBe(kind);
      expect(proof.run.stopReason).toBe("halted");
      expect(
        "steps" in proof.run &&
          proof.run.steps.every((each) =>
            verifyMidgardCekCoreStep(each.pre, each.post, each.witness),
          ),
      ).toBe(true);
      const verdict = consOnto(shape, "verdict").run;
      expect(verdict.stopReason).toBe("halted");
      expect(hashMidgardCekMachineState(verdict.terminalState)).toEqual(
        hashMidgardCekMachineState(proof.run.terminalState),
      );
    },
  );
});

/** A Data list of `count` zeros: `count + 2` payload bytes. */
const dataListArgument = (count: number): MidgardCekDirectValueWitness => {
  const { typeCbor, payloadCbor } = encodeMidgardCekCanonicalDataConstant(
    L(Array.from({ length: count }, () => I(0n))),
  );
  return { kind: "constant", witness: { typeCbor, payloadCbor } };
};

// A builtin that fails has no result, so its witness fits exactly when its
// arguments do. unIData (45) fails on a list.
describe("a failing builtin whose arguments are at the payload bound", () => {
  it.each([
    { at: "one byte under", count: DIRECT_BOUND - 3, fits: true },
    { at: "exactly", count: DIRECT_BOUND - 2, fits: true },
    { at: "one byte over", count: DIRECT_BOUND - 1, fits: false },
  ])("arguments $at the bound fit: $fits", ({ count, fits }) => {
    const argument = dataListArgument(count);
    expect(
      argument.kind === "constant" && argument.witness.payloadCbor.length,
    ).toBe(count + 2);
    expect(midgardCekDirectBuiltinFits(45n, [argument])).toBe(fits);
  });
});
