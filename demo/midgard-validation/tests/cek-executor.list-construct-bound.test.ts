import { createHash } from "node:crypto";

import { hashMidgardCekMachineState } from "@al-ft/midgard-core";
import { constT, UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  type MidgardCekExecutionMode,
  type MidgardCekStructuralExecution,
} from "../src/cek-executor.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { program } from "./helpers/cek-differential-builders.js";
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
      const run = runMidgardCekProgram(GROWN.prepare(), mode);
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
    const run = runMidgardCekProgram(GROWN.prepare(), "proof");
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
