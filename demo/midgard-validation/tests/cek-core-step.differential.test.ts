import { hashMidgardCekMachineState } from "@al-ft/midgard-core";
import {
  type AikenStepEvaluator,
  makeAikenStepEvaluator,
} from "@al-ft/midgard-test-support/aiken-evaluator";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import { encodeMidgardCekCoreStepDataCbor } from "../src/cek-data.js";
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
  type MidgardCekExecutionStep,
  type MidgardCekStructuralExecution,
} from "../src/cek-executor.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.js";
import * as vectors from "./fixtures/cek-core-step-v1.vectors.mjs";
import {
  adjudicateCases,
  prepareExecution,
} from "./fuzz/cek-step-fuzz.harness.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

/**
 * The curated V1 CEK core-step programs (`fixtures/cek-core-step-v1.vectors.mjs`)
 * against the Aiken machine. Every step the `src/` trace builder emits for
 * them, and one perturbed copy per negative twist, goes to
 * `cek_machine_v1.verify_core_step_v1` under the pinned Aiken evaluator, and
 * each genuine step's post-state hash to `cek_machine_v1.hash_state_v1`; every
 * disagreement fails. Between them the programs reach all 42 witness arms.
 * A program that names `pinBuiltinStep` is adjudicated at that builtin step
 * alone, as its definition explains.
 *
 * The evaluator passes a step's states and witness as Data, so the evidence's
 * bytes are pinned apart, by the `cek-core-step-v1` channel
 * (`scripts/generate-cek-core-step-v1-goldens.mjs`).
 */

type Run = {
  readonly execution: MidgardCekStructuralExecution;
  /** The steps the program is adjudicated at. */
  readonly selected: readonly MidgardCekExecutionStep[];
};

const run = (definition: vectors.CekCoreStepProgramV1): Run => {
  const program = buildCekProgramFromRawFlat(
    vectors.compileMidgardCekGoldenProgram(
      definition.term(),
      encodeMidgardCekCardanoFlatProgram,
    ),
  );
  const graph = buildMidgardCekExecutionGraph(
    program.termRoot,
    program.material.values(),
    encodeMidgardCekPlutusData(definition.context()),
  );
  const execution = executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: definition.maxSteps,
    executionBudget: definition.executionBudget,
  });
  return {
    execution,
    selected:
      definition.pinBuiltinStep === undefined
        ? execution.steps
        : execution.steps.filter(
            (step) =>
              (step.witness.kind === "executeBuiltinSemantic" ||
                step.witness.kind === "executeBuiltinSemanticFailure") &&
              step.witness.tag === definition.pinBuiltinStep,
          ),
  };
};

const definitionFor = (label: string): vectors.CekCoreStepProgramV1 => {
  const definition = [
    ...vectors.CEK_CORE_STEP_PROGRAMS,
    ...vectors.CEK_CONSTR_DATA_INDEX_PROGRAMS,
  ].find((candidate) => candidate.label === label);
  if (definition === undefined) {
    throw new Error(`no curated program ${label}`);
  }
  return definition;
};

const hashOf = (state: MidgardCekExecutionStep["pre"]): string =>
  hashMidgardCekMachineState(state).toString("hex");

const everyStep = (steps: readonly unknown[]): readonly number[] =>
  steps.map((_, index) => index);

describe("V1 CEK core-step programs against the Aiken machine", () => {
  let aiken: AikenStepEvaluator | undefined;

  // The exports are cached by their compiler inputs; see
  // `cek-executor.differential.test.ts` for what a cold cache costs.
  beforeAll(async () => {
    aiken = await makeAikenStepEvaluator();
  }, 300_000);

  afterAll(() => aiken?.close());

  it("exports the program definitions its declaration file declares", () => {
    // The declaration file beside the vectors module is asserted, not derived
    // (see its header), so the export list is the one thing this suite can
    // check about it directly.
    expect(Object.keys(vectors).sort()).toEqual(
      [
        "CEK_CONSTR_DATA_INDEX_PROGRAMS",
        "CEK_CORE_STEP_PROGRAMS",
        "CEK_CORE_STEP_TWISTS",
        "CEK_CORE_STEP_WITNESS_KINDS",
        "CEK_CORE_STEP_WITNESS_KINDS_UNCOVERED",
        "EMPTY_CONTEXT",
        "INITIAL_MODE",
        "compileMidgardCekGoldenProgram",
      ].sort(),
    );
    expect(
      new Set(vectors.CEK_CORE_STEP_PROGRAMS.map((program) => program.label))
        .size,
    ).toBe(vectors.CEK_CORE_STEP_PROGRAMS.length);
  });

  it("pins the witness-arm coverage of the curated set", () => {
    const reached = new Set(
      vectors.CEK_CORE_STEP_PROGRAMS.flatMap((definition) =>
        run(definition).execution.steps.map((step) => step.witness.kind),
      ),
    );
    expect(
      vectors.CEK_CORE_STEP_WITNESS_KINDS.filter((kind) => !reached.has(kind)),
    ).toEqual([...vectors.CEK_CORE_STEP_WITNESS_KINDS_UNCOVERED]);
    expect(vectors.CEK_CORE_STEP_WITNESS_KINDS).toHaveLength(42);
    for (const kind of reached) {
      expect(vectors.CEK_CORE_STEP_WITNESS_KINDS).toContain(kind);
    }
  });

  it.each([
    ["zero", true],
    ["maximum", true],
    ["overflow", false],
    ["negative", false],
  ] as const)("pins constrData %s through the machine", (boundary, accepts) => {
    const { execution, selected } = run(
      definitionFor(`constr_data_index_${boundary}`),
    );
    expect(execution.stopReason).toBe("halted");
    expect(execution.terminalState.mode).toBe(
      accepts ? "haltSuccess" : "haltError",
    );
    expect(selected).toHaveLength(1);
    const step = selected[0]!;
    expect(step.witness.kind).toBe(
      accepts ? "executeBuiltinSemantic" : "executeBuiltinSemanticFailure",
    );
    expect(verifyMidgardCekCoreStep(step.pre, step.post, step.witness)).toBe(
      true,
    );
    if (!accepts) {
      expect(step.post.auxiliary).toBe(7n); // BuiltinFailure
      expect(step.post.cpu - step.pre.cpu).toBe(0n);
      expect(step.post.memory - step.pre.memory).toBe(0n);
      for (const chargedPost of [
        { ...step.post, cpu: step.post.cpu + 1n },
        { ...step.post, memory: step.post.memory + 1n },
      ]) {
        expect(
          verifyMidgardCekCoreStep(step.pre, chargedPost, step.witness),
        ).toBe(false);
      }
    }
  });

  for (const definition of vectors.CEK_CORE_STEP_PROGRAMS) {
    it(definition.label, { timeout: 300_000 }, () => {
      const { execution, selected } = run(definition);
      expect(execution.initialState.mode).toBe(vectors.INITIAL_MODE);
      expect(selected.length).toBeGreaterThan(0);
      expect(selected.length).toBeLessThanOrEqual(definition.maxSteps);
      for (const step of execution.steps) {
        expect(
          verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
        ).toBe(true);
      }
      // The selected steps continue one another.
      selected.slice(1).forEach((step, index) => {
        expect(hashOf(step.pre)).toBe(hashOf(selected[index]!.post));
      });
      // Each twist keeps the step well-formed and changes only its
      // post-state, so a refusal is the verifier's verdict rather than a
      // decoder's.
      for (const twist of vectors.CEK_CORE_STEP_TWISTS) {
        const step = selected[twist.select(selected)]!;
        const twisted = twist.apply(step);
        expect(twisted.pre).toEqual(step.pre);
        expect(twisted.witness).toEqual(step.witness);
        expect(encodeMidgardCekCoreStepDataCbor(twisted)).not.toEqual(
          encodeMidgardCekCoreStepDataCbor(step),
        );
      }

      const [result] = adjudicateCases(aiken!, [
        prepareExecution(
          definition,
          { ...execution, steps: selected },
          0,
          everyStep,
        ),
      ]);
      expect(result!.divergences).toStrictEqual([]);
      expect(result!.steps.filter((step) => step.catalogued !== null)).toEqual(
        [],
      );
      expect(result!.steps).toHaveLength(
        selected.length + vectors.CEK_CORE_STEP_TWISTS.length,
      );
    });
  }
});
