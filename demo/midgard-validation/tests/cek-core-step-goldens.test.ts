import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { hashMidgardCekMachineState } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

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
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

/**
 * The TypeScript half of the V1 CEK **core-step** cross-language golden
 * channel.
 *
 * Every value in the generated fixture is recomputed here from the `src/`
 * trace builder driven by the same program definitions the generator uses, so
 * a drifting builder, state hash or evidence encoder fails on this side. The
 * generated Aiken module (`onchain/aiken/lib/midgard/cek-core-step-goldens/*.test.ak`)
 * decodes the same CBOR into `CoreStepEvidenceV1` and re-verifies it with
 * `verify_core_step_v1` under the fork runner, so a divergence between the
 * two machines fails on that side. Regenerate both with
 * `pnpm run fixtures:cek-core-step-v1:sync`.
 *
 * The fixture is loaded, never trusted: nothing below reads a value out of it
 * except to compare against what `src/` produces now.
 */

type GoldenState = {
  readonly mode: string;
  readonly executionIndex: string;
  readonly focusRoot: string;
  readonly environmentRoot: string;
  readonly continuationRoot: string;
  readonly auxiliary: string;
  readonly cpu: string;
  readonly memory: string;
};

type GoldenStep = {
  readonly index: number;
  readonly kind: string;
  readonly preHash: string;
  readonly postHash: string;
  readonly evidenceCbor: string;
};

type GoldenNegative = {
  readonly twist: string;
  readonly stepIndex: number;
  readonly kind: string;
  readonly evidenceCbor: string;
};

type GoldenProgram = {
  readonly label: string;
  readonly note: string;
  readonly maxSteps: number;
  readonly pinBuiltinStep?: string;
  readonly executionBudget?: { readonly cpu: string; readonly memory: string };
  readonly scriptFlat: string;
  readonly contextCbor: string;
  readonly uplcVersion: readonly string[];
  readonly termRoot: string;
  readonly root: string;
  readonly contextValueRoot: string;
  readonly initialState: GoldenState;
  readonly initialStateHash: string;
  readonly steps: readonly GoldenStep[];
  readonly terminalState: GoldenState;
  readonly terminalStateHash: string;
  readonly stopReason: string;
  readonly negatives: readonly GoldenNegative[];
};

type Golden = {
  readonly generator: string;
  readonly aikenModules: readonly string[];
  readonly aikenStepsPerTest: number;
  readonly witnessKindsCovered: readonly string[];
  readonly witnessKindsUncovered: readonly string[];
  readonly programs: readonly GoldenProgram[];
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL("./fixtures/cek-core-step-v1.generated.json", import.meta.url),
    ),
    "utf8",
  ),
) as Golden;

const hex = (value: Uint8Array): string => Buffer.from(value).toString("hex");

const stateJson = (
  state: MidgardCekStructuralExecution["initialState"],
): GoldenState => ({
  mode: state.mode,
  executionIndex: state.executionIndex.toString(10),
  focusRoot: hex(state.focusRoot),
  environmentRoot: hex(state.environmentRoot),
  continuationRoot: hex(state.continuationRoot),
  auxiliary: state.auxiliary.toString(10),
  cpu: state.cpu.toString(10),
  memory: state.memory.toString(10),
});

type Recomputed = {
  readonly execution: MidgardCekStructuralExecution;
  readonly vector: Omit<GoldenProgram, "note" | "negatives">;
};

const recompute = (definition: vectors.CekCoreStepProgramV1): Recomputed => {
  const scriptFlat = vectors.compileMidgardCekGoldenProgram(
    definition.term(),
    encodeMidgardCekCardanoFlatProgram,
  );
  const program = buildCekProgramFromRawFlat(scriptFlat);
  const contextCbor = encodeMidgardCekPlutusData(definition.context());
  const graph = buildMidgardCekExecutionGraph(
    program.termRoot,
    program.material.values(),
    contextCbor,
  );
  const execution = executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: definition.maxSteps,
    executionBudget: definition.executionBudget,
  });
  const selectedSteps =
    definition.pinBuiltinStep === undefined
      ? execution.steps
      : execution.steps.filter(
          (step) =>
            (step.witness.kind === "executeBuiltinSemantic" ||
              step.witness.kind === "executeBuiltinSemanticFailure") &&
            step.witness.tag === definition.pinBuiltinStep,
        );
  const initialState =
    definition.pinBuiltinStep === undefined
      ? execution.initialState
      : selectedSteps[0]!.pre;
  const terminalState =
    definition.pinBuiltinStep === undefined
      ? execution.terminalState
      : selectedSteps.at(-1)!.post;
  return {
    execution,
    vector: {
      label: definition.label,
      maxSteps: definition.maxSteps,
      ...(definition.pinBuiltinStep === undefined
        ? {}
        : { pinBuiltinStep: definition.pinBuiltinStep.toString(10) }),
      ...(definition.executionBudget === undefined
        ? {}
        : {
            executionBudget: {
              cpu: definition.executionBudget.cpu.toString(10),
              memory: definition.executionBudget.memory.toString(10),
            },
          }),
      scriptFlat: hex(scriptFlat),
      contextCbor: hex(contextCbor),
      uplcVersion: program.uplcVersion.map((part) => part.toString(10)),
      termRoot: hex(program.termRoot),
      root: hex(graph.root),
      contextValueRoot: hex(graph.contextValueRoot),
      initialState: stateJson(initialState),
      initialStateHash: hex(hashMidgardCekMachineState(initialState)),
      steps: selectedSteps.map((step) => ({
        index: execution.steps.indexOf(step),
        kind: step.witness.kind,
        preHash: hex(hashMidgardCekMachineState(step.pre)),
        postHash: hex(hashMidgardCekMachineState(step.post)),
        evidenceCbor: hex(encodeMidgardCekCoreStepDataCbor(step)),
      })),
      terminalState: stateJson(terminalState),
      terminalStateHash: hex(hashMidgardCekMachineState(terminalState)),
      stopReason:
        definition.pinBuiltinStep === undefined
          ? execution.stopReason
          : "selectedBuiltinStep",
    },
  };
};

const definitionFor = (label: string): vectors.CekCoreStepProgramV1 => {
  const definition = [
    ...vectors.CEK_CORE_STEP_PROGRAMS,
    ...vectors.CEK_CONSTR_DATA_INDEX_PROGRAMS,
  ].find((candidate) => candidate.label === label);
  if (definition === undefined) {
    throw new Error(`fixture program ${label} has no shared definition`);
  }
  return definition;
};

describe("V1 CEK core-step golden vectors", () => {
  it("shares its program definitions with the generator", () => {
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
    expect(golden.generator).toBe(
      "demo/midgard-validation/scripts/generate-cek-core-step-v1-goldens.mjs",
    );
    expect(golden.aikenModules).toEqual(
      vectors.CEK_CORE_STEP_PROGRAMS.map(
        (program) =>
          `onchain/aiken/lib/midgard/cek-core-step-goldens/${program.label.replace(/_/g, "-")}.test.ak`,
      ),
    );
    expect(golden.programs.map((program) => program.label)).toEqual(
      vectors.CEK_CORE_STEP_PROGRAMS.map((program) => program.label),
    );
    expect(
      new Set(vectors.CEK_CORE_STEP_PROGRAMS.map((program) => program.label))
        .size,
    ).toBe(vectors.CEK_CORE_STEP_PROGRAMS.length);
  });

  it("pins the witness-arm coverage of the curated set", () => {
    const recomputedKinds = new Set(
      vectors.CEK_CORE_STEP_PROGRAMS.flatMap((definition) =>
        recompute(definition).execution.steps.map((step) => step.witness.kind),
      ),
    );
    const covered = vectors.CEK_CORE_STEP_WITNESS_KINDS.filter((kind) =>
      recomputedKinds.has(kind),
    );
    const uncovered = vectors.CEK_CORE_STEP_WITNESS_KINDS.filter(
      (kind) => !recomputedKinds.has(kind),
    );
    expect(covered).toEqual(golden.witnessKindsCovered);
    expect(uncovered).toEqual(golden.witnessKindsUncovered);
    expect(uncovered).toEqual([
      ...vectors.CEK_CORE_STEP_WITNESS_KINDS_UNCOVERED,
    ]);
    expect(vectors.CEK_CORE_STEP_WITNESS_KINDS).toHaveLength(42);
    for (const kind of recomputedKinds) {
      expect(vectors.CEK_CORE_STEP_WITNESS_KINDS).toContain(kind);
    }
  });

  it.each([
    ["zero", true],
    ["maximum", true],
    ["overflow", false],
    ["negative", false],
  ] as const)("pins constrData %s through the machine", (boundary, accepts) => {
    const definition = definitionFor(`constr_data_index_${boundary}`);
    const { execution, vector } = recompute(definition);
    expect(execution.stopReason).toBe("halted");
    expect(execution.terminalState.mode).toBe(
      accepts ? "haltSuccess" : "haltError",
    );
    expect(vector.steps).toHaveLength(1);
    const step = execution.steps[vector.steps[0]!.index]!;
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

  for (const fixtureProgram of golden.programs) {
    describe(fixtureProgram.label, () => {
      const definition = definitionFor(fixtureProgram.label);
      let computed: Recomputed | undefined;
      const resultForTest = (): Recomputed =>
        (computed ??= recompute(definition));

      it("recomputes every emitted vector byte-for-byte", () => {
        const { execution, vector } = resultForTest();
        const {
          note: _note,
          negatives: _negatives,
          ...pinned
        } = fixtureProgram;
        expect(vector).toEqual(pinned);
        expect(fixtureProgram.note).toBe(definition.note);
        expect(execution.initialState.mode).toBe(vectors.INITIAL_MODE);
      });

      it("chains the pinned state hashes from the initial to the terminal state", () => {
        let expectedPre = fixtureProgram.initialStateHash;
        for (const step of fixtureProgram.steps) {
          expect(step.preHash).toBe(expectedPre);
          expectedPre = step.postHash;
        }
        expect(fixtureProgram.terminalStateHash).toBe(expectedPre);
        expect(fixtureProgram.steps.length).toBeGreaterThan(0);
        expect(fixtureProgram.steps.length).toBeLessThanOrEqual(
          fixtureProgram.maxSteps,
        );
      });

      it("is accepted step by step by the TypeScript verifier", () => {
        const { execution } = resultForTest();
        for (const step of execution.steps) {
          expect(
            verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
          ).toBe(true);
        }
      });

      it("rejects every pinned negative twist", () => {
        const { execution, vector } = resultForTest();
        expect(
          fixtureProgram.negatives.map((negative) => negative.twist),
        ).toEqual(vectors.CEK_CORE_STEP_TWISTS.map((twist) => twist.label));
        for (const twist of vectors.CEK_CORE_STEP_TWISTS) {
          const pinned = fixtureProgram.negatives.find(
            (negative) => negative.twist === twist.label,
          );
          const selectedSteps = vector.steps.map(
            (step) => execution.steps[step.index]!,
          );
          const step: MidgardCekExecutionStep | undefined =
            selectedSteps[twist.select(selectedSteps)];
          const stepIndex =
            step === undefined ? -1 : execution.steps.indexOf(step);
          expect(step).toBeDefined();
          if (step === undefined || pinned === undefined) {
            continue;
          }
          const twisted = twist.apply(step);
          expect(pinned.stepIndex).toBe(stepIndex);
          expect(pinned.kind).toBe(twisted.witness.kind);
          expect(hex(encodeMidgardCekCoreStepDataCbor(twisted))).toBe(
            pinned.evidenceCbor,
          );
          // The twist must keep the step well-formed and change exactly the
          // post-state, so the refusal is the verifier's verdict rather than a
          // decoder's.
          expect(hex(encodeMidgardCekCoreStepDataCbor(twisted))).not.toBe(
            hex(encodeMidgardCekCoreStepDataCbor(step)),
          );
          expect(twisted.pre).toEqual(step.pre);
          expect(twisted.witness).toEqual(step.witness);
          expect(
            verifyMidgardCekCoreStep(
              twisted.pre,
              twisted.post,
              twisted.witness,
            ),
          ).toBe(false);
        }
      });
    });
  }
});
