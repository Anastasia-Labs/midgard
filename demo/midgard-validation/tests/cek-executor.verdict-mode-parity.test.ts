import { hashMidgardCekMachineState } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "../src/cek-executor.js";
import {
  buildMidgardCanonicalCekProgram,
  encodeMidgardCekCardanoFlatProgram,
} from "../src/cek-program.js";
import * as vectors from "./fixtures/cek-core-step-v1.vectors.mjs";

/**
 * Verdict mode skips per-step verification and retention. Over the CEK
 * core-step golden corpus (successes, every error class, a budget stop) it
 * must reach the same terminal state, stop reason, budget and step count as
 * proof mode.
 */

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

const prepare = (definition: vectors.CekCoreStepProgramV1) => {
  const program = buildMidgardCanonicalCekProgram(
    vectors.compileMidgardCekGoldenProgram(
      definition.term(),
      encodeMidgardCekCardanoFlatProgram,
    ),
  );
  const graph = buildMidgardCekExecutionGraph(
    program.envelope,
    program.material.values(),
    encodeMidgardCekPlutusData(definition.context()),
  );
  return {
    root: graph.root,
    material: [...graph.material.values()],
    constantWitnesses: graph.constantWitnesses,
  };
};

describe("CEK verdict mode", () => {
  it("covers successes, errors and a budget stop in the corpus", () => {
    const outcomes = new Set(
      vectors.CEK_CORE_STEP_PROGRAMS.map((definition) => {
        const verdict = executeMidgardCekStructuralProgram({
          ...prepare(definition),
          maxSteps: definition.maxSteps,
          executionBudget: definition.executionBudget,
          mode: "verdict",
        });
        return verdict.stopReason === "budgetExceeded"
          ? "budgetExceeded"
          : verdict.terminalState.mode;
      }),
    );
    expect([...outcomes].sort()).toEqual([
      "budgetExceeded",
      "haltError",
      "haltSuccess",
    ]);
  });

  it.each(vectors.CEK_CORE_STEP_PROGRAMS.map((d) => [d.label, d] as const))(
    "%s: verdict mode matches proof mode",
    (_label, definition) => {
      const proof = executeMidgardCekStructuralProgram({
        ...prepare(definition),
        maxSteps: definition.maxSteps,
        executionBudget: definition.executionBudget,
      });
      const verdict = executeMidgardCekStructuralProgram({
        ...prepare(definition),
        maxSteps: definition.maxSteps,
        executionBudget: definition.executionBudget,
        mode: "verdict",
      });

      expect("steps" in verdict).toBe(false);
      expect(verdict.stepCount).toBe(proof.steps.length);
      expect(verdict.stopReason).toBe(proof.stopReason);
      expect(verdict.terminalState).toEqual(proof.terminalState);
      expect(hex(hashMidgardCekMachineState(verdict.terminalState))).toBe(
        hex(hashMidgardCekMachineState(proof.terminalState)),
      );
      expect(hex(hashMidgardCekMachineState(verdict.initialState))).toBe(
        hex(hashMidgardCekMachineState(proof.initialState)),
      );
      expect(verdict.terminalState.cpu).toBe(proof.terminalState.cpu);
      expect(verdict.terminalState.memory).toBe(proof.terminalState.memory);
    },
  );

  it("enforces the same exact step bound as proof mode", () => {
    const definition = vectors.CEK_CORE_STEP_PROGRAMS.find(
      (candidate) => candidate.label === "omega_budget_exceeded",
    )!;
    const run = (mode: "proof" | "verdict") => () =>
      executeMidgardCekStructuralProgram({
        ...prepare(definition),
        maxSteps: 7,
        mode,
      });
    expect(run("proof")).toThrow(
      "CEK execution exceeded the exact 7-step bound",
    );
    expect(run("verdict")).toThrow(
      "CEK execution exceeded the exact 7-step bound",
    );
  });
});
