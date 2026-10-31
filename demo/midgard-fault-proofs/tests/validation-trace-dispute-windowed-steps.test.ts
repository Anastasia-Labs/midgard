/** The claim material's one-step reads at a mid-size trace (the longest
 * failing script cut off at a sixteenth of the admitted budget): the
 * states, work witnesses, one-step arguments, recorded script selections
 * and state indices it builds from windowed commit replays are exactly the
 * ones the proof build's whole witnessed trace holds. */
import { isDeepStrictEqual } from "node:util";

import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationOneStepArgument,
  type DeterministicValidationMachineTrace,
  openValidationTraceStepsV1,
  type ValidationTraceSteps,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import type { ValidationTraceClaimMaterial } from "../src/validation-trace-dispute/claim-material.js";
import { recordedSelectionOf } from "../src/validation-trace-dispute/selected-scripts.js";
import {
  ADMITTED_TX_EX_UNITS,
  plutusExecutionBlockFixture,
} from "./helpers/plutus-execution-block-fixture.js";
import {
  claimMaterialOf,
  rejectedReplayInputOf,
} from "./support/validation-trace-claim-material.js";
import { LONGEST_FAILING_SCRIPT } from "./support/validation-trace-dispute-max-shape-case.js";

const MID_SIZE_EX_UNITS = [
  ADMITTED_TX_EX_UNITS[0] / 16n,
  ADMITTED_TX_EX_UNITS[1] / 16n,
] as const;

/** Step `stateIndex`'s one-step argument built over `trace`, or why it
 * cannot be built. */
const argumentOver = (
  trace: ValidationTraceSteps,
  stateIndex: number,
): unknown => {
  try {
    return buildValidationOneStepArgument({ trace, stateIndex });
  } catch (error) {
    return error instanceof Error ? error.message : String(error);
  }
};

/** States and witnesses `stateIndex` and `stateIndex + 1` of `steps`. */
const windowOf = (
  steps: ValidationTraceSteps,
  stateIndex: number,
): ValidationTraceSteps => ({
  states: {
    [stateIndex]: steps.states[stateIndex],
    [stateIndex + 1]: steps.states[stateIndex + 1],
  },
  witnesses: {
    [stateIndex]: steps.witnesses[stateIndex],
    [stateIndex + 1]: steps.witnesses[stateIndex + 1],
  },
});

describe("validation-trace claim material: windowed one-step reads at a mid-size trace", () => {
  let whole: DeterministicValidationMachineTrace;
  let kept: ValidationTraceSteps;
  let material: ValidationTraceClaimMaterial;
  let stepCount: number;

  beforeAll(async () => {
    const fixture = await plutusExecutionBlockFixture("failing", {
      spend: { script: LONGEST_FAILING_SCRIPT, exUnits: MID_SIZE_EX_UNITS },
    });
    const input = await rejectedReplayInputOf(fixture);
    material = await claimMaterialOf(fixture);
    stepCount = Number(material.stepCount);
    whole = await Effect.runPromise(
      buildDeterministicValidationMachineTrace(input),
    );
    kept = await Effect.runPromise(
      openValidationTraceStepsV1(input, () => true),
    );
  }, 600_000);

  it("is the claim's trace, long enough to cross many CEK steps", () => {
    expect(stepCount).toBeGreaterThan(10_000);
    expect(whole.states).toHaveLength(stepCount + 1);
    expect(whole.tree.descriptor.traceRoot.toString("hex")).toBe(
      material.challengerRoot,
    );
  });

  it("keeps every state and work witness exactly as the proof build holds it", () => {
    expect(Object.keys(kept.states)).toHaveLength(stepCount + 1);
    const differing = whole.states.flatMap((state, index) =>
      isDeepStrictEqual(kept.states[index], state) &&
      isDeepStrictEqual(kept.witnesses[index], whole.witnesses[index])
        ? []
        : [index],
    );
    expect(differing).toStrictEqual([]);
  });

  it("builds every step's one-step argument from its two-state window exactly as from the whole trace", () => {
    const differing: number[] = [];
    for (let index = 0; index < stepCount; index += 1)
      if (
        !isDeepStrictEqual(
          argumentOver(windowOf(kept, index), index),
          argumentOver(whole, index),
        )
      )
        differing.push(index);
    expect(differing).toStrictEqual([]);
  }, 600_000);

  it("reads each step's window, the recorded selections and the state indices as the whole trace holds them", async () => {
    const firstCek = whole.witnesses.findIndex(
      (witness) => witness.auxiliary?.kind === "cekCoreStep",
    );
    expect(firstCek).toBeGreaterThan(0);
    for (const index of [
      0,
      firstCek,
      Math.floor(stepCount / 2),
      stepCount - 1,
    ]) {
      const steps = await material.steps(index);
      expect(Object.keys(steps.states)).toStrictEqual([
        String(index),
        String(index + 1),
      ]);
      expect(steps).toStrictEqual(windowOf(whole, index));
    }
    const selections = whole.witnesses.flatMap((witness) => {
      const selection = recordedSelectionOf(witness);
      return selection === undefined ? [] : [selection];
    });
    expect(selections.length).toBeGreaterThan(0);
    expect(await material.selections()).toStrictEqual(selections);
    for (const index of [0, 1, firstCek, Math.floor(stepCount / 2), stepCount])
      expect(
        material.stateIndexOf(
          hashMidgardValidationMachineState(whole.states[index]!).toString(
            "hex",
          ),
        ),
      ).toBe(index);
    expect(material.stateIndexOf("5c".repeat(32))).toBeUndefined();
  }, 600_000);
});
