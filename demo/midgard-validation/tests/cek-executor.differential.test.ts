import { createHash } from "node:crypto";

import { hashMidgardCekMachineState } from "@al-ft/midgard-core";
import {
  type AikenStepEvaluator,
  makeAikenStepEvaluator,
} from "@al-ft/midgard-test-support/aiken-evaluator";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  type MidgardCekStructuralExecution,
  type MidgardCekStructuralVerdict,
} from "../src/cek-executor.js";
import type { CataloguedClass } from "./fuzz/cek-step-fuzz.catalogue.js";
import {
  adjudicateCases,
  type AdjudicatedStep,
  prepareExecution,
} from "./fuzz/cek-step-fuzz.harness.js";
import type { ClassCounts } from "./fuzz/cek-step-fuzz.pins.js";
import { MIDGARD_CEK_DIFFERENTIAL_CORPUS } from "./helpers/cek-differential-corpus.js";
import { runMidgardCekProgram } from "./helpers/cek-uplc-terms.js";

/**
 * The structural executor against the Aiken machine, over the differential
 * corpus. For every program, the executor's proof and verdict modes must
 * reach the same step count, stop reason and terminal state. A sample of the
 * proof run's steps (`prepareExecution`: the last step, the first of each
 * kind, the builtin-executing steps, a seeded few more, and one perturbed
 * copy per negative twist) goes to `cek_machine_v1.verify_core_step_v1`
 * under the pinned Aiken evaluator, and each genuine step's post-state hash
 * to `cek_machine_v1.hash_state_v1`. Every disagreement fails, except a
 * genuine step in a catalogued class (`fuzz/cek-step-fuzz.catalogue.ts`)
 * that the Aiken machine does not accept; each program's steps per class
 * must equal `CATALOGUED_STEPS`.
 */

/**
 * Per program label, the sampled steps of each catalogued class; programs
 * with none are not listed. When the fix that removes a class composes, its
 * pins go with it.
 */
const CATALOGUED_STEPS: Readonly<Record<string, ClassCounts>> = {
  "shape serialiseData of a grown structure n=9": { serialiseLoop: 1 },
  "serialiseData of a list at 9212 bytes": { serialiseLoop: 1 },
  "serialiseData of a list at 9213 bytes": { serialiseLoop: 1 },
  "serialiseData of a list at 9214 bytes": { serialiseLoop: 1 },
  "serialiseData of a list at 9215 bytes": { serialiseLoop: 1 },
  "serialiseData of a list at 9216 bytes": { serialiseLoop: 1 },
  "serialiseData of a list at 9217 bytes": { serialiseLoop: 1 },
  "serialiseData of a list at 9218 bytes": { serialiseLoop: 1 },
  "serialiseData of a list at 9220 bytes": { serialiseLoop: 1 },
  "serialiseData of a constructor at 9214 bytes": { serialiseLoop: 1 },
  "serialiseData of a constructor at 9215 bytes": { serialiseLoop: 1 },
  "serialiseData of a constructor at 9216 bytes": { serialiseLoop: 1 },
  "serialiseData of a constructor at 9217 bytes": { serialiseLoop: 1 },
  "serialiseData of a constructor at 9218 bytes": { serialiseLoop: 1 },
  "serialiseData of a constructor at 9219 bytes": { serialiseLoop: 1 },
  "serialiseData of a constructor at 9220 bytes": { serialiseLoop: 1 },
  "serialiseData of a constructor at 9222 bytes": { serialiseLoop: 1 },
  "list of integer lists grown past the bound at runtime": {
    nestedConstantCost: 1,
  },
  "serialiseData of a large list context": { serialiseLoop: 1 },
  "serialiseData of a large script context": { serialiseLoop: 1 },
  "map with duplicate keys": { serialiseLoop: 1 },
  "nested lists": { serialiseLoop: 1 },
};

const outcomeOf = (
  run: MidgardCekStructuralExecution | MidgardCekStructuralVerdict,
) => ({
  steps: "steps" in run ? run.steps.length : run.stepCount,
  stopReason: run.stopReason,
  terminal: hashMidgardCekMachineState(run.terminalState).toString("hex"),
});

/** A sample seed fixed by the program's label, not its corpus position. */
const sampleSeedOf = (label: string): number =>
  createHash("sha256").update(label).digest().readUInt32BE(0);

const cataloguedOf = (steps: readonly AdjudicatedStep[]): ClassCounts => {
  const counts: Partial<Record<CataloguedClass, number>> = {};
  for (const { catalogued } of steps) {
    if (catalogued !== null) counts[catalogued] = (counts[catalogued] ?? 0) + 1;
  }
  return counts;
};

describe("structural executor differential against the Aiken machine", () => {
  let aiken: AikenStepEvaluator | undefined;

  // The exports are cached by their compiler inputs: a cold cache compiles
  // the on-chain project twice (about 50 s each on a loaded machine, a few
  // minutes on a loaded CI runner), and a file that starts meanwhile waits
  // for those compiles.
  beforeAll(async () => {
    aiken = await makeAikenStepEvaluator();
  }, 300_000);

  afterAll(() => aiken?.close());

  it("pins catalogued steps only for corpus programs", () => {
    const labels = new Set(
      MIDGARD_CEK_DIFFERENTIAL_CORPUS.map((entry) => entry.label),
    );
    expect(
      Object.keys(CATALOGUED_STEPS).filter((label) => !labels.has(label)),
    ).toStrictEqual([]);
  });

  for (const entry of MIDGARD_CEK_DIFFERENTIAL_CORPUS) {
    it(entry.label, { timeout: 300_000 }, () => {
      const prepared = entry.prepare();
      const proof = runMidgardCekProgram(prepared, "proof");
      expect(outcomeOf(runMidgardCekProgram(prepared, "verdict"))).toEqual(
        outcomeOf(proof),
      );
      const [result] = adjudicateCases(aiken!, [
        prepareExecution(entry, proof, sampleSeedOf(entry.label)),
      ]);
      expect({
        divergences: result!.divergences,
        catalogued: cataloguedOf(result!.steps),
      }).toStrictEqual({
        divergences: [],
        catalogued: CATALOGUED_STEPS[entry.label] ?? {},
      });
    });
  }
});
