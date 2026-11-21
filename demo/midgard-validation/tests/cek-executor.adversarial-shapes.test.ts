import { describe, expect, it } from "vitest";

import {
  MIDGARD_CEK_ADVERSARIAL_HALF_CAP_SIZES as HALF_CAP_SIZES,
  MIDGARD_CEK_ADVERSARIAL_SHAPES,
  prepareMidgardCekShape,
} from "./helpers/cek-adversarial-shapes.js";
import {
  MIDGARD_CEK_TX_BUDGET,
  runMidgardCekProgram,
} from "./helpers/cek-uplc-terms.js";
import { measureWork } from "./helpers/work-count.js";

/**
 * Verdict-mode scaling gate. Each shape is run at a size `n` and at `2n`,
 * where `2n` exhausts at least 90% of the per-transaction execution budget in
 * memory or CPU. A structural executor whose per-step work is bounded by the
 * nodes the step creates and the bytes of the step's own arguments does the
 * same work per step at `2n` as at `n`. Work is counted twice, deterministically
 * (`measureWork`): the bytes the run hashes and the reads it makes of the
 * semantic store. The at-`2n` run may do at most 5% more of either per step
 * than the at-`n` run, so re-hashing a growing value and walking a growing
 * list on each step both fail. Wall-clock time at the cap is measured outside
 * the gate, by `bench/cek-adversarial-shapes.bench.test.ts`, which also sees
 * work neither counter counts.
 *
 * The appendByteString and deep-continuation shapes build no Data, list or
 * pair value, so they read the semantic store zero times and its check holds
 * trivially; their hashing is gated as for every other shape.
 *
 * The deep-environment shape is not gated here: the test's Flat encoder
 * cannot author it at a size near the budget cap.
 */

const MAX_WORK_PER_STEP_GROWTH = 1.05;

type Measured = {
  readonly hashedBytes: number;
  readonly storeReads: number;
  readonly stepCount: number;
  readonly halted: boolean;
  readonly budgetShare: number;
};

const measureVerdict = async (label: string, n: number): Promise<Measured> => {
  const shape = MIDGARD_CEK_ADVERSARIAL_SHAPES.find(
    (entry) => entry.label === label,
  );
  if (shape === undefined) throw new Error(`unknown shape ${label}`);
  const prepared = prepareMidgardCekShape(shape, n);
  const {
    result: run,
    hashedBytes,
    storeReads,
  } = await measureWork(() => runMidgardCekProgram(prepared, "verdict"));
  const memory = Number(run.terminalState.memory);
  const cpu = Number(run.terminalState.cpu);
  return {
    hashedBytes,
    storeReads,
    stepCount: run.stepCount,
    halted:
      run.stopReason === "halted" && run.terminalState.mode === "haltSuccess",
    budgetShare: Math.max(
      memory / Number(MIDGARD_CEK_TX_BUDGET.memory),
      cpu / Number(MIDGARD_CEK_TX_BUDGET.cpu),
    ),
  };
};

describe("structural executor verdict-mode scaling", () => {
  it("gates every adversarial shape but the deep environment", () => {
    expect(
      MIDGARD_CEK_ADVERSARIAL_SHAPES.map((shape) => shape.label).filter(
        (label) => HALF_CAP_SIZES[label] === undefined,
      ),
    ).toEqual(["deep environment"]);
  });

  for (const [label, n] of Object.entries(HALF_CAP_SIZES)) {
    // Two runs, the larger one near the budget cap: a few seconds on a
    // developer machine and several times that on a loaded CI runner.
    it(
      `${label} scales linearly up to the budget cap`,
      { timeout: 180_000 },
      async () => {
        const half = await measureVerdict(label, n);
        const full = await measureVerdict(label, 2 * n);
        expect(half.halted).toBe(true);
        expect(full.halted).toBe(true);
        expect(full.budgetShare).toBeGreaterThanOrEqual(0.9);
        expect(full.budgetShare).toBeLessThanOrEqual(1);
        expect(half.hashedBytes).toBeGreaterThan(0);
        expect(full.stepCount).toBeGreaterThan(half.stepCount);
        expect(full.hashedBytes / full.stepCount).toBeLessThanOrEqual(
          MAX_WORK_PER_STEP_GROWTH * (half.hashedBytes / half.stepCount),
        );
        expect(full.storeReads / full.stepCount).toBeLessThanOrEqual(
          MAX_WORK_PER_STEP_GROWTH * (half.storeReads / half.stepCount),
        );
      },
    );
  }
});
