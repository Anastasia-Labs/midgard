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
import { measureHashWork } from "./helpers/hash-work.js";

/**
 * Verdict-mode scaling gate. Each shape is run at a size `n` and at `2n`,
 * where `2n` exhausts at least 90% of the per-transaction execution budget in
 * memory or CPU. A structural executor whose per-step work is bounded by the
 * nodes the step creates and the bytes of the step's own arguments does the
 * same work per step at `2n` as at `n`. Work is counted as the bytes the run
 * hashes (`measureHashWork`), which is deterministic: the at-`2n` run may hash
 * at most 5% more bytes per step than the at-`n` run. Wall-clock time at the
 * cap is measured outside the gate, by `bench/cek-adversarial-shapes.bench.test.ts`.
 *
 * The deep-environment shape is not gated here: the test's Flat encoder
 * cannot author it at a size near the budget cap.
 */

const MAX_WORK_PER_STEP_GROWTH = 1.05;

type Measured = {
  readonly bytes: number;
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
  const { result: run, bytes } = await measureHashWork(() =>
    runMidgardCekProgram(prepared, "verdict"),
  );
  const memory = Number(run.terminalState.memory);
  const cpu = Number(run.terminalState.cpu);
  return {
    bytes,
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
        expect(half.bytes).toBeGreaterThan(0);
        expect(full.stepCount).toBeGreaterThan(half.stepCount);
        expect(full.bytes / full.stepCount).toBeLessThanOrEqual(
          MAX_WORK_PER_STEP_GROWTH * (half.bytes / half.stepCount),
        );
      },
    );
  }
});
