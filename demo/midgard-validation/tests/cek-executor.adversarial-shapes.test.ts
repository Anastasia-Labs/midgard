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

/**
 * Verdict-mode scaling gate. Each shape is run at a size `n` and at `2n`,
 * where `2n` exhausts at least 90% of the per-transaction execution budget in
 * memory or CPU. A structural executor whose per-step work is bounded by the
 * nodes the step creates and the bytes of the step's own arguments runs the
 * `2n` program in at most 2.3 times the `n` program's time, and the at-cap
 * program in at most 10 s (a quarter of the projected commit cap). Each
 * size is timed up to three times and the fastest run counts, so a
 * transiently loaded machine does not fail the gate.
 *
 * The deep-environment shape is not gated here: the test's Flat encoder
 * cannot author it at a size near the budget cap.
 */

const MAX_RATIO = 2.3;
const MAX_AT_CAP_MS = 10_000;
const ATTEMPTS = 3;

type Timed = {
  readonly ms: number;
  readonly halted: boolean;
  readonly budgetShare: number;
};

const timeVerdict = (label: string, n: number): Timed => {
  const shape = MIDGARD_CEK_ADVERSARIAL_SHAPES.find(
    (entry) => entry.label === label,
  );
  if (shape === undefined) throw new Error(`unknown shape ${label}`);
  const prepared = prepareMidgardCekShape(shape, n);
  const started = performance.now();
  const run = runMidgardCekProgram(prepared, "verdict");
  const ms = performance.now() - started;
  const memory = Number(run.terminalState.memory);
  const cpu = Number(run.terminalState.cpu);
  return {
    ms,
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
    it(
      `${label} scales linearly up to the budget cap`,
      {
        timeout: 180_000,
      },
      () => {
        let best: { half: Timed; full: Timed } | undefined;
        for (let attempt = 0; attempt < ATTEMPTS; attempt += 1) {
          const half = timeVerdict(label, n);
          const full = timeVerdict(label, 2 * n);
          best = {
            half:
              best === undefined || half.ms < best.half.ms ? half : best.half,
            full:
              best === undefined || full.ms < best.full.ms ? full : best.full,
          };
          if (
            best.full.ms <= MAX_RATIO * best.half.ms &&
            best.full.ms <= MAX_AT_CAP_MS
          ) {
            break;
          }
        }
        expect(best!.half.halted).toBe(true);
        expect(best!.full.halted).toBe(true);
        expect(best!.full.budgetShare).toBeGreaterThanOrEqual(0.9);
        expect(best!.full.budgetShare).toBeLessThanOrEqual(1);
        expect(best!.full.ms).toBeLessThanOrEqual(MAX_RATIO * best!.half.ms);
        expect(best!.full.ms).toBeLessThanOrEqual(MAX_AT_CAP_MS);
      },
    );
  }
});
