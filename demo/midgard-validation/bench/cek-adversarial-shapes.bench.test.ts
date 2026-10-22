import { describe, expect, it } from "vitest";

import {
  MIDGARD_CEK_ADVERSARIAL_HALF_CAP_SIZES as HALF_CAP_SIZES,
  MIDGARD_CEK_ADVERSARIAL_SHAPES,
  prepareMidgardCekShape,
} from "../tests/helpers/cek-adversarial-shapes.js";
import {
  MIDGARD_CEK_TX_BUDGET,
  runMidgardCekProgram,
} from "../tests/helpers/cek-uplc-terms.js";

/**
 * Verdict-mode wall-clock benchmark, run by `pnpm bench`, not by `test`.
 * Each shape is run at a size `n` and at `2n`, where `2n` exhausts at least
 * 90% of the per-transaction execution budget. The at-cap program should run
 * in at most 2.3 times the `n` program's time and in at most 10 s (a quarter
 * of the projected commit cap) on a developer machine. Each size is timed up
 * to three times and the fastest run counts.
 *
 * Wall-clock time depends on the machine and its load, so this is not a
 * gate: `tests/cek-executor.adversarial-shapes.test.ts` gates the same shapes
 * on deterministic hashing work per step.
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

describe("structural executor verdict-mode wall clock", () => {
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
