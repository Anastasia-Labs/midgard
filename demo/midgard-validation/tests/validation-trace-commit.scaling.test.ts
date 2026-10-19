import { UPLCConst, UPLCProgram } from "@harmoniclabs/uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import {
  commitValidationTraceV1,
  type ValidationMachineReplayInput,
} from "../src/index.js";
import {
  MIDGARD_CEK_ADVERSARIAL_HALF_CAP_SIZES,
  MIDGARD_CEK_ADVERSARIAL_SHAPES,
} from "./helpers/cek-adversarial-shapes.js";
import { app, lam, seq, v } from "./helpers/cek-uplc-terms.js";
import {
  MIDGARD_TX_EX_UNITS_CAP,
  plutusV3SpendInput,
} from "./helpers/plutus-spend-input.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

/**
 * Commit-mode scaling gate. Each adversarial shape is the validator of a
 * PlutusV3 spend whose redeemer declares the per-transaction execution-unit
 * cap, and the transaction's trace is committed in streaming commit mode at
 * shape size `n` and `2n` (near the cap). Commit mode hashes every state of
 * the trace, so its time is linear in the step count: the `2n` commit takes
 * at most 2.3 times the `n` commit, and at most 10 s. Each size is timed up
 * to three times and the fastest run counts.
 */

const MAX_RATIO = 2.3;
const MAX_AT_CAP_MS = 10_000;
const ATTEMPTS = 3;

/** The shape's script, returning unit so the spend is accepted. */
const shapeSpendInput = (
  label: string,
  n: number,
): Promise<ValidationMachineReplayInput> => {
  const shape = MIDGARD_CEK_ADVERSARIAL_SHAPES.find(
    (entry) => entry.label === label,
  );
  if (shape === undefined) throw new Error(`unknown shape ${label}`);
  const script = lam(seq(app(shape.term(n), v(0)), UPLCConst.unit));
  return plutusV3SpendInput(
    buildCekProgramFromRawFlat(
      encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], script)),
    ),
    MIDGARD_TX_EX_UNITS_CAP,
  );
};

type Timed = {
  readonly ms: number;
  readonly stepCount: number;
  readonly accepted: boolean;
};

const timeCommit = async (
  input: ValidationMachineReplayInput,
): Promise<Timed> => {
  const started = performance.now();
  const commitment = await Effect.runPromise(commitValidationTraceV1(input));
  return {
    ms: performance.now() - started,
    stepCount: commitment.descriptor.stepCount,
    accepted: commitment.descriptor.verdict === "accepted",
  };
};

describe("validation trace commit-mode scaling", () => {
  for (const [label, n] of Object.entries(
    MIDGARD_CEK_ADVERSARIAL_HALF_CAP_SIZES,
  )) {
    it(
      `${label} commits linearly up to the budget cap`,
      { timeout: 300_000 },
      async () => {
        const halfInput = await shapeSpendInput(label, n);
        const fullInput = await shapeSpendInput(label, 2 * n);
        let best: { half: Timed; full: Timed } | undefined;
        for (let attempt = 0; attempt < ATTEMPTS; attempt += 1) {
          const half = await timeCommit(halfInput);
          const full = await timeCommit(fullInput);
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
        // The measured basis of the commit wall cap, kept in the test log.
        console.info(
          JSON.stringify({
            gate: "commit-mode scaling",
            label,
            n,
            halfSteps: best!.half.stepCount,
            fullSteps: best!.full.stepCount,
            halfMs: Math.round(best!.half.ms),
            fullMs: Math.round(best!.full.ms),
            ratio: Number((best!.full.ms / best!.half.ms).toFixed(2)),
          }),
        );
        expect(best!.half.accepted).toBe(true);
        expect(best!.full.accepted).toBe(true);
        expect(best!.full.stepCount).toBeGreaterThan(best!.half.stepCount);
        expect(best!.full.ms).toBeLessThanOrEqual(MAX_RATIO * best!.half.ms);
        expect(best!.full.ms).toBeLessThanOrEqual(MAX_AT_CAP_MS);
      },
    );
  }
});
