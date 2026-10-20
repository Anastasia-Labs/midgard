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
import { measureHashWork } from "./helpers/hash-work.js";
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
 * the trace into an O(log n) frontier, so its work per step does not grow with
 * the trace. Work is counted as the bytes the commit hashes
 * (`measureHashWork`), which is deterministic: the `2n` commit may hash at
 * most 5% more bytes per step than the `n` commit. Wall-clock time at the cap
 * is measured outside the gate, by
 * `bench/validation-trace-commit.bench.test.ts`.
 */

const MAX_WORK_PER_STEP_GROWTH = 1.05;

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

type Measured = {
  readonly bytes: number;
  readonly stepCount: number;
  readonly accepted: boolean;
};

const measureCommit = async (
  input: ValidationMachineReplayInput,
): Promise<Measured> => {
  const { result: commitment, bytes } = await measureHashWork(() =>
    Effect.runPromise(commitValidationTraceV1(input)),
  );
  return {
    bytes,
    stepCount: commitment.descriptor.stepCount,
    accepted: commitment.descriptor.verdict === "accepted",
  };
};

describe("validation trace commit-mode scaling", () => {
  for (const [label, n] of Object.entries(
    MIDGARD_CEK_ADVERSARIAL_HALF_CAP_SIZES,
  )) {
    // Two commits, the larger one near the budget cap: about ten seconds on
    // a developer machine and several times that on a loaded CI runner.
    it(
      `${label} commits linearly up to the budget cap`,
      { timeout: 300_000 },
      async () => {
        const half = await measureCommit(await shapeSpendInput(label, n));
        const full = await measureCommit(await shapeSpendInput(label, 2 * n));
        expect(half.accepted).toBe(true);
        expect(full.accepted).toBe(true);
        expect(half.bytes).toBeGreaterThan(0);
        expect(full.stepCount).toBeGreaterThan(half.stepCount);
        expect(full.bytes / full.stepCount).toBeLessThanOrEqual(
          MAX_WORK_PER_STEP_GROWTH * (half.bytes / half.stepCount),
        );
      },
    );
  }
});
