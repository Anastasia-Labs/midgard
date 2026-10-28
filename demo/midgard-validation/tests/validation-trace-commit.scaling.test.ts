import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import {
  commitValidationTraceV1,
  type ValidationMachineReplayInput,
} from "../src/index.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { UPLCProgram } from "../src/uplc-term.js";
import {
  MIDGARD_CEK_ADVERSARIAL_HALF_CAP_SIZES,
  MIDGARD_CEK_ADVERSARIAL_SHAPES,
} from "./helpers/cek-adversarial-shapes.js";
import { app, lam, seq, v } from "./helpers/cek-uplc-terms.js";
import {
  MIDGARD_TX_EX_UNITS_CAP,
  plutusV3SpendInput,
} from "./helpers/plutus-spend-input.js";
import { measureWork } from "./helpers/work-count.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

/**
 * Commit-mode scaling gate. Each adversarial shape is the validator of a
 * PlutusV3 spend whose redeemer declares the per-transaction execution-unit
 * cap, and the transaction's trace is committed in streaming commit mode at
 * shape size `n` and `2n` (near the cap). Commit mode hashes every state of
 * the trace into an O(log n) frontier, so its work per step does not grow with
 * the trace. Work is counted twice, deterministically (`measureWork`): the
 * bytes the commit hashes and the reads it makes of the CEK semantic store.
 * The `2n` commit may do at most 5% more of either per step than the `n`
 * commit. Wall-clock time at the cap is measured outside the gate, by
 * `bench/validation-trace-commit.bench.test.ts`, which also sees work neither
 * counter counts. The appendByteString and deep-continuation shapes read the
 * store zero times, so its check holds trivially for them.
 *
 * This gate guards the commitment's per-step cost. Its hashing count is about
 * twenty times less sensitive to an executor regression than the one in
 * `cek-executor.adversarial-shapes.test.ts`: a commit hashes about 1.7 KB per
 * step, nearly all of it state hashing, so an executor regression must add
 * about 84 bytes per step to cross 5% here, against about 4 bytes there. The
 * verdict-mode gate is the executor's guard.
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
  readonly hashedBytes: number;
  readonly storeReads: number;
  readonly stepCount: number;
  readonly accepted: boolean;
};

const measureCommit = async (
  input: ValidationMachineReplayInput,
): Promise<Measured> => {
  const {
    result: commitment,
    hashedBytes,
    storeReads,
  } = await measureWork(() =>
    Effect.runPromise(commitValidationTraceV1(input)),
  );
  return {
    hashedBytes,
    storeReads,
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
