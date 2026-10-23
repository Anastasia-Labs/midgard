import { Builtin } from "@harmoniclabs/uplc";
import { describe, it } from "vitest";

import type { Budget } from "./helpers/cek-staged-failure-charge.js";
import {
  bool,
  constant,
  expectStagedFailureProved,
  integer,
  small,
} from "./helpers/cek-staged-failure-rows.js";

/**
 * integerToByteString of width 0 over an input of 65,536 bytes, the longest
 * operand of the staged failure rows
 * (cek-executor.staged-bitwise-failures.test.ts), fails after its charge as
 * the PV11 evaluator charges it. Building the input at run time passes the
 * transaction budget, so the run has a larger one. Its conversions take
 * seconds, so it runs in its own file.
 */

const INTEGER_TO_BYTES = 73n;
const LONGEST = 65_536;

/** A budget the conversion of the longest input to an integer fits. */
const RAISED_BUDGET: Budget = { cpu: 40_000_000_000n, memory: 16_500_000n };

describe("an integerToByteString step over the longest staged input", () => {
  it("is proved with the PV11 evaluator's verdict and charge", () => {
    expectStagedFailureProved(
      INTEGER_TO_BYTES,
      Builtin.integerToByteString,
      [constant(bool(false)), small(0n), integer(LONGEST)],
      "evaluator",
      RAISED_BUDGET,
    );
  });
});
