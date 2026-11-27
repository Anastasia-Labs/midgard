/** The validation-trace dispute runner at a staged addInteger step (arm 31,
 * semantics group 8) over a 10,023-byte literal integer operand past the
 * direct constant bound, 10,340 staged bytes, through the stagedInteger arm,
 * from Init to the award and the spend back of the staging outputs
 * (support/validation-trace-dispute-one-step-staged-max-case.ts). */
import { describe } from "vitest";

import { STAGED_INTEGER_ARMS } from "./support/validation-trace-dispute-one-step-staged-drive.js";
import { stagedMaxCase } from "./support/validation-trace-dispute-one-step-staged-max-case.js";
import { STAGED_ADD_LONG_LITERAL_SCRIPT } from "./support/validation-trace-dispute-staged-scripts.js";

describe("validation-trace dispute runner: a staged addInteger step over a 10,023-byte literal integer operand past the direct constant bound", () =>
  stagedMaxCase({
    hex: STAGED_ADD_LONG_LITERAL_SCRIPT,
    tag: 0n,
    operandBytes: [10_340],
    arms: STAGED_INTEGER_ARMS,
  }));
