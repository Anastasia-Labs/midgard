/** The validation-trace dispute runner at a staged sha2_256 step (arm 31,
 * semantics group 7) over an operand of 65,536 bytes, as long as staging
 * takes, from Init to the award and the spend back of the staging outputs
 * (support/validation-trace-dispute-one-step-staged-max-case.ts). */
import { describe } from "vitest";

import { stagedMaxCase } from "./support/validation-trace-dispute-one-step-staged-max-case.js";
import { STAGED_SHA2_MAX_SCRIPT } from "./support/validation-trace-dispute-staged-scripts.js";

describe("validation-trace dispute runner: a staged sha2_256 step over an operand as long as staging takes", () =>
  stagedMaxCase({
    hex: STAGED_SHA2_MAX_SCRIPT,
    tag: 18n,
    operandBytes: [65_536],
  }));
