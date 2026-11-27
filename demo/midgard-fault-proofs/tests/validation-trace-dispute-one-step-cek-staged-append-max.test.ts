/** The validation-trace dispute runner at a staged appendByteString step
 * (arm 31, semantics group 7) over two operands of 65,536 bytes, each as long
 * as staging takes, from Init to the award and the spend back of the staging
 * outputs (support/validation-trace-dispute-one-step-staged-max-case.ts). */
import { describe } from "vitest";

import { stagedMaxCase } from "./support/validation-trace-dispute-one-step-staged-max-case.js";
import { STAGED_APPEND_MAX_SCRIPT } from "./support/validation-trace-dispute-staged-scripts.js";

describe("validation-trace dispute runner: a staged appendByteString step over two operands as long as staging takes", () =>
  stagedMaxCase({
    hex: STAGED_APPEND_MAX_SCRIPT,
    tag: 10n,
    operandBytes: [65_536, 65_536],
  }));
