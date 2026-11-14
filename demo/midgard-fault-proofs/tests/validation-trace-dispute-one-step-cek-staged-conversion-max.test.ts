/** The validation-trace dispute runner at a staged byteStringToInteger step
 * (arm 31, semantics group 11) over the longest input a transaction builds
 * and converts within its budget, 49,784 bytes (49,785 take the run past
 * it), proved by the staged conversion stage, from Init to the award and the
 * spend back of the staging outputs
 * (support/validation-trace-dispute-one-step-loop-case.ts), the failing
 * transaction declaring the whole admitted execution budget. */
import { describe } from "vitest";

import { ADMITTED_TX_EX_UNITS } from "./helpers/plutus-execution-scripts.js";
import { conversionScript } from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a staged byteStringToInteger step over the longest input a transaction converts", () =>
  loopCase({
    hex: conversionScript(49_784),
    exUnits: ADMITTED_TX_EX_UNITS,
    group: 11,
    hops: 1,
    stagedBytes: [49_784],
  }));
