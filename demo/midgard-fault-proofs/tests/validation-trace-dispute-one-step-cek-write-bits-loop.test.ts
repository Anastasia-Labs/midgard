/** The validation-trace dispute runner at a writeBits step (arm 31,
 * semantics group 12) over 200 indices, past the 64 the direct route takes,
 * proved by the writeBits loop in three hops, from Init to the award
 * (support/validation-trace-dispute-one-step-loop-case.ts). The step reads
 * no staging. */
import { describe } from "vitest";

import { WRITE_BITS_SCRIPT } from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a writeBits step proved by the writeBits loop", () =>
  loopCase({
    hex: WRITE_BITS_SCRIPT,
    group: 12,
    hops: 3,
    stagedBytes: [],
  }));
