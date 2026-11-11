/** The validation-trace dispute runner at a writeBits failure (arm 36, core
 * group 7) over 200 indices, past the 64 the direct route takes, the last
 * outside the input: group 7's roots route it to the writeBits loop, which
 * walks the indices in three hops to the one outside the input, from Init to
 * the award (support/validation-trace-dispute-one-step-loop-case.ts). The
 * step reads no staging. */
import { describe } from "vitest";

import { WRITE_BITS_FAILURE_SCRIPT } from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a writeBits failure proved by the writeBits loop", () =>
  loopCase({
    hex: WRITE_BITS_FAILURE_SCRIPT,
    failure: "writeBits",
    hops: 3,
    stagedBytes: [],
  }));
