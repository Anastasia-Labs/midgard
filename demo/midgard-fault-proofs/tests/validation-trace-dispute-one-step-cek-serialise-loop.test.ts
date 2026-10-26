/** The validation-trace dispute runner at a serialiseData step (arm 31,
 * semantics group 10) over a 16,384-byte byte string built at run time,
 * proved by the serialise loop in two hops over its staged result, from Init
 * to the award and the spend back of the staging outputs
 * (support/validation-trace-dispute-one-step-loop-case.ts). */
import { describe } from "vitest";

import { SERIALISE_SCRIPT } from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a serialiseData step proved by the serialise loop", () =>
  loopCase({
    hex: SERIALISE_SCRIPT,
    group: 10,
    hops: 2,
    // The result's CBOR: 256 chunks of 64 bytes, indefinite length.
    stagedBytes: [16_898],
  }));
