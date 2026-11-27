/** The validation-trace dispute runner at a staged builtin failure (arm 36,
 * core group 7): a decodeUtf8 of a 16,384-byte string, past the staging
 * chunk, that is not UTF-8, whose string group 7's roots route to the staged
 * failure hop to read from our staging, from Init to the award and the spend
 * back of the staging outputs
 * (support/validation-trace-dispute-one-step-loop-case.ts). */
import { describe } from "vitest";

import { DECODE_UTF8_FAILURE_SCRIPT } from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a staged decodeUtf8 failure proved by the staged failure hop", () =>
  loopCase({
    hex: DECODE_UTF8_FAILURE_SCRIPT,
    failure: "staged",
    hops: 1,
    stagedBytes: [16_384],
  }));
