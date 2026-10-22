/** The validation-trace dispute runner from Init to conviction at the
 * deepest trace admission takes, the failing transaction's redeemer field one
 * byte over the inline observe limit, so the observe move reads a published
 * copy, and the operator's thread at the trace's last CEK machine step
 * (validation-trace-dispute-max-shape-case.ts). */
import { describe } from "vitest";

import { VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-canonical-decode.js";
import { deepestCase } from "./support/validation-trace-dispute-max-shape-case.js";

describe("validation-trace dispute runner at the deepest trace admission takes", () => {
  deepestCase({
    fieldBytes: VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES + 1,
    group: 1,
  });
});
