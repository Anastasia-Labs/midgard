/**
 * The honest operator's root at the shape of the accepted L2 transaction
 * whose fault proof takes the most steps
 * (submit-init-emulator-transition-trace-final-maximum-lifecycle.test.ts) is
 * refused at the terminal comparison. In a file of its own so that it plays
 * in parallel with the conviction at the same shape.
 */
import { describe, it } from "vitest";

import { transitionTraceFinalCaseRunner } from "./support/transition-trace-final-cases.register-transition-trace-final-cases.js";
import {
  MAXIMUM,
  MAXIMUM_LIFECYCLE_TIMEOUT,
} from "./support/transition-trace-final-maximum-lifecycle.js";

const run = transitionTraceFinalCaseRunner();

describe("the longest accepted-transaction fault proof", () => {
  it(
    "refuses the honest operator's root at the terminal comparison at the maximum shape",
    () => run({ ...MAXIMUM, honest: true }),
    MAXIMUM_LIFECYCLE_TIMEOUT,
  );
});
