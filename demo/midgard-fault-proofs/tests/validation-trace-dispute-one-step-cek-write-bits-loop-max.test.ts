/** The validation-trace dispute runner at a writeBits step (arm 31,
 * semantics group 12) over the longest index list a transaction builds,
 * 57,049 indices (28,761 in a list constant filling the script witness
 * field and 442 rounds of 64 prepended at run time), proved by the writeBits
 * loop in 839 hops, from Init to the award
 * (support/validation-trace-dispute-one-step-loop-case.ts), the failing
 * transaction declaring the whole admitted execution budget. */
import { describe } from "vitest";

import { ADMITTED_TX_EX_UNITS } from "./helpers/plutus-execution-scripts.js";
import { WRITE_BITS_MAX_SCRIPT } from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a writeBits step over the longest index list a transaction builds", () =>
  loopCase({
    hex: WRITE_BITS_MAX_SCRIPT,
    exUnits: ADMITTED_TX_EX_UNITS,
    group: 12,
    hops: 839,
    stagedBytes: [],
  }));
