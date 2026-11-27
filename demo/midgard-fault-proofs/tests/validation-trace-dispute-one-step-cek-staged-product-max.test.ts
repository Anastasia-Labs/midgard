/** The validation-trace dispute runner at a staged multiplyInteger step
 * (arm 31, semantics group 9) of the largest integer argument, 2^262143 - 1
 * (32,768 bytes, built at run time), by the 8,192-byte 2^65535: the largest
 * staged operand, proved by the staged product loop in six hops, from Init
 * to the award and the spend back of the staging outputs
 * (support/validation-trace-dispute-one-step-loop-case.ts), the failing
 * transaction declaring the whole admitted execution budget. */
import { describe } from "vitest";

import { ADMITTED_TX_EX_UNITS } from "./helpers/plutus-execution-scripts.js";
import { PRODUCT_MAX_SCRIPT } from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a staged multiplyInteger step of the largest integer argument", () =>
  loopCase({
    hex: PRODUCT_MAX_SCRIPT,
    exUnits: ADMITTED_TX_EX_UNITS,
    group: 9,
    hops: 6,
    // The two factors' CBOR and the product's, which the loop compares.
    stagedBytes: [33_795, 8_451, 42_243],
  }));
