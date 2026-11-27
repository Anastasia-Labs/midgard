/** The validation-trace dispute runner at staged steps of semantics group 9
 * (arm 31), each proved by the staged product loop in two hops, from Init to
 * the award and the spend back of the staging outputs
 * (support/validation-trace-dispute-one-step-loop-case.ts): a
 * multiplyInteger of a 16,000 by 8,000-byte product, and a modInteger of a
 * 16,000-byte dividend by a negative 8,000-byte divisor, whose negative
 * result the loop reads with the signs reversed and one carried in. */
import { describe } from "vitest";

import {
  DIVISION_SCRIPT,
  PRODUCT_SCRIPT,
} from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a staged multiplyInteger step proved by the staged product loop", () =>
  loopCase({
    hex: PRODUCT_SCRIPT,
    group: 9,
    hops: 2,
    // The two factors' CBOR and the product's, which the loop compares.
    stagedBytes: [16_503, 8_253, 24_753],
  }));

describe("validation-trace dispute runner: a staged modInteger step with a negative result proved by the staged product loop", () =>
  loopCase({
    hex: DIVISION_SCRIPT,
    group: 9,
    hops: 2,
    // The dividend's and the divisor's CBOR, and the quotient's, which is
    // the result's too and is staged once.
    stagedBytes: [16_503, 8_253, 8_253],
  }));
