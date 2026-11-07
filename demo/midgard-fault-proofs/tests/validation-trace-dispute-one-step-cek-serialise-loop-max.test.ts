/** The validation-trace dispute runner at a serialiseData step (arm 31,
 * semantics group 10) at the longest result a transaction builds and
 * serialises within its budget: a list of six 62,384-byte byte strings,
 * 386,018 bytes of CBOR (six of 62,386 bytes take the run past the budget).
 * The serialise loop proves it in 32 hops over its staged result, from Init
 * to the award and the spend back of the staging outputs
 * (support/validation-trace-dispute-one-step-loop-case.ts), the failing
 * transaction declaring the whole admitted execution budget. */
import { describe } from "vitest";

import { ADMITTED_TX_EX_UNITS } from "./helpers/plutus-execution-scripts.js";
import { serialiseListScript } from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a serialiseData step at the longest result a transaction serialises", () =>
  loopCase({
    hex: serialiseListScript(6, 62_384),
    exUnits: ADMITTED_TX_EX_UNITS,
    group: 10,
    hops: 32,
    stagedBytes: [386_018],
  }));
