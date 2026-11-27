/** The validation-trace dispute runner from Init to conviction at the
 * deepest trace admission takes, the failing transaction's redeemer field at
 * the inline observe limit and the operator's thread at the trace's last CEK
 * compute step (validation-trace-dispute-max-shape-case.ts). The same
 * transaction in an honest block, which stamps it TxIsInvalid, starts no
 * workflow and signs nothing. */
import { describe } from "vitest";

import { VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-canonical-decode.js";
import { ADMITTED_TX_EX_UNITS } from "./helpers/plutus-execution-block-fixture.js";
import {
  deepestCase,
  honestBlock,
  LONGEST_FAILING_SCRIPT,
} from "./support/validation-trace-dispute-max-shape-case.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";

describe("validation-trace dispute runner at the deepest trace admission takes", () => {
  deepestCase({
    group: 0,
    successor: {
      fieldBytes: VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES,
    },
  });

  honestBlock({
    failingScript: LONGEST_FAILING_SCRIPT,
    failingRedeemerData: redeemerDataForFieldPreimage(
      VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES,
      ADMITTED_TX_EX_UNITS,
    ),
  });
});
