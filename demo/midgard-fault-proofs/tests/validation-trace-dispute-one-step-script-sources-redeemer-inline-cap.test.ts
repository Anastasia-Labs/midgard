/** The validation-trace dispute runner at the script-sources stage-one
 * redeemer step, which reads the redeemer field of a failing transaction
 * whose one redeemer is grown a byte at a time, a field the prepare and
 * semantic moves carry whole, inline: at, one byte over, and far over the
 * inline script-sources limit for the redeemer field
 * (support/validation-trace-dispute-one-step-inline-cap.ts). */
import { VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
import { describeInlineCap } from "./support/validation-trace-dispute-one-step-inline-cap.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";

describeInlineCap({
  field: "redeemer field",
  phase: "scriptSources",
  semantic: 15,
  at: "scriptSourcesStageOneRedeemerSemantic",
  evidence: () => ({ auxiliary: 27 }),
  limit: VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_MAX_PREIMAGE_BYTES,
  unit: "byte",
  fieldBytes: (bytes) => bytes,
  stepOf: () => 48,
  installs: (bytes) => ({
    failingRedeemerData: redeemerDataForFieldPreimage(bytes),
  }),
});
