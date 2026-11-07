/** The validation-trace dispute runner at the script-sources stage-one
 * redeemer step, which reads the redeemer field of a failing transaction
 * whose one redeemer is grown a byte at a time, a field the prepare and
 * semantic moves carry whole, inline: at, one byte over, and far over the
 * inline script-sources limit for the redeemer field, up to the consensus
 * bound on a field, read from publications under the field-preimage
 * certificate (support/validation-trace-dispute-one-step-inline-cap.ts). */
import { MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";

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
  // The canonical decode of a field this large takes more steps before
  // the stage: 8 more at the consensus field's largest.
  stepOf: (bytes) =>
    bytes < MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES ? 48 : 56,
  installs: (bytes) => ({
    failingRedeemerData: redeemerDataForFieldPreimage(bytes),
  }),
  // Each reference input adds 14 steps before the stage; the evidence
  // there carries 6 bytes more, and its state indices 2.
  coShape: {
    label: "510 reference inputs",
    installs: { failingReferenceInputs: 510 },
    steps: 14 * 510,
    besideBytes: 8,
  },
  overAt: [
    [
      "at the consensus field's largest",
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    ],
  ],
});
