/** The validation-trace dispute runner at the script-sources stage-seven
 * step that reads the first required observer of a failing transaction
 * requiring more and more native scripts it witnesses, a field of 28-byte
 * script hashes the prepare and semantic moves carry whole, inline: at, one
 * item over, and far over the inline limit for a script-sources observer
 * (support/validation-trace-dispute-one-step-inline-cap.ts). */
import { VALIDATION_TRACE_SCRIPT_SOURCES_OBSERVER_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
import {
  firstObserverHash,
  observedBy,
} from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describeInlineCap } from "./support/validation-trace-dispute-one-step-inline-cap.js";

describeInlineCap({
  field: "required-observer field",
  phase: "scriptSources",
  semantic: 25,
  at: "scriptSourcesStageSevenObserverSemantic",
  evidence: (items) => ({
    fieldIndex: 3,
    itemIndex: 0,
    claims: [firstObserverHash(items), BigInt(items)],
    yieldFields: ["scriptSourcesObserverItem", "scriptSourcesObserverBound"],
  }),
  limit: VALIDATION_TRACE_SCRIPT_SOURCES_OBSERVER_INLINE_MAX_PREIMAGE_BYTES,
  itemBytes: 30,
  // Each observer past the first adds its native script's steps, a decode
  // step and its script-sources steps before the stage.
  stepOf: (items) => 102 + 26 * (items - 1),
  installs: observedBy,
  // Each reference input adds 15 steps before the stage, and grows what
  // the stage's control carries beside the field: 303 bytes more there.
  coShape: {
    label: "510 reference inputs",
    installs: { failingReferenceInputs: 510 },
    steps: 15 * 510,
    besideBytes: 304,
  },
});
