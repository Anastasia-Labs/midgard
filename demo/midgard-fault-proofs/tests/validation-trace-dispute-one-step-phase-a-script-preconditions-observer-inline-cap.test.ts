/** The validation-trace dispute runner at the phase-A script-preconditions
 * step that reads the first required observer of a failing transaction
 * requiring more and more native scripts it witnesses, a field of 28-byte
 * script hashes the prepare and semantic moves carry whole, inline: at, one
 * item over, and far over the inline limit for a required observer
 * (support/validation-trace-dispute-one-step-inline-cap.ts). */
import { VALIDATION_TRACE_REQUIRED_OBSERVER_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
import { observedBy } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describeInlineCap } from "./support/validation-trace-dispute-one-step-inline-cap.js";

describeInlineCap({
  field: "required-observer field",
  phase: "phaseAScriptPreconditions",
  semantic: 1,
  at: "phaseAScriptPreconditionsItemSemantic",
  evidence: () => ({ fieldIndex: 3, itemIndex: 0 }),
  limit: VALIDATION_TRACE_REQUIRED_OBSERVER_INLINE_MAX_PREIMAGE_BYTES,
  itemBytes: 30,
  // Each observer past the first adds its native script's steps and a
  // decode step before the phase.
  stepOf: (items) => 27 + 9 * (items - 1),
  installs: observedBy,
  // Each reference input adds 2 steps before the phase; the evidence there
  // carries 2 bytes more.
  coShape: {
    label: "510 reference inputs",
    installs: { failingReferenceInputs: 510 },
    steps: 2 * 510,
    besideBytes: 2,
  },
});
