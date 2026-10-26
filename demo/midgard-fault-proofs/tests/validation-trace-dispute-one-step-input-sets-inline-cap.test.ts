/** The validation-trace dispute runner at an input-sets step whose item
 * belongs to the failing transaction's reference-input field, a field of
 * 40-byte out-refs the prepare and semantic moves carry whole, inline: at,
 * one item over, and far over the inline input-sets limit
 * (support/validation-trace-dispute-one-step-inline-cap.ts). */
import { VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
import { describeInlineCap } from "./support/validation-trace-dispute-one-step-inline-cap.js";

describeInlineCap({
  field: "reference-input field",
  phase: "inputSets",
  semantic: 1,
  at: "inputSetsItemSemantic",
  // The input-sets merge seeds from the spend field, then opens the
  // reference field's last item; its control fixes the item, so the
  // evidence names the field alone.
  evidence: () => ({ fieldIndex: 1 }),
  limit: VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES,
  itemBytes: 40,
  stepOf: (items) => items + 11,
  installs: (items) => ({ failingReferenceInputs: items }),
});
