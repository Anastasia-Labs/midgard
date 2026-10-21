/** The validation-trace dispute runner at the signatures step that reads
 * the first address witness of a failing transaction its owner and other
 * keys sign, a field of 103-byte witnesses the prepare and semantic moves
 * carry whole, inline: at, one item over, and far over the inline
 * signatures limit for an address witness
 * (support/validation-trace-dispute-one-step-inline-cap.ts). */
import { VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
import { describeInlineCap } from "./support/validation-trace-dispute-one-step-inline-cap.js";

describeInlineCap({
  field: "address-witness field",
  phase: "signatures",
  semantic: 1,
  at: "signaturesAddressItemSemantic",
  evidence: () => ({ fieldIndex: 7 }),
  limit: VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_MAX_PREIMAGE_BYTES,
  itemBytes: 103,
  // Each witness past the owner's adds a canonical-decode step before the
  // signatures phase.
  stepOf: (items) => items + 11,
  installs: (items) => ({ failingSigners: { extraSigners: items - 1 } }),
});
