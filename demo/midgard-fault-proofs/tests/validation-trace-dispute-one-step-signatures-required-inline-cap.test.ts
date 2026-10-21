/** The validation-trace dispute runner at the signatures step that reads
 * the first required signer of a failing transaction listing its signing
 * owner again and again, a field of 30-byte key hashes the prepare and
 * semantic moves carry whole, inline: at, one item over, and far over the
 * inline signatures limit for a required signer
 * (support/validation-trace-dispute-one-step-inline-cap.ts). */
import { VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
import { describeInlineCap } from "./support/validation-trace-dispute-one-step-inline-cap.js";

describeInlineCap({
  field: "required-signer field",
  phase: "signatures",
  semantic: 2,
  at: "signaturesRequiredItemSemantic",
  evidence: () => ({ requiredSigner: true }),
  limit: VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_MAX_PREIMAGE_BYTES,
  itemBytes: 30,
  // Each required signer past the first adds a canonical-decode step before
  // the signatures phase, whose address witness comes first.
  stepOf: (items) => items + 12,
  installs: (items) => ({ failingSigners: { requiredOwner: items } }),
});
