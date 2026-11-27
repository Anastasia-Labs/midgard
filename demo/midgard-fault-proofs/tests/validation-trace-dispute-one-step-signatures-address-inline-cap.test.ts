/** The validation-trace dispute runner at the signatures step that reads
 * the first address witness of a failing transaction its owner and other
 * keys sign, a field of 103-byte witnesses the prepare and semantic moves
 * carry whole, inline, at the inline signatures limit for an address
 * witness; past it, read by reference from one publication of ours or, past
 * one publication's bytes, from publications under the field-preimage
 * certificate, up to the consensus bound on a field
 * (support/validation-trace-dispute-one-step-inline-cap.ts). */
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "@al-ft/midgard-core/codec/native-tx-field-access";

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
  // Each reference input adds 2 steps before the phase; the evidence there
  // carries 3 bytes more, and its state indices 2.
  coShape: {
    label: "510 reference inputs",
    installs: { failingReferenceInputs: 510 },
    steps: 2 * 510,
    besideBytes: 5,
  },
  overAt: [
    ["at one raw publication's largest", MIDGARD_CHUNK_BYTES_K],
    [
      "at the consensus field's largest",
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    ],
  ],
});
