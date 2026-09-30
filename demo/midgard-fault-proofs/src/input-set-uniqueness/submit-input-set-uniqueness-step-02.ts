/**
 * `input-set-uniqueness` step-02 submitter — the step that both concludes and
 * finalizes the proof: the claimed items are opened through the §8.8 door
 * against the thread's anchored transaction id, the byte-equality conviction
 * is re-checked locally, and then the computation-thread NFT burns while the
 * permanent fraud-proof token mints to the fraud-proof address.
 *
 * Three claim arms, mirroring the validator:
 *
 * - `duplicateSpendInputs` / `duplicateReferenceInputs` open one field (0 or
 *   1) via `opened_field_view` and compare two of its §5.3 items at
 *   `first_index < second_index`.
 * - `spendReferenceOverlap` pays the §3 anchor once (`anchored_native_tx`),
 *   opens both fields against it, and compares one item of each — no index
 *   relation, since the same position in two different lists is only a fault
 *   when the out-refs match.
 *
 * Every conviction predicate is twinned locally fail-closed before anything
 * is paid for: indices in range, `first < second` where the arm requires it,
 * and byte equality of the claimed items — §2.5 fields 0/1 share the §5.3
 * out-ref item encoding, so item byte equality *is* out-ref equality.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "../field-opening.js";
import "../runtime.js";
import "../step-support.js";
import "../tx-layout.js";
import "../witness-reference-scripts.js";
import "../workflow/transaction-boundary.js";
import "./submit-common.js";
import "./submit-input-set-uniqueness-step-02.assert-input-set-uniqueness-claim-convicts.js";
import "./submit-input-set-uniqueness-step-02.submit-input-set-uniqueness-step02.js";
export {
  assertInputSetUniquenessClaimConvicts,
  type SubmitInputSetUniquenessStep02Result,
} from "./submit-input-set-uniqueness-step-02.assert-input-set-uniqueness-claim-convicts.js";
export { submitInputSetUniquenessStep02 } from "./submit-input-set-uniqueness-step-02.submit-input-set-uniqueness-step02.js";
