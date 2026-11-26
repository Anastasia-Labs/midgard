/**
 * `zero-input` step-02 submitter — the step that concludes the proof.
 *
 * The step opens §2.5 field 0 of the disputed transaction through the door and
 * asserts the *authenticated item count* is zero, which is why this builder
 * takes the disputed transaction's compact CBOR: under §4's plain hashing the
 * empty commitment is the same 32 bytes for every field of every transaction,
 * so a hash equality would prove only that *some* field was empty.
 *
 * The pre-flight below makes the same check off-chain: "does field 0 of *this
 * anchored transaction* open to no items", and it is
 * {@link planFaultProofFieldOpening} that ties the bytes to the slot.
 */

import "@al-ft/midgard-sdk/core";
import "@al-ft/midgard-sdk/state-queue";
import "@al-ft/midgard-sdk/fraud-proof";
import "@lucid-evolution/lucid";
import "./field-opening.js";
import "./runtime.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-zero-input-step-02.make-zero-input-step02-spend-redeemer.js";
import "./submit-zero-input-step-02.submit-zero-input-step02-v1.js";
export { buildZeroInputStep02V1 } from "./submit-zero-input-step-02.submit-zero-input-step02-v1.js";
