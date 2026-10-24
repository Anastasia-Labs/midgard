/**
 * `zero-input` step-02 submitter — the step that concludes the proof.
 *
 * **Re-derived onto the §8.8 door by #604.** The step used to compare the
 * `spend_inputs_hash` its thread carried against the pinned empty-field
 * constant. It now opens §2.5 field 0 of the disputed transaction through the
 * door and asserts the *authenticated item count* is zero, which is why this
 * builder takes the disputed transaction's compact CBOR: under §4's plain
 * hashing the empty commitment is the same 32 bytes for every field of every
 * transaction, so a hash equality proved only that *some* field was empty.
 *
 * The pre-flight below is the same strengthening off-chain. It no longer asks
 * "is this hash the empty one" but "does field 0 of *this anchored transaction*
 * open to no items", and it is
 * {@link planFaultProofFieldOpening} that ties the bytes to the slot.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "./field-opening.js";
import "./legacy-submission-boundary.js";
import "./runtime.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-zero-input-step-02.make-zero-input-step02-spend-redeemer.js";
import "./submit-zero-input-step-02.submit-zero-input-step02-v1.js";
export {
  type SubmitZeroInputStep02CliConfig,
  type SubmitZeroInputStep02Result,
} from "./submit-zero-input-step-02.make-zero-input-step02-spend-redeemer.js";
export {
  submitZeroInputStep02FromFiles,
  submitZeroInputStep02V1,
} from "./submit-zero-input-step-02.submit-zero-input-step02-v1.js";
