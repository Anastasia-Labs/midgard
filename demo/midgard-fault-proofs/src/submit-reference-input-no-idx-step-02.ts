/**
 * `reference-input-no-idx` step-02 submitter (Goal task `Q31`).
 *
 * Opens §2.5 field **1** of the transaction carried by step 01 and forwards the
 * challenged reference input to step 03.
 *
 * The shared door is specified in
 * `docs/fault-proofs/decisions/0001-reference-input-field-evidence.md`.
 * Thread state carries `verified_tx_id`, the §2.5 anchor; the
 * redeemer carries a `FieldOpening` rather than a reproduced
 * `reference_inputs_preimage`.
 *
 * **Position, not encoding, is what separates field 1 from field 0.** The header
 * this replaces claimed a spend-inputs preimage "can never open this commitment"
 * because the items were committed under a different `from_items` index; §4
 * removed field-index domain separation, so identical items commit identically
 * in both slots and it is the index named at the door — mirrored here by
 * {@link planFaultProofFieldOpening} — that refuses the substitution.
 *
 * This family's on-chain step 02 takes a single flat `Args` record: there is no
 * `Complete`/`CompletePublished`/`FoldStart`/`FoldNext` sum, hence no fold to
 * drive from here. What varies now is only §8's carriage tier, and the plan
 * chooses it from the preimage's own length.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "./field-opening.js";
import "./json-file.js";
import "./runtime.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-reference-input-no-idx-step-02.parse-submit-reference-input-no-idx-reference-inputs-preimage.js";
import "./submit-reference-input-no-idx-step-02.submit-reference-input-no-idx-step02.js";
export {
  parseSubmitReferenceInputNoIdxReferenceInputsPreimage,
  type SubmitReferenceInputNoIdxReferenceInputsPreimage,
  type SubmitReferenceInputNoIdxStep02CliConfig,
  type SubmitReferenceInputNoIdxStep02Result,
} from "./submit-reference-input-no-idx-step-02.parse-submit-reference-input-no-idx-reference-inputs-preimage.js";
export {
  submitReferenceInputNoIdxStep02,
  submitReferenceInputNoIdxStep02FromFiles,
} from "./submit-reference-input-no-idx-step-02.submit-reference-input-no-idx-step02.js";
