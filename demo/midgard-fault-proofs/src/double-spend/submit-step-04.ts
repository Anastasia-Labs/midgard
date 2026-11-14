/**
 * `double-spend` step-04 submitter — opens tx2's spend-input field and concludes
 * the proof.
 *
 * **Re-derived onto the §8.8 door by #604.** Like step-03, this step lost its
 * bespoke published witness UTxO: `tx2_spend_inputs_ref_input_index` is replaced
 * by `tx2_spend_inputs_opening`, and §8's carriage ladder decides whether
 * anything is published at all. Thread state carries `verified_tx2_id`, the §2.5
 * anchor, rather than tx2's field-0 commitment.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "../field-opening.js";
import "../runtime.js";
import "../spend-input-witness.js";
import "../step-support.js";
import "../tx-layout.js";
import "../witness-reference-scripts.js";
import "../workflow/transaction-boundary.js";
import "./submit-step-04.make-step04-spend-redeemer.js";
import "./submit-step-04.submit-step04.js";
export { type SubmitStep04Result } from "./submit-step-04.make-step04-spend-redeemer.js";
export { submitStep04 } from "./submit-step-04.submit-step04.js";
