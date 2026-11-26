/**
 * `reference-input-no-idx` step-04 submitter.
 *
 * Finalizes the proof: burns the computation thread, mints the permanent
 * fraud-proof token and locks it at the always-fails fraud-proof address.
 *
 * The redeemer carries a `FieldOpening` over §2.5 field **2**, and the door's
 * authenticated item count is the output count the out-of-range verdict rests
 * on (§5.2). Thread state carries that transaction's `producing_tx_id`, which
 * is why this builder takes the producing transaction's compact CBOR.
 *
 * Nothing in the prepared file is trusted. The complete outputs preimage is
 * re-encoded with the canonical `encode_midgard_tx_output` twin, checked against
 * the commitment the producing transaction carries *at field 2*, and the rule
 * itself is then re-run locally (`bad_reference_input_output_index >=
 * |outputs|`), so a thread whose challenged index exists in its producing
 * transaction cannot be finalized off-chain either.
 */

import "@al-ft/midgard-sdk/core";
import "@al-ft/midgard-sdk/state-queue";
import "@al-ft/midgard-sdk/fraud-proof";
import "@lucid-evolution/lucid";
import "./field-opening.js";
import "./json-file.js";
import "./prepare-input-no-idx.js";
import "./runtime.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-reference-input-no-idx-step-04.make-reference-input-no-idx-step04-spend-redeemer.js";
import "./submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04.js";
import "./submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04-from-files.js";
export {
  parseSubmitReferenceInputNoIdxOutputsPreimage,
  type SubmitReferenceInputNoIdxOutputsPreimage,
  type SubmitReferenceInputNoIdxStep04CliConfig,
  type SubmitReferenceInputNoIdxStep04Result,
} from "./submit-reference-input-no-idx-step-04.make-reference-input-no-idx-step04-spend-redeemer.js";
export { buildReferenceInputNoIdxStep04 } from "./submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04.js";
export { submitReferenceInputNoIdxStep04FromFiles } from "./submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04-from-files.js";
