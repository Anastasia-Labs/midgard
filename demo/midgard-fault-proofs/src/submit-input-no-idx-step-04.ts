/**
 * `input-no-idx` step-04 submitter (Goal task `Q13`, §9.1 output 8).
 *
 * Finalizes the proof: burns the computation thread, mints the permanent
 * fraud-proof token and locks it at the always-fails fraud-proof address.
 *
 * **Re-derived onto the §8.8 door by #604.** The redeemer used to reproduce the
 * producing transaction's whole `outputs_preimage: List<MidgardTxOutput>`; it now
 * carries a `FieldOpening` over §2.5 field **2**, and the door's authenticated
 * item count is the output count the out-of-range verdict rests on (§5.2). Thread
 * state carries that transaction's `producing_tx_id` rather than its
 * `outputs_hash`, which is why this builder takes the producing transaction's
 * compact CBOR.
 *
 * Nothing in the prepared file is trusted. The complete outputs preimage is
 * re-encoded with the canonical `encode_midgard_tx_output` twin and checked
 * against the commitment the producing transaction carries *at field 2*; the rule
 * itself is then re-run locally (`bad_input_output_index >= |outputs|`), so a
 * thread whose challenged index exists in its producing transaction cannot be
 * finalized off-chain either.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "./field-opening.js";
import "./json-file.js";
import "./prepare-input-no-idx.js";
import "./runtime.js";
import "./spend-input-witness.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-input-no-idx-step-04.make-input-no-idx-step04-spend-redeemer.js";
import "./submit-input-no-idx-step-04.submit-input-no-idx-step04.js";
import "./submit-input-no-idx-step-04.submit-input-no-idx-step04-from-files.js";
export {
  parseSubmitInputNoIdxOutputsPreimage,
  type SubmitInputNoIdxOutputsPreimage,
  type SubmitInputNoIdxStep04CliConfig,
  type SubmitInputNoIdxStep04Result,
} from "./submit-input-no-idx-step-04.make-input-no-idx-step04-spend-redeemer.js";
export { submitInputNoIdxStep04 } from "./submit-input-no-idx-step-04.submit-input-no-idx-step04.js";
export { submitInputNoIdxStep04FromFiles } from "./submit-input-no-idx-step-04.submit-input-no-idx-step04-from-files.js";
