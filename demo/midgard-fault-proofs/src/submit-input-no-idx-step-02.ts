/**
 * `input-no-idx` step-02 submitter.
 *
 * Opens §2.5 field 0 of the transaction carried by step-01 and forwards the
 * challenged `(tx_id, output_index)` to step-03.
 *
 * The on-chain `Args` is a flat record with one route: the §8.8 door hashes the
 * preimage once and reads item `n` by arithmetic. The prover's only choice is
 * *how the preimage travels*, which is §8's carriage ladder. A published
 * preimage is §8.5 raw carriage — a nothing-but-bytes inline datum published
 * through `buildUnsignedFieldPreimagePublicationV1Program` and located by
 * *content* (§8.7), so a republished copy is interchangeable with the
 * original and the publication is bound to no computation thread or prover.
 *
 * Nothing in the prepared file is trusted: the anchor is read from the
 * **on-chain** step-01 datum, and the supplied list must be the §5.1 preimage the
 * anchored transaction commits at field 0 — checked by
 * {@link planFaultProofFieldOpening} before a transaction is built.
 */

import "@al-ft/midgard-sdk/core";
import "@al-ft/midgard-sdk/state-queue";
import "@al-ft/midgard-sdk/fraud-proof";
import "@lucid-evolution/lucid";
import "./field-opening.js";
import "./json-file.js";
import "./runtime.js";
import "./spend-input-witness.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-input-no-idx-step-02.parse-submit-input-no-idx-inputs-preimage.js";
import "./submit-input-no-idx-step-02.submit-input-no-idx-step02.js";
export {
  parseSubmitInputNoIdxInputsPreimage,
  type SubmitInputNoIdxInputsPreimage,
  type SubmitInputNoIdxStep02CliConfig,
  type SubmitInputNoIdxStep02Result,
} from "./submit-input-no-idx-step-02.parse-submit-input-no-idx-inputs-preimage.js";
export {
  buildInputNoIdxStep02,
  submitInputNoIdxStep02FromFiles,
} from "./submit-input-no-idx-step-02.submit-input-no-idx-step02.js";
