/**
 * `invalid-signature` step-02 submitter (Goal task `Q15`, §9.1 output 8).
 *
 * Finalizes the proof: burns the computation thread, mints the permanent
 * fraud-proof token and locks it at the always-fails fraud-proof address.
 *
 * **Re-derived onto the §8.8 door by #604, and this is the family that shows why
 * `NativeTxAnchorV1` has two arms.** Field 7 lives in the witness set, and §3's
 * transaction-id preimage is the body alone, so the id does not commit it. The
 * thread therefore carries `bad_tx_witness_set_hash` — read by step-01 off the
 * compact structure the block committed — and this step's opening must be the
 * `WitnessFieldOpening` arm, carrying the transaction's
 * `NativeTxWitnessSetCompact` for the door to check against it. Both the arm and
 * the §8.3 erratum E2 tier-3 refusal are derived from the field index by
 * `fieldOpeningV1ForField`, never chosen here.
 *
 * Nothing in the prepared JSON is trusted. The anchor and the committed
 * `witness_set_hash` are read back from the **on-chain** step-01 datum; the
 * supplied witness set must hash to that value and the supplied witness list
 * must be the §5.1 preimage the transaction committed *at field 7*; and the
 * accused witness is re-tested with the same Ed25519 verification the validator
 * performs. A thread that cannot conclude therefore fails here instead of
 * burning a submission on-chain.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "./field-opening.js";
import "./json-file.js";
import "./runtime.js";
import "./step-support.js";
import "./submit-invalid-signature-step-01.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-invalid-signature-step-02.make-invalid-signature-step02-spend-redeemer.js";
import "./submit-invalid-signature-step-02.submit-invalid-signature-step02.js";
import "./submit-invalid-signature-step-02.submit-invalid-signature-step02-from-files.js";
export {
  parseSubmitInvalidSignatureAddrTxWitsPreimage,
  type SubmitInvalidSignatureStep02CliConfig,
  type SubmitInvalidSignatureStep02Result,
} from "./submit-invalid-signature-step-02.make-invalid-signature-step02-spend-redeemer.js";
export { submitInvalidSignatureStep02 } from "./submit-invalid-signature-step-02.submit-invalid-signature-step02.js";
export { submitInvalidSignatureStep02FromFiles } from "./submit-invalid-signature-step-02.submit-invalid-signature-step02-from-files.js";
