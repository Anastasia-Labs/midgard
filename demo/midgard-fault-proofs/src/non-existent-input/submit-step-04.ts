/**
 * `non-existent-input` step-04 submitter — the transactions-root absence proof
 * that concludes the family.
 *
 * **Unchanged by #604's re-derivation, and checked to be.** Its state
 * (`missing_input_tx_id`, `blocks_transactions_root`) and its redeemer
 * (a non-membership carriage, not a field preimage) are exactly what
 * `midgard/fraud_proofs/no_input/step_04` declares. The #575 rebind touched
 * step-02 only.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "../proof-chunk-carriage.js";
import "../runtime.js";
import "../step-support.js";
import "../tx-layout.js";
import "../witness-reference-scripts.js";
import "../workflow/transaction-boundary.js";
import "./submit-step-04.ne-submit-step04-result.js";
import "./submit-step-04.ne-submit-step04.js";
export { neSubmitStep04 } from "./submit-step-04.ne-submit-step04.js";
export { type NeSubmitStep04Result } from "./submit-step-04.ne-submit-step04-result.js";
