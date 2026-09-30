/**
 * `no-reference-input` step-04 submitter — a non-membership proof, not a field
 * opening.
 *
 * **Unchanged by #604's re-derivation, and checked to be.** The #575 rebind moved
 * this family's step-02 state and redeemer onto the §2.5 anchor and the §8.8
 * door; this step's state and redeemer are exactly what
 * `midgard/fraud_proofs/no_reference_input/step_04` still declares.
 *
 * `no-reference-input` step-04 submitter (Goal task `Q18`, §9.1 output 8).
 *
 * Structural mirror of the `non-existent-input` chain's step 04
 * (`non-existent-input/submit-step-04.ts`): proves the challenged input's producing transaction
 * is absent from the block's transactions trie, burns the computation-thread
 * token, and mints the fraud-proof token at the fraud-proof spending address.
 * Only the threaded field differs — `missing_reference_input_tx_id` rather than
 * `missing_input_tx_id`.
 *
 * Nothing in the prepared JSON is trusted for anything the chain re-derives:
 * the exclusion key (the producing transaction id) and the `transactions_root`
 * the proof must open are both read back from the **on-chain** step-04 datum.
 * The prepared file supplies only the MPF proof itself.
 *
 * Proof carriage mirrors Q11 exactly: fitting proofs use the direct pexcludes
 * withdrawal and larger proofs use authenticated published chunks through the
 * shared `NonMembershipCarriage` ABI.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "./proof-chunk-carriage.js";
import "./runtime.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-no-reference-input-step-04.submit-no-reference-input-step04-result.js";
import "./submit-no-reference-input-step-04.submit-no-reference-input-step04.js";
import "./submit-no-reference-input-step-04.submit-no-reference-input-step04-from-files.js";
export { submitNoReferenceInputStep04 } from "./submit-no-reference-input-step-04.submit-no-reference-input-step04.js";
export { submitNoReferenceInputStep04FromFiles } from "./submit-no-reference-input-step-04.submit-no-reference-input-step04-from-files.js";
export {
  type SubmitNoReferenceInputStep04CliConfig,
  type SubmitNoReferenceInputStep04Result,
} from "./submit-no-reference-input-step-04.submit-no-reference-input-step04-result.js";
