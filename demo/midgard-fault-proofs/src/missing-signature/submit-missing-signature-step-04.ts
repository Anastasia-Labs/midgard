/**
 * `missing-signature` step-04 submitter (offchain plan §4.2, §5 frontier 3).
 *
 * Opens witness field 7 (`address_witnesses`) through the §8.8 door — the
 * `WitnessAnchor` arm, checked against the thread-anchored
 * `verified_witness_set_hash`, never a locally derived one — and walks it in
 * deterministic bounded batches. An interior batch self-loops step-04 with a
 * canonical checkpoint hash; the terminal batch proves absence, burns the
 * computation thread, mints the permanent fraud-proof token, and locks it at
 * the always-fails fraud-proof address.
 *
 * The absence predicate is re-run locally first with the exact twin: a preimage
 * in which the accused key IS present would be `NotAFault` (valid witness)
 * or `invalid-signature`'s fault (present-but-invalid, §7.3/D6) — either
 * way, refused here rather than burned on-chain.
 *
 * Field 7 is the family's fat field (~96 bytes per witness), so unlike
 * step-02 the tier is genuinely load-bearing: whatever the door's planner
 * picks, this submitter publishes and reads back.
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
import "./evidence.js";
import "./submit-common.js";
import "./submit-missing-signature-step-04.submit-missing-signature-step04-result.js";
import "./submit-missing-signature-step-04.submit-missing-signature-step04.js";
export { submitMissingSignatureStep04 } from "./submit-missing-signature-step-04.submit-missing-signature-step04.js";
export { type SubmitMissingSignatureStep04Result } from "./submit-missing-signature-step-04.submit-missing-signature-step04-result.js";
