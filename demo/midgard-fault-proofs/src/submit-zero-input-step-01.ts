/**
 * `zero-input` step-01 submitter.
 *
 * Thread state carries the §2.5 anchor — the disputed transaction's **id**.
 * Step-02 re-opens field 0 through the §8.8 door rather than comparing a
 * forwarded commitment against the pinned empty-field constant: under §4's
 * plain hashing that constant is the same 32 bytes in all nine slots, so a hash
 * could not say *which* field was empty. See
 * `docs/fault-proofs/decisions/0001-reference-input-field-evidence.md`.
 */

import "@al-ft/midgard-sdk/core";
import "@al-ft/midgard-sdk/state-queue";
import "@al-ft/midgard-sdk/fraud-proof";
import "@lucid-evolution/lucid";
import "./proof-chunk-carriage.js";
import "./runtime.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-zero-input-step-01.types.js";
import "./submit-zero-input-step-01.submit-zero-input-step01.js";
export { buildZeroInputStep01 } from "./submit-zero-input-step-01.submit-zero-input-step01.js";
