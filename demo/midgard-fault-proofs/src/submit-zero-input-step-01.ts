/**
 * `zero-input` step-01 submitter.
 *
 * **Re-derived onto the flat field commitments by #604.** Thread state now
 * carries the §2.5 anchor — the disputed transaction's **id** — where it used to
 * carry that transaction's `spend_inputs_hash`. Step-02 re-opens field 0 through
 * the §8.8 door rather than comparing a forwarded commitment against the pinned
 * empty-field constant, and under §4's plain hashing that constant is the same
 * 32 bytes in all nine slots, so the forwarded hash could not say *which* field
 * was empty. The rebind is recorded once in
 * `docs/fault-proofs/decisions/0001-reference-input-field-evidence.md`.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "./legacy-submission-boundary.js";
import "./proof-chunk-carriage.js";
import "./runtime.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-zero-input-step-01.types.js";
import "./submit-zero-input-step-01.submit-zero-input-step01.js";
import "./submit-zero-input-step-01.submit-zero-input-step01-from-files.js";
export { submitZeroInputStep01 } from "./submit-zero-input-step-01.submit-zero-input-step01.js";
export { submitZeroInputStep01FromFiles } from "./submit-zero-input-step-01.submit-zero-input-step01-from-files.js";
export {
  type SubmitZeroInputStep01CliConfig,
  type SubmitZeroInputStep01Result,
} from "./submit-zero-input-step-01.types.js";
