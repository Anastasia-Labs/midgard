/**
 * `input-no-idx` step-02 submitter (Goal task `Q13`, §9.1 output 8).
 *
 * Opens §2.5 field 0 of the transaction carried by step-01 and forwards the
 * challenged `(tx_id, output_index)` to step-03.
 *
 * **Re-derived onto the §8.8 door by #604, and this step lost two mechanisms to
 * it.** The on-chain `Args` used to be a four-arm sum, and this module drove all
 * four:
 *
 *   * `Complete` reproduced the whole input list in the redeemer;
 *   * `CompletePublished` referenced a **bespoke** `PublishedSpendInputsV1`
 *     typed datum that this module published, matched by out-ref, and checked
 *     field by field;
 *   * `FoldStart`/`FoldNext` streamed the collection one counted opening at a
 *     time, resuming through the computation thread itself.
 *
 * All four existed because the collection had to be reproduced *inside the step*
 * to re-hash it against the commitment the thread carried. §4's flat commitment
 * and the §8.8 door removed that need entirely: the door hashes the preimage
 * once and reads item `n` by arithmetic. So the redeemer has exactly one route,
 * and the prover's only remaining choice is *how the preimage travels* — which
 * is §8's carriage ladder, not a family-specific mechanism.
 *
 * Concretely:
 *
 *   * the typed publication is **deleted**, not re-pointed. Its replacement is
 *     §8.5 raw carriage — a nothing-but-bytes inline datum published through
 *     `buildUnsignedFieldPreimagePublicationV1Program` and located by *content*
 *     (§8.7), so a republished copy is interchangeable with the one it replaces.
 *     The bespoke datum could not be: it bound the publication to one computation
 *     thread and one prover, which is precisely the coupling §8.7 forbids;
 *   * the ordered fold is **gone**, and with it
 *     `submitInputNoIdxStep02UntilTerminal` and the `submit-input-no-idx-fold`
 *     command. There is no `FoldStart` arm on-chain to emit.
 *
 * Nothing in the prepared file is trusted: the anchor is read from the
 * **on-chain** step-01 datum, and the supplied list must be the §5.1 preimage the
 * anchored transaction commits at field 0 — checked by
 * {@link planFaultProofFieldOpening} before a transaction is built.
 */

import "@al-ft/midgard-sdk";
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
  submitInputNoIdxStep02,
  submitInputNoIdxStep02FromFiles,
} from "./submit-input-no-idx-step-02.submit-input-no-idx-step02.js";
