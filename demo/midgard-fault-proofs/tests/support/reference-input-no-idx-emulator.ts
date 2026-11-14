/**
 * Shared real-contract emulator fixtures for the `reference-input-no-idx`
 * family (Goal task `Q31`).
 *
 * The fault is the reference-input mirror of `input-no-idx`: a committed
 * transaction *reads* `(producing_tx_id, N)` where `producing_tx_id` is itself
 * committed in the same block, yet `N >= |producer.outputs|`. Proving it needs
 * a **two-transaction block**, because step-01 proves membership of the bad
 * transaction and step-03 proves membership of the *producing* transaction
 * under the same counted `transactions_root`. `buildSingleTxBlockFixture` in
 * `submit-init-emulator-registered-families.test.ts` commits one leaf, so this
 * module builds the two-leaf PHAS trie and returns an inclusion proof for each
 * transaction.
 *
 * It also materializes the disputed transaction directly from its canonical
 * §5.1 preimages: `makeNativeTx` pins §2.5 field 1 to at most one opaque
 * 32-byte item, and this family's evidence needs a caller-chosen list of
 * canonical 38-byte §5.3 out-ref items with the challenged one among them.
 *
 * The raw builders at the bottom duplicate the production submitters' exact
 * transaction shapes minus their local fail-closed guards, so the adversarial
 * polarity watches the **validator** refuse rather than the builder.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "../../src/field-opening.js";
import "../../src/prepare-input-no-idx.js";
import "../../src/runtime.js";
import "../../src/step-support.js";
import "../../src/tx-layout.js";
import "../../src/witness-reference-scripts.js";
import "./submit-init-emulator-shared.js";
import "./reference-input-no-idx-emulator.build-reference-input-no-idx-block-fixture.js";
import "./reference-input-no-idx-emulator.submit-raw-reference-input-no-idx-step02.js";
import "./reference-input-no-idx-emulator.submit-raw-reference-input-no-idx-step04.js";
export { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";
export {
  buildReferenceInputNoIdxBlockFixture,
  makeReferenceInputNoIdxEmulatorHarness,
  nativeOutputCbor,
  PRODUCING_OUTPUT_ITEM_STRIDE_BYTES,
  REFERENCE_INPUT_ITEM_STRIDE_BYTES,
  REFERENCE_INPUT_NO_IDX_TIER2_PRODUCING_OUTPUT_COUNT,
  REFERENCE_INPUT_NO_IDX_TIER2_REFERENCE_INPUT_COUNT,
  type ReferenceInputNoIdxBlockFixture,
  type ReferenceInputNoIdxHarness,
} from "./reference-input-no-idx-emulator.build-reference-input-no-idx-block-fixture.js";
export {
  publishReferenceInputNoIdxReferenceScripts,
  submitRawReferenceInputNoIdxStep02,
} from "./reference-input-no-idx-emulator.submit-raw-reference-input-no-idx-step02.js";
export { submitRawReferenceInputNoIdxStep04 } from "./reference-input-no-idx-emulator.submit-raw-reference-input-no-idx-step04.js";
