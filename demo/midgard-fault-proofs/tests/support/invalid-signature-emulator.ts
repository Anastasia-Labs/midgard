/**
 * Shared real-contract emulator fixtures for the `invalid-signature` family
 * (Goal task `Q15`).
 *
 * The family is a two-real-step chain — `init` → `step-01` (bind the committed
 * transaction against the block's counted `transactions_root` and forward
 * `WitnessAnchor`) → `step-02` (open §2.5 field 7 through the §8.8 door and
 * finalize) — followed by fraudulent-block removal.
 *
 * Everything here builds *committed* material: the subject transaction's
 * address-witness list is the block's own field-7 preimage, so the fixture's
 * signatures are the ones the on-chain `verify_ed25519_signature` re-tests.
 * That is what makes the honest polarity expressible at all: an honest block
 * commits witnesses that genuinely sign the transaction id, and the only way to
 * accuse it is to bypass the submitter's local guard — which
 * {@link submitRawInvalidSignatureStep02} does, so the refusal comes from the
 * validator rather than from the builder.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "../../src/field-opening.js";
import "../../src/runtime.js";
import "../../src/step-support.js";
import "../../src/tx-layout.js";
import "../../src/witness-reference-scripts.js";
import "./submit-init-emulator-fixtures.js";
import "./submit-init-emulator-shared.js";
import "./invalid-signature-emulator.build-invalid-signature-subject.js";
import "./invalid-signature-emulator.submit-raw-invalid-signature-step02.js";
export {
  buildInvalidSignatureBlockFixture,
  buildInvalidSignatureSubject,
  honestAddressWitness,
  INVALID_SIGNATURE_ADDRESS_WITNESS_STRIDE,
  INVALID_SIGNATURE_FIRST_RAW_WITNESS_COUNT,
  invalidAddressWitness,
  type InvalidSignatureEmulatorHarness,
  type InvalidSignatureSubject,
  makeInvalidSignatureEmulatorHarness,
  publishInvalidSignatureReferenceScripts,
  setupInvalidSignatureScenario,
} from "./invalid-signature-emulator.build-invalid-signature-subject.js";
export { submitRawInvalidSignatureStep02 } from "./invalid-signature-emulator.submit-raw-invalid-signature-step02.js";
