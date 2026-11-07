/** Shared real-contract emulator fixtures for the missing-signature family. */

import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/cbor";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "@lucid-evolution/scalus-uplc";
import "effect";
import "../../src/field-opening.js";
import "../../src/missing-signature/index.js";
import "../../src/spend-input-witness.js";
import "../../src/step-support.js";
import "../../src/tx-layout.js";
import "../../src/witness-reference-scripts.js";
import "./native-script-decoding-emulator.js";
import "./submit-init-emulator-shared.js";
import "./missing-signature-emulator.build-missing-signature-subject.js";
import "./missing-signature-emulator.submit-raw-missing-signature-step04.js";
export {
  buildMissingSignatureSubject,
  makeMissingSignatureEmulatorHarness,
  MISSING_SIGNATURE_EMULATOR_PROVER_POLICY,
  MISSING_SIGNATURE_FIRST_RAW_WITNESS_COUNT,
  MISSING_SIGNATURE_MAX_ADMISSIBLE_WITNESS_COUNT,
  MISSING_SIGNATURE_TARGET_HASH,
  MISSING_SIGNATURE_TARGET_VKEY,
  missingSignatureFinding,
  missingSignatureProverDeps,
  type MissingSignatureScenario,
  publishMissingSignatureReferenceScripts,
  setupMissingSignatureScenario,
} from "./missing-signature-emulator.build-missing-signature-subject.js";
export {
  publishMissingSignatureField07Certificate,
  submitRawMissingSignatureStep04,
} from "./missing-signature-emulator.submit-raw-missing-signature-step04.js";
