/**
 * Shared emulator fixtures for the `input-set-uniqueness` family.
 *
 * The family convicts an operator-ACCEPTED committed transaction whose
 * intra-transaction input sets violate uniqueness/disjointness. What every
 * scenario needs and no existing helper produces is a committed transaction
 * with a **caller-chosen reference-input list**: `makeNativeTx` fixes field 1
 * to at most one opaque 32-byte item, but this family's claims name canonical
 * §5.3 out-ref items in both fields, so the fixture materializes the native
 * transaction directly from its canonical preimages.
 *
 * Fixtures are deliberately tiny: two or three items per field decide every
 * sub-variant, and the openings always ride tier-1 inline carriage.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "../../src/input-set-uniqueness/submit-common.js";
import "../../src/runtime.js";
import "../../src/step-support.js";
import "../../src/tx-layout.js";
import "../../src/witness-reference-scripts.js";
import "./emulator/reference-scripts.js";
import "./submit-init-emulator-fixtures.js";
import "./submit-init-emulator-shared.js";
import "./input-set-uniqueness-emulator.build-input-set-uniqueness-fixture.js";
import "./input-set-uniqueness-emulator.submit-raw-input-set-uniqueness-bind.js";
import "./input-set-uniqueness-emulator.submit-raw-input-set-uniqueness-finalize.js";
export {
  buildInputSetUniquenessFixture,
  type InputSetUniquenessFixture,
  type InputSetUniquenessHarness,
  isuItemCbor,
  isuOutRef,
  makeInputSetUniquenessEmulatorHarness,
  publishInputSetUniquenessReferenceScripts,
  setupInputSetUniquenessScenario,
} from "./input-set-uniqueness-emulator.build-input-set-uniqueness-fixture.js";
export {
  type RawInputSetUniquenessFinalizeLayout,
  submitRawInputSetUniquenessBind,
} from "./input-set-uniqueness-emulator.submit-raw-input-set-uniqueness-bind.js";
export { submitRawInputSetUniquenessFinalize } from "./input-set-uniqueness-emulator.submit-raw-input-set-uniqueness-finalize.js";
export { expectOnchainRefusal } from "./native-script-decoding-emulator.js";
