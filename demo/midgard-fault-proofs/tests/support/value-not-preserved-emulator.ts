/**
 * Shared emulator fixtures for the `value-not-preserved` family.
 *
 * The family convicts an operator-ACCEPTED committed transaction that does
 * not conserve one claimed asset. What every scenario needs and no existing
 * helper produces is a committed transaction with STRUCTURED outputs and
 * mint fields (the §5.5/§5.6 canonical encodings the step-03 fold decodes)
 * plus a pre-state ledger MPF whose descriptors commit the spent values the
 * step-02 fold authenticates — so the fixture materializes the native
 * transaction directly from its canonical preimages and files genuine
 * `LedgerOutputCommitmentV1` descriptors under the header's
 * `prev_utxos_root`.
 *
 * Tier sizing is deliberately two-sided (§8.4): the tier-1 scenarios carry
 * realistically small fields, and the tier-2 scenario's outputs preimage is
 * pushed past the 14,336-byte tier-1 cap by large inline datums — data size
 * alone selects `RawUtxo`, no override exists anywhere.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "effect";
import "../../src/runtime.js";
import "../../src/step-support.js";
import "../../src/submit-init.js";
import "../../src/tx-layout.js";
import "../../src/value-not-preserved/contracts.js";
import "../../src/value-not-preserved/evidence.js";
import "../../src/value-not-preserved/schemas.js";
import "../../src/value-not-preserved/submit-common.js";
import "../../src/value-not-preserved/submit-value-not-preserved-step-01.js";
import "../../src/value-not-preserved/submit-value-not-preserved-step-02.js";
import "../../src/value-not-preserved/submit-value-not-preserved-step-03.js";
import "../../src/value-not-preserved/submit-value-not-preserved-step-04.js";
import "../../src/witness-reference-scripts.js";
import "./emulator/reference-scripts.js";
import "./submit-init-emulator-fixtures.js";
import "./submit-init-emulator-shared.js";
import "./value-not-preserved-emulator.build-value-not-preserved-fixture.js";
import "./value-not-preserved-emulator.commit-header-after-anchor-block.js";
import "./value-not-preserved-emulator.run-value-not-preserved-thread.js";
import "./value-not-preserved-emulator.submit-raw-value-not-preserved-bind.js";
import "./value-not-preserved-emulator.submit-raw-value-not-preserved-finalize.js";
export { expectOnchainRefusal } from "./submit-init-emulator-shared.js";
export {
  buildValueNotPreservedFixture,
  type ValueNotPreservedFixture,
  type ValueNotPreservedFixtureSpentInput,
  vnpLargeDatumCbor,
  vnpOutput,
  vnpOutRef,
  vnpValue,
} from "./value-not-preserved-emulator.build-value-not-preserved-fixture.js";
export {
  makeValueNotPreservedEmulatorHarness,
  type ValueNotPreservedHarness,
} from "./value-not-preserved-emulator.commit-header-after-anchor-block.js";
export {
  publishValueNotPreservedReferenceScripts,
  runValueNotPreservedThread,
  setupValueNotPreservedScenario,
  type ValueNotPreservedThreadRun,
} from "./value-not-preserved-emulator.run-value-not-preserved-thread.js";
export {
  publishTamperedFieldPreimagePublication,
  submitRawValueNotPreservedBind,
} from "./value-not-preserved-emulator.submit-raw-value-not-preserved-bind.js";
export { submitRawValueNotPreservedFinalize } from "./value-not-preserved-emulator.submit-raw-value-not-preserved-finalize.js";
