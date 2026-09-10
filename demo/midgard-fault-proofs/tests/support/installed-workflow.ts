/** Shared emulator infrastructure for real manifest-bound integration tests. */
export {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "../helpers/canonical-block-evidence-fixture.js";
export { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
export { realBlueprintPath } from "./emulator/blueprints.js";
export { buildCatalogueDeploymentInfo } from "./emulator/catalogue.js";
export {
  alignUnixTimeToEmulatorSlotBoundary,
  fundedProverEmulatorAccount,
} from "./emulator/emulator-context.js";
export { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
export { measureCompleteSignedTransaction } from "./emulator/measurement.js";
export {
  publishAuthenticatedValidationDisputeControl,
  publishFaultProofWitnessReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
} from "./emulator/reference-scripts.js";
export { submitSetupTx } from "./emulator/setup-tx.js";
export {
  buildWidthForcedFixture,
  mintFieldTx,
  widthNativeTx,
} from "./field-item-width-illegal-shapes.js";
