/**
 * Emulator fixtures and stage drivers for the `outputReferenceScriptDecoding`
 * lifecycle suite: canonical accepted and forced blocks whose subject outputs
 * carry a versioned reference script, the registered six-step chain, and one
 * recorder for every complete signed measurement the Van Rossem fit ledger
 * keeps.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import "../../src/field-opening.js";
import "../../src/output-reference-script-decoding/index.js";
import "../../src/remove-fraudulent-block.js";
import "../../src/step-support.js";
import "../../src/transition-trace/phas.js";
import "../../src/transition-trace/reconstruct.js";
import "../../src/transition-trace/witnesses.js";
import "./emulator/harness.js";
import "./emulator/measurement.js";
import "./emulator/native-tx.js";
import "./emulator/reference-scripts.js";
import "./emulator/registered-chain.js";
import "./emulator/removal-deployment.js";
import "./submit-init-emulator-fixtures.js";
import "./submit-init-emulator-shared.js";
import "./output-reference-script-decoding-emulator.commit-accepted-block.js";
import "./output-reference-script-decoding-emulator.commit-forced-block.js";
import "./output-reference-script-decoding-emulator.make-output-reference-stages.js";
import "./output-reference-script-decoding-emulator.types.js";
export {
  type AcceptedSubject,
  commitAcceptedBlock,
  createMeasurementRecorder,
  type ForcedLeafSpec,
  type Harness,
  makeOutputReferenceHarness,
  MAXIMUM_SHAPE,
  maximumWideScriptOutput,
  type Measurement,
  type MeasurementRecorder,
  nestedScript,
  network,
  OUTPUT_REFERENCE_CATEGORY_ID,
  OUTPUT_REFERENCE_MAX_OUTPUT_BYTES,
  OUTPUT_REFERENCE_REASON_ARMS,
  type OutputReferenceContext,
  type OutputReferenceReasonArm,
  outputWithNativeScript,
  outputWithRawNativePayload,
  rawNativeOutputOfLength,
  registeredContracts,
  signatureScript,
  subjectTransaction,
  wideScript,
} from "./output-reference-script-decoding-emulator.commit-accepted-block.js";
export {
  commitForcedBlock,
  type ForcedLeaf,
  foreignForcedMembership,
  publishFamilyReferences,
} from "./output-reference-script-decoding-emulator.commit-forced-block.js";
export { makeOutputReferenceStages } from "./output-reference-script-decoding-emulator.make-output-reference-stages.js";
export { type OutputReferenceStages } from "./output-reference-script-decoding-emulator.types.js";
