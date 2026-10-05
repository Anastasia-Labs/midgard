/**
 * `executionSourceScriptDecoding` registered-chain emulator support (§5.3).
 *
 * The subject of this family is one execution of one native script source
 * inside a committed native transaction: the retained validation-machine
 * witness authenticates the purpose/source/execution frontiers, and the exact
 * inline field-6 item is opened by bounded-item chunk proofs. Fixtures here
 * build the canonical transaction, its deterministic machine trace, the
 * committed block and the retained evidence from canonical material only;
 * the stages drive the five registered validators on the shared Van Rossem
 * emulator parameters with local UPLC evaluation.
 */
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@al-ft/midgard-validation/tests/validation-fixtures";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../../src/execution-source-script-decoding/index.js";
import "../../src/remove-fraudulent-block.js";
import "../../src/transition-trace/phas.js";
import "../../src/transition-trace/witnesses.js";
import "./emulator/measurement.js";
import "./emulator/registered-chain.js";
import "./native-script-decoding-emulator.js";
import "./submit-init-emulator-shared.js";
import "./execution-source-script-decoding-emulator.build-canonical-trace.js";
import "./execution-source-script-decoding-emulator.build-subject-fixture.js";
import "./execution-source-script-decoding-emulator.publish-family-references.js";
import "./execution-source-script-decoding-emulator.make-execution-source-stages.js";
import "./execution-source-script-decoding-emulator.types.js";

import { network } from "./submit-init-emulator-shared.js";
export {
  createMeasurementRecorder,
  emptyAllScript,
  EXECUTION_SOURCE_CATEGORY_ID,
  EXECUTION_SOURCE_MAX_FIELD_BYTES,
  EXECUTION_SOURCE_REASON_ARMS,
  EXECUTION_SOURCE_REJECTION_CODES,
  type ExecutionSourceContext,
  type ExecutionSourceReasonArm,
  forcedReason,
  type Harness,
  makeExecutionSourceHarness,
  malformedItemOfFieldBytes,
  MAXIMUM_SHAPE,
  maximumMalformedItem,
  maximumWideScript,
  type Measurement,
  type MeasurementRecorder,
  nestedScript,
  rawNativeItem,
  registeredContracts,
  signatureScript,
  type SubjectItem,
  wideScript,
} from "./execution-source-script-decoding-emulator.build-canonical-trace.js";
export {
  buildSubjectFixture,
  buildSubjectReplay,
  commitSubjectBlock,
  foreignForcedMembership,
  type SubjectFixture,
} from "./execution-source-script-decoding-emulator.build-subject-fixture.js";
export { makeExecutionSourceStages } from "./execution-source-script-decoding-emulator.make-execution-source-stages.js";
export { publishFamilyReferences } from "./execution-source-script-decoding-emulator.publish-family-references.js";
export { type ExecutionSourceStages } from "./execution-source-script-decoding-emulator.types.js";

export { network };

export { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";
