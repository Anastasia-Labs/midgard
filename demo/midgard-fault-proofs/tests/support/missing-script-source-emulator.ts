/**
 * Shared fixtures and stage drivers for the missingScriptSource emulator
 * suites: a canonical retained-DA fixture over any purpose kind and source
 * location, the committed block the thread disputes, the retained universe
 * a prover reconstructs from it, and measured drivers for every physical
 * step of the applied chain.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../../../midgard-validation/tests/validation-fixtures.js";
import "../../src/missing-script-source/authenticated-replay.js";
import "../../src/missing-script-source/contracts.js";
import "../../src/missing-script-source/retained-script-universe.js";
import "../../src/missing-script-source/submit-cancel.js";
import "../../src/missing-script-source/submit-init.js";
import "../../src/missing-script-source/submit-step-01.js";
import "../../src/missing-script-source/submit-step-02.js";
import "../../src/missing-script-source/submit-step-03.js";
import "../../src/missing-script-source/submit-step-04.js";
import "../../src/missing-script-source/submit-step-05.js";
import "../../src/missing-script-source/submit-step-06.js";
import "../../src/remove-fraudulent-block.js";
import "../../src/transition-trace/phas.js";
import "../../src/transition-trace/witnesses.js";
import "./emulator/catalogue.js";
import "./emulator/measurement.js";
import "./emulator/proof-fit.js";
import "./missing-script-source-shapes.js";
import "./native-script-decoding-emulator.js";
import "./submit-init-emulator-fixtures.js";
import "./submit-init-emulator-shared.js";
import "./missing-script-source-emulator.build-missing-script-source-fixture.js";
import "./missing-script-source-emulator.commit-missing-script-source-block.js";
import "./missing-script-source-emulator.make-missing-script-source-stages.js";
import "./missing-script-source-emulator.run-missing-script-source-thread.js";
export {
  buildMissingScriptSourceFixture,
  type MissingScriptSourceFixture,
  type MissingScriptSourceFixtureShape,
  type MissingScriptSourceLocation,
  type MissingScriptSourcePurposeKind,
  missingScriptSourceReason,
} from "./missing-script-source-emulator.build-missing-script-source-fixture.js";
export {
  buildMissingScriptSourceUniverse,
  claimMissingScriptSourcePrefix,
  commitMissingScriptSourceBlock,
  makeMissingScriptSourceHarness,
  type MissingScriptSourceBlock,
  missingScriptSourceEvidence,
  type MissingScriptSourceHarness,
  type MissingScriptSourceStageRow,
  missingScriptSourceSubject,
} from "./missing-script-source-emulator.commit-missing-script-source-block.js";
export { makeMissingScriptSourceStages } from "./missing-script-source-emulator.make-missing-script-source-stages.js";
export {
  type MissingScriptSourceStages,
  runMissingScriptSourceThread,
} from "./missing-script-source-emulator.run-missing-script-source-thread.js";
