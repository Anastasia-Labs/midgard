/**
 * Emulator support for the `resolvedOutputNonCanonical` (`00000026`)
 * lifecycle: retained prior-ledger fixtures, accepted and forced block
 * commitment on the registered chain, the family submitters, and the raw
 * continuations that hand a substituted redeemer or successor to the
 * validator so every seam is refused on chain rather than by a builder.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import "../../src/field-opening.js";
import "../../src/linear-fault-family.js";
import "../../src/linear-fault-finalize.js";
import "../../src/linear-fault-submit.js";
import "../../src/remove-fraudulent-block.js";
import "../../src/resolved-output-non-canonical/index.js";
import "../../src/step-support.js";
import "../../src/transition-trace/phas.js";
import "../../src/tx-layout.js";
import "./emulator/harness.js";
import "./emulator/measurement.js";
import "./emulator/native-tx.js";
import "./emulator/reference-scripts.js";
import "./emulator/registered-chain.js";
import "./emulator/removal-deployment.js";
import "./submit-init-emulator-fixtures.js";
import "./submit-init-emulator-shared.js";
import "./resolved-output-non-canonical-emulator.build-prior-ledger.js";
import "./resolved-output-non-canonical-emulator.commit-block.js";
import "./resolved-output-non-canonical-emulator.resolved-output-evidence.js";
import "./resolved-output-non-canonical-emulator.make-resolved-output-stages.js";
export {
  buildPriorLedger,
  buildSubjectTransaction,
  descriptorFor,
  FAMILY,
  makeResolvedOutputContext,
  MAXIMUM_INPUT_ITEM_COUNT,
  maximumCanonicalOutput,
  maximumMalformedOutput,
  network,
  type PriorLedgerFixture,
  RESOLVED_OUTPUT_MAXIMUM_BYTES,
  RESOLVED_OUTPUT_REASON_ARM,
  type ResolvedOutputContext,
  resolvedOutputReason,
  smallCanonicalOutput,
  smallMalformedOutput,
  subjectTransactionFor,
} from "./resolved-output-non-canonical-emulator.build-prior-ledger.js";
export {
  commitBlock,
  type CommittedBlock,
} from "./resolved-output-non-canonical-emulator.commit-block.js";
export { makeResolvedOutputStages } from "./resolved-output-non-canonical-emulator.make-resolved-output-stages.js";
export {
  type Captured,
  forcedSourceOf,
  resolvedOutputEvidence,
} from "./resolved-output-non-canonical-emulator.resolved-output-evidence.js";
