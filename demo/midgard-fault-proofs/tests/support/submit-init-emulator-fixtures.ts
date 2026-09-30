/**
 * Emulator journey fixtures shared by the `submit-init-emulator-*.test.ts`
 * family.
 *
 * These builders were local to `submit-init-emulator.test.ts` until that file
 * was broken up: `@lucid-evolution/uplc` through 0.2.22 leaked the wasm linear
 * memory it allocated per script evaluation, vitest isolates per FILE, and a
 * single file holding every heavy journey walked into the ~4 GiB wasm32
 * ceiling on CI. That leak is fixed upstream; lifting the fixtures here still
 * lets each journey theme run in its own worker while sharing one definition
 * of every fixture.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-core/codec/hash";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../../src/index.js";
import "../../src/ne-proofs.js";
import "./emulator/reference-script-publisher.js";
import "./emulator/reference-scripts.js";
import "./legacy-submit-emulator.js";
import "./submit-init-emulator-shared.js";
import "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
import "./submit-init-emulator-fixtures.build-transaction-inclusion-fixture.js";
import "./submit-init-emulator-fixtures.build-invalid-range-transaction-inclusion-fixture.js";
import "./submit-init-emulator-fixtures.build-non-existent-input-fixture.js";
import "./submit-init-emulator-fixtures.build-invalid-forced-transition-trace-fixture.js";
import "./submit-init-emulator-fixtures.build-invalid-forced-validation-dispute-fixture.js";
import "./submit-init-emulator-fixtures.submit-successor-block-tx.js";
import "./submit-init-emulator-fixtures.instrument-lucid-for-removal.js";
import "./submit-init-emulator-fixtures.build-proved-double-spend-fixture.js";
import "./submit-init-emulator-fixtures.minimum-lovelace-for-inline-value.js";
import "./submit-init-emulator-fixtures.retire-fixture-operator-after-inactivity.js";
import "./submit-init-emulator-fixtures.setup-fraudulent-block.js";
export { buildInvalidForcedTransitionTraceFixture } from "./submit-init-emulator-fixtures.build-invalid-forced-transition-trace-fixture.js";
export { buildInvalidForcedValidationDisputeFixture } from "./submit-init-emulator-fixtures.build-invalid-forced-validation-dispute-fixture.js";
export {
  buildInvalidRangeTransactionInclusionFixture,
  buildZeroInputTransactionInclusionFixture,
} from "./submit-init-emulator-fixtures.build-invalid-range-transaction-inclusion-fixture.js";
export {
  buildNonExistentInputFixture,
  countedTransactionsRoot,
  registerPexcludesExclusionRewardAccount,
  sortedDaEntries,
  transitionTraceRawEntry,
} from "./submit-init-emulator-fixtures.build-non-existent-input-fixture.js";
export { buildProvedDoubleSpendFixture } from "./submit-init-emulator-fixtures.build-proved-double-spend-fixture.js";
export {
  buildTransactionInclusionFixture,
  insertAdversarialMembershipSiblings,
  membershipProofBranchLevelByteCeiling,
  membershipProofBranchLevelsReachableWithWork,
  type MembershipProofShape,
  membershipProofShape,
} from "./submit-init-emulator-fixtures.build-transaction-inclusion-fixture.js";
export {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE,
  adversarialMembershipSiblingKeys,
  compactTxEntry,
  decodeSpendInputCbors,
  expectStateQueueHeaderOrder,
  largeFittingOutputCbor,
  midgardTxInput,
  MPF_BRANCH_PROOF_STEP_CBOR_BYTES,
  outputReferenceCbor,
  positiveNonAdaAssets,
  PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
  spendInputFiller,
  spendInputsOfCardinality,
  type TestOutputReference,
  type TransactionInclusionEntry,
  tx1InputsPreimage,
  tx2InputsPreimage,
} from "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
export {
  createRecordingLeaseCoordinator,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  eventIndexes,
  instrumentLucidForRemoval,
  type ProvedDoubleSpendFixture,
  type RemovalEvent,
} from "./submit-init-emulator-fixtures.instrument-lucid-for-removal.js";
export { retireFixtureOperatorAfterInactivity } from "./submit-init-emulator-fixtures.retire-fixture-operator-after-inactivity.js";
export {
  expectRemovedFraudProofState,
  setupFraudulentBlock,
  submitRemovalForFixture,
} from "./submit-init-emulator-fixtures.setup-fraudulent-block.js";
export {
  DOUBLE_SPEND_STEP_REFERENCE_NAMES,
  submitSuccessorBlockTx,
  type SuccessorBlockFixture,
} from "./submit-init-emulator-fixtures.submit-successor-block-tx.js";
