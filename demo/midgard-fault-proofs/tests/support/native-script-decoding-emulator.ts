/**
 * Shared emulator fixtures for the `native-script-decoding` family (#635,
 * #633), offchain plan §8.2 suites 4–7.
 *
 * Three things every decoding emulator scenario needs and none of the
 * existing helpers produce:
 *
 * 1. **A committed block whose transition trace carries the accused step.**
 *    `tests/helpers/canonical-block-evidence-fixture.ts` emits an empty
 *    transition trace, but this family's step-02 opens the event→step leaf
 *    AND the transition step whose `pre_utxos_root` becomes the thread's
 *    `prior_ledger_root`. {@link buildDecodingBlockFixture} assembles the
 *    whole `DaPayload` — counted roots, dense trace, forced leaf and its
 *    DA preimage — the way `tests/transition-trace-challenger.test.ts` does,
 *    but under an emulator-committable header.
 * 2. **A pre-state ledger trie holding the accused outpoint's descriptor.**
 *    {@link buildDecodingLedgerFixture} files a
 *    `MidgardLedgerOutputCommitmentV1` under the §5.3 38-byte out-ref key.
 *    The reference-script facts are supplied rather than derived, because the
 *    whole premise of a direction-A decoding fault is a descriptor the
 *    operator admitted over bytes the canonical builder would have refused.
 * 3. **The six step validators published as reference scripts.** The former
 *    step-03 is split into open-subject, bind-descriptor, and
 *    advance-or-close validators so every applied body remains below the
 *    transaction-size ceiling. No step in this family inline-attaches.
 *
 * The payload fixtures are deliberately tiny (§8.2's "a handful of nodes"):
 * the multi-chunk direction-A item crosses exactly one 4,095-byte chunk
 * boundary and refuses after three primitive steps, so one Scan transaction
 * and one Verdict cover the whole machine route.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/codec/cbor";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-core/lucid-data";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "effect";
import "../../src/native-script-decoding/evidence.js";
import "../../src/native-script-decoding/prover.js";
import "../../src/native-script-decoding/submit-common.js";
import "../../src/runtime.js";
import "../../src/spend-input-witness.js";
import "../../src/step-support.js";
import "../../src/transition-trace/phas.js";
import "../../src/transition-trace/reconstruct.js";
import "../../src/tx-layout.js";
import "../../src/witness-reference-scripts.js";
import "./submit-init-emulator-shared.js";
import "./native-script-decoding-emulator.build-decoding-ledger-fixture.js";
import "./native-script-decoding-emulator.build-decoding-block-fixture.js";
import "./native-script-decoding-emulator.setup-decoding-scenario.js";
import "./native-script-decoding-emulator.submit-raw-decoding-step.js";
export { buildDecodingBlockFixture } from "./native-script-decoding-emulator.build-decoding-block-fixture.js";
export {
  buildDecodingLedgerFixture,
  DECODING_SIGNER_KEY,
  type DecodingBlockFixture,
  decodingCanonicalItem,
  decodingItemFromPayload,
  type DecodingLedgerFixture,
  decodingMalformedMaximumItem,
  decodingMalformedMultiChunkItem,
  decodingPlutusItem,
  type DecodingSubjectSource,
  decodingSubjectTransaction,
} from "./native-script-decoding-emulator.build-decoding-ledger-fixture.js";
export {
  DECODING_ACCUSED_TX_ID,
  DECODING_EMULATOR_PROVER_POLICY,
  decodingProverDeps,
  type DecodingScenario,
  type DecodingScenarioSource,
  makeDecodingEmulatorHarness,
  publishDecodingReferenceScripts,
  setupDecodingScenario,
} from "./native-script-decoding-emulator.setup-decoding-scenario.js";
export {
  fundDecodingOutsider,
  type RawDecodingStepLayout,
  submitRawDecodingCancel,
  submitRawDecodingStep,
} from "./native-script-decoding-emulator.submit-raw-decoding-step.js";

/**
 * Asserts a transaction the validator must refuse does not land, and that it
 * died IN THE VALIDATOR rather than in the transaction builder.
 *
 * `localUPLCEval: true` runs the script during `.complete()`, so a validator
 * abort surfaces as an evaluator failure here. The `failed script execution`
 * requirement is what keeps these negatives honest: an offchain builder
 * error — a missing fee input, an unresolvable layout — would otherwise read
 * as a passing security assertion.
 */
export { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";
