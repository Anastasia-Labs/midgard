/**
 * Shared real-contract emulator fixtures for the `mint-authorization` family.
 *
 * The family convicts an operator-ACCEPTED committed transaction that mints
 * under a policy whose native script is either absent from the transaction's
 * machine-consulted source surface (direction A) or present-but-unsatisfied
 * against the committed signer set and validity interval (direction B).
 *
 * What every scenario needs and no existing helper produces is a committed
 * transaction with caller-chosen §2.5 field-5 (mint), field-6 (script
 * witnesses), field-7 (address witnesses) and field-1 (reference inputs)
 * preimages, materialised directly from canonical bytes so the §8.8 field
 * doors the five steps open reproduce the committed commitments exactly.
 *
 * The subject preimages of the four doors the steps consume are always spelt
 * with {@link encodeMidgardFieldPreimage} — byte-for-byte what the planner
 * re-envelopes — so a door's carriage tier is only ever selected by the
 * preimage's own length, never forced.
 */
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "@lucid-evolution/scalus-uplc";
import "effect";
import "../../src/mint-authorization/evidence.js";
import "../../src/mint-authorization/submit-common.js";
import "../../src/spend-input-witness.js";
import "../../src/step-support.js";
import "../../src/tx-layout.js";
import "./emulator/emulator-context.js";
import "./emulator/reference-scripts.js";
import "./native-script-decoding-emulator.js";
import "./submit-init-emulator-shared.js";
import "./mint-authorization-emulator.large-mint-item-cbors.js";
import "./mint-authorization-emulator.build-mint-authorization-subject.js";
import "./mint-authorization-emulator.submit-raw-mint-authorization-step02-tampered-mint.js";

import { network } from "./submit-init-emulator-shared.js";
export {
  buildMintAuthorizationLedgerFixture,
  buildMintAuthorizationSubject,
  makeMintAuthorizationEmulatorHarness,
  type MintAuthorizationHarness,
  type MintAuthorizationScenario,
  publishMintAuthorizationReferenceScripts,
  publishRawFieldPreimageCarriage,
  setupMintAuthorizationScenario,
  tamperFieldPreimageBytes,
} from "./mint-authorization-emulator.build-mint-authorization-subject.js";
export {
  addressWitnessItemCbors,
  directionAPresentScript,
  directionBNativeScript,
  directionBSatisfiedNativeScript,
  largeAddressWitnessItemCbors,
  largeMintItemCbors,
  type MintAuthorizationSubject,
  mintItemCborV1,
  policyIdByte,
  referenceInputItemCbor,
  smallMintItemCbors,
} from "./mint-authorization-emulator.large-mint-item-cbors.js";
export { submitRawMintAuthorizationStep02TamperedMint } from "./mint-authorization-emulator.submit-raw-mint-authorization-step02-tampered-mint.js";
export { expectOnchainRefusal } from "./native-script-decoding-emulator.js";

export { network };
