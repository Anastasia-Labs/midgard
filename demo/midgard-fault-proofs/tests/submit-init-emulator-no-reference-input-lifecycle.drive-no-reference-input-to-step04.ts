import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import {
  submitNoReferenceInputStep01,
  submitNoReferenceInputStep02,
  submitNoReferenceInputStep03,
} from "../src/index.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID,
  type NoReferenceInputFixture,
  noReferenceInputOutRef,
  publishNoReferenceInputReferenceScripts,
} from "./support/no-reference-input-emulator.js";
import {
  expectStateQueueHeaderOrder,
  registerPexcludesExclusionRewardAccount,
  setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  makeFaultProofEmulatorHarness,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

export const makeNoReferenceInputHarness = async () =>
  await makeFaultProofEmulatorHarness({
    contractOptions: {
      realNoReferenceInput: true,
      alwaysFraudProofCatalogue: true,
    },
    // Steps 03 and 04 both delegate their non-membership claim to
    // `pexcludes.exclusion.withdraw`, whose reward account must exist first.
    registerAdditionalRewardAccounts: registerPexcludesExclusionRewardAccount,
  });

type NoReferenceInputHarness = Awaited<
  ReturnType<typeof makeNoReferenceInputHarness>
>;

/**
 * init → step-01 → step-02 → step-03, the segment all three journeys share.
 *
 * Everything up to and including step-03 is satisfiable for an honest block
 * too: the challenged out-ref genuinely is absent from a block's prev ledger
 * when its producer sits inside the same block. Only step-04 separates the two
 * cases, which is why the adversarial journey drives this same helper.
 */
export const driveNoReferenceInputToStep04 = async ({
  harness,
  fixture,
  publishRemoval = false,
}: {
  readonly harness: NoReferenceInputHarness;
  readonly fixture: NoReferenceInputFixture;
  readonly publishRemoval?: boolean;
}) => {
  const {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    contracts,
    catalogue,
  } = harness;
  const steps = contracts.fraudProofContracts.noReferenceInput.steps;
  // The harness's one-shot nonce is the funder's first UTxO, so nothing may
  // spend from the funder wallet before `setupFraudulentBlock` consumes it.
  const setup = await setupFraudulentBlock({
    funderLucid,
    emulator,
    contracts,
    catalogue,
    fixture,
  });
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [setup.headerHash],
  });
  // Removal must source its seven validators from reference inputs to stay
  // inside the 16,384-byte L1 envelope; only the journey that removes pays for
  // publishing them.
  const removalPublications = publishRemoval
    ? await publishRemovalReferenceScripts({ lucid: proverLucid, contracts })
    : undefined;
  const stepReferences = await publishNoReferenceInputReferenceScripts({
    lucid: funderLucid,
    steps,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
    ...(removalPublications === undefined
      ? {}
      : { removalReferenceScripts: removalPublications.published }),
  });

  const init = await submitInit({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    fraudCategory: "noReferenceInput",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  expect(init.fraudCategoryName).toBe("noReferenceInput");
  expect(init.computationThreadAssetName).toBe(
    `${catalogue.categories.noReferenceInput.categoryId}${setup.headerHash}`,
  );
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    init.firstStepAddress,
    init.computationThreadUnit,
  );

  const step01 = await submitNoReferenceInputStep01({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(firstStepUtxo),
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: fixture.inclusion,
    referenceScriptUtxo: stepReferences[0],
    awaitConfirmation: true,
  });
  // The §2.5 anchor is the disputed transaction's id, and the two roots the
  // later steps open are the ones the on-chain header carries: an EMPTY prev
  // ledger and the block's RAW transactions PHAS root.
  expect(step01.badTxId).toBe(fixture.subjectTxId);
  expect(step01.blocksPrevUtxosRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  expect(step01.blocksTransactionsRoot).toBe(fixture.transactionsRoot);

  const step02 = await submitNoReferenceInputStep02({
    lucid: proverLucid,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: step01.nextThreadOutRef,
    referenceInputsPreimage: fixture.referenceInputsPreimage,
    nativeTxCompactCbor: fixture.nativeTxCompactCbor,
    badReferenceInputIndex: fixture.badReferenceInputIndex,
    referenceScriptUtxo: stepReferences[1],
    awaitConfirmation: true,
  });
  expect(step02.referenceInputsItemCount).toBe(fixture.referenceInputs.length);
  expect(step02.missingReferenceInput).toStrictEqual(
    fixture.missingReferenceInput,
  );

  const step03 = await submitNoReferenceInputStep03({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: step02.nextThreadOutRef,
    ledgerNonMembershipProofCbor: fixture.ledgerNonMembershipProofCbor,
    referenceScriptUtxo: stepReferences[2],
    awaitConfirmation: true,
  });
  expect(step03.missingReferenceInputTxId).toBe(
    fixture.missingReferenceInput.tx_id,
  );

  return {
    setup,
    deploymentInfo,
    stepReferences,
    init,
    step01,
    step02,
    step03,
  };
};

/**
 * §8.4's tier-1 redeemer bound is 14,336 bytes. A §5.3 out-ref item is a
 * constant 38 bytes and contributes a constant 40-byte stride to the §5.1
 * preimage, so the partition sits between 358 items (14,323 bytes: tier 1) and
 * 359 (14,363: tier 2). 365 items — 14,600 bytes of stride plus §5.1's 3-byte
 * envelope = 14,603 — is a round count comfortably past the bound and still
 * inside the single-publication tier-2 window `(14,336, 15,148]`. The
 * challenged reference input is item 200 of the 365: the fault is a property
 * of one item, so the rest are free to be decoys.
 */
export const TIER2_REFERENCE_INPUT_COUNT = 365;

export const TIER2_BAD_REFERENCE_INPUT_INDEX = 200;

export const tier2ReferenceInputs = (): readonly SDK.MidgardTxInput[] => {
  const decoy = (index: number): SDK.MidgardTxInput =>
    noReferenceInputOutRef((index + 1).toString(16).padStart(64, "0"), 0);
  const items: SDK.MidgardTxInput[] = [];
  for (let index = 0; index < TIER2_REFERENCE_INPUT_COUNT - 1; index += 1) {
    items.push(decoy(index));
  }
  items.splice(
    TIER2_BAD_REFERENCE_INPUT_INDEX,
    0,
    noReferenceInputOutRef(NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID, 0),
  );
  return items;
};
