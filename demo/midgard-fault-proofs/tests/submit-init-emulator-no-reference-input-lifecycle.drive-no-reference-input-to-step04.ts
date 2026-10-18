import { outRefLabel } from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import {
  submitNoReferenceInputStep01,
  submitNoReferenceInputStep02,
  submitNoReferenceInputStep03,
} from "../src/index.js";
import {
  inputExistenceExecutionContext,
  publishInputExistenceSelectionCarriage,
} from "./support/input-existence-execution-context.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  NO_REFERENCE_INPUT_ABSENT_PRODUCER_TX_ID,
  type NoReferenceInputFixture,
  noReferenceInputOutRef,
} from "./support/no-reference-input-emulator.js";
import { registerPexcludesExclusionRewardAccount } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  makeFaultProofEmulatorHarness,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

export const makeNoReferenceInputHarness = async () =>
  makeFaultProofEmulatorHarness({
    contractOptions: {
      realNoReferenceInput: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerPexcludesExclusionRewardAccount,
  });
type NoReferenceInputHarness = Awaited<
  ReturnType<typeof makeNoReferenceInputHarness>
>;

/** Current source→selection→event pre-state; terminal ledger evidence is a
 * separate physical step. Removal/operator assertions remain in each journey. */
export const driveNoReferenceInputToStep04 = async ({
  harness,
  fixture,
  publishRemoval = false,
}: {
  readonly harness: NoReferenceInputHarness;
  readonly fixture: NoReferenceInputFixture;
  readonly publishRemoval?: boolean;
}) => {
  const context = await inputExistenceExecutionContext({
    harness,
    nativeTx: fixture.nativeTx,
    field: 1,
    inputIndex: fixture.badReferenceInputIndex,
    present: fixture.missingProducerIsCommitted,
  });
  const removalPublications = publishRemoval
    ? await publishRemovalReferenceScripts({
        lucid: harness.proverLucid,
        contracts: harness.contracts,
      })
    : undefined;
  const deploymentInfo = buildRemovalDeploymentInfo(
    harness.contracts,
    harness.catalogue,
    removalPublications === undefined
      ? {}
      : { removalReferenceScripts: removalPublications.published },
  );
  const stageArguments = { ...context.arguments, deploymentInfo };
  const init = await submitInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    fraudCategory: "noReferenceInput",
    fraudulentBlockOutRef: context.setup.fraudulentBlockOutRef,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });
  const first = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    init.firstStepAddress,
    init.computationThreadUnit,
  );
  const step01 = await submitNoReferenceInputStep01({
    ...stageArguments,
    threadOutRef: outRefLabel(first),
  });
  expect(step01.sourceEventKey).toEqual(context.prepared.source.eventKey);
  const fieldCarriage = await publishInputExistenceSelectionCarriage(
    harness,
    context.prepared,
  );
  const step02 = await submitNoReferenceInputStep02({
    ...stageArguments,
    ...fieldCarriage,
    threadOutRef: step01.nextThreadOutRef,
  });
  expect(step02.selectedInput).toEqual(context.prepared.selectedInput);
  const step03 = await submitNoReferenceInputStep03({
    ...stageArguments,
    threadOutRef: step02.nextThreadOutRef,
  });
  expect(step03.eventPreUtxosRoot).toBe(
    context.prepared.transitionMembership.value.pre_utxos_root,
  );
  return {
    setup: context.setup,
    prepared: context.prepared,
    stageArguments,
    deploymentInfo,
    stepReferences: context.stepReferences,
    referenceScripts: context.referenceScripts,
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
