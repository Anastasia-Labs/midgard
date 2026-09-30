import { outRefLabel } from "@al-ft/midgard-core";
import { expect } from "vitest";

import {
  neSubmitStep01,
  neSubmitStep02,
  submitInit,
} from "./support/legacy-submit-emulator.js";
import {
  buildNonExistentInputFixture,
  countedTransactionsRoot,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/**
 * Q11's correction path up to and including the step that opens the preimage,
 * with the challenged transaction spending `cardinality` inputs and the phantom
 * one last.
 */
export const runNoInputCardinalityJourney = async (
  cardinality: number,
  { publishCarriage = false }: { readonly publishCarriage?: boolean } = {},
): Promise<{
  readonly stages: Record<string, CompleteSignedTransactionMeasurement>;
  readonly carriageTiers: Record<string, string>;
}> => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realNonExistentInput: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
  } = harness;
  const fixture = await buildNonExistentInputFixture({
    spendInputCardinality: cardinality,
  });
  expect(fixture.inputsPreimage.length).toBe(cardinality);
  expect(fixture.badInputIndex).toBe(BigInt(cardinality - 1));

  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const fraudulentHeader = makeHeader(
    await funderPaymentKeyHash(funderLucid),
    headerStartTime,
    await countedTransactionsRoot(
      fixture.transactionsRoot,
      fixture.l2TransactionCount,
    ),
    fixture.l2TransactionCount,
  );
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: fraudulentHeader,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);

  const initResult = await submitInit({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    fraudCategory: "nonExistentInput",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const step01Capture = await captureEmulatorSubmission(emulator, async () =>
    neSubmitStep01({
      lucid: proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts.fraudProofNonExistentInput!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.inclusion,
      awaitConfirmation: true,
    }),
  );
  const secondStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step01Capture.result.secondStepAddress,
    initResult.computationThreadUnit,
  );
  const step02Capture = await captureEmulatorSubmission(emulator, async () =>
    neSubmitStep02({
      lucid: proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts.fraudProofNonExistentInputStep02!
          .utxo,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      inputsPreimage: fixture.inputsPreimage,
      nativeTxCompactCbor: fixture.inclusion.nativeTxCompactCbor,
      badInputIndex: fixture.badInputIndex,
      publishCarriage,
      awaitConfirmation: true,
    }),
  );
  // Inline, step-02 is one submission; routed — by size or by the #612
  // demotion — its §8.7 carriage publication precedes it and is a stage of
  // its own.
  const step02Routed =
    step02Capture.result.spendInputsCarriageTier !== "Inline";
  expect(step02Capture.measurements.length).toBe(step02Routed ? 2 : 1);
  return {
    stages: {
      "step-01": step01Capture.measurement,
      ...(step02Routed
        ? { "step-02-carriage": step02Capture.measurements[0]! }
        : {}),
      "step-02": step02Capture.measurement,
    },
    carriageTiers: {
      "step-02": step02Capture.result.spendInputsCarriageTier,
    },
  };
};
