/**
 * An input-no-idx thread driven through steps 01-03 to its step-04 UTxO, and
 * the conviction step 04 leaves, shared by the step-04 journeys.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  submitInputNoIdxStep02,
  submitInputNoIdxStep03,
  type submitInputNoIdxStep04,
} from "../../src/index.js";
import type {
  InputNoIdxBlockFixture,
  makeEmulatorHarness,
} from "../submit-init-emulator-input-no-idx.build-input-no-idx-block-fixture.js";
import { startInputNoIdxStep02Thread } from "../submit-init-emulator-input-no-idx.measure-step02-proof-transaction.js";
import {
  expectSingleUtxoWithUnit,
  network,
} from "./submit-init-emulator-shared.js";

export type InputNoIdxEmulatorHarness = Awaited<
  ReturnType<typeof makeEmulatorHarness>
>;

/** Steps 01-03 of a thread against `fixture`, up to the step-04 UTxO. */
export const advanceInputNoIdxToStep04 = async (
  harness: InputNoIdxEmulatorHarness,
  fixture: InputNoIdxBlockFixture,
) => {
  const { deploymentInfo, initResult, secondStepUtxo, setup } =
    await startInputNoIdxStep02Thread({ harness, fixture });
  const step02 = await submitInputNoIdxStep02({
    lucid: harness.proverLucid,
    referenceScriptUtxo:
      harness.faultProofReferenceScripts
        .fraudProofNonExistentInputNoIndexStep02!.utxo,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    threadOutRef: outRefLabel(secondStepUtxo),
    inputsPreimage: {
      inputsPreimage: fixture.badInputs,
      badInputsIndex: fixture.badInputsIndex,
    },
    nativeTxCompactCbor: fixture.badTxInclusion.nativeTxCompactCbor,
    awaitConfirmation: true,
  });
  expect(step02.carriageTier).toBe("Inline");
  const thirdStepUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    step02.thirdStepAddress,
    initResult.computationThreadUnit,
  );
  const step03 = await submitInputNoIdxStep03({
    lucid: harness.proverLucid,
    referenceScriptUtxo:
      harness.faultProofReferenceScripts
        .fraudProofNonExistentInputNoIndexStep03!.utxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    threadOutRef: outRefLabel(thirdStepUtxo),
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: fixture.producingTxInclusion,
    awaitConfirmation: true,
  });
  const fourthStepUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    step03.fourthStepAddress,
    initResult.computationThreadUnit,
  );
  return { deploymentInfo, fourthStepOutRef: outRefLabel(fourthStepUtxo) };
};

export const expectInputNoIdxConviction = async (
  harness: InputNoIdxEmulatorHarness,
  result: Awaited<ReturnType<typeof submitInputNoIdxStep04>>,
) => {
  const fraudProofUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    result.fraudProofAddress,
    result.fraudProofUnit,
  );
  expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
    fraud_prover: harness.proverSigner.paymentKeyHash,
  });
};
