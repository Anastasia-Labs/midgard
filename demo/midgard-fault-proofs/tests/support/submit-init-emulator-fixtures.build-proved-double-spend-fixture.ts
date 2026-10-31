import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitStep04,
} from "./legacy-submit-emulator.js";
import { bindCanonicalFixtureHeader } from "./submit-init-emulator-fixtures.deployment-context.js";
import {
  midgardTxInput,
  positiveNonAdaAssets,
} from "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
import { type ProvedDoubleSpendFixture } from "./submit-init-emulator-fixtures.instrument-lucid-for-removal.js";
import {
  type DoubleSpendFixtureOptions,
  stageDoubleSpendStep04,
} from "./submit-init-emulator-fixtures.stage-double-spend-step04.js";
import {
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  network,
} from "./submit-init-emulator-shared.js";

export const buildProvedDoubleSpendFixture = async ({
  successorsAfterProofCount = 0,
  ...options
}: DoubleSpendFixtureOptions & {
  /**
   * Successors committed after the fraud-proof token is minted and before any
   * removal, appended after the `successorCount` ones. `successors` lists both
   * sets in queue order; `fraudulentBlockOutRef` stays the one the proof used.
   */
  readonly successorsAfterProofCount?: number;
} = {}): Promise<
  ProvedDoubleSpendFixture & {
    readonly canonicalEvidence?: Awaited<
      ReturnType<typeof bindCanonicalFixtureHeader>
    >;
  }
> => {
  const {
    emulator,
    realBlueprint,
    funderLucid,
    proverLucid,
    proverSigner,
    contracts,
    catalogue,
    canonical,
    transactionInclusion,
    fraudulentHeader,
    headerHash,
    setup,
    successors,
    appendSuccessors,
    deploymentInfo,
    removalReferenceScriptPublications,
    fraudulentBlockOutRef,
    submitInitResult,
    submitInitMeasurement,
    doubleSpendStepReferenceScripts,
    witnessReferenceScripts,
    proverPaymentKeyHash,
    step03Result,
    fourthStepUtxo,
  } = await stageDoubleSpendStep04(options);
  const step04Capture = await captureEmulatorSubmission(emulator, () =>
    submitStep04({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      tx2SpendInputCbors: parseSpendInputCbors(
        transactionInclusion.tx2SpendInputCbors,
        "--tx2-inputs",
      ),
      nativeTxCompactCbor: parseSubmitStep01TxInclusion(
        transactionInclusion.tx2.inclusion,
      ).nativeTxCompactCbor,
      doubleSpentInputIndex: 1n,
      referenceScriptUtxo:
        doubleSpendStepReferenceScripts["fraudProofDoubleSpendStep04"]!.utxo,
      witnessReferenceScripts,
      awaitConfirmation: true,
    }),
  );
  const step04Result = step04Capture.result;
  const step04Measurement = step04Capture.measurement;

  expect(step04Result.txHash).toHaveLength(64);
  expect(step04Result.doubleSpentInputIndex).toBe(1);
  expect(step04Result.doubleSpentInput).toEqual(
    midgardTxInput(transactionInclusion.tx2InputsPreimage[1]!),
  );
  expect(step04Result.doubleSpentInputCbor).toEqual(
    transactionInclusion.tx2SpendInputCbors[1],
  );
  expect(step04Result.fraudProofAssetName).toBe(
    submitInitResult.computationThreadAssetName,
  );
  expect(step04Result.fraudProofUnit).toBe(
    toUnit(
      contracts.fraudProof.policyId,
      submitInitResult.computationThreadAssetName,
    ),
  );
  expect(step04Result.fraudProofMintRedeemerIndex).not.toBe(
    step04Result.computationThreadMintRedeemerIndex,
  );

  const remainingFourthStepUtxos = await proverLucid.utxosAtWithUnit(
    step03Result.fourthStepAddress,
    submitInitResult.computationThreadUnit,
  );
  expect(remainingFourthStepUtxos).toHaveLength(0);
  const fraudProofUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step04Result.fraudProofAddress,
    step04Result.fraudProofUnit,
  );
  const fraudProofDatum = Data.from(
    fraudProofUtxo.datum!,
    FraudProofTokenDatum,
  );
  expect(fraudProofDatum).toEqual({
    fraud_prover: proverPaymentKeyHash,
  });
  expect(fraudProofUtxo.assets[step04Result.fraudProofUnit]).toBe(1n);
  expect(positiveNonAdaAssets(fraudProofUtxo)).toEqual([
    [step04Result.fraudProofUnit, 1n],
  ]);

  await appendSuccessors(successorsAfterProofCount, true);

  return {
    emulator,
    realBlueprint,
    funderLucid,
    proverLucid,
    proverSigner,
    contracts,
    catalogue,
    transactionInclusion,
    fraudulentHeader,
    headerHash,
    setup,
    successors,
    deploymentInfo,
    removalReferenceScriptPublications,
    fraudulentBlockOutRef,
    submitInitResult,
    submitInitMeasurement,
    step04Result,
    step04Measurement,
    doubleSpendStepReferenceScripts,
    witnessReferenceScripts,
    fraudProofUtxo,
    proverPaymentKeyHash,
    ...(canonical === undefined
      ? {}
      : {
          canonicalEvidence: await bindCanonicalFixtureHeader(
            canonical,
            fraudulentHeader,
            headerHash,
          ),
        }),
  };
};
