import { outRefLabel } from "@al-ft/midgard-core";
import { type DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  type FraudProofCatalogueDeploymentInfo,
  FraudProofTokenDatum,
  Header,
  type MidgardValidators,
} from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  Lucid,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { resolveProverSigner } from "../../../src/index.js";
import {
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitStep04,
} from "../legacy-submit-emulator.js";
import {
  type Blueprint,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  expectSingleUtxoWithUnit,
  network,
  publishFaultProofWitnessReferenceScripts,
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "../submit-init-emulator-shared.js";
import { bindCanonicalFixtureHeader } from "./deployment.js";
import {
  type DoubleSpendFixtureOptions,
  stageDoubleSpendStep04,
} from "./double-spend-staging.js";
import { buildTransactionInclusionFixture } from "./inclusion.js";
import { type SuccessorBlockFixture } from "./successor.js";
import {
  midgardTxInput,
  positiveNonAdaAssets,
} from "./transaction-material.js";

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

export type ProvedDoubleSpendFixture = {
  readonly emulator: Emulator;
  readonly realBlueprint: Blueprint;
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverSigner: ReturnType<typeof resolveProverSigner>;
  readonly contracts: MidgardValidators & {
    /** The slot mapping the deployment's commit yield was built with. */
    readonly l1SlotConfig: L1SlotConfig;
  };
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly transactionInclusion: Awaited<
    ReturnType<typeof buildTransactionInclusionFixture>
  >;
  readonly fraudulentHeader: Header;
  readonly headerHash: string;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly successors: readonly SuccessorBlockFixture[];
  readonly deploymentInfo:
    | ReturnType<typeof buildRemovalDeploymentInfo>
    | DeploymentManifest;
  readonly removalReferenceScriptPublications: Awaited<
    ReturnType<typeof publishRemovalReferenceScripts>
  >;
  readonly fraudulentBlockOutRef: string;
  readonly submitInitResult: Awaited<ReturnType<typeof submitInit>>;
  readonly submitInitMeasurement: CompleteSignedTransactionMeasurement;
  readonly step04Result: Awaited<ReturnType<typeof submitStep04>>;
  readonly step04Measurement: CompleteSignedTransactionMeasurement;
  readonly doubleSpendStepReferenceScripts: Awaited<
    ReturnType<typeof publishFraudProofChainReferenceScripts>
  >;
  readonly witnessReferenceScripts: Awaited<
    ReturnType<typeof publishFaultProofWitnessReferenceScripts>
  >;
  readonly fraudProofUtxo: UTxO;
  readonly proverPaymentKeyHash: string;
};
