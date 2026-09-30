import * as SDK from "@al-ft/midgard-sdk";
import { type Script, type UTxO } from "@lucid-evolution/lucid";

import { fetchUtxoByOutRef, parseOutRef } from "../../src/runtime.js";
import {
  prepareWithdrawalMistag,
  submitRemoveWithdrawalMistagFraudulentBlock,
  submitWithdrawalMistagInit,
  submitWithdrawalMistagStep01,
  submitWithdrawalMistagStep02,
  submitWithdrawalMistagStep03,
  submitWithdrawalMistagStep04,
  submitWithdrawalMistagStep05,
} from "../../src/withdrawal-mistag/index.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeHeader,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
  WITHDRAWAL_MISTAG_REMOVAL_DEPLOYMENT_ENTRY,
} from "./submit-init-emulator-shared.js";
import {
  buildWithdrawalMistagEvidenceMaterial,
  makeWithdrawalMistagEmulatorHarness,
  type WithdrawalMistagDirectionFixture,
} from "./withdrawal-mistag-emulator.build-withdrawal-mistag-evidence-material.js";

export const setupWithdrawalMistagScenario = async ({
  harness,
  direction,
  outputBytes = 0,
  assetCount = 0,
  payoutDatumBytes = 0,
  proofLevels = 0,
  maximumAssetNames = false,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly direction: WithdrawalMistagDirectionFixture;
  readonly outputBytes?: number;
  readonly assetCount?: number;
  readonly payoutDatumBytes?: number;
  readonly proofLevels?: number;
  readonly maximumAssetNames?: boolean;
}) => {
  const material = await buildWithdrawalMistagEvidenceMaterial(
    direction,
    false,
    outputBytes,
    assetCount,
    payoutDatumBytes,
    proofLevels,
    maximumAssetNames,
  );
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.Header = {
    ...makeHeader(operatorVkey, startTime),
    withdrawalsRoot: material.source.root,
    withdrawalCount: material.source.count,
    totalEventCount: material.source.count,
    transitionStepCount: material.trace.count,
    eventToStepRoot: material.event.root,
    transitionTraceRoot: material.trace.root,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  const prepared = await prepareWithdrawalMistag({
    challengedHeaderHash: setup.headerHash,
    ...material.args,
  });
  return { header, setup, prepared };
};

export const publishWithdrawalMistagScripts = async ({
  harness,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
}) => {
  const refs: UTxO[] = [];
  const publicationMeasurements = [];
  for (const [index, step] of harness.withdrawalMistag.steps.entries()) {
    const { utxo, publicationMeasurement } =
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript as Script,
        label: `withdrawal-mistag step-0${(index + 1).toString()}`,
      });
    if (publicationMeasurement.l1ByteMargin < 1_024) {
      throw new Error(
        `withdrawal-mistag step-0${(index + 1).toString()} publication has only ${publicationMeasurement.l1ByteMargin.toString()} bytes of L1 headroom`,
      );
    }
    refs.push(utxo);
    publicationMeasurements.push(publicationMeasurement);
  }
  return {
    refs: refs as unknown as readonly [UTxO, UTxO, UTxO, UTxO, UTxO],
    publicationMeasurements,
  };
};

export const driveWithdrawalMistagToFraud = async ({
  harness,
  scenario,
  refs,
  evidenceReferences,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly scenario: Awaited<ReturnType<typeof setupWithdrawalMistagScenario>>;
  readonly evidenceReferences?: readonly (readonly UTxO[])[];
  readonly refs: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
}) => {
  const transactionMeasurements: Record<
    string,
    CompleteSignedTransactionMeasurement
  > = {};
  const stage = async <T>(label: string, run: () => Promise<T>): Promise<T> => {
    try {
      const captured = await captureEmulatorSubmission(harness.emulator, run);
      const measurement = captured.measurement;
      const { maxTxExMem, maxTxExSteps } = harness.emulator.protocolParameters;
      if (
        measurement.l1ByteMargin <= 0 ||
        measurement.executionMemory > maxTxExMem ||
        measurement.executionSteps > maxTxExSteps
      ) {
        throw new Error(
          `${label} exceeds the real L1 transaction envelope: ${JSON.stringify({
            completeSignedBytes: measurement.completeSignedBytes,
            l1ByteMargin: measurement.l1ByteMargin,
            executionMemory: measurement.executionMemory.toString(),
            maxTxExMem: maxTxExMem.toString(),
            executionSteps: measurement.executionSteps.toString(),
            maxTxExSteps: maxTxExSteps.toString(),
          })}`,
        );
      }
      transactionMeasurements[label] = measurement;
      return captured.result;
    } catch (error) {
      throw new Error(
        `withdrawal-mistag ${label} failed: ${JSON.stringify(error)}`,
      );
    }
  };
  const init = await stage("init", () =>
    initWithdrawalMistagThread({ harness, scenario }),
  );
  const blockUtxo = await withdrawalMistagBlockUtxo({ harness, scenario });
  const step01 = await stage("step-01", () =>
    submitWithdrawalMistagStep01({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: init.nextThreadOutRef,
      hubOracleUtxo: scenario.setup.hubOracle,
      stateQueueBlockUtxo: blockUtxo,
      referenceScriptUtxo: refs[0],
      evidenceReferences: evidenceReferences?.[0],
    }),
  );
  const step02 = await stage("step-02", () =>
    submitWithdrawalMistagStep02({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step01.nextThreadOutRef,
      referenceScriptUtxo: refs[1],
      evidenceReferences: evidenceReferences?.[1],
    }),
  );
  const step03 = await stage("step-03", () =>
    submitWithdrawalMistagStep03({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step02.nextThreadOutRef,
      referenceScriptUtxo: refs[2],
      evidenceReferences: evidenceReferences?.[2],
    }),
  );
  const step04 = await stage("step-04", () =>
    submitWithdrawalMistagStep04({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step03.nextThreadOutRef,
      referenceScriptUtxo: refs[3],
      evidenceReferences: evidenceReferences?.[3],
    }),
  );
  const fraud = await stage("step-05", () =>
    submitWithdrawalMistagStep05({
      lucid: harness.proverLucid,
      contracts: harness.withdrawalMistag,
      signer: harness.proverSigner,
      prepared: scenario.prepared,
      threadOutRef: step04.nextThreadOutRef,
      referenceScriptUtxo: refs[4],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  return {
    init,
    step01,
    step02,
    step03,
    step04,
    fraud,
    transactionMeasurements,
  };
};

export const initWithdrawalMistagThread = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly scenario: Awaited<ReturnType<typeof setupWithdrawalMistagScenario>>;
}) =>
  await submitWithdrawalMistagInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.withdrawalMistag,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });

export const withdrawalMistagBlockUtxo = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly scenario: Awaited<ReturnType<typeof setupWithdrawalMistagScenario>>;
}) =>
  await fetchUtxoByOutRef({
    lucid: harness.proverLucid,
    outRef: parseOutRef(
      scenario.setup.fraudulentBlockOutRef,
      "fraudulent block",
    ),
    label: "withdrawal-mistag fraudulent block",
  });

export const removeWithdrawalMistagBlock = async ({
  harness,
  scenario,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeWithdrawalMistagEmulatorHarness>
  >;
  readonly scenario: Awaited<ReturnType<typeof setupWithdrawalMistagScenario>>;
}) => {
  const removalReferenceScripts = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(
    harness.contracts,
    harness.catalogue,
    { removalReferenceScripts: removalReferenceScripts.published },
  );
  const now = BigInt(harness.emulator.now());
  return await submitRemoveWithdrawalMistagFraudulentBlock({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    contracts: harness.withdrawalMistag,
    firstStepDeploymentEntry: WITHDRAWAL_MISTAG_REMOVAL_DEPLOYMENT_ENTRY,
    fraudulentHeaderHash: scenario.setup.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
};
