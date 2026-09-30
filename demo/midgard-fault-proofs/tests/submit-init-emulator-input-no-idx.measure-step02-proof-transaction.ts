import { createHash } from "node:crypto";

import { outRefLabel } from "@al-ft/midgard-core";
import { CML, PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { submitInputNoIdxStep01 } from "../src/index.js";
import {
  HALF_CANONICAL_MATURITY_MS,
  type InputNoIdxBlockFixture,
  makeEmulatorHarness,
  STEP02_RELEASE_CPU_LIMIT,
  STEP02_RELEASE_MEMORY_LIMIT,
} from "./submit-init-emulator-input-no-idx.build-input-no-idx-block-fixture.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import { setupFraudulentBlock as setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
} from "./support/submit-init-emulator-shared.js";

/*
 * The byte-exact `CompletePublished` proof-fit pin that stood here is **gone,
 * not relaxed.** It measured a transaction shape that no longer exists: the
 * retired route referenced a bespoke `PublishedSpendInputsV1` datum, and #604
 * replaced it with §8.5 raw carriage under a different redeemer. Every measured
 * quantity — signed bytes, fee, execution units, the CBOR sha256 — moves with
 * that change, so re-pinning here would mean inventing numbers rather than
 * measuring them.
 *
 * Re-measurement is **#580**, which this ticket blocks by owner order precisely
 * so it runs against working builders. Until then the structural fit assertions
 * (`expectStep02ProofFit`, the release memory/cpu margins, the reference-input
 * shape) are what hold, and they are the ones that would catch a regression in
 * kind rather than in degree.
 */

export const measureStep02ProofTransaction = ({
  transactionCbor,
  outputIndex,
  elapsedMs,
}: {
  readonly transactionCbor: string;
  readonly outputIndex: number;
  readonly elapsedMs?: number;
}) => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const output = body.outputs().get(outputIndex);
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  let executionMemory = 0n;
  let executionCpu = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const units = redeemers!.get(index).ex_units();
    executionMemory += units.mem();
    executionCpu += units.steps();
  }
  const signedTxBytes = transactionCbor.length / 2;
  const outputValueBytes = output.amount().to_cbor_bytes().length;
  const localBuildSubmitConfirmWallMs =
    elapsedMs === undefined ? undefined : Number(elapsedMs.toFixed(3));
  return {
    signedTxBytes,
    signedTxSha256: createHash("sha256")
      .update(Buffer.from(transactionCbor, "hex"))
      .digest("hex"),
    txByteMargin: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize - signedTxBytes,
    fee: body.fee(),
    executionMemory,
    executionCpu,
    releaseMemoryMargin: STEP02_RELEASE_MEMORY_LIMIT - executionMemory,
    releaseCpuMargin: STEP02_RELEASE_CPU_LIMIT - executionCpu,
    inputCount: body.inputs().len(),
    referenceInputCount: body.reference_inputs()?.len() ?? 0,
    outputCount: body.outputs().len(),
    collateralInputCount: body.collateral_inputs()?.len() ?? 0,
    vkeyWitnessCount: transaction.witness_set().vkeywitnesses()?.len() ?? 0,
    redeemerCount: redeemers?.len() ?? 0,
    outputLovelace: output.amount().coin(),
    outputMinAda: CML.min_ada_required(
      output,
      BigInt(PROTOCOL_PARAMETERS_DEFAULT.coinsPerUtxoByte),
    ),
    outputValueBytes,
    valueByteMargin: PROTOCOL_PARAMETERS_DEFAULT.maxValSize - outputValueBytes,
    ...(localBuildSubmitConfirmWallMs === undefined
      ? {}
      : {
          localBuildSubmitConfirmWallMs,
          localHalfMaturityMarginMs:
            HALF_CANONICAL_MATURITY_MS - localBuildSubmitConfirmWallMs,
        }),
  };
};

export const expectStep02ProofFit = (
  measurement: ReturnType<typeof measureStep02ProofTransaction>,
): void => {
  expect(measurement.txByteMargin).toBeGreaterThanOrEqual(0);
  expect(measurement.releaseMemoryMargin).toBeGreaterThanOrEqual(0n);
  expect(measurement.releaseCpuMargin).toBeGreaterThanOrEqual(0n);
  expect(measurement.outputLovelace).toBeGreaterThanOrEqual(
    measurement.outputMinAda,
  );
  expect(measurement.valueByteMargin).toBeGreaterThanOrEqual(0);
  expect(measurement.vkeyWitnessCount).toBe(1);
  expect(measurement.redeemerCount).toBe(1);
  if (measurement.localBuildSubmitConfirmWallMs !== undefined) {
    expect(measurement.localBuildSubmitConfirmWallMs).toBeGreaterThan(0);
    expect(measurement.localHalfMaturityMarginMs).toBeGreaterThan(0);
  }
};

export const startInputNoIdxStep02Thread = async ({
  harness,
  fixture,
}: {
  readonly harness: Awaited<ReturnType<typeof makeEmulatorHarness>>;
  readonly fixture: InputNoIdxBlockFixture;
}) => {
  const setup = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(
    harness.contracts,
    harness.catalogue,
  );
  const initResult = await submitInit({
    lucid: harness.proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    fraudCategory: "nonExistentInputNoIndex",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const step01Result = await submitInputNoIdxStep01({
    lucid: harness.proverLucid,
    referenceScriptUtxo:
      harness.faultProofReferenceScripts.fraudProofNonExistentInputNoIndex!
        .utxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    threadOutRef: outRefLabel(firstStepUtxo),
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: fixture.badTxInclusion,
    awaitConfirmation: true,
  });
  const secondStepUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    step01Result.secondStepAddress,
    initResult.computationThreadUnit,
  );
  return {
    deploymentInfo,
    initResult,
    secondStepUtxo,
    setup,
    step01Result,
  };
};
