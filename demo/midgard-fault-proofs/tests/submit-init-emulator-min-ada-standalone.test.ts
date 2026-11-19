import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { buildMinAdaInit } from "../src/min-ada/submit-init.js";
import { buildMinAdaTxStep01 } from "../src/min-ada/submit-step-01.js";
import { buildMinAdaTxStep02 } from "../src/min-ada/submit-step-02.submit-min-ada-tx-step02.js";
import { buildMinAdaStep03 } from "../src/min-ada/submit-step-03.js";
import { buildMinAdaStep04 } from "../src/min-ada/submit-step-04.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import { network } from "./support/emulator/blueprints.js";
import { expectSingleUtxoWithUnit } from "./support/emulator/emulator-context.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import {
  buildMinAdaTxEmulatorFixture,
  makeMinAdaEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./support/final-catalogue-emulator.build-min-ada-tx-emulator-fixture.js";
import { submitFamilyCancel } from "./support/linear-fault-cancel.js";
import { pinStepTranscript } from "./support/step-transcript.js";
import { setupFraudulentBlock } from "./support/submit-init/removal.js";
import {
  buildAndSubmitUnjournaled,
  submitTurnsUnjournaled,
} from "./support/submit-unjournaled.js";

describe("min-ada standalone emulator lifecycle", () => {
  it("proves the transaction polarity, cancels and resumes, removes the header, and retains the permanent proof", async () => {
    pinStepTranscript();
    const harness = await makeMinAdaEmulatorHarness();
    let dispatcherPublicationBytes: number | undefined;
    const refs = await publishFinalFamilyReferenceScripts({
      lucid: harness.proverLucid,
      family: harness.family,
      label: "min-ada",
      onPublication: (stepIndex, publication) => {
        if (stepIndex === 1) {
          dispatcherPublicationBytes =
            publication.publicationMeasurement.completeSignedBytes;
        }
      },
    });
    expect(dispatcherPublicationBytes).toBeGreaterThan(0);
    expect(dispatcherPublicationBytes).toBeLessThanOrEqual(16_384);
    const fixture = await buildMinAdaTxEmulatorFixture();
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture,
    });
    const yieldRefs = setup.minAdaYieldReferenceScripts;
    if (yieldRefs === undefined) {
      throw new Error("min-ada setup omitted authenticated yield publications");
    }
    expect(
      yieldRefs.tx.publicationMeasurement.completeSignedBytes,
    ).toBeLessThanOrEqual(16_384);
    const prepared = { ...fixture.prepared, headerHash: setup.headerHash };
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
    );
    const initParams = {
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    } as const;

    const cancelledInit = await buildAndSubmitUnjournaled(
      buildMinAdaInit,
      initParams,
    );
    const cancelled = await submitFamilyCancel("min-ada", {
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: `${cancelledInit.txHash}#${cancelledInit.firstStepOutputIndex.toString()}`,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(0);

    const init = await buildAndSubmitUnjournaled(buildMinAdaInit, initParams);
    const step01 = await buildAndSubmitUnjournaled(buildMinAdaTxStep01, {
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      prepared,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step02 = await submitTurnsUnjournaled(buildMinAdaTxStep02, {
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      prepared,
      referenceScriptUtxo: refs[1],
      yieldReferenceScriptUtxo: yieldRefs.tx.utxo,
    });
    const step03 = await submitTurnsUnjournaled(buildMinAdaStep03, {
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      outputItemCbors: prepared.outputItemCbors,
      referenceScriptUtxo: refs[2],
    });
    const proof = await buildAndSubmitUnjournaled(buildMinAdaStep04, {
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      referenceScriptUtxo: refs[3],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.family.fraudProof.spendingScriptAddress,
      proof.fraudProofUnit,
    );
    expect(Data.from(proofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
    });

    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deployment = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalRefs.published },
    );
    const now = BigInt(harness.emulator.now());
    await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: deployment,
      network,
      signer: harness.proverSigner,
      fraudCategory: "minAda",
      fraudulentHeaderHash: setup.headerHash,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    expect(
      outRefLabel(
        await expectSingleUtxoWithUnit(
          harness.proverLucid,
          harness.family.fraudProof.spendingScriptAddress,
          proof.fraudProofUnit,
        ),
      ),
    ).toBe(outRefLabel(proofUtxo));
  }, 300_000);
});
