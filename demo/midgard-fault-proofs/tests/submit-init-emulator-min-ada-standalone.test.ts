import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { MIDGARD_COINS_PER_UTXO_BYTE } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitMinAdaCancel,
  submitMinAdaInit,
  submitMinAdaStep05,
  submitMinAdaTxStep01,
  submitMinAdaTxStep02,
  submitMinAdaUtxoStep01,
  submitMinAdaUtxoStep02,
  submitMinAdaUtxoStep03,
  submitMinAdaUtxoStep04,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import {
  buildMinAdaPostUtxoEmulatorFixture,
  buildMinAdaTxEmulatorFixture,
  makeMinAdaEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./support/final-catalogue-emulator-v1.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("min-ada standalone emulator lifecycle", () => {
  it("proves the transaction polarity, cancels and resumes, removes the header, and retains the permanent proof", async () => {
    const harness = await makeMinAdaEmulatorHarness();
    let dispatcherPublicationBytes: number | undefined;
    const refs = await publishFinalFamilyReferenceScripts({
      lucid: harness.proverLucid,
      family: harness.family,
      label: "min-ada",
      enforceL1Envelope: true,
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
    expect(
      yieldRefs.utxo.publicationMeasurement.completeSignedBytes,
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

    const cancelledInit = await submitMinAdaInit(initParams);
    const cancelled = await submitMinAdaCancel({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: `${cancelledInit.txHash}#${cancelledInit.firstStepOutputIndex.toString()}`,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(0);

    const init = await submitMinAdaInit(initParams);
    const step01 = await submitMinAdaTxStep01({
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
    const step02 = await submitMinAdaTxStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      prepared,
      referenceScriptUtxo: refs[1],
      yieldReferenceScriptUtxo: yieldRefs.tx.utxo,
    });
    const step03 = await submitMinAdaUtxoStep03({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      coinsPerUtxoByte: MIDGARD_COINS_PER_UTXO_BYTE,
      referenceScriptUtxo: refs[2],
    });
    const proof = await submitMinAdaStep05({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      referenceScriptUtxo: refs[4],
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

  it("proves the post-UTxO polarity and removes the header without consuming permanent evidence", async () => {
    const harness = await makeMinAdaEmulatorHarness();
    const refs = await publishFinalFamilyReferenceScripts({
      lucid: harness.proverLucid,
      family: harness.family,
      label: "min-ada",
      enforceL1Envelope: true,
    });
    const fixture = await buildMinAdaPostUtxoEmulatorFixture({
      emptyPrevious: true,
    });
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
    expect(
      yieldRefs.utxo.publicationMeasurement.completeSignedBytes,
    ).toBeLessThanOrEqual(16_384);
    const prepared = {
      ...fixture.prepared,
      headerHash: setup.headerHash,
    };
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
    );
    const init = await submitMinAdaInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step01 = await submitMinAdaUtxoStep01({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      prepared,
      referenceScriptUtxo: refs[0],
    });
    const step02 = await submitMinAdaUtxoStep02({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      prepared,
      referenceScriptUtxo: refs[1],
      yieldReferenceScriptUtxo: yieldRefs.utxo.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step03 = await submitMinAdaUtxoStep03({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      coinsPerUtxoByte: MIDGARD_COINS_PER_UTXO_BYTE,
      referenceScriptUtxo: refs[2],
    });
    const step04 = await submitMinAdaUtxoStep04({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      predecessorNonMembershipProofCbor:
        prepared.predecessorNonMembershipProofCbor,
      referenceScriptUtxo: refs[3],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const proof = await submitMinAdaStep05({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step04.nextThreadOutRef,
      referenceScriptUtxo: refs[4],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.family.fraudProof.spendingScriptAddress,
      proof.fraudProofUnit,
    );
    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const now = BigInt(harness.emulator.now());
    await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(
        harness.contracts,
        harness.catalogue,
        { removalReferenceScripts: removalRefs.published },
      ),
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
