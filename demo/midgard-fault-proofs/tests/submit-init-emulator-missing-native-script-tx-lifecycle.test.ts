import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  prepareMissingNativeScriptTxV1,
  submitMissingNativeScriptTxCancel,
  submitMissingNativeScriptTxInit,
  submitMissingNativeScriptTxStep01,
  submitMissingNativeScriptTxStep02,
  submitMissingNativeScriptTxStep03,
  submitMissingNativeScriptTxStep04,
  submitMissingNativeScriptTxStep05,
  submitMissingNativeScriptTxStep06,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import {
  makeMissingNativeScriptTxEmulatorHarnessV1,
  publishMissingNativeScriptTxReferenceScriptsV1,
  setupMissingNativeScriptTxFixtureV1,
} from "./support/missing-native-script-tx-emulator-v1.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("missing-native-script-tx emulator lifecycle", () => {
  it("proves the absent script through six reference-script steps, cancels explicitly, and removes the fraudulent commitment", async () => {
    const harness = await makeMissingNativeScriptTxEmulatorHarnessV1();
    const fixture = await setupMissingNativeScriptTxFixtureV1({ harness });
    const refs = await publishMissingNativeScriptTxReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const badInclusion = fixture.block.txInclusion;
    const producingInclusion = fixture.block.txInclusions.get(
      fixture.producingTxId,
    );
    if (badInclusion === null || producingInclusion === undefined) {
      throw new Error("two-transaction fixture is missing inclusion evidence");
    }
    const prepared = prepareMissingNativeScriptTxV1({
      badTxInclusion: badInclusion,
      badTxSpendInputs: fixture.badTxSpendInputs,
      badInputIndex: 0n,
      producingTxInclusion: producingInclusion,
      producingOutputItemCbors: fixture.producingOutputItemCbors,
      missingNativeScriptBytes: fixture.nativeScriptBytes,
      badTxWitnessSet: fixture.badTxWitnessSet,
      badTxScriptWitnessItemCbors: fixture.badTxScriptWitnessItemCbors,
      owner: harness.proverSigner.paymentKeyHash,
    });
    expect(prepared.expectedMissingScriptHash).toBe(fixture.expectedScriptHash);
    const init = await submitMissingNativeScriptTxInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: fixture.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step01 = await submitMissingNativeScriptTxStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: fixture.setup.fraudulentBlockOutRef,
      txInclusion: badInclusion,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(step01.badTxWitnessSetHash).toBe(
      badInclusion.nativeTx.witness_set_hash,
    );
    const step02 = await submitMissingNativeScriptTxStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      nativeTxCompactCbor: badInclusion.nativeTxCompactCbor,
      spendInputs: fixture.badTxSpendInputs,
      badInputIndex: 0n,
      referenceScriptUtxo: refs[1],
    });
    expect(step02.inputWithMissingScript.tx_id).toBe(fixture.producingTxId);
    const step03 = await submitMissingNativeScriptTxStep03({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      stateQueueBlockOutRef: fixture.setup.fraudulentBlockOutRef,
      txInclusion: producingInclusion,
      referenceScriptUtxo: refs[2],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step04 = await submitMissingNativeScriptTxStep04({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      nativeTxCompactCbor: producingInclusion.nativeTxCompactCbor,
      outputItemCbors: fixture.producingOutputItemCbors,
      referenceScriptUtxo: refs[3],
    });
    expect(step04.expectedMissingScriptHash).toBe(fixture.expectedScriptHash);
    const step05 = await submitMissingNativeScriptTxStep05({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step04.nextThreadOutRef,
      missingNativeScriptBytes: fixture.nativeScriptBytes,
      referenceScriptUtxo: refs[4],
    });
    const step06 = await submitMissingNativeScriptTxStep06({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step05.nextThreadOutRef,
      nativeTxCompactCbor: badInclusion.nativeTxCompactCbor,
      witnessSet: fixture.badTxWitnessSet,
      scriptTxWitsItems: fixture.badTxScriptWitnessItemCbors,
      referenceScriptUtxo: refs[5],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });

    const threadUnit = toUnit(
      harness.family.computationThread.policyId,
      `${harness.category.categoryId}${fixture.setup.headerHash}`,
    );
    for (const step of harness.family.steps) {
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          threadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.family.fraudProof.spendingScriptAddress,
      step06.fraudProofUnit,
    );
    expect(outRefLabel(fraudProofUtxo)).toBe(step06.fraudProofOutRef);
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: harness.proverSigner.paymentKeyHash });

    // Cancellation is a second explicit prover decision, here from step 02.
    const cancelInit = await submitMissingNativeScriptTxInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: fixture.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const cancelStep01 = await submitMissingNativeScriptTxStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: cancelInit.nextThreadOutRef,
      stateQueueBlockOutRef: fixture.setup.fraudulentBlockOutRef,
      txInclusion: badInclusion,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const cancellation = await submitMissingNativeScriptTxCancel({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: cancelStep01.nextThreadOutRef,
      referenceScriptUtxo: refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancellation.cancelledStepIndex).toBe(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.family.steps[1].spendingScriptAddress,
        threadUnit,
      ),
    ).resolves.toHaveLength(0);

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
    const removal = await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: deployment,
      network,
      signer: harness.proverSigner,
      fraudCategory: "missingNativeScriptTx",
      fraudulentHeaderHash: fixture.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removal.fraudCategory).toBe("missingNativeScriptTx");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        fixture.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [root] = await harness.proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      fixture.setup.stateQueueRootUnit,
    );
    if (root === undefined) throw new Error("Removal lost the queue root");
    const finalRoot = await Effect.runPromise(
      SDK.utxoToStateQueueUTxO(root, harness.contracts.stateQueue.policyId),
    );
    expect(finalRoot.datum.next).toBe("Empty");
    const [scheduler] = await harness.proverLucid.utxosAtWithUnit(
      harness.contracts.scheduler.spendingScriptAddress,
      toUnit(harness.contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
    );
    expect(Data.from(scheduler!.datum!, SDK.SchedulerDatum)).toBe(
      "NoActiveOperators",
    );
    const retained = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.family.fraudProof.spendingScriptAddress,
      step06.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(step06.fraudProofOutRef);
    await expect(
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: deployment,
        network,
        signer: harness.proverSigner,
        fraudCategory: "missingNativeScriptTx",
        fraudulentHeaderHash: fixture.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/u);
  }, 600_000);
});
