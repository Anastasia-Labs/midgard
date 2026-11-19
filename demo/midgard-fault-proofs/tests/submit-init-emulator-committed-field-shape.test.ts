import { outRefLabel } from "@al-ft/midgard-core";
import {
  CommittedFieldShapeStep02Datum,
  FraudProofTokenDatum,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { prepareCommittedFieldShapeFromCanonicalTx } from "../src/committed-field-shape/prepare-committed-field-shape.js";
import { buildCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { buildCommittedFieldShapeStep01 } from "../src/committed-field-shape/submit-committed-field-shape-step-01.submit-committed-field-shape-step01.js";
import { buildCommittedFieldShapeStep02 } from "../src/committed-field-shape/submit-committed-field-shape-step-02.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import {
  makeCommittedFieldShapeEmulatorHarness,
  publishCommittedFieldShapeReferenceScripts,
  setupCommittedFieldShapeScenario,
} from "./support/committed-field-shape-emulator.committed-field-shape-scenario-material.js";
import { network } from "./support/emulator/blueprints.js";
import { expectSingleUtxoWithUnit } from "./support/emulator/emulator-context.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import { pinStepTranscript } from "./support/step-transcript.js";
import { buildAndSubmitUnjournaled } from "./support/submit-unjournaled.js";

describe("committed-field-shape emulator lifecycle", () => {
  it("proves a real wrong-stride commitment through mint and removes its block", async () => {
    pinStepTranscript();
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const scenario = await setupCommittedFieldShapeScenario({
      harness,
      kind: "wrong-stride",
    });
    if (scenario.canonicalTx === null) {
      throw new Error("wrong-stride scenario must be canonical-grammar valid");
    }
    const prepared = prepareCommittedFieldShapeFromCanonicalTx({
      tx: scenario.canonicalTx,
      fieldIndex: 0,
    });
    expect(prepared.evidence.committedPreimage).toBe("8144deadbeef");
    expect(prepared.evidence.verdict).toBe(3);
    expect(prepared.evidence.isViolation).toBe(true);

    const [step01Reference, step02Reference] =
      await publishCommittedFieldShapeReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.committedFieldShape,
      });
    expect(step01Reference.scriptRef).not.toBeNull();
    expect(step02Reference.scriptRef).not.toBeNull();

    const init = await buildAndSubmitUnjournaled(buildCommittedFieldShapeInit, {
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.committedFieldShape,
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
    expect(init.fraudCategoryId).toBe("00000012");
    expect(init.fraudulentHeaderHash).toBe(scenario.setup.headerHash);

    const step01Capture = await captureEmulatorSubmission(
      harness.emulator,
      () =>
        buildAndSubmitUnjournaled(buildCommittedFieldShapeStep01, {
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          contracts: harness.committedFieldShape,
          categoryId: harness.category.categoryId,
          network,
          signer: harness.proverSigner,
          threadOutRef: init.nextThreadOutRef,
          stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
          txInclusion: scenario.inclusion,
          prepared,
          referenceScriptUtxo: step01Reference,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    const step01 = step01Capture.result;
    expect(step01Capture.measurement.l1ByteMargin).toBeGreaterThan(0);
    expect(step01Capture.measurement.completeSignedBytes).toBeLessThanOrEqual(
      16_384,
    );
    expect(
      step01Capture.measurement.referenceInputCount,
    ).toBeGreaterThanOrEqual(3);
    expect(step01.proofCarriage).toBe("redeemer");
    // A one-item field is a single batch: the step that reads it decides it.
    expect(step01.complete).toBe(true);
    expect(step01.nextStepAddress).toBe(
      harness.committedFieldShape.steps[1].spendingScriptAddress,
    );
    expect(step01.step02State).toStrictEqual({
      bad_tx_id: scenario.nativeTxId,
      field_index: 0n,
      verdict: 3n,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.committedFieldShape.steps[0].spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.committedFieldShape.steps[1].spendingScriptAddress,
      init.computationThreadUnit,
    );
    expect(
      Data.from(secondStepUtxo.datum!, CommittedFieldShapeStep02Datum),
    ).toStrictEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: {
        bad_tx_id: scenario.nativeTxId,
        field_index: 0n,
        verdict: 3n,
      },
    });

    // Crash-resume is state based: the consumed step-01 out-ref cannot replay.
    await expect(
      buildAndSubmitUnjournaled(buildCommittedFieldShapeStep01, {
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.inclusion,
        prepared,
        referenceScriptUtxo: step01Reference,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/No live UTxO|not found|Expected exactly one/u);

    const step02Capture = await captureEmulatorSubmission(
      harness.emulator,
      () =>
        buildAndSubmitUnjournaled(buildCommittedFieldShapeStep02, {
          lucid: harness.proverLucid,
          contracts: harness.committedFieldShape,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: step01.nextThreadOutRef,
          referenceScriptUtxo: step02Reference,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    const step02 = step02Capture.result;
    expect(step02Capture.measurement.l1ByteMargin).toBeGreaterThan(0);
    expect(step02Capture.measurement.completeSignedBytes).toBeLessThanOrEqual(
      16_384,
    );
    expect(
      step02Capture.measurement.referenceInputCount,
    ).toBeGreaterThanOrEqual(1);
    expect(step02.fraudProofAssetName).toBe(init.computationThreadAssetName);
    for (const step of harness.committedFieldShape.steps) {
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          init.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(
      Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: harness.proverSigner.paymentKeyHash });

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const removeNow = BigInt(harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      fraudCategory: "committedFieldShape",
      fraudulentHeaderHash: scenario.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removal.fraudCategory).toBe("committedFieldShape");
    expect(removal.fraudCategoryId).toBe("00000012");
    expect(removal.transactions.map(({ kind }) => kind)).toStrictEqual([
      "remove-target",
    ]);
    expect(removal.transactions[0]!.slashingApproach).toBe(
      "SlashActiveOperator",
    );
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [root] = await harness.proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      scenario.setup.stateQueueRootUnit,
    );
    if (root === undefined) {
      throw new Error("removal did not preserve the state-queue root");
    }
    const rootView = await Effect.runPromise(
      utxoToStateQueueUTxO(root, harness.contracts.stateQueue.policyId),
    );
    expect(rootView.datum.next).toBe("Empty");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.activeOperators.spendingScriptAddress,
        scenario.setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const [scheduler] = await harness.proverLucid.utxosAtWithUnit(
      harness.contracts.scheduler.spendingScriptAddress,
      toUnit(harness.contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
    );
    if (scheduler === undefined) {
      throw new Error("removal did not preserve scheduler state");
    }
    expect(Data.from(scheduler.datum!, SchedulerDatum)).toBe(
      "NoActiveOperators",
    );
    const retained = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(outRefLabel(fraudProofUtxo));

    await expect(
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "committedFieldShape",
        fraudulentHeaderHash: scenario.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/u);
  }, 600_000);
});
