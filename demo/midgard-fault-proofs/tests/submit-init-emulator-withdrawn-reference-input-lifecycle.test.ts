import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  submitRemoveFraudulentBlock,
  submitWithdrawnReferenceInputInit,
  submitWithdrawnReferenceInputStep01,
  submitWithdrawnReferenceInputStep02,
  submitWithdrawnReferenceInputStep03,
} from "../src/index.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import {
  makeWithdrawnReferenceInputEmulatorHarnessV1,
  publishWithdrawnReferenceInputReferenceScriptsV1,
  setupWithdrawnReferenceInputScenarioV1,
  withdrawnReferenceInputRemovalCategoryV1,
} from "./support/withdrawn-reference-input-emulator-v1.js";

describe("withdrawn-reference-input emulator lifecycle", () => {
  it("proves the same-block conflict, mints permanent evidence, and removes the fraudulent block", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarnessV1();
    const scenario = await setupWithdrawnReferenceInputScenarioV1({ harness });
    const [step01Ref, step02Ref, step03Ref] =
      await publishWithdrawnReferenceInputReferenceScriptsV1({
        lucid: harness.funderLucid,
        contracts: harness.family,
      });

    const init = await submitWithdrawnReferenceInputInit({
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
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    });
    expect(init.computationThreadAssetName).toBe(
      `${harness.category.categoryId}${scenario.setup.headerHash}`,
    );

    const step01 = await submitWithdrawnReferenceInputStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.prepared.txInclusion,
      referenceScriptUtxo: step01Ref,
    });
    expect(step01.blocksWithdrawalsRoot).toBe(scenario.header.withdrawalsRoot);
    expect(step01.blocksWithdrawalCount).toBe(1n);

    const step02 = await submitWithdrawnReferenceInputStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      referenceInputs: scenario.prepared.referenceInputs,
      nativeTxCompactCbor: scenario.prepared.txInclusion.nativeTxCompactCbor,
      badReferenceInputIndex: BigInt(scenario.prepared.badReferenceInputIndex),
      referenceScriptUtxo: step02Ref,
    });
    expect(step02.missingReferenceInput).toStrictEqual(
      scenario.prepared.missingReferenceInput,
    );

    const step03 = await submitWithdrawnReferenceInputStep03({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      withdrawalMembership: scenario.prepared.withdrawalMembership,
      referenceScriptUtxo: step03Ref,
    });

    for (const step of harness.family.steps) {
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          init.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProof = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.family.fraudProof.spendingScriptAddress,
      step03.fraudProofUnit,
    );
    expect(outRefLabel(fraudProof)).toBe(step03.fraudProofOutRef);
    expect(
      Data.from(fraudProof.datum!, SDK.FraudProofTokenDatum),
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
    const now = BigInt(harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      fraudCategory: withdrawnReferenceInputRemovalCategoryV1(harness),
      fraudulentHeaderHash: scenario.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removal.fraudCategory).toBe("withdrawnReferenceInput");
    expect(removal.transactions).toHaveLength(1);
    expect(removal.transactions[0]).toMatchObject({
      kind: "remove-target",
      slashingApproach: "SlashActiveOperator",
      removedOperator: scenario.header.operatorVkey,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [rootUtxo] = await harness.proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      scenario.setup.stateQueueRootUnit,
    );
    expect(rootUtxo).toBeDefined();
    const root = await Effect.runPromise(
      SDK.utxoToStateQueueUTxO(
        rootUtxo!,
        harness.contracts.stateQueue.policyId,
      ),
    );
    expect(root.datum.next).toBe("Empty");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.activeOperators.spendingScriptAddress,
        scenario.setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
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
      step03.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(step03.fraudProofOutRef);
    await expect(
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: withdrawnReferenceInputRemovalCategoryV1(harness),
        fraudulentHeaderHash: scenario.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/);
  }, 600_000);
});
