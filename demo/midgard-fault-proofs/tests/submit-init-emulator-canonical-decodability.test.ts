/** Real body-field fault: init -> bind -> mint -> fraudulent-block removal. */
import { outRefLabel } from "@al-ft/midgard-core";
import {
  CanonicalDecodabilityStep02Datum,
  FraudProofTokenDatum,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitCanonicalDecodabilityInit,
  submitCanonicalDecodabilityStep01,
  submitCanonicalDecodabilityStep02,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import {
  buildCanonicalDecodabilityBodyFixtureV1,
  makeCanonicalDecodabilityEmulatorHarnessV1,
  network,
  publishCanonicalDecodabilityReferenceScriptsV1,
  setupCanonicalDecodabilityScenarioV1,
} from "./support/canonical-decodability-emulator-v1.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("canonical-decodability real-fault lifecycle", () => {
  it("mints permanent evidence and removes the fraudulent commitment", async () => {
    const harness = await makeCanonicalDecodabilityEmulatorHarnessV1();
    const {
      realBlueprint,
      emulator,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
      canonicalDecodability,
      category,
    } = harness;
    const [step01Ref, step02Ref] =
      await publishCanonicalDecodabilityReferenceScriptsV1({
        lucid: proverLucid,
        contracts: canonicalDecodability,
      });
    const removalReferenceScripts = await publishRemovalReferenceScripts({
      lucid: proverLucid,
      contracts,
    });
    const fixture = await buildCanonicalDecodabilityBodyFixtureV1();
    if (fixture.prepared === null)
      throw new Error("Expected violating fixture");
    const setup = await setupCanonicalDecodabilityScenarioV1({
      harness,
      fixture,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
      removalReferenceScripts: removalReferenceScripts.published,
    });

    const init = await submitCanonicalDecodabilityInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: canonicalDecodability,
      category,
      catalogue: {
        policyId: contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(init.fraudCategoryId).toBe("00000011");
    expect(init.fraudulentHeaderHash).toBe(setup.headerHash);
    const firstStep = await expectSingleUtxoWithUnit(
      proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );

    const step01 = await submitCanonicalDecodabilityStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: canonicalDecodability,
      categoryId: category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStep),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      fieldIndex: fixture.fieldIndex,
      committedPreimage: fixture.committedPreimage,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(step01.verdict).toBe(fixture.prepared.evidence.verdict);
    await expect(
      proverLucid.utxosAtWithUnit(
        init.firstStepAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    const secondStep = await expectSingleUtxoWithUnit(
      proverLucid,
      step01.secondStepAddress,
      init.computationThreadUnit,
    );
    const paymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    if (paymentCredential?.type !== "Key") {
      throw new Error("Expected prover payment key credential");
    }
    expect(
      Data.from(secondStep.datum!, CanonicalDecodabilityStep02Datum),
    ).toEqual({
      fraud_prover: paymentCredential.hash,
      data: fixture.prepared.step02State,
    });

    const step02 = await submitCanonicalDecodabilityStep02({
      lucid: proverLucid,
      contracts: canonicalDecodability,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStep),
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(step02.fraudProofUnit).toBe(
      toUnit(
        canonicalDecodability.fraudProof.policyId,
        init.computationThreadAssetName,
      ),
    );
    const proofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(Data.from(proofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: paymentCredential.hash,
    });

    const removeNow = BigInt(emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "canonicalDecodability",
      fraudulentHeaderHash: setup.headerHash,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removal.fraudCategory).toBe("canonicalDecodability");
    expect(removal.fraudCategoryId).toBe(category.categoryId);
    expect(removal.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    await expect(
      proverLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retained = await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(outRefLabel(proofUtxo));
    await expect(
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "canonicalDecodability",
        fraudulentHeaderHash: setup.headerHash,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/u);
  }, 240_000);
});
