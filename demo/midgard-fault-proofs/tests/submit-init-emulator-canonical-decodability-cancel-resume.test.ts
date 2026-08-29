/** Shared cancellation at both positions, plus positional crash-resume. */
import { outRefLabel } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  submitCanonicalDecodabilityCancel,
  submitCanonicalDecodabilityInit,
  submitCanonicalDecodabilityStep01,
  submitCanonicalDecodabilityStep02,
} from "../src/index.js";
import {
  buildCanonicalDecodabilityBodyFixtureV1,
  makeCanonicalDecodabilityEmulatorHarnessV1,
  network,
  publishCanonicalDecodabilityReferenceScriptsV1,
  setupCanonicalDecodabilityScenarioV1,
} from "./support/canonical-decodability-emulator-v1.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const initializedScenario = async () => {
  const harness = await makeCanonicalDecodabilityEmulatorHarnessV1();
  const fixture = await buildCanonicalDecodabilityBodyFixtureV1();
  const references = await publishCanonicalDecodabilityReferenceScriptsV1({
    lucid: harness.proverLucid,
    contracts: harness.canonicalDecodability,
  });
  const setup = await setupCanonicalDecodabilityScenarioV1({
    harness,
    fixture,
  });
  const init = await submitCanonicalDecodabilityInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.canonicalDecodability,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });
  return { harness, fixture, references, setup, init };
};

describe("canonical-decodability cancellation and resume", () => {
  it("cancels an initialized thread at step 01 through its reference script", async () => {
    const { harness, references, init } = await initializedScenario();
    const [step01Ref] = references;
    const thread = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    const cancelled = await submitCanonicalDecodabilityCancel({
      lucid: harness.proverLucid,
      contracts: harness.canonicalDecodability,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(thread),
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        init.firstStepAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 180_000);

  it("re-discovers a step-02 thread after the bind and resumes to mint", async () => {
    const { harness, fixture, references, setup, init } =
      await initializedScenario();
    const [step01Ref, step02Ref] = references;
    const first = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    await submitCanonicalDecodabilityStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.canonicalDecodability,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(first),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      fieldIndex: fixture.fieldIndex,
      committedPreimage: fixture.committedPreimage,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    // Simulated restart: ignore the submitter result and locate the NFT by
    // its next-step address and unit.
    const resumed = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.canonicalDecodability.steps[1].spendingScriptAddress,
      init.computationThreadUnit,
    );
    const final = await submitCanonicalDecodabilityStep02({
      lucid: harness.proverLucid,
      contracts: harness.canonicalDecodability,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(resumed),
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      final.fraudProofAddress,
      final.fraudProofUnit,
    );
  }, 180_000);

  it("cancels a bound thread at step 02", async () => {
    const { harness, fixture, references, setup, init } =
      await initializedScenario();
    const [step01Ref, step02Ref] = references;
    const first = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    const bound = await submitCanonicalDecodabilityStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.canonicalDecodability,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(first),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      fieldIndex: fixture.fieldIndex,
      committedPreimage: fixture.committedPreimage,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const cancelled = await submitCanonicalDecodabilityCancel({
      lucid: harness.proverLucid,
      contracts: harness.canonicalDecodability,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.nextThreadOutRef,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.canonicalDecodability.steps[1].spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 180_000);
});
