/** Public mint-authorization cancellation at the final evidence frontier, then restart. */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  submitMintAuthorizationCancel,
  submitMintAuthorizationInit,
  submitMintAuthorizationStep01,
  submitMintAuthorizationStep02,
  submitMintAuthorizationStep03WitnessAbsence,
  submitMintAuthorizationStep04AdvanceComplete,
} from "../src/mint-authorization/index.js";
import {
  buildMintAuthorizationSubjectV1,
  makeMintAuthorizationEmulatorHarnessV1,
  publishMintAuthorizationReferenceScriptsV1,
  setupMintAuthorizationScenarioV1,
  smallMintItemCborsV1,
} from "./support/mint-authorization-emulator-v1.js";
import { network } from "./support/submit-init-emulator-shared.js";

describe("mint-authorization cancellation and restart", () => {
  it("cancels through the public submitter after all evidence is resolved and starts a fresh thread", async () => {
    const harness = await makeMintAuthorizationEmulatorHarnessV1();
    const subject = buildMintAuthorizationSubjectV1({
      mintItemCbors: smallMintItemCborsV1(),
    });
    const { block, setup } = await setupMintAuthorizationScenarioV1({
      harness,
      subject,
    });
    if (block.txInclusion === null) {
      throw new Error(
        "mint-authorization fixture has no transaction inclusion",
      );
    }
    const refs = await publishMintAuthorizationReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const catalogue = {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    };
    const init = await submitMintAuthorizationInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      category: harness.category,
      catalogue,
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step01 = await submitMintAuthorizationStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step02 = await submitMintAuthorizationStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      policyIndex: 0n,
      direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      mintItemCbors: subject.mintItemCbors,
      referenceScriptUtxo: refs[1],
    });
    const step03 = await submitMintAuthorizationStep03WitnessAbsence({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      witnessSet: subject.witnessSetCompact,
      scriptTxWitsItemCbors: subject.scriptWitnessItemCbors,
      referenceScriptUtxo: refs[2],
    });
    const step04 = await submitMintAuthorizationStep04AdvanceComplete({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      referenceInputsItemCbors: subject.referenceInputItemCbors,
      referenceScriptUtxo: refs[3],
    });

    const cancelled = await submitMintAuthorizationCancel({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step04.nextThreadOutRef,
      referenceScriptUtxo: refs[4],
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(4);
    expect(cancelled.fraudulentHeaderHash).toBe(setup.headerHash);
    for (const step of harness.family.steps) {
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          cancelled.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }

    const restarted = await submitMintAuthorizationInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      category: harness.category,
      catalogue,
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    expect(restarted.computationThreadUnit).toBe(
      cancelled.computationThreadUnit,
    );
    const [freshThread] = await harness.proverLucid.utxosAtWithUnit(
      harness.family.steps[0].spendingScriptAddress,
      restarted.computationThreadUnit,
    );
    expect(freshThread).toBeDefined();
    expect(outRefLabel(freshThread!)).toBe(restarted.nextThreadOutRef);
  }, 600_000);
});
