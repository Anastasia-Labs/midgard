/** Public mint-authorization cancellation at the final evidence frontier, then restart. */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/fraud-proof";
import { describe, expect, it } from "vitest";

import { buildLinearFaultCancel } from "../src/linear-fault-cancel.js";
import { MINT_AUTHORIZATION_CATEGORY_LABEL } from "../src/mint-authorization/contracts.js";
import { buildMintAuthorizationStep01 } from "../src/mint-authorization/submit-mint-authorization-step-01.js";
import { buildMintAuthorizationStep02 } from "../src/mint-authorization/submit-mint-authorization-step-02.submit-mint-authorization-step02.js";
import { buildMintAuthorizationStep03WitnessAbsence } from "../src/mint-authorization/submit-mint-authorization-step-03.submit-mint-authorization-step03-witness-absence.js";
import { buildMintAuthorizationStep04AdvanceComplete } from "../src/mint-authorization/submit-mint-authorization-step-04.js";
import { buildResolvedInit } from "../src/submit-init.submit-resolved-init.js";
import { network } from "./support/emulator/blueprints.js";
import {
  buildMintAuthorizationSubject,
  makeMintAuthorizationEmulatorHarness,
  publishMintAuthorizationReferenceScripts,
  setupMintAuthorizationScenario,
  smallMintItemCbors,
} from "./support/mint-authorization-emulator.js";
import {
  buildAndSubmitUnjournaled,
  submitTurnsUnjournaled,
} from "./support/submit-unjournaled.js";

describe("mint-authorization cancellation and restart", () => {
  it("cancels through the public submitter after all evidence is resolved and starts a fresh thread", async () => {
    const harness = await makeMintAuthorizationEmulatorHarness();
    const subject = buildMintAuthorizationSubject({
      mintItemCbors: smallMintItemCbors(),
    });
    const { block, setup } = await setupMintAuthorizationScenario({
      harness,
      subject,
    });
    if (block.txInclusion === null) {
      throw new Error(
        "mint-authorization fixture has no transaction inclusion",
      );
    }
    const refs = await publishMintAuthorizationReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const catalogue = {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    };
    const init = await buildAndSubmitUnjournaled(buildResolvedInit, {
      label: MINT_AUTHORIZATION_CATEGORY_LABEL,
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
    const step01 = await buildAndSubmitUnjournaled(
      buildMintAuthorizationStep01,
      {
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
      },
    );
    const step02 = await submitTurnsUnjournaled(buildMintAuthorizationStep02, {
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      policyIndex: 0n,
      direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      mintItemCbors: subject.mintItemCbors,
      referenceScriptUtxo: refs[1],
    });
    const step03 = await submitTurnsUnjournaled(
      buildMintAuthorizationStep03WitnessAbsence,
      {
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        witnessSet: subject.witnessSetCompact,
        scriptTxWitsItemCbors: subject.scriptWitnessItemCbors,
        referenceScriptUtxo: refs[2],
      },
    );
    const advance = (threadOutRef: string) =>
      submitTurnsUnjournaled(buildMintAuthorizationStep04AdvanceComplete, {
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        referenceInputsItemCbors: subject.referenceInputItemCbors,
        spendInputsItemCbors: subject.spendInputItemCbors,
        referenceScriptUtxo: refs[3],
      });
    const step04 = await advance(
      (await advance(step03.nextThreadOutRef)).nextThreadOutRef,
    );

    const cancelled = await buildAndSubmitUnjournaled(buildLinearFaultCancel, {
      lucid: harness.proverLucid,
      family: MINT_AUTHORIZATION_CATEGORY_LABEL,
      steps: harness.family.steps,
      computationThread: harness.family.computationThread,
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

    const restarted = await buildAndSubmitUnjournaled(buildResolvedInit, {
      label: MINT_AUTHORIZATION_CATEGORY_LABEL,
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
