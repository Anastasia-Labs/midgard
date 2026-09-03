/** Honest commitment: local refusal plus exact on-chain asymmetry. */
import { outRefLabel } from "@al-ft/midgard-core";
import type {
  CanonicalDecodabilityStep02State,
  CommittedFieldClaim,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  submitCanonicalDecodabilityInit,
  submitCanonicalDecodabilityStep01,
  submitCanonicalDecodabilityStep02,
} from "../src/index.js";
import {
  buildCanonicalDecodabilityBodyFixture,
  makeCanonicalDecodabilityEmulatorHarness,
  network,
  publishCanonicalDecodabilityReferenceScripts,
  setupCanonicalDecodabilityScenario,
  submitCanonicalDecodabilityStep01Raw,
  submitCanonicalDecodabilityStep02Raw,
} from "./support/canonical-decodability-emulator-v1.js";
import { expectOnchainRefusal } from "./support/native-script-decoding-emulator-v1.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

describe("canonical-decodability honest-commitment adversary", () => {
  it("binds verdict 0 but cannot fabricate or finalize a conviction", async () => {
    const harness = await makeCanonicalDecodabilityEmulatorHarness();
    const {
      realBlueprint,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
      canonicalDecodability,
      category,
      witnessReferenceScripts,
    } = harness;
    const [step01Ref, step02Ref] =
      await publishCanonicalDecodabilityReferenceScripts({
        lucid: proverLucid,
        contracts: canonicalDecodability,
      });
    const fixture = await buildCanonicalDecodabilityBodyFixture({
      grammatical: true,
    });
    expect(fixture.prepared).toBeNull();
    const setup = await setupCanonicalDecodabilityScenario({
      harness,
      fixture,
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
      witnessReferenceScripts,
    });
    const firstStep = await expectSingleUtxoWithUnit(
      proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );

    await expect(
      submitCanonicalDecodabilityStep01({
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
        witnessReferenceScripts,
      }),
    ).rejects.toThrow(/verdict 0.*valid block cannot be challenged/u);

    const truthfulClaim: CommittedFieldClaim = {
      BodyFieldClaim: {
        field_index: BigInt(fixture.fieldIndex),
        carriage: {
          Inline: { preimage: fixture.committedPreimage.toString("hex") },
        },
      },
    };
    const truthfulState: CanonicalDecodabilityStep02State = {
      bad_tx_id: fixture.badTxId,
      field_index: BigInt(fixture.fieldIndex),
      verdict: 0n,
    };

    await expectOnchainRefusal(() =>
      submitCanonicalDecodabilityStep01Raw({
        lucid: proverLucid,
        blueprint: realBlueprint,
        contracts: canonicalDecodability,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStep),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        claim: truthfulClaim,
        step02State: { ...truthfulState, verdict: 10n },
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts,
      }),
    );
    await expectOnchainRefusal(() =>
      submitCanonicalDecodabilityStep01Raw({
        lucid: proverLucid,
        blueprint: realBlueprint,
        contracts: canonicalDecodability,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStep),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        claim: {
          BodyFieldClaim: {
            field_index: BigInt(fixture.fieldIndex),
            carriage: { Inline: { preimage: "00" } },
          },
        },
        step02State: truthfulState,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts,
      }),
    );
    await expectOnchainRefusal(() =>
      submitCanonicalDecodabilityStep01Raw({
        lucid: proverLucid,
        blueprint: realBlueprint,
        contracts: canonicalDecodability,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStep),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: {
          ...fixture.txInclusion,
          transactionsPhasRoot: "ff".repeat(32),
        },
        claim: truthfulClaim,
        step02State: truthfulState,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts,
      }),
    );
    const untouchedFirst = await expectSingleUtxoWithUnit(
      proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    expect(outRefLabel(untouchedFirst)).toBe(outRefLabel(firstStep));

    const bind = await submitCanonicalDecodabilityStep01Raw({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: canonicalDecodability,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStep),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      claim: truthfulClaim,
      step02State: truthfulState,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts,
    });
    const secondStep = await expectSingleUtxoWithUnit(
      proverLucid,
      canonicalDecodability.steps[1].spendingScriptAddress,
      init.computationThreadUnit,
    );
    expect(outRefLabel(secondStep)).toBe(bind.nextThreadOutRef);
    await expect(
      submitCanonicalDecodabilityStep02({
        lucid: proverLucid,
        contracts: canonicalDecodability,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: bind.nextThreadOutRef,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts,
      }),
    ).rejects.toThrow(/does not describe a violation/u);
    await expectOnchainRefusal(() =>
      submitCanonicalDecodabilityStep02Raw({
        lucid: proverLucid,
        contracts: canonicalDecodability,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: bind.nextThreadOutRef,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts,
      }),
    );
    const untouchedSecond = await expectSingleUtxoWithUnit(
      proverLucid,
      canonicalDecodability.steps[1].spendingScriptAddress,
      init.computationThreadUnit,
    );
    expect(outRefLabel(untouchedSecond)).toBe(outRefLabel(secondStep));
  }, 240_000);
});
