/** Witness-side reachability through witness_set_hash re-derivation. */
import { outRefLabel } from "@al-ft/midgard-core";
import { MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { buildCanonicalDecodabilityInit } from "../src/canonical-decodability/submit-canonical-decodability-init.js";
import { buildCanonicalDecodabilityStep01 } from "../src/canonical-decodability/submit-canonical-decodability-step-01.js";
import { buildCanonicalDecodabilityStep02 } from "../src/canonical-decodability/submit-canonical-decodability-step-02.js";
import {
  buildCanonicalDecodabilityWitnessFixture,
  makeCanonicalDecodabilityEmulatorHarness,
  network,
  publishCanonicalDecodabilityReferenceScripts,
  setupCanonicalDecodabilityScenario,
  submitCanonicalDecodabilityStep01Raw,
} from "./support/canonical-decodability-emulator.js";
import { expectSingleUtxoWithUnit } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { buildAndSubmitUnjournaled } from "./support/submit-unjournaled.js";

describe("canonical-decodability witness-field lifecycle", () => {
  it("opens field 6 through its committed witness set and mints", async () => {
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
    const fixture = await buildCanonicalDecodabilityWitnessFixture();
    if (fixture.prepared === null || fixture.witnessSet === undefined) {
      throw new Error("Expected prepared witness fixture");
    }
    const prepared = fixture.prepared;
    expect(prepared.evidence.verdict).toBe(
      MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
    );
    const setup = await setupCanonicalDecodabilityScenario({
      harness,
      fixture,
    });
    const init = await buildAndSubmitUnjournaled(
      buildCanonicalDecodabilityInit,
      {
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
      },
    );
    const firstStep = await expectSingleUtxoWithUnit(
      proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
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
            carriage: {
              Inline: { preimage: fixture.committedPreimage.toString("hex") },
            },
          },
        },
        successor: { step: 2, state: prepared.step02State },
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts,
      }),
    );
    expect(
      outRefLabel(
        await expectSingleUtxoWithUnit(
          proverLucid,
          init.firstStepAddress,
          init.computationThreadUnit,
        ),
      ),
    ).toBe(outRefLabel(firstStep));
    const step01 = await buildAndSubmitUnjournaled(
      buildCanonicalDecodabilityStep01,
      {
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
        witnessSet: fixture.witnessSet,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts,
      },
    );
    const secondStep = await expectSingleUtxoWithUnit(
      proverLucid,
      step01.nextStepAddress,
      init.computationThreadUnit,
    );
    const step02 = await buildAndSubmitUnjournaled(
      buildCanonicalDecodabilityStep02,
      {
        lucid: proverLucid,
        contracts: canonicalDecodability,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStep),
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts,
      },
    );
    expect(step02.state).toEqual(prepared.step02State);
    await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
  }, 180_000);
});
