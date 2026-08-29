/** Witness-side reachability through witness_set_hash re-derivation. */
import { outRefLabel } from "@al-ft/midgard-core";
import { MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER_V1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  submitCanonicalDecodabilityInit,
  submitCanonicalDecodabilityStep01,
  submitCanonicalDecodabilityStep02,
} from "../src/index.js";
import {
  buildCanonicalDecodabilityWitnessFixtureV1,
  makeCanonicalDecodabilityEmulatorHarnessV1,
  network,
  publishCanonicalDecodabilityReferenceScriptsV1,
  setupCanonicalDecodabilityScenarioV1,
  submitCanonicalDecodabilityStep01RawV1,
} from "./support/canonical-decodability-emulator-v1.js";
import { expectOnchainRefusalV1 } from "./support/native-script-decoding-emulator-v1.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

describe("canonical-decodability witness-field lifecycle", () => {
  it("opens field 6 through its committed witness set and mints", async () => {
    const harness = await makeCanonicalDecodabilityEmulatorHarnessV1();
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
      await publishCanonicalDecodabilityReferenceScriptsV1({
        lucid: proverLucid,
        contracts: canonicalDecodability,
      });
    const fixture = await buildCanonicalDecodabilityWitnessFixtureV1();
    if (fixture.prepared === null || fixture.witnessSet === undefined) {
      throw new Error("Expected prepared witness fixture");
    }
    const prepared = fixture.prepared;
    expect(prepared.evidence.verdict).toBe(
      MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER_V1,
    );
    const setup = await setupCanonicalDecodabilityScenarioV1({
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
    await expectOnchainRefusalV1(() =>
      submitCanonicalDecodabilityStep01RawV1({
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
        step02State: prepared.step02State,
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
      witnessSet: fixture.witnessSet,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts,
    });
    const secondStep = await expectSingleUtxoWithUnit(
      proverLucid,
      step01.secondStepAddress,
      init.computationThreadUnit,
    );
    const step02 = await submitCanonicalDecodabilityStep02({
      lucid: proverLucid,
      contracts: canonicalDecodability,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStep),
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts,
    });
    expect(step02.state).toEqual(prepared.step02State);
    await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
  }, 180_000);
});
