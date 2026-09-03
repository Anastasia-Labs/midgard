/** Public value-not-preserved cancellation after the evidence fold, then restart. */
import { outRefLabel } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  submitValueNotPreservedCancel,
  submitValueNotPreservedInit,
} from "../src/value-not-preserved/index.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  buildValueNotPreservedFixture,
  makeValueNotPreservedEmulatorHarness,
  publishValueNotPreservedReferenceScripts,
  runValueNotPreservedThread,
  setupValueNotPreservedScenario,
  vnpOutput,
  vnpOutRef,
  vnpValue,
} from "./support/value-not-preserved-emulator-v1.js";

describe("value-not-preserved cancellation and restart", () => {
  it("cancels through the public submitter after the full input fold and starts a fresh thread", async () => {
    const harness = await makeValueNotPreservedEmulatorHarness();
    const fixture = await buildValueNotPreservedFixture({
      spentInputs: [
        { input: vnpOutRef("31", 0), spentValue: vnpValue(10_000_000n) },
      ],
      outputs: [vnpOutput({ value: vnpValue(20_000_000n) })],
      fee: 1_000_000n,
    });
    const { setup } = await setupValueNotPreservedScenario({
      harness,
      fixture,
    });
    const refs = await publishValueNotPreservedReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const run = await runValueNotPreservedThread({
      harness,
      fixture,
      setup,
      refs,
      claimedAsset: "AdaAsset",
      claimedDirection: "ClaimedAssetInflated",
      through: "finish",
    });

    const cancelled = await submitValueNotPreservedCancel({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: run.finish.nextThreadOutRef,
      referenceScriptUtxo: refs[2],
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(2);
    expect(cancelled.fraudulentHeaderHash).toBe(setup.headerHash);
    for (const step of harness.family.steps) {
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          cancelled.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }

    const restarted = await submitValueNotPreservedInit({
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
