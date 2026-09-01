/** Public value-not-preserved cancellation after the evidence fold, then restart. */
import { outRefLabel } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  submitValueNotPreservedCancel,
  submitValueNotPreservedInit,
} from "../src/value-not-preserved/index.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  buildValueNotPreservedFixtureV1,
  makeValueNotPreservedEmulatorHarnessV1,
  publishValueNotPreservedReferenceScriptsV1,
  runValueNotPreservedThreadV1,
  setupValueNotPreservedScenarioV1,
  vnpOutputV1,
  vnpOutRefV1,
  vnpValueV1,
} from "./support/value-not-preserved-emulator-v1.js";

describe("value-not-preserved cancellation and restart", () => {
  it("cancels through the public submitter after the full input fold and starts a fresh thread", async () => {
    const harness = await makeValueNotPreservedEmulatorHarnessV1();
    const fixture = await buildValueNotPreservedFixtureV1({
      spentInputs: [
        { input: vnpOutRefV1("31", 0), spentValue: vnpValueV1(10_000_000n) },
      ],
      outputs: [vnpOutputV1({ value: vnpValueV1(20_000_000n) })],
      fee: 1_000_000n,
    });
    const { setup } = await setupValueNotPreservedScenarioV1({
      harness,
      fixture,
    });
    const refs = await publishValueNotPreservedReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const run = await runValueNotPreservedThreadV1({
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
