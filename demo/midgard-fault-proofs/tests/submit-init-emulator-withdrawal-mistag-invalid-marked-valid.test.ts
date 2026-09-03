/** Standalone withdrawal-mistag lifecycle: unpayable truth marked valid. */
import { outRefLabel } from "@al-ft/midgard-core";
import { toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";
import {
  driveWithdrawalMistagToFraud,
  makeWithdrawalMistagEmulatorHarness,
  publishWithdrawalMistagScripts,
  removeWithdrawalMistagBlock,
  setupWithdrawalMistagScenario,
} from "./support/withdrawal-mistag-emulator-v1.js";

describe("withdrawal-mistag invalid marked valid emulator lifecycle", () => {
  it("proves UnpayableWithdrawalValue, mints permanent evidence, and removes the block", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarness();
    const scenario = await setupWithdrawalMistagScenario({
      harness,
      direction: "invalid-marked-valid",
    });
    expect(scenario.prepared.direction).toBe("invalid-marked-valid");
    expect(scenario.prepared.coreValid).toBe(true);
    expect(scenario.prepared.payable).toBe(false);
    expect(scenario.prepared.actualValid).toBe(false);

    const published = await publishWithdrawalMistagScripts({ harness });
    for (const measurement of published.publicationMeasurements) {
      expect(measurement.l1ByteMargin).toBeGreaterThanOrEqual(1_024);
    }
    const lifecycle = await driveWithdrawalMistagToFraud({
      harness,
      scenario,
      refs: published.refs,
    });
    expect(Object.keys(lifecycle.transactionMeasurements)).toEqual([
      "init",
      "step-01",
      "step-02",
      "step-03",
      "step-04",
      "step-05",
    ]);
    const threadUnit = toUnit(
      harness.withdrawalMistag.computationThread.policyId,
      lifecycle.fraud.fraudProofAssetName,
    );
    for (const step of harness.withdrawalMistag.steps) {
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          threadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const beforeRemoval = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.withdrawalMistag.fraudProof.spendingScriptAddress,
      lifecycle.fraud.fraudProofUnit,
    );
    expect(outRefLabel(beforeRemoval)).toBe(lifecycle.fraud.fraudProofOutRef);

    const removal = await removeWithdrawalMistagBlock({
      harness,
      scenario,
    });
    expect(removal.fraudCategory).toBe("withdrawalMistag");
    expect(removal.fraudCategoryId).toBe("00000014");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retained = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.withdrawalMistag.fraudProof.spendingScriptAddress,
      lifecycle.fraud.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(lifecycle.fraud.fraudProofOutRef);
  }, 600_000);
});
