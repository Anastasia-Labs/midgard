/** Standalone withdrawal-mistag lifecycle: exact-valid marked invalid. */
import { outRefLabel } from "@al-ft/midgard-core";
import { toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";
import {
  driveWithdrawalMistagToFraudV1,
  makeWithdrawalMistagEmulatorHarnessV1,
  publishWithdrawalMistagScriptsV1,
  removeWithdrawalMistagBlockV1,
  setupWithdrawalMistagScenarioV1,
} from "./support/withdrawal-mistag-emulator-v1.js";

describe("withdrawal-mistag valid marked invalid emulator lifecycle", () => {
  it("mints permanent fraud evidence and removes the fraudulent block", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarnessV1();
    const scenario = await setupWithdrawalMistagScenarioV1({
      harness,
      direction: "valid-marked-invalid",
    });
    expect(scenario.prepared.direction).toBe("valid-marked-invalid");
    expect(scenario.prepared.actualValid).toBe(true);

    const published = await publishWithdrawalMistagScriptsV1({ harness });
    for (const measurement of published.publicationMeasurements) {
      expect(measurement.l1ByteMargin).toBeGreaterThanOrEqual(1_024);
    }
    const lifecycle = await driveWithdrawalMistagToFraudV1({
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

    const removal = await removeWithdrawalMistagBlockV1({
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
