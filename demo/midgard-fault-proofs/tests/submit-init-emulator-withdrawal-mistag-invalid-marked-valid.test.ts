/** Standalone withdrawal-mistag lifecycle: unpayable truth marked valid. */
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

describe("withdrawal-mistag invalid marked valid emulator lifecycle", () => {
  it("proves UnpayableWithdrawalValue, mints permanent evidence, and removes the block", async () => {
    const harness = await makeWithdrawalMistagEmulatorHarnessV1();
    const scenario = await setupWithdrawalMistagScenarioV1({
      harness,
      direction: "invalid-marked-valid",
    });
    expect(scenario.prepared.direction).toBe("invalid-marked-valid");
    expect(scenario.prepared.coreValid).toBe(true);
    expect(scenario.prepared.payable).toBe(false);
    expect(scenario.prepared.actualValid).toBe(false);

    const refs = await publishWithdrawalMistagScriptsV1({ harness });
    const lifecycle = await driveWithdrawalMistagToFraudV1({
      harness,
      scenario,
      refs,
    });
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
