import { describe, expect, it } from "vitest";

import { submitWithdrawnInputStep03 } from "../src/index.js";
import {
  advanceWithdrawnInputToStep03,
  makeWithdrawnInputEmulatorScenario,
} from "./support/withdrawn-input-emulator.js";

describe("withdrawn-input invalid-withdrawal refusal", () => {
  it("refuses on-chain even when the invalid leaf targets the spend input", async () => {
    const scenario =
      await makeWithdrawnInputEmulatorScenario("invalidWithdrawal");
    const { step02 } = await advanceWithdrawnInputToStep03(scenario);
    await expect(
      submitWithdrawnInputStep03({
        lucid: scenario.harness.proverLucid,
        contracts: scenario.contracts,
        categoryId: scenario.category.categoryId,
        signer: scenario.harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: scenario.fixture.withdrawalMembership,
        referenceScriptUtxo: scenario.references[2],
        witnessReferenceScripts: scenario.harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/failed script execution.*Spend/su);
    await expect(
      scenario.harness.proverLucid.utxosAtWithUnit(
        scenario.contracts.steps[2].spendingScriptAddress,
        scenario.init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
  });
});
