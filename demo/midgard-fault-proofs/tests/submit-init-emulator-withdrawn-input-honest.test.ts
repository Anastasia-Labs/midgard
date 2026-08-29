import { describe, expect, it } from "vitest";

import { submitWithdrawnInputStep03 } from "../src/index.js";
import {
  advanceWithdrawnInputToStep03V1,
  makeWithdrawnInputEmulatorScenarioV1,
} from "./support/withdrawn-input-emulator-v1.js";

describe("withdrawn-input honest-block refusal", () => {
  it("refuses on-chain when the withdrawals root commits a different out-ref", async () => {
    const scenario = await makeWithdrawnInputEmulatorScenarioV1(
      "honestDifferentWithdrawal",
    );
    const { step02 } = await advanceWithdrawnInputToStep03V1(scenario);
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
