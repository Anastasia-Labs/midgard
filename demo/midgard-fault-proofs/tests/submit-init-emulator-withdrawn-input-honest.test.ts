import { describe, expect, it } from "vitest";

import { buildWithdrawnInputStep03 } from "../src/withdrawn-input/submit-withdrawn-input-step-03.js";
import { buildAndSubmit } from "./support/submit-steps.js";
import {
  advanceWithdrawnInputToStep03,
  makeWithdrawnInputEmulatorScenario,
} from "./support/withdrawn-input-emulator.js";

describe("withdrawn-input honest-block refusal", () => {
  it("refuses on-chain when the withdrawals root commits a different out-ref", async () => {
    const scenario = await makeWithdrawnInputEmulatorScenario(
      "honestDifferentWithdrawal",
    );
    const { step02 } = await advanceWithdrawnInputToStep03(scenario);
    await expect(
      buildAndSubmit(buildWithdrawnInputStep03, {
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
