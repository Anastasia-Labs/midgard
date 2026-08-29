import { describe, expect, it } from "vitest";

import {
  submitWithdrawnInputCancel,
  submitWithdrawnInputStep01,
  submitWithdrawnInputStep02,
} from "../src/index.js";
import { network } from "./support/submit-init-emulator-shared.js";
import { makeWithdrawnInputEmulatorScenarioV1 } from "./support/withdrawn-input-emulator-v1.js";

describe("withdrawn-input resume and cancellation", () => {
  it("refuses a wrong step reference, resumes, then cancels at step 03", async () => {
    const scenario = await makeWithdrawnInputEmulatorScenarioV1("fault");
    await expect(
      submitWithdrawnInputStep01({
        lucid: scenario.harness.proverLucid,
        blueprint: scenario.harness.realBlueprint,
        contracts: scenario.contracts,
        categoryId: scenario.category.categoryId,
        network,
        signer: scenario.harness.proverSigner,
        threadOutRef: scenario.init.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.fixture.txInclusion,
        referenceScriptUtxo: scenario.references[1],
        witnessReferenceScripts: scenario.harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/reference script/u);

    const step01 = await submitWithdrawnInputStep01({
      lucid: scenario.harness.proverLucid,
      blueprint: scenario.harness.realBlueprint,
      contracts: scenario.contracts,
      categoryId: scenario.category.categoryId,
      network,
      signer: scenario.harness.proverSigner,
      threadOutRef: scenario.init.nextThreadOutRef,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.fixture.txInclusion,
      referenceScriptUtxo: scenario.references[0],
      witnessReferenceScripts: scenario.harness.witnessReferenceScripts,
    });
    const step02 = await submitWithdrawnInputStep02({
      lucid: scenario.harness.proverLucid,
      contracts: scenario.contracts,
      categoryId: scenario.category.categoryId,
      signer: scenario.harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      evidence: {
        inputs: scenario.fixture.spendInputs,
        badInputIndex: 0,
        nativeTxCompactCbor: scenario.fixture.txInclusion.nativeTxCompactCbor,
      },
      referenceScriptUtxo: scenario.references[1],
    });
    const cancelled = await submitWithdrawnInputCancel({
      lucid: scenario.harness.proverLucid,
      contracts: scenario.contracts,
      categoryId: scenario.category.categoryId,
      signer: scenario.harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      referenceScriptUtxo: scenario.references[2],
      witnessReferenceScripts: scenario.harness.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(2);
    await expect(
      scenario.harness.proverLucid.utxosAtWithUnit(
        scenario.contracts.steps[2].spendingScriptAddress,
        scenario.init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
  });
});
