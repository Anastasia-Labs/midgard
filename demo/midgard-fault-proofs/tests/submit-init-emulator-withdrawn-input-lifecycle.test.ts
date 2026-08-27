import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitRemoveFraudulentBlock,
  submitWithdrawnInputStep03,
} from "../src/index.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishRemovalReferenceScripts,
  WITHDRAWN_INPUT_REMOVAL_DEPLOYMENT_ENTRY_V1,
} from "./support/submit-init-emulator-shared.js";
import {
  advanceWithdrawnInputToStep03V1,
  makeWithdrawnInputEmulatorScenarioV1,
} from "./support/withdrawn-input-emulator-v1.js";

describe("withdrawn-input emulator lifecycle", () => {
  it("mints the permanent fault token and removes the fraudulent block", async () => {
    const scenario = await makeWithdrawnInputEmulatorScenarioV1("fault");
    const { step02 } = await advanceWithdrawnInputToStep03V1(scenario);
    const final = await submitWithdrawnInputStep03({
      lucid: scenario.harness.proverLucid,
      contracts: scenario.contracts,
      categoryId: scenario.category.categoryId,
      signer: scenario.harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      withdrawalMembership: scenario.fixture.withdrawalMembership,
      referenceScriptUtxo: scenario.references[2],
    });
    const [faultToken] = await scenario.harness.proverLucid.utxosAtWithUnit(
      final.fraudProofAddress,
      final.fraudProofUnit,
    );
    expect(faultToken).toBeDefined();
    expect(Data.from(faultToken!.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: scenario.harness.proverSigner.paymentKeyHash,
    });

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: scenario.harness.proverLucid,
      contracts: scenario.harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      scenario.harness.contracts,
      scenario.harness.catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(scenario.harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: scenario.harness.proverLucid,
      blueprint: scenario.harness.realBlueprint,
      deploymentInfo,
      network,
      signer: scenario.harness.proverSigner,
      fraudCategory: {
        name: "withdrawnInput",
        categoryId: scenario.category.categoryId,
        firstStepDeploymentEntry: WITHDRAWN_INPUT_REMOVAL_DEPLOYMENT_ENTRY_V1,
        firstStepScriptHash: scenario.contracts.steps[0].spendingScriptHash,
        fraudProof: scenario.contracts.fraudProof,
      },
      fraudulentHeaderHash: scenario.setup.headerHash,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removal.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    expect(removal.fraudCategory).toBe("withdrawnInput");
    await expectStateQueueHeaderOrder({
      lucid: scenario.harness.funderLucid,
      contracts: scenario.harness.contracts,
      expectedHeaderHashes: [],
    });
    const [retained] = await scenario.harness.proverLucid.utxosAtWithUnit(
      final.fraudProofAddress,
      final.fraudProofUnit,
    );
    expect(outRefLabel(retained!)).toBe(outRefLabel(faultToken!));
  });
});
