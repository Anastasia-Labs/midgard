import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import { buildWithdrawnInputStep03 } from "../src/withdrawn-input/submit-withdrawn-input-step-03.js";
import { network } from "./support/emulator/blueprints.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import { pinStepTranscript } from "./support/step-transcript.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init/queue-assertions.js";
import { buildAndSubmitUnjournaled } from "./support/submit-unjournaled.js";
import {
  advanceWithdrawnInputToStep03,
  makeWithdrawnInputEmulatorScenario,
} from "./support/withdrawn-input-emulator.js";

describe("withdrawn-input emulator lifecycle", () => {
  it("mints the permanent fault token and removes the fraudulent block", async () => {
    pinStepTranscript();
    const scenario = await makeWithdrawnInputEmulatorScenario("fault");
    const { step02 } = await advanceWithdrawnInputToStep03(scenario);
    const final = await buildAndSubmitUnjournaled(buildWithdrawnInputStep03, {
      lucid: scenario.harness.proverLucid,
      contracts: scenario.contracts,
      categoryId: scenario.category.categoryId,
      signer: scenario.harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      withdrawalMembership: scenario.fixture.withdrawalMembership,
      referenceScriptUtxo: scenario.references[2],
      witnessReferenceScripts: scenario.harness.witnessReferenceScripts,
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
      fraudCategory: "withdrawnInput",
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
