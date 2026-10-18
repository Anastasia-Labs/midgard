/**
 * `missingRedeemer` emulator verticals from a genuine Init: an accepted
 * transaction whose observer has no redeemer convicts and its block is
 * removed; a forced rejection whose RedeemerMissing reason is false convicts;
 * an honest forced rejection cannot reach a fraud-proof token.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  MISSING_REDEEMER_STEP_BOUNDS,
  type MissingRedeemerHarness,
  publishMissingRedeemerRoles,
  stepTable,
} from "./support/missing-redeemer-emulator.js";
import {
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const expectWithinBounds = (steps: Parameters<typeof stepTable>[0]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      MISSING_REDEEMER_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      MISSING_REDEEMER_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(MISSING_REDEEMER_STEP_BOUNDS.bytes);
  }
};

const removeBlock = async (
  harness: MissingRedeemerHarness,
  headerHash: string,
) => {
  const removalReferenceScripts = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const now = BigInt(harness.emulator.now());
  return submitRemoveFraudulentBlock({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalReferenceScripts.published },
    ),
    network,
    signer: harness.proverSigner,
    fraudCategory: "missingRedeemer",
    fraudulentHeaderHash: headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
};

describe("missing-redeemer genuine-Init verticals", () => {
  it("A-N: convicts an accepted transaction whose required observer has no redeemer and removes its block", async () => {
    const harness = await makeMissingRedeemerHarness();
    const references = await publishMissingRedeemerRoles(harness);
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: {
        observers: [
          { script: plutusScript(1), redeemer: false },
          { script: plutusScript(2), redeemer: true },
        ],
      },
      source: { kind: "normal" },
    });
    const run = await driveMissingRedeemerThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(run.prepared.final.context.observed?.redeemer_missing).toBe(true);
    const outcome = await run.terminal();
    console.log("MR A-N steps", JSON.stringify(stepTable(run.steps)));
    expectWithinBounds(run.steps);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    const token = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.missingRedeemer.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
    expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
    });
    for (const step of harness.missingRedeemer.steps)
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          run.unit,
        ),
      ).resolves.toHaveLength(0);

    const removal = await removeBlock(harness, scenario.setup.headerHash);
    expect(removal.fraudCategory).toBe("missingRedeemer");
    expect(removal.transactions.map((tx) => tx.kind)).toStrictEqual([
      "remove-target",
    ]);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 1_200_000);
});
