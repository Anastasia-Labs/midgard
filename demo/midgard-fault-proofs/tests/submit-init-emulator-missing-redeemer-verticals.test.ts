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
  MissingRedeemerHonestLeaf,
  requireConvictingMissingRedeemerProof,
} from "../src/missing-redeemer/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  MISSING_REDEEMER_STEP_BOUNDS,
  type MissingRedeemerHarness,
  publishMissingRedeemerRoles,
  stepTable,
} from "./support/missing-redeemer-emulator.js";
import {
  honestForcedObserverShape,
  nativeScript,
  plutusScript,
  redeemerMissingVerdict,
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

/** Drives a convicting thread to its terminal and checks the token. */
const convictAndCheck = async (
  harness: MissingRedeemerHarness,
  scenario: Awaited<ReturnType<typeof setupMissingRedeemerScenario>>,
  label: string,
) => {
  const references = await publishMissingRedeemerRoles(harness);
  const run = await driveMissingRedeemerThread({
    harness,
    scenario,
    references,
  });
  expect(run.prepared.convicts).toBe(true);
  const outcome = await run.terminal();
  console.log(`MR ${label} steps`, JSON.stringify(stepTable(run.steps)));
  expectWithinBounds(run.steps);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  await expectSingleUtxoWithUnit(
    harness.proverLucid,
    harness.missingRedeemer.fraudProof.spendingScriptAddress,
    outcome.fraudProofUnit,
  );
  return run;
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

  it("C-4: convicts an accepted transaction whose redeemer field is empty", async () => {
    const harness = await makeMissingRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: { observers: [{ script: plutusScript(3), redeemer: false }] },
      source: { kind: "normal" },
    });
    expect(scenario.material.fields[8]).toHaveLength(0);
    const run = await convictAndCheck(harness, scenario, "C-4");
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(run.prepared.final.context.observed?.redeemer_missing).toBe(true);
  }, 1_200_000);

  it("R-F: convicts a forced rejection whose RedeemerMissing names a native observer", async () => {
    const harness = await makeMissingRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: { observers: [{ script: nativeScript(1n), redeemer: false }] },
      source: { kind: "forced", verdict: redeemerMissingVerdict(2n, 0n) },
    });
    const run = await convictAndCheck(harness, scenario, "R-F false");
    expect(run.prepared.final.claim).toHaveProperty("NativeSource");
  }, 1_200_000);

  it("R-F: refuses a forced rejection whose claimed observer redeemer is absent", async () => {
    const harness = await makeMissingRedeemerHarness();
    // The subject the node itself classifies RedeemerMissing { 2, 0 }: it
    // spends a signed-for output and sources its observer from a reference
    // input, so its block commits after an anchor carrying that ledger root.
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: honestForcedObserverShape(5),
      source: { kind: "forced", verdict: redeemerMissingVerdict(2n, 0n) },
    });
    await expect(
      requireConvictingMissingRedeemerProof({
        material: scenario.material,
        contracts: harness.missingRedeemer,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(MissingRedeemerHonestLeaf);
    const references = await publishMissingRedeemerRoles(harness);
    const run = await driveMissingRedeemerThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(false);
    expect(run.prepared.final.claim).toHaveProperty("RedeemerAbsent");
    expectWithinBounds(run.steps);
    // The claimed redeemer is absent, as the leaf says: the terminal's
    // conviction rule returns false and no fraud-proof token can mint.
    await expectOnchainRefusal(() => run.terminal(false), {
      refusedBy: "fraud_proofs/direct_script_derivation/claim_terminal",
      check: /^Validator returned false$/u,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.missingRedeemer.steps.at(-1)!.spendingScriptAddress,
        run.unit,
      ),
    ).resolves.toHaveLength(1);
  }, 1_200_000);
});
