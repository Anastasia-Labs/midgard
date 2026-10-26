/** Shared row drivers for the `missingScriptSource` genuine-Init emulator
 * suites: bounds, conviction to a fraud-proof token, the terminal's refusal
 * of an honest leaf, and removal of the convicted block. */
import { expect } from "vitest";

import { submitRemoveFraudulentBlock } from "../../src/index.js";
import { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";
import type { MissingRedeemerScenario } from "./missing-redeemer-emulator.subject.js";
import {
  driveMissingScriptSourceThread,
  MISSING_SCRIPT_SOURCE_STEP_BOUNDS,
  type MissingScriptSourceHarness,
  publishMissingScriptSourceRoles,
  stepTable,
} from "./missing-script-source-emulator.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./submit-init-emulator-shared.js";

export const expectWithinBounds = (
  steps: Parameters<typeof stepTable>[0],
): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      MISSING_SCRIPT_SOURCE_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      MISSING_SCRIPT_SOURCE_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(MISSING_SCRIPT_SOURCE_STEP_BOUNDS.bytes);
  }
};

/** Drives a convicting thread to its terminal and checks the token. */
export const convictAndCheck = async (
  harness: MissingScriptSourceHarness,
  scenario: MissingRedeemerScenario,
  label: string,
) => {
  const references = await publishMissingScriptSourceRoles(harness);
  const run = await driveMissingScriptSourceThread({
    harness,
    scenario,
    references,
  });
  expect(run.prepared.convicts).toBe(true);
  const outcome = await run.terminal();
  console.log(`MSS ${label} steps`, JSON.stringify(stepTable(run.steps)));
  expectWithinBounds(run.steps);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  await expectSingleUtxoWithUnit(
    harness.proverLucid,
    harness.missingScriptSource.fraudProof.spendingScriptAddress,
    outcome.fraudProofUnit,
  );
  return { run, outcome };
};

/** The planned custody reaches the terminal, which returns false: no
 * fraud-proof token mints and the thread stays at the terminal. */
export const expectTerminalRefusal = async (
  harness: MissingScriptSourceHarness,
  run: Awaited<ReturnType<typeof driveMissingScriptSourceThread>>,
) => {
  expect(run.prepared.convicts).toBe(false);
  expectWithinBounds(run.steps);
  await expectOnchainRefusal(() => run.terminal(false), {
    refusedBy: "fraud_proofs/direct_script_derivation/claim_terminal",
    check: /^Validator returned false$/u,
  });
  await expect(
    harness.proverLucid.utxosAtWithUnit(
      harness.missingScriptSource.steps.at(-1)!.spendingScriptAddress,
      run.unit,
    ),
  ).resolves.toHaveLength(1);
};

/** Removes the convicted block and checks it left the state queue. */
export const removeConvictedBlock = async (
  harness: MissingScriptSourceHarness,
  scenario: MissingRedeemerScenario,
) => {
  const removalReferenceScripts = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const now = BigInt(harness.emulator.now());
  const removal = await submitRemoveFraudulentBlock({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalReferenceScripts.published },
    ),
    network,
    signer: harness.proverSigner,
    fraudCategory: "missingScriptSource",
    fraudulentHeaderHash: scenario.setup.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
  expect(removal.fraudCategory).toBe("missingScriptSource");
  await expect(
    harness.proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      scenario.setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(0);
  return removal;
};
