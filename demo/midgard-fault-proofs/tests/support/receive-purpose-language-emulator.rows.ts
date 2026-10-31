/** Shared row drivers for the `receivePurposeLanguage` genuine-Init emulator
 * suites: bounds, conviction to a fraud-proof token, the terminal's refusal
 * of an honest leaf, and removal of the convicted block. */
import type * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import { submitRemoveFraudulentBlock } from "../../src/index.js";
import { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";
import type { MissingRedeemerScenario } from "./missing-redeemer-emulator.subject.js";
import {
  driveReceivePurposeLanguageThread,
  publishReceivePurposeLanguageRoles,
  RECEIVE_PURPOSE_LANGUAGE_STEP_BOUNDS,
  type ReceivePurposeLanguageHarness,
  stepTable,
} from "./receive-purpose-language-emulator.js";
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
      RECEIVE_PURPOSE_LANGUAGE_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      RECEIVE_PURPOSE_LANGUAGE_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(RECEIVE_PURPOSE_LANGUAGE_STEP_BOUNDS.bytes);
  }
};

/** Drives a convicting thread to its terminal and checks the token. */
export const convictAndCheck = async (
  harness: ReceivePurposeLanguageHarness,
  scenario: MissingRedeemerScenario,
  label: string,
  /** The receive purpose to observe; the first convicting one otherwise. */
  observed?: SDK.DirectPurposeCoordinate,
) => {
  const references = await publishReceivePurposeLanguageRoles(harness);
  const run = await driveReceivePurposeLanguageThread({
    harness,
    scenario,
    references,
    ...(observed === undefined ? {} : { observed }),
  });
  expect(run.prepared.convicts).toBe(true);
  const outcome = await run.terminal();
  console.log(`RPL ${label} steps`, JSON.stringify(stepTable(run.steps)));
  expectWithinBounds(run.steps);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  await expectSingleUtxoWithUnit(
    harness.proverLucid,
    harness.receivePurposeLanguage.fraudProof.spendingScriptAddress,
    outcome.fraudProofUnit,
  );
  return { run, outcome };
};

/** The planned custody reaches the terminal, which returns false: no
 * fraud-proof token mints and the thread stays at the terminal. */
export const expectTerminalRefusal = async (
  harness: ReceivePurposeLanguageHarness,
  run: Awaited<ReturnType<typeof driveReceivePurposeLanguageThread>>,
) => {
  expect(run.prepared.convicts).toBe(false);
  expectWithinBounds(run.steps);
  await expectOnchainRefusal(() => run.terminal(false), {
    refusedBy: "fraud_proofs/receive_purpose_language/terminal",
    check: /^Validator returned false$/u,
  });
  await expect(
    harness.proverLucid.utxosAtWithUnit(
      harness.receivePurposeLanguage.steps.at(-1)!.spendingScriptAddress,
      run.unit,
    ),
  ).resolves.toHaveLength(1);
};

/** Removes the convicted block and checks it left the state queue. */
export const removeConvictedBlock = async (
  harness: ReceivePurposeLanguageHarness,
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
    fraudCategory: "receivePurposeLanguage",
    fraudulentHeaderHash: scenario.setup.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
  expect(removal.fraudCategory).toBe("receivePurposeLanguage");
  await expect(
    harness.proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      scenario.setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(0);
  return removal;
};
