/**
 * `scriptIntegrityHashMismatch` emulator verticals from a genuine Init. An
 * accepted transaction, normal or forced, whose script integrity hash is not
 * the one its walked languages and redeemer witnesses give convicts, and the
 * normal one's block is removed. A forced rejection that records this arm for
 * a transaction whose hash is the expected one convicts, as does one that
 * records a reason ranked after phase 10 for a transaction with a mismatch.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  requireConvictingScriptIntegrityHashMismatchProof,
  ScriptIntegrityHashMismatchHonestLeaf,
} from "../src/script-integrity-hash-mismatch/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveScriptIntegrityHashMismatchThread,
  makeScriptIntegrityHashMismatchHarness,
  publishScriptIntegrityHashMismatchRoles,
  SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS,
  type ScriptIntegrityHashMismatchHarness,
  stepTable,
} from "./support/script-integrity-hash-mismatch-emulator.js";
import {
  expectedHash,
  honestForcedShape,
  type IntegrityScenario,
  type IntegritySource,
  type IntegritySubjectShape,
  machineForcedVerdict,
  nativeScript,
  plutusScript,
  rejectedFor,
  scriptLockedEntries,
  setupIntegrityScenario,
  WRONG_HASH,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const expectWithinBounds = (steps: Parameters<typeof stepTable>[0]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS.bytes);
  }
};

/** Every purpose kind: a script-locked spend, a mint, a native and a
 * MidgardV1 observer, and a protected receive output. Its walked languages
 * are PlutusV3 and MidgardV1. */
const everyPurpose = (
  integrity: IntegritySubjectShape["integrity"],
): IntegritySubjectShape => {
  const spent = plutusScript(11);
  return {
    spend: scriptLockedEntries(0x41, [spent]),
    witnesses: [spent],
    spendRedeemers: [0n],
    mints: [{ script: plutusScript(12), redeemer: true }],
    observers: [
      { script: nativeScript(1n), redeemer: false },
      { script: plutusScript(13, "MidgardV1"), redeemer: true },
    ],
    receives: [{ script: plutusScript(14), redeemer: true }],
    integrity,
  };
};

/** One Plutus V3 observer. */
const oneObserver = (
  integrity: IntegritySubjectShape["integrity"],
): IntegritySubjectShape => ({
  observers: [{ script: plutusScript(1), redeemer: true }],
  integrity,
});

const removeBlock = async (
  harness: ScriptIntegrityHashMismatchHarness,
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
    fraudCategory: "scriptIntegrityHashMismatch",
    fraudulentHeaderHash: headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
};

/** Drives a convicting thread to its terminal and checks the token. */
const convictAndCheck = async (
  shape: IntegritySubjectShape,
  source: IntegritySource,
  label: string,
): Promise<{
  readonly harness: ScriptIntegrityHashMismatchHarness;
  readonly scenario: IntegrityScenario;
  readonly run: Awaited<
    ReturnType<typeof driveScriptIntegrityHashMismatchThread>
  >;
}> => {
  const harness = await makeScriptIntegrityHashMismatchHarness();
  const scenario = await setupIntegrityScenario({ harness, shape, source });
  const references = await publishScriptIntegrityHashMismatchRoles(harness);
  const run = await driveScriptIntegrityHashMismatchThread({
    harness,
    scenario,
    references,
  });
  expect(run.prepared.convicts).toBe(true);
  const outcome = await run.terminal();
  console.log(`SIHM ${label} steps`, JSON.stringify(stepTable(run.steps)));
  expectWithinBounds(run.steps);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  const token = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    harness.scriptIntegrityHashMismatch.fraudProof.spendingScriptAddress,
    outcome.fraudProofUnit,
  );
  expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
  expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
    fraud_prover: harness.proverSigner.paymentKeyHash,
  });
  for (const step of harness.scriptIntegrityHashMismatch.steps)
    await expect(
      harness.proverLucid.utxosAtWithUnit(step.spendingScriptAddress, run.unit),
    ).resolves.toHaveLength(0);
  return { harness, scenario, run };
};

describe("script-integrity-hash-mismatch genuine-Init verticals", () => {
  it("A-N: convicts an accepted transaction whose hash omits a walked language and removes its block", async () => {
    const { harness, scenario, run } = await convictAndCheck(
      everyPurpose(expectedHash("PlutusV3")),
      { kind: "normal" },
      "A-N every purpose",
    );
    expect(run.prepared.final.facts.language_bitmap).toBe(3n);
    expect(run.prepared.mismatch).toBe(true);
    const removal = await removeBlock(harness, scenario.setup.headerHash);
    expect(removal.fraudCategory).toBe("scriptIntegrityHashMismatch");
    expect(removal.transactions.map((tx) => tx.kind)).toStrictEqual([
      "remove-target",
    ]);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 1_800_000);

  it("A-F: convicts a forced acceptance whose hash is not the expected one", async () => {
    const { run } = await convictAndCheck(
      oneObserver(WRONG_HASH),
      { kind: "forced", verdict: "ForcedTxValid" },
      "A-F",
    );
    expect(run.prepared.final.facts.language_bitmap).toBe(1n);
    expect(run.prepared.mismatch).toBe(true);
  }, 1_200_000);

  it("R-F: convicts a forced rejection recording this arm for the expected hash", async () => {
    const { run } = await convictAndCheck(
      oneObserver(expectedHash("PlutusV3")),
      rejectedFor("ScriptIntegrityHashMismatch"),
      "R-F false",
    );
    expect(run.prepared.mismatch).toBe(false);
  }, 1_200_000);

  it("R-F: convicts a forced rejection recording a phase-12 reason for a mismatch", async () => {
    const { run } = await convictAndCheck(
      oneObserver(WRONG_HASH),
      rejectedFor("ValueNotPreserved"),
      "R-F later reason",
    );
    expect(run.prepared.mismatch).toBe(true);
  }, 1_200_000);

  it.each([
    {
      name: "an accepted transaction carrying the expected hash",
      shape: async () => everyPurpose(expectedHash("PlutusV3", "MidgardV1")),
      source: async (): Promise<IntegritySource> => ({ kind: "normal" }),
      mismatch: false,
    },
    {
      name: "an honest forced ScriptIntegrityHashMismatch rejection",
      shape: async () => honestForcedShape(WRONG_HASH),
      source: async (): Promise<IntegritySource> => ({
        kind: "forced",
        verdict: await machineForcedVerdict(honestForcedShape(WRONG_HASH)),
      }),
      mismatch: true,
    },
    {
      name: "a forced rejection recording a phase-6 reason for a mismatch",
      shape: async () => oneObserver(WRONG_HASH),
      source: async () => rejectedFor("ScriptIntegrityHashMissing"),
      mismatch: true,
    },
  ])(
    "refuses $name: the terminal returns false",
    async (row) => {
      const harness = await makeScriptIntegrityHashMismatchHarness();
      const scenario = await setupIntegrityScenario({
        harness,
        shape: await row.shape(),
        source: await row.source(),
      });
      await expect(
        requireConvictingScriptIntegrityHashMismatchProof({
          material: scenario.material,
          contracts: harness.scriptIntegrityHashMismatch,
          prover: harness.proverSigner.paymentKeyHash,
        }),
      ).rejects.toThrow(ScriptIntegrityHashMismatchHonestLeaf);
      const references = await publishScriptIntegrityHashMismatchRoles(harness);
      const run = await driveScriptIntegrityHashMismatchThread({
        harness,
        scenario,
        references,
      });
      expect(run.prepared.convicts).toBe(false);
      expect(run.prepared.mismatch).toBe(row.mismatch);
      expectWithinBounds(run.steps);
      await expectOnchainRefusal(() => run.terminal(false), {
        refusedBy: "fraud_proofs/script_integrity_hash_mismatch/terminal",
        check: /^Validator returned false$/u,
      });
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          harness.scriptIntegrityHashMismatch.steps.at(-1)!
            .spendingScriptAddress,
          run.unit,
        ),
      ).resolves.toHaveLength(1);
    },
    1_200_000,
  );
});
