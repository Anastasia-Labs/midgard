/**
 * `unusedScriptWitness` emulator verticals from a genuine Init. An accepted
 * transaction, normal or forced, carrying a field-6 witness that is not the
 * executed source of its script hash convicts, and the normal one's block is
 * removed. A forced rejection recording this arm for a witness that is used,
 * or that field 6 does not carry, convicts, as does one recording a reason
 * ranked after the unused-witness audit for a transaction with an unused
 * witness.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  prepareUnusedScriptWitnessProof,
  requireConvictingUnusedScriptWitnessProof,
  UNUSED_SCRIPT_WITNESS_DIRECT_WALK,
  UnusedScriptWitnessHonestLeaf,
} from "../src/unused-script-witness/index.js";
import {
  DIRECT_WALK_STEP_BOUNDS,
  type DirectWalkHarness,
  driveDirectWalkThread,
  makeDirectWalkHarness,
  publishDirectWalkRoles,
  stepTable,
} from "./support/direct-walk-emulator.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  type IntegritySource,
  type IntegritySubjectShape,
  machineForcedVerdict,
  plutusScript,
  rejectedFor,
  setupIntegrityScenario,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import {
  admissibleForcedWith,
  EVERY_PURPOSE_FIRST_EXTRA,
  everyPurposeWith,
  holdingNativeScript,
  nativeLockedForced,
} from "./support/unused-script-witness-emulator.subject.js";

const expectWithinBounds = (steps: Parameters<typeof stepTable>[0]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      DIRECT_WALK_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      DIRECT_WALK_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(DIRECT_WALK_STEP_BOUNDS.bytes);
  }
};

const witnessReason = (script_index: bigint): SDK.RejectionReason => ({
  UnusedScriptWitness: { script_index },
});

const removeBlock = async (harness: DirectWalkHarness, headerHash: string) => {
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
    fraudCategory: "unusedScriptWitness",
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
) => {
  const harness = await makeDirectWalkHarness(
    UNUSED_SCRIPT_WITNESS_DIRECT_WALK,
  );
  const scenario = await setupIntegrityScenario({ harness, shape, source });
  const references = await publishDirectWalkRoles(
    harness,
    UNUSED_SCRIPT_WITNESS_DIRECT_WALK,
  );
  const run = await driveDirectWalkThread({
    walk: UNUSED_SCRIPT_WITNESS_DIRECT_WALK,
    prepare: prepareUnusedScriptWitnessProof,
    harness,
    scenario,
    references,
  });
  expect(run.prepared.convicts).toBe(true);
  const outcome = await run.terminal();
  console.log(`USW ${label} steps`, JSON.stringify(stepTable(run.steps)));
  expectWithinBounds(run.steps);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  const token = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    harness.direct.fraudProof.spendingScriptAddress,
    outcome.fraudProofUnit,
  );
  expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
  expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
    fraud_prover: harness.proverSigner.paymentKeyHash,
  });
  for (const step of harness.direct.steps)
    await expect(
      harness.proverLucid.utxosAtWithUnit(step.spendingScriptAddress, run.unit),
    ).resolves.toHaveLength(0);
  return { harness, scenario, run };
};

describe("unused-script-witness genuine-Init verticals", () => {
  it("A-N: convicts an accepted transaction carrying a witness no purpose executes and removes its block", async () => {
    const { harness, scenario, run } = await convictAndCheck(
      everyPurposeWith([plutusScript(15)]),
      { kind: "normal" },
      "A-N every purpose",
    );
    expect(run.prepared.scriptIndex).toBe(EVERY_PURPOSE_FIRST_EXTRA);
    expect(run.prepared.unused).toBe(true);
    const removal = await removeBlock(harness, scenario.setup.headerHash);
    expect(removal.fraudCategory).toBe("unusedScriptWitness");
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

  it("A-F: convicts a forced acceptance carrying an unused native witness that holds", async () => {
    const { run } = await convictAndCheck(
      admissibleForcedWith([holdingNativeScript(2)]),
      { kind: "forced", verdict: "ForcedTxValid" },
      "A-F",
    );
    expect(run.prepared.scriptIndex).toBe(0n);
    expect(run.prepared.unused).toBe(true);
  }, 1_200_000);

  it("R-F: convicts a forced rejection recording this arm for a used witness", async () => {
    const { run } = await convictAndCheck(
      nativeLockedForced(),
      rejectedFor(witnessReason(0n)),
      "R-F used",
    );
    expect(run.prepared.scriptIndex).toBe(0n);
    expect(run.prepared.unused).toBe(false);
  }, 1_200_000);

  it("R-F: convicts a forced rejection recording this arm past the last witness", async () => {
    const { run } = await convictAndCheck(
      nativeLockedForced(),
      rejectedFor(witnessReason(3n)),
      "R-F absent",
    );
    expect(run.prepared.scriptIndex).toBe(3n);
    expect(run.prepared.selection).toBe(null);
    expect(run.prepared.unused).toBe(false);
  }, 1_200_000);

  it("R-F: convicts a forced rejection recording a phase-12 reason for an unused witness", async () => {
    const { run } = await convictAndCheck(
      admissibleForcedWith([holdingNativeScript(2)]),
      rejectedFor("ValueNotPreserved"),
      "R-F later reason",
    );
    expect(run.prepared.scriptIndex).toBe(0n);
    expect(run.prepared.unused).toBe(true);
  }, 1_200_000);

  it.each([
    {
      name: "an accepted transaction whose every witness is used",
      shape: () => everyPurposeWith(),
      source: async (): Promise<IntegritySource> => ({ kind: "normal" }),
      scriptIndex: 0n,
      unused: false,
    },
    {
      name: "an honest forced UnusedScriptWitness rejection",
      shape: () => admissibleForcedWith([holdingNativeScript(2)]),
      source: async (): Promise<IntegritySource> => {
        const verdict = await machineForcedVerdict(
          admissibleForcedWith([holdingNativeScript(2)]),
        );
        expect(verdict).toStrictEqual({
          ForcedTxInvalid: { reason: witnessReason(0n) },
        });
        return { kind: "forced", verdict };
      },
      scriptIndex: 0n,
      unused: true,
    },
    {
      name: "a forced rejection recording an earlier reason for an unused witness",
      shape: () => admissibleForcedWith([holdingNativeScript(2)]),
      source: async () => rejectedFor("FeeBelowMinimum"),
      scriptIndex: 0n,
      unused: true,
    },
  ])(
    "refuses $name: the terminal returns false",
    async (row) => {
      const harness = await makeDirectWalkHarness(
        UNUSED_SCRIPT_WITNESS_DIRECT_WALK,
      );
      const scenario = await setupIntegrityScenario({
        harness,
        shape: row.shape(),
        source: await row.source(),
      });
      await expect(
        requireConvictingUnusedScriptWitnessProof({
          material: scenario.material,
          contracts: harness.direct,
          prover: harness.proverSigner.paymentKeyHash,
          scriptIndex: row.scriptIndex,
        }),
      ).rejects.toThrow(UnusedScriptWitnessHonestLeaf);
      const references = await publishDirectWalkRoles(
        harness,
        UNUSED_SCRIPT_WITNESS_DIRECT_WALK,
      );
      const run = await driveDirectWalkThread({
        walk: UNUSED_SCRIPT_WITNESS_DIRECT_WALK,
        prepare: (input) =>
          prepareUnusedScriptWitnessProof({
            ...input,
            scriptIndex: row.scriptIndex,
          }),
        harness,
        scenario,
        references,
      });
      expect(run.prepared.convicts).toBe(false);
      expect(run.prepared.unused).toBe(row.unused);
      expectWithinBounds(run.steps);
      await expectOnchainRefusal(() => run.terminal(false), {
        refusedBy: "fraud_proofs/unused_script_witness/terminal",
        check: /^Validator returned false$/u,
      });
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          harness.direct.steps.at(-1)!.spendingScriptAddress,
          run.unit,
        ),
      ).resolves.toHaveLength(1);
    },
    1_200_000,
  );
});
