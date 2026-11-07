/**
 * `executionNativeScriptInvalid` emulator verticals from a genuine Init. An
 * accepted transaction, normal or forced, whose observer's native script
 * evaluates false convicts, and the normal one's block is removed. A forced
 * rejection recording `ExecutionNativeScriptFalse` at an observer whose
 * script holds convicts, as does one recording a reason ranked after native
 * evaluation for a transaction whose native script is false. The honest
 * rejection, an accepted transaction whose script holds, and a rejection
 * recording an earlier reason all reach the terminal, which returns false.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ExecutionNativeScriptInvalidHonestLeaf,
  requireConvictingExecutionNativeScriptInvalidProof,
} from "../src/execution-native-script-invalid/index.js";
import { submitRemoveFraudulentBlock } from "../src/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveExecutionNativeScriptInvalidThread,
  EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS,
  type ExecutionNativeScriptInvalidHarness,
  makeExecutionNativeScriptInvalidHarness,
  publishExecutionNativeScriptInvalidRoles,
  stepTable,
} from "./support/execution-native-script-invalid-emulator.js";
import {
  admissibleNativeObservers,
  machineNativeVerdict,
  native,
  nativeFalseAt,
  type NativeScenario,
  type NativeSource,
  type NativeSubjectShape,
  PAYER,
  setupNativeScenario,
  sig,
  signingKey,
} from "./support/execution-native-script-invalid-emulator.subject.js";
import { rejectedFor } from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const expectWithinBounds = (steps: Parameters<typeof stepTable>[0]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS.bytes);
  }
};

const COSIGNER = signingKey(1);
const ABSENT = signingKey(2);

/** Signed by the payer, but valid only from slot 100: false for a
 * transaction valid from slot 0. */
const NOT_YET = native({
  type: "all",
  scripts: [sig(PAYER), { type: "after", slot: 100n }],
});
/** An absent key or an upper bound below 10: false without an upper bound. */
const NEITHER = native({
  type: "any",
  scripts: [sig(ABSENT), { type: "before", slot: 10n }],
});
/** Two of the payer, the co-signer and an absent key: holds. */
const TWO_OF_THREE = native({
  type: "atLeast",
  required: 2n,
  scripts: [sig(PAYER), sig(ABSENT), sig(COSIGNER)],
});

const notYetShape = () =>
  admissibleNativeObservers([NOT_YET], { validityStart: 0n });
const neitherShape = () => admissibleNativeObservers([NEITHER]);
const twoOfThreeShape = () =>
  admissibleNativeObservers([TWO_OF_THREE], { signers: [COSIGNER] });

const OBSERVER = { kind: 2n, index: 0n };

const removeBlock = async (
  harness: ExecutionNativeScriptInvalidHarness,
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
    fraudCategory: "executionNativeScriptInvalid",
    fraudulentHeaderHash: headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
};

/** Drives a convicting thread to its terminal and checks the token. */
const convictAndCheck = async (
  shape: NativeSubjectShape,
  source: NativeSource,
  label: string,
): Promise<{
  readonly harness: ExecutionNativeScriptInvalidHarness;
  readonly scenario: NativeScenario;
  readonly run: Awaited<
    ReturnType<typeof driveExecutionNativeScriptInvalidThread>
  >;
}> => {
  const harness = await makeExecutionNativeScriptInvalidHarness();
  const scenario = await setupNativeScenario({ harness, shape, source });
  const references = await publishExecutionNativeScriptInvalidRoles(harness);
  const run = await driveExecutionNativeScriptInvalidThread({
    harness,
    scenario,
    references,
  });
  expect(run.prepared.convicts).toBe(true);
  const outcome = await run.terminal();
  console.log(`ENSI ${label} steps`, JSON.stringify(stepTable(run.steps)));
  expectWithinBounds(run.steps);
  expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
  const token = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    harness.executionNativeScriptInvalid.fraudProof.spendingScriptAddress,
    outcome.fraudProofUnit,
  );
  expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
  expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
    fraud_prover: harness.proverSigner.paymentKeyHash,
  });
  for (const step of harness.executionNativeScriptInvalid.steps)
    await expect(
      harness.proverLucid.utxosAtWithUnit(step.spendingScriptAddress, run.unit),
    ).resolves.toHaveLength(0);
  return { harness, scenario, run };
};

const evaluated = (satisfied: boolean) => ({
  Evaluated: { coordinate: OBSERVER, satisfied },
});

describe("execution-native-script-invalid genuine-Init verticals", () => {
  it("A-N: convicts an accepted transaction whose observer's native script is not yet valid and removes its block", async () => {
    const { harness, scenario, run } = await convictAndCheck(
      notYetShape(),
      { kind: "normal" },
      "A-N",
    );
    expect(run.prepared.final.fact).toStrictEqual(evaluated(false));
    const removal = await removeBlock(harness, scenario.setup.headerHash);
    expect(removal.fraudCategory).toBe("executionNativeScriptInvalid");
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

  it("A-F: convicts a forced acceptance whose observer's native script is false", async () => {
    const { scenario, run } = await convictAndCheck(
      neitherShape(),
      { kind: "forced", verdict: "ForcedTxValid" },
      "A-F",
    );
    expect(scenario.block.txInclusion).toBeNull();
    expect(run.prepared.final.fact).toStrictEqual(evaluated(false));
  }, 1_200_000);

  it("R-F: convicts a forced rejection recording ExecutionNativeScriptFalse at an observer whose script holds", async () => {
    const { run } = await convictAndCheck(
      twoOfThreeShape(),
      rejectedFor(nativeFalseAt(2n, 0n)),
      "R-F holds",
    );
    expect(run.prepared.final.fact).toStrictEqual(evaluated(true));
  }, 1_200_000);

  it("R-F: convicts a forced rejection recording a phase-12 reason for a native script that is false", async () => {
    const { run } = await convictAndCheck(
      neitherShape(),
      rejectedFor("ValueNotPreserved"),
      "R-F later reason",
    );
    expect(run.prepared.final.fact).toStrictEqual(evaluated(false));
  }, 1_200_000);

  it.each([
    {
      name: "the node's own ExecutionNativeScriptFalse rejection",
      shape: notYetShape,
      source: async (): Promise<NativeSource> => {
        const verdict = await machineNativeVerdict(notYetShape());
        expect(verdict).toStrictEqual({
          ForcedTxInvalid: { reason: nativeFalseAt(2n, 0n) },
        });
        return { kind: "forced", verdict };
      },
      satisfied: false,
    },
    {
      name: "an accepted transaction whose native script holds",
      shape: twoOfThreeShape,
      source: async (): Promise<NativeSource> => ({ kind: "normal" }),
      satisfied: true,
    },
    {
      name: "a forced rejection recording an earlier reason for a false native script",
      shape: neitherShape,
      source: async () => rejectedFor("FeeBelowMinimum"),
      satisfied: false,
    },
  ])(
    "refuses $name: the terminal returns false",
    async (row) => {
      const harness = await makeExecutionNativeScriptInvalidHarness();
      const scenario = await setupNativeScenario({
        harness,
        shape: row.shape(),
        source: await row.source(),
      });
      await expect(
        requireConvictingExecutionNativeScriptInvalidProof({
          material: scenario.material,
          contracts: harness.executionNativeScriptInvalid,
          prover: harness.proverSigner.paymentKeyHash,
          entry: { Observe: OBSERVER },
        }),
      ).rejects.toThrow(ExecutionNativeScriptInvalidHonestLeaf);
      const references =
        await publishExecutionNativeScriptInvalidRoles(harness);
      const run = await driveExecutionNativeScriptInvalidThread({
        harness,
        scenario,
        references,
        entry: { Observe: OBSERVER },
      });
      expect(run.prepared.convicts).toBe(false);
      expect(run.prepared.final.fact).toStrictEqual(evaluated(row.satisfied));
      expectWithinBounds(run.steps);
      await expectOnchainRefusal(() => run.terminal(false), {
        refusedBy: "fraud_proofs/execution_native_script_invalid/terminal",
        check: /^Validator returned false$/u,
      });
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          harness.executionNativeScriptInvalid.steps.at(-1)!
            .spendingScriptAddress,
          run.unit,
        ),
      ).resolves.toHaveLength(1);
    },
    1_200_000,
  );
});
