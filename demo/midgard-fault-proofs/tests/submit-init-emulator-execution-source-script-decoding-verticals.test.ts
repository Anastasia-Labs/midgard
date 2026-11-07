/**
 * `executionSourceScriptDecoding` emulator verticals from a genuine Init. A
 * forced rejection recording any of the three native script decoding arms
 * convicts, and its block is removed: the execution phase decodes no native
 * script, so no transaction can earn those arms. The terminal reads the
 * recorded arm alone, so a rejection whose inputs have no predecessor
 * descriptor convicts too. A leaf recording any other verdict reaches the
 * terminal, which returns false.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ExecutionSourceScriptDecodingHonestLeaf,
  requireConvictingExecutionSourceScriptDecodingProof,
} from "../src/execution-source-script-decoding/index.js";
import { submitRemoveFraudulentBlock } from "../src/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveExecutionSourceScriptDecodingThread,
  EXECUTION_SOURCE_SCRIPT_DECODING_STEP_BOUNDS,
  type ExecutionSourceScriptDecodingHarness,
  makeExecutionSourceScriptDecodingHarness,
  publishExecutionSourceScriptDecodingRoles,
  stepTable,
} from "./support/execution-source-script-decoding-emulator.js";
import {
  honestForcedObserverShape,
  machineForcedVerdict,
  type MissingRedeemerSource,
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
      EXECUTION_SOURCE_SCRIPT_DECODING_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      EXECUTION_SOURCE_SCRIPT_DECODING_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(EXECUTION_SOURCE_SCRIPT_DECODING_STEP_BOUNDS.bytes);
  }
};

/** An admissible forced subject: a signed key spend whose observer's Plutus
 * source only a reference input carries. */
const SHAPE = honestForcedObserverShape(5);

const rejected = (reason: SDK.RejectionReason): MissingRedeemerSource => ({
  kind: "forced",
  verdict: { ForcedTxInvalid: { reason } },
});

const coordinate = { purpose_kind: 2n, purpose_index: 0n };

const removeBlock = async (
  harness: ExecutionSourceScriptDecodingHarness,
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
    fraudCategory: "executionSourceScriptDecoding",
    fraudulentHeaderHash: headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
};

describe("execution-source-script-decoding genuine-Init verticals", () => {
  it.each([
    ["ExecutionNativeScriptMalformed", true],
    ["ExecutionNativeScriptNodeLimit", false],
    ["ExecutionNativeScriptDepthLimit", false],
  ] as const)(
    "R-F: convicts a forced rejection recording %s",
    async (arm, remove) => {
      const harness = await makeExecutionSourceScriptDecodingHarness();
      const references =
        await publishExecutionSourceScriptDecodingRoles(harness);
      const scenario = await setupMissingRedeemerScenario({
        harness,
        shape: SHAPE,
        source: rejected({ [arm]: coordinate } as SDK.RejectionReason),
      });
      expect(scenario.block.txInclusion).toBeNull();
      const run = await driveExecutionSourceScriptDecodingThread({
        harness,
        scenario,
        references,
      });
      expect(run.prepared.convicts).toBe(true);
      expect(run.prepared.final.subject.rejection_reason).toStrictEqual({
        [arm]: coordinate,
      });
      const outcome = await run.terminal();
      console.log(
        `ESSD R-F ${arm} steps`,
        JSON.stringify(stepTable(run.steps)),
      );
      expect(run.steps.map(({ label }) => label)).toHaveLength(4);
      expectWithinBounds(run.steps);
      expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
      const token = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        harness.essd.fraudProof.spendingScriptAddress,
        outcome.fraudProofUnit,
      );
      expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
      expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
        fraud_prover: harness.proverSigner.paymentKeyHash,
      });
      for (const step of harness.essd.steps)
        await expect(
          harness.proverLucid.utxosAtWithUnit(
            step.spendingScriptAddress,
            run.unit,
          ),
        ).resolves.toHaveLength(0);
      if (!remove) return;
      const removal = await removeBlock(harness, scenario.setup.headerHash);
      expect(removal.fraudCategory).toBe("executionSourceScriptDecoding");
      expect(removal.transactions.map((tx) => tx.kind)).toStrictEqual([
        "remove-target",
      ]);
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          harness.contracts.stateQueue.spendingScriptAddress,
          scenario.setup.stateQueueBlockUnit,
        ),
      ).resolves.toHaveLength(0);
    },
    1_200_000,
  );

  it("R-F: convicts a forced rejection recording ExecutionNativeScriptMalformed over inputs with no predecessor descriptor", async () => {
    const harness = await makeExecutionSourceScriptDecodingHarness();
    const references = await publishExecutionSourceScriptDecodingRoles(harness);
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: SHAPE,
      source: rejected({ ExecutionNativeScriptMalformed: coordinate }),
      ledger: [],
    });
    expect(scenario.block.header.prevUtxosRoot).toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
    const run = await driveExecutionSourceScriptDecodingThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    const outcome = await run.terminal();
    expectWithinBounds(run.steps);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    const token = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.essd.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
  }, 1_200_000);

  it.each([
    {
      name: "the node's own forced rejection of the subject",
      source: async (): Promise<MissingRedeemerSource> => {
        const verdict = await machineForcedVerdict(SHAPE);
        expect(verdict).toStrictEqual({
          ForcedTxInvalid: { reason: { RedeemerMissing: coordinate } },
        });
        return { kind: "forced", verdict };
      },
    },
    {
      name: "a forced rejection recording ExecutionNativeScriptFalse",
      source: async () => rejected({ ExecutionNativeScriptFalse: coordinate }),
    },
    {
      name: "a forced acceptance",
      source: async (): Promise<MissingRedeemerSource> => ({
        kind: "forced",
        verdict: "ForcedTxValid",
      }),
    },
    {
      name: "an accepted transaction",
      source: async (): Promise<MissingRedeemerSource> => ({ kind: "normal" }),
    },
  ])(
    "refuses $name: the terminal returns false",
    async (row) => {
      const harness = await makeExecutionSourceScriptDecodingHarness();
      const scenario = await setupMissingRedeemerScenario({
        harness,
        shape: SHAPE,
        source: await row.source(),
      });
      expect(() =>
        requireConvictingExecutionSourceScriptDecodingProof({
          material: scenario.material,
          prover: harness.proverSigner.paymentKeyHash,
        }),
      ).toThrow(ExecutionSourceScriptDecodingHonestLeaf);
      const references =
        await publishExecutionSourceScriptDecodingRoles(harness);
      const run = await driveExecutionSourceScriptDecodingThread({
        harness,
        scenario,
        references,
      });
      expect(run.prepared.convicts).toBe(false);
      expectWithinBounds(run.steps);
      await expectOnchainRefusal(() => run.terminal(false), {
        refusedBy: "fraud_proofs/execution_source_script_decoding/terminal",
        check: /^Validator returned false$/u,
      });
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          harness.essd.steps.at(-1)!.spendingScriptAddress,
          run.unit,
        ),
      ).resolves.toHaveLength(1);
    },
    1_200_000,
  );
});
