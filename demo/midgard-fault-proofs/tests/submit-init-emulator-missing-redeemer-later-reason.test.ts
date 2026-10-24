/**
 * `missingRedeemer` emulator rows from a genuine Init for a forced rejection
 * whose recorded reason names no purpose coordinate: the subject's
 * reference-input observer has a Plutus source and no redeemer. Under a reason
 * the purpose phase does not admit before that missing redeemer, the observed
 * route reaches a fraud-proof token; under an earlier reason the plan is
 * refused and the terminal returns false.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  MissingRedeemerHonestLeaf,
  requireConvictingMissingRedeemerProof,
} from "../src/missing-redeemer/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  MISSING_REDEEMER_STEP_BOUNDS,
  publishMissingRedeemerRoles,
  stepTable,
} from "./support/missing-redeemer-emulator.js";
import {
  honestForcedObserverShape,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

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

const rejectedFor = async (reason: SDK.RejectionReason) => {
  const harness = await makeMissingRedeemerHarness();
  const scenario = await setupMissingRedeemerScenario({
    harness,
    shape: honestForcedObserverShape(5),
    source: { kind: "forced", verdict: { ForcedTxInvalid: { reason } } },
  });
  return { harness, scenario };
};

describe("missing-redeemer genuine-Init rows under a later recorded reason", () => {
  it.each([
    [
      "PlutusExecutionFailed",
      { PlutusExecutionFailed: { purpose_kind: 2n, purpose_index: 0n } },
    ],
    ["UnusedRedeemer", { UnusedRedeemer: { redeemer_index: 0n } }],
  ] as const)(
    "reaches a fraud-proof token for a missing redeemer under a recorded %s",
    async (label, reason) => {
      const { harness, scenario } = await rejectedFor(reason);
      const references = await publishMissingRedeemerRoles(harness);
      const run = await driveMissingRedeemerThread({
        harness,
        scenario,
        references,
      });
      expect(run.prepared.convicts).toBe(true);
      expect(run.prepared.final.claim).toBe("NotRequired");
      expect(run.prepared.final.context.observed?.redeemer_missing).toBe(true);
      expect(
        run.prepared.final.context.observed?.authenticated.selection.coordinate,
      ).toStrictEqual({ kind: 2n, index: 0n });
      const outcome = await run.terminal();
      console.log(`MR ${label} steps`, JSON.stringify(stepTable(run.steps)));
      expectWithinBounds(run.steps);
      expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
      await expectSingleUtxoWithUnit(
        harness.proverLucid,
        harness.missingRedeemer.fraudProof.spendingScriptAddress,
        outcome.fraudProofUnit,
      );
    },
    1_200_000,
  );

  it("refuses a missing redeemer under an earlier recorded reason", async () => {
    const { harness, scenario } = await rejectedFor(
      "ScriptIntegrityHashMissing",
    );
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
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(run.prepared.final.context.observed?.redeemer_missing).toBe(true);
    expectWithinBounds(run.steps);
    // The recorded reason precedes the purpose phase: the terminal returns
    // false and no fraud-proof token can mint.
    await expectOnchainRefusal(() => run.terminal(false), {
      refusedBy: "fraud_proofs/missing_redeemer/step_05",
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
