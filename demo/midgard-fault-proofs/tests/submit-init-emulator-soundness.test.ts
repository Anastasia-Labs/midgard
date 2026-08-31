import { PROTOCOL_PARAMETERS_DEFAULT, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { validationResolverIndexV1 } from "../src/index.js";
import {
  buildAcceptedClaimOverRejectingTransactionFixture,
  buildNonEmptyClaimedLedgerDeltaRootV1,
  EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

// One emulator journey per file, on purpose. Each journey leaks wasm linear
// memory that `@lucid-evolution/uplc` never reclaims, and vitest isolates per
// FILE, so co-locating journeys walks the worker into the wasm32 ceiling and
// surfaces it as `EvaluatorError: unreachable`. See
// tests/support/uplc-heap-guard.ts. The two siblings of this file are
// submit-init-emulator-soundness-cleared-delta.test.ts and
// submit-init-emulator-soundness-honest-operator.test.ts.
describe("validation-dispute soundness with a non-empty claimed ledger delta", () => {
  it("lets a challenger win against an operator who claimed Accepted over a non-empty claimed ledger delta", async () => {
    const claimedLedgerDeltaRoot =
      await buildNonEmptyClaimedLedgerDeltaRootV1();
    // Guard against the fixture silently degrading back into the empty-delta
    // special case that hid VM-DEFECT-2.
    expect(claimedLedgerDeltaRoot).toHaveLength(32);
    expect(
      claimedLedgerDeltaRoot.equals(EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1),
    ).toBe(false);

    let removalReferenceScriptPublicationAttempts = 0;
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildAcceptedClaimOverRejectingTransactionFixture({
          operatorVkey,
          now,
          claimedLedgerDeltaRoot,
        }),
      {
        onRemovalReferenceScriptPublicationAttempt: () => {
          removalReferenceScriptPublicationAttempts += 1;
        },
      },
    );
    expect(removalReferenceScriptPublicationAttempts).toBe(1);
    const { fixture, lowIndex, highIndex } = result;

    // The disputed boundary is the rejecting terminal itself, which is the
    // only boundary at which `rejected_successor_is_exact` is exercised.
    expect(highIndex).toBe(lowIndex + 1);
    expect(highIndex).toBe(fixture.challengerTrace.states.length - 1);
    const preState = fixture.challengerTrace.states[lowIndex]!;
    const challengerSuccessor = fixture.challengerTrace.states[highIndex]!;
    expect(preState.phase).toBe("inputSets");
    expect(preState.verdict).toBe("pending");
    expect(challengerSuccessor.phase).toBe("terminal");
    expect(challengerSuccessor.verdict).toBe("rejected");
    // Non-vacuity: both endpoints of the proved transition carry the same
    // genuinely non-empty claimed delta. Under the deleted clause the
    // successor would have had to carry frontier_commitment(0, []) instead.
    expect(preState.ledgerDeltaRoot.equals(claimedLedgerDeltaRoot)).toBe(true);
    expect(
      challengerSuccessor.ledgerDeltaRoot.equals(claimedLedgerDeltaRoot),
    ).toBe(true);
    expect(
      challengerSuccessor.ledgerDeltaRoot.equals(
        EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1,
      ),
    ).toBe(false);
    // The operator really did commit `Accepted`, which the forced-source
    // binding forces from `verdict: ForcedTxValid`.
    expect(fixture.operatorTrace.tree.descriptor.verdict).toBe("accepted");
    expect(fixture.challengerTrace.tree.descriptor.verdict).toBe("rejected");
    expect(fixture.evidence.moves.length).toBeGreaterThan(0);
    expect(fixture.evidence.oneStepArgument.resolverIndex).toBe(
      validationResolverIndexV1("InputSets"),
    );

    // The challenger reached the award and removed the operator's block.
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.awardResult?.fraudProofUnit).toBe(
      toUnit(
        result.contracts.fraudProof.policyId,
        result.initResult.computationThreadAssetName,
      ),
    );
    await expect(
      result.challengerLucid!.utxosAtWithUnit(
        result.contracts.fraudProof.spendingScriptAddress,
        result.awardResult!.fraudProofUnit,
      ),
    ).resolves.toHaveLength(1);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);

    // Winning the dispute is worthless if the correction cannot be executed on
    // L1. Every removal transaction must fit the literal 16,384-byte envelope
    // with zero attached validator bodies -- each script is reference-sourced.
    const removalMeasurements = result.removalMeasurements ?? [];
    expect(removalMeasurements.length).toBeGreaterThan(0);
    for (const measurement of removalMeasurements) {
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
        PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
      );
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(measurement.plutusV1ScriptCount).toBe(0);
      expect(measurement.plutusV2ScriptCount).toBe(0);
      expect(measurement.plutusV3ScriptCount).toBe(0);
      expect(measurement.nativeScriptCount).toBe(0);
      // The scripts have to come from somewhere: reference inputs carry them.
      expect(measurement.referenceInputCount).toBeGreaterThanOrEqual(7);
    }
    // And every validator the correction needs is itself publishable on L1 —
    // every one but `stateQueueMint`, whose 16,835-byte body no longer fits
    // the 16,384-byte envelope at all. That publication rides the documented
    // oversized escape and its measurement stays deliberately unasserted
    // until the validator shrinks (Anastasia-Labs/midgard#649). The roster
    // grew to eight when the correction lock joined it.
    const referenceScriptMeasurements = Object.entries(
      result.removalReferenceScriptMeasurements ?? {},
    );
    expect(referenceScriptMeasurements).toHaveLength(8);
    for (const [name, measurement] of referenceScriptMeasurements) {
      if (name === "stateQueueMint") {
        continue;
      }
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 600_000);
});
