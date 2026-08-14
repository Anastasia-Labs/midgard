import { PROTOCOL_PARAMETERS_DEFAULT, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { validationResolverIndexV1 } from "../src/index.js";
import {
  buildAcceptedClaimOverRejectingTransactionFixture,
  buildHonestAcceptedValidationDisputeFixture,
  buildNonEmptyClaimedLedgerDeltaRootV1,
  EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

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
    // binding forces from `operator_validity: TxIsValid`.
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
    // And every validator the correction needs is itself publishable on L1.
    const referenceScriptMeasurements = Object.values(
      result.removalReferenceScriptMeasurements ?? {},
    );
    expect(referenceScriptMeasurements).toHaveLength(7);
    for (const measurement of referenceScriptMeasurements) {
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 600_000);

  it("rejects the cleared-delta rejection successor the deleted VM-DEFECT-2 clause required", async () => {
    const claimedLedgerDeltaRoot =
      await buildNonEmptyClaimedLedgerDeltaRootV1();
    expect(
      claimedLedgerDeltaRoot.equals(EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1),
    ).toBe(false);

    let removalReferenceScriptPublicationAttempts = 0;
    await expect(
      runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildAcceptedClaimOverRejectingTransactionFixture({
            operatorVkey,
            now,
            claimedLedgerDeltaRoot,
            clearChallengerTerminalDelta: true,
          }),
        {
          stopAfter: "semantic-resolution",
          onRemovalReferenceScriptPublicationAttempt: () => {
            removalReferenceScriptPublicationAttempts += 1;
          },
        },
      ),
    ).rejects.toThrow(/emulator lifecycle stage prepare-selected failed/u);
    expect(removalReferenceScriptPublicationAttempts).toBe(0);
  }, 600_000);

  // RED, and it is the direction that matters. This is the symmetric-soundness
  // case: a FORGED dispute against an HONEST operator must be refused at
  // `prepare-selected` or `semantic-resolution`, and as of #579's close it is
  // ACCEPTED instead ("promise resolved instead of rejecting"). The sibling
  // positive case above — a challenger legitimately winning — passes, so this is
  // not a broken harness; it is specifically the refusal direction that is not
  // refusing.
  //
  // Owner-ordered tracking: **#605** (blocked_by #604, blocking #581). It is
  // deliberately NOT filed under #604's offchain-builder staleness and NOT
  // written off as accepted baseline: it is either a downstream effect of that
  // staleness producing a malformed forgery that happens to be accepted, or a
  // genuine soundness defect, and #579 did not bisect which. Do not re-pin,
  // skip, or weaken this assertion to get a green suite — the whole value of the
  // row is that it fails while the question is open.
  it("cannot be defeated when the operator honestly accepted a valid transaction carrying a non-empty ledger delta", async () => {
    // Same disputed instruction, same rejection code, same non-empty claimed
    // delta as the challenger-wins case; only the transaction's validity
    // differs. Soundness must be symmetric (GOAL_SPEC §3 invariant 9).
    let observed:
      | Awaited<ReturnType<typeof buildHonestAcceptedValidationDisputeFixture>>
      | undefined;
    await expect(
      runForcedValidationDisputeScenario(async ({ operatorVkey, now }) => {
        observed = await buildHonestAcceptedValidationDisputeFixture({
          operatorVkey,
          now,
        });
        return observed;
      }),
    ).rejects.toThrow(
      /emulator lifecycle stage (prepare-selected|semantic-resolution) failed/u,
    );

    const fixture = observed;
    if (fixture === undefined) {
      throw new Error("honest-operator mirror fixture was never constructed");
    }
    expect(fixture.disputedPhase).toBe("inputSets");
    expect(fixture.operatorTrace.tree.descriptor.verdict).toBe("accepted");
    expect(fixture.challengerTrace.tree.descriptor.verdict).toBe("rejected");
    expect(fixture.claimedLedgerDeltaRoot).toHaveLength(32);
    expect(
      fixture.claimedLedgerDeltaRoot.equals(EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1),
    ).toBe(false);
    const { lowIndex, highIndex } = fixture.evidence.finalDispute;
    expect(highIndex).toBe(lowIndex + 1);
    expect(fixture.challengerTrace.states[lowIndex]!.phase).toBe("inputSets");
    const forgedSuccessor = fixture.challengerTrace.states[highIndex]!;
    expect(forgedSuccessor.phase).toBe("terminal");
    expect(forgedSuccessor.verdict).toBe("rejected");
    // The forgery is maximally strong: the immutable claimed delta is carried
    // through unchanged, so it is not rejected for a context mismatch.
    expect(
      forgedSuccessor.ledgerDeltaRoot.equals(fixture.claimedLedgerDeltaRoot),
    ).toBe(true);
  }, 900_000);
});
