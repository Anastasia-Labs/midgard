import { describe, expect, it } from "vitest";

import {
  buildHonestAcceptedValidationDisputeFixture,
  EMPTY_CLAIMED_LEDGER_DELTA_ROOT,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

// Split out of submit-init-emulator-soundness.test.ts: one emulator journey per
// file, on purpose. The split was made while `@lucid-evolution/uplc` (through
// 0.2.22) leaked wasm linear memory on every script evaluation and vitest
// isolates per FILE; that leak is fixed upstream, and the split is kept so each
// file runs in its own fresh process.
describe("validation-dispute soundness with a non-empty claimed ledger delta", () => {
  // RED, and it is the direction that matters. This is the symmetric-soundness
  // case: a FORGED dispute against an HONEST operator must be refused at
  // `prepare-selected` or `semantic-resolution`, and as of #579's close it is
  // ACCEPTED instead ("promise resolved instead of rejecting"). The sibling
  // positive case in submit-init-emulator-soundness.test.ts — a challenger
  // legitimately winning — passes, so this is not a broken harness; it is
  // specifically the refusal direction that is not refusing.
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
      fixture.claimedLedgerDeltaRoot.equals(EMPTY_CLAIMED_LEDGER_DELTA_ROOT),
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
