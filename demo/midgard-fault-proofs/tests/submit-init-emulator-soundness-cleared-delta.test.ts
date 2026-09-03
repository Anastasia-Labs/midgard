import { describe, expect, it } from "vitest";

import {
  buildAcceptedClaimOverRejectingTransactionFixture,
  buildNonEmptyClaimedLedgerDeltaRoot,
  EMPTY_CLAIMED_LEDGER_DELTA_ROOT,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

// Split out of submit-init-emulator-soundness.test.ts: one emulator journey
// per file, on purpose. Each journey leaks wasm linear memory that
// `@lucid-evolution/uplc` never reclaims, and vitest isolates per FILE, so
// co-locating journeys walks the worker into the wasm32 ceiling and surfaces
// it as `EvaluatorError: unreachable`. See tests/support/uplc-heap-guard.ts.
describe("validation-dispute soundness with a non-empty claimed ledger delta", () => {
  it("rejects the cleared-delta rejection successor the deleted VM-DEFECT-2 clause required", async () => {
    const claimedLedgerDeltaRoot = await buildNonEmptyClaimedLedgerDeltaRoot();
    expect(claimedLedgerDeltaRoot.equals(EMPTY_CLAIMED_LEDGER_DELTA_ROOT)).toBe(
      false,
    );

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
});
