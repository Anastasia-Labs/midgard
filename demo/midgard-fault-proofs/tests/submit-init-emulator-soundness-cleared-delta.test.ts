import { describe, expect, it } from "vitest";

import {
  buildAcceptedClaimOverRejectingTransactionFixture,
  buildNonEmptyClaimedLedgerDeltaRoot,
  EMPTY_CLAIMED_LEDGER_DELTA_ROOT,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

// Split out of submit-init-emulator-soundness.test.ts: one emulator journey per
// file, on purpose. The split was made while `@lucid-evolution/uplc` (through
// 0.2.22) leaked wasm linear memory on every script evaluation and vitest
// isolates per FILE; that leak is fixed upstream, and the split is kept so each
// file runs in its own fresh process.
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
