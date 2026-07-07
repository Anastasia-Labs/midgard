import { describe, expect, it } from "vitest";

import {
  decideUnsubmittedPendingBlockRecovery,
  type PendingBlockConfirmation,
  pendingBlockHasSubmittedTx,
  shouldDeferUnsubmittedPendingBlockRecovery,
} from "@/workers/utils/confirm-block-commitments.js";

const pendingBlock = (
  submittedTxHash: PendingBlockConfirmation["submittedTxHash"],
): PendingBlockConfirmation => ({
  expectedHeaderHash: "aa".repeat(32),
  submittedTxHash,
  blockEndTimeMs: Date.now() + 60_000,
  updatedAtMs: Date.now(),
});

describe("confirm-block-commitments utilities", () => {
  it("classifies pending journals without submitted tx hashes as unsubmitted", () => {
    expect(pendingBlockHasSubmittedTx(pendingBlock(""))).toBe(false);
    expect(pendingBlockHasSubmittedTx(pendingBlock("bb".repeat(32)))).toBe(
      true,
    );
  });

  it("defers unsubmitted pending recovery until the block end plus grace has elapsed", () => {
    const block = pendingBlock("");
    const recoveryGraceMs = 30_000;

    expect(
      shouldDeferUnsubmittedPendingBlockRecovery({
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs,
        recoveryGraceMs,
      }),
    ).toBe(true);
    expect(
      shouldDeferUnsubmittedPendingBlockRecovery({
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs + 1,
        recoveryGraceMs,
      }),
    ).toBe(false);
  });

  it("recovers canonical unsubmitted snapshots before considering stale recovery", () => {
    const block = pendingBlock("");
    const recoveryGraceMs = 30_000;

    expect(
      decideUnsubmittedPendingBlockRecovery({
        canonicalMatchFound: true,
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs + 1,
        recoveryGraceMs,
      }),
    ).toBe("recover_canonical");
    expect(
      decideUnsubmittedPendingBlockRecovery({
        canonicalMatchFound: false,
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs,
        recoveryGraceMs,
      }),
    ).toBe("defer");
    expect(
      decideUnsubmittedPendingBlockRecovery({
        canonicalMatchFound: false,
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs + 1,
        recoveryGraceMs,
      }),
    ).toBe("recover_stale");
  });
});
