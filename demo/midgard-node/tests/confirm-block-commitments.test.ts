import { describe, expect, it } from "vitest";

import {
  pendingBlockHasSubmittedTx,
  type PendingBlockConfirmation,
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
});
