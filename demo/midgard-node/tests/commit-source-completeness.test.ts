import { describe, expect, it } from "vitest";

import { commitUserEventSourceIdSetsAreExact } from "../src/workers/commit-block-header/submission.js";

const exactSources = {
  pendingDepositIds: ["deposit-a"],
  includedDepositIds: ["deposit-a"],
  pendingForcedTransactionIds: ["forced-a"],
  includedForcedTransactionIds: ["forced-a"],
  pendingWithdrawalIds: ["withdrawal-a"],
  includedWithdrawalIds: ["withdrawal-a"],
} as const;

describe("commit source completeness", () => {
  it("accepts the exact due source sets independent of ordering", () => {
    expect(
      commitUserEventSourceIdSetsAreExact({
        ...exactSources,
        pendingDepositIds: ["deposit-b", "deposit-a"],
        includedDepositIds: ["deposit-a", "deposit-b"],
      }),
    ).toBe(true);
  });

  it("rejects a source that becomes due through the finalized header end", () => {
    expect(
      commitUserEventSourceIdSetsAreExact({
        ...exactSources,
        pendingWithdrawalIds: ["withdrawal-a", "withdrawal-late"],
      }),
    ).toBe(false);
  });

  it("rejects replacement and duplicate source identities", () => {
    expect(
      commitUserEventSourceIdSetsAreExact({
        ...exactSources,
        pendingForcedTransactionIds: ["forced-b"],
      }),
    ).toBe(false);
    expect(
      commitUserEventSourceIdSetsAreExact({
        ...exactSources,
        pendingDepositIds: ["deposit-a", "deposit-a"],
        includedDepositIds: ["deposit-a", "deposit-a"],
      }),
    ).toBe(false);
  });
});
