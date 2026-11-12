import { describe, expect, it } from "vitest";

import {
  rootsMatchConfirmedHeader,
  shouldSkipIdleCommitBehindUnmergedTail,
} from "../src/workers/utils/commit-block-planner.js";

describe("commit recovery planner", () => {
  it("checks computed roots against confirmed header roots", () => {
    expect(
      rootsMatchConfirmedHeader({
        computedUtxoRoot: "aabb",
        computedTxRoot: "ccdd",
        confirmedUtxoRoot: "aabb",
        confirmedTxRoot: "ccdd",
      }),
    ).toBe(true);

    expect(
      rootsMatchConfirmedHeader({
        computedUtxoRoot: "aabb",
        computedTxRoot: "ccdd",
        confirmedUtxoRoot: "eeee",
        confirmedTxRoot: "ccdd",
      }),
    ).toBe(false);
  });

  it("skips idle commit attempts behind an unmerged tail only when no work is pending", () => {
    expect(
      shouldSkipIdleCommitBehindUnmergedTail({
        stateQueueHasUnmergedTail: true,
        mempoolTxCount: 0,
        processedTxCount: 0,
        pendingUserEventCount: 0,
      }),
    ).toBe(true);

    expect(
      shouldSkipIdleCommitBehindUnmergedTail({
        stateQueueHasUnmergedTail: true,
        mempoolTxCount: 1,
        processedTxCount: 0,
        pendingUserEventCount: 0,
      }),
    ).toBe(false);

    expect(
      shouldSkipIdleCommitBehindUnmergedTail({
        stateQueueHasUnmergedTail: false,
        mempoolTxCount: 0,
        processedTxCount: 0,
        pendingUserEventCount: 0,
      }),
    ).toBe(false);
  });
});
