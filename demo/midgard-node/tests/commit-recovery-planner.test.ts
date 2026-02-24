import { describe, expect, it } from "vitest";
import {
  shouldDeferCommitSubmission,
  shouldAttemptLocalFinalizationRecovery,
  rootsMatchConfirmedHeader,
} from "@/workers/utils/commit-block-planner.js";

describe("commit recovery planner", () => {
  it("attempts recovery only when pending + confirmed block available", () => {
    expect(
      shouldAttemptLocalFinalizationRecovery({
        localFinalizationPending: true,
        hasAvailableConfirmedBlock: true,
      }),
    ).toBe(true);

    expect(
      shouldAttemptLocalFinalizationRecovery({
        localFinalizationPending: true,
        hasAvailableConfirmedBlock: false,
      }),
    ).toBe(false);

    expect(
      shouldAttemptLocalFinalizationRecovery({
        localFinalizationPending: false,
        hasAvailableConfirmedBlock: true,
      }),
    ).toBe(false);
  });

  it("defers new submission while waiting for confirmation of pending local finalization", () => {
    expect(
      shouldDeferCommitSubmission({
        localFinalizationPending: true,
        hasAvailableConfirmedBlock: false,
      }),
    ).toBe(true);

    expect(
      shouldDeferCommitSubmission({
        localFinalizationPending: true,
        hasAvailableConfirmedBlock: true,
      }),
    ).toBe(false);

    expect(
      shouldDeferCommitSubmission({
        localFinalizationPending: false,
        hasAvailableConfirmedBlock: false,
      }),
    ).toBe(false);
  });

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
});
