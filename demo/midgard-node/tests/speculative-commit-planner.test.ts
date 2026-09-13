import { describe, expect, it } from "vitest";

import { shouldSkipScheduledLegacyCommitForSpeculation } from "../src/fibers/block-commitment.js";
import {
  barrierWatermarksAreFresh,
  decideSpeculativeInvalidation,
  idleSpeculativeCommitState,
  minimumBarrierWatermarkMs,
  reduceSpeculativeCommitState,
  sameSpeculativeSourceIdSet,
  shouldRetrySpeculativeConfirmationWake,
  speculationOverlapEfficiency,
  type SpeculativeCandidateSummary,
  type SpeculativeCommitEvent,
  type SpeculativeCommitState,
  type SpeculativeInvalidationReason,
} from "../src/fibers/speculative-commit-state.js";
import { mergeBarrierWatermarks } from "../src/fibers/user-event-barrier-refresher.js";

const candidate = (
  baseHeaderHash = "aa".repeat(28),
): SpeculativeCandidateSummary => ({
  candidateId: "candidate-1",
  baseHeaderHash,
  endTimeMs: 2_000,
  builtAtMs: 1_500,
  buildDurationMs: 500,
  invalidationKey: `${baseHeaderHash}:2000:1000`,
  watermarks: {
    depositMs: 1_000,
    withdrawalMs: 1_100,
    txOrderMs: 1_200,
    refreshedAtMs: 1_250,
  },
  expectedUserEventCounts: {
    deposits: 1,
    forcedTransactions: 2,
    withdrawals: 3,
  },
  expectedL2TransactionCount: 1,
  roots: {
    utxos: "01".repeat(32),
    rawTransactions: "08".repeat(32),
    transactions: "02".repeat(32),
    deposits: "03".repeat(32),
    forcedTransactions: "04".repeat(32),
    withdrawals: "05".repeat(32),
    transitionTrace: "06".repeat(32),
    eventToStep: "07".repeat(32),
  },
});

const building = (): Extract<
  SpeculativeCommitState,
  { readonly _tag: "Building" }
> => ({
  _tag: "Building",
  baseHeaderHash: "aa".repeat(28),
  rebuildAttempts: 0,
  startedAtMs: 1_000,
});

describe("speculative commit state reducer", () => {
  it("keeps flag-off behavior exact and degrades back to the legacy path", () => {
    const activeStates: readonly SpeculativeCommitState[] = [
      building(),
      reduceSpeculativeCommitState(
        building(),
        { _tag: "CandidateReady", candidate: candidate() },
        3,
      ),
    ];
    for (const state of activeStates) {
      expect(
        shouldSkipScheduledLegacyCommitForSpeculation({
          enabled: false,
          state,
          recoveryMustRun: false,
        }),
      ).toBe(false);
      expect(
        shouldSkipScheduledLegacyCommitForSpeculation({
          enabled: true,
          state,
          recoveryMustRun: false,
        }),
      ).toBe(true);
      expect(
        shouldSkipScheduledLegacyCommitForSpeculation({
          enabled: true,
          state,
          recoveryMustRun: true,
        }),
      ).toBe(false);
    }
    expect(
      shouldSkipScheduledLegacyCommitForSpeculation({
        enabled: true,
        state: {
          _tag: "Degraded",
          baseHeaderHash: candidate().baseHeaderHash,
          rebuildAttempts: 3,
          reason: "T7",
          degradedAtMs: 3_000,
        },
        recoveryMustRun: false,
      }),
    ).toBe(false);
  });

  it("runs the normative submit-on-confirm happy path", () => {
    const baseHeaderHash = "aa".repeat(28);
    const first = reduceSpeculativeCommitState(
      idleSpeculativeCommitState(),
      { _tag: "SubmittedBase", baseHeaderHash, atMs: 1_000 },
      3,
    );
    const ready = reduceSpeculativeCommitState(
      first,
      { _tag: "CandidateReady", candidate: candidate() },
      3,
    );
    const submitting = reduceSpeculativeCommitState(
      ready,
      {
        _tag: "ConfirmationObserved",
        confirmedHeaderHash: baseHeaderHash,
        atMs: 3_000,
      },
      3,
    );
    expect(submitting).toMatchObject({
      _tag: "Submitting",
      baseHeaderHash,
      confirmationObservedAtMs: 3_000,
    });
    expect(
      reduceSpeculativeCommitState(
        submitting,
        {
          _tag: "SubmitSucceeded",
          submittedHeaderHash: "bb".repeat(28),
          atMs: 4_000,
        },
        3,
      ),
    ).toMatchObject({
      _tag: "Building",
      baseHeaderHash: "bb".repeat(28),
      rebuildAttempts: 0,
    });
  });

  it("handles double invalidation, wake-without-candidate and stale ready results", () => {
    const firstInvalidation = reduceSpeculativeCommitState(
      building(),
      { _tag: "Invalidate", reason: "T1", atMs: 2_000 },
      3,
    );
    expect(
      reduceSpeculativeCommitState(
        firstInvalidation,
        { _tag: "Invalidate", reason: "T3", atMs: 2_100 },
        3,
      ),
    ).toEqual(firstInvalidation);
    expect(
      reduceSpeculativeCommitState(
        idleSpeculativeCommitState(),
        {
          _tag: "ConfirmationObserved",
          confirmedHeaderHash: "aa",
          atMs: 2_000,
        },
        3,
      ),
    ).toEqual({ _tag: "Idle" });
    expect(
      reduceSpeculativeCommitState(
        building(),
        { _tag: "CandidateReady", candidate: candidate("bb".repeat(28)) },
        3,
      ),
    ).toMatchObject({ _tag: "Invalidated", reason: "T2" });
  });

  it("allows the configured rebuild count and degrades only after the budget", () => {
    const first = reduceSpeculativeCommitState(
      building(),
      { _tag: "Invalidate", reason: "T3", atMs: 2_000 },
      2,
    );
    const firstRebuild = reduceSpeculativeCommitState(
      first,
      { _tag: "RebuildStarted", atMs: 2_100 },
      2,
    );
    const second = reduceSpeculativeCommitState(
      firstRebuild,
      { _tag: "Invalidate", reason: "T4", atMs: 2_200 },
      2,
    );
    expect(second).toMatchObject({
      _tag: "Invalidated",
      rebuildAttempts: 2,
    });
    const secondRebuild = reduceSpeculativeCommitState(
      second,
      { _tag: "RebuildStarted", atMs: 2_300 },
      2,
    );
    expect(
      reduceSpeculativeCommitState(
        secondRebuild,
        { _tag: "Invalidate", reason: "T5", atMs: 2_400 },
        2,
      ),
    ).toMatchObject({
      _tag: "Degraded",
      reason: "T5",
      rebuildAttempts: 3,
    });
  });

  it("returns a contended submitter to ready without rebuilding", () => {
    const ready = reduceSpeculativeCommitState(
      building(),
      { _tag: "CandidateReady", candidate: candidate() },
      3,
    );
    const submitting = reduceSpeculativeCommitState(
      ready,
      {
        _tag: "ConfirmationObserved",
        confirmedHeaderHash: candidate().baseHeaderHash,
        atMs: 2_000,
      },
      3,
    );
    expect(
      reduceSpeculativeCommitState(
        submitting,
        { _tag: "SubmissionDeferred" },
        3,
      ),
    ).toEqual(ready);
  });

  it("defines every state/event pair without throwing", () => {
    const states: readonly SpeculativeCommitState[] = [
      { _tag: "Idle" },
      building(),
      { ...building(), _tag: "ReadyToSubmit", candidate: candidate() },
      {
        ...building(),
        _tag: "Submitting",
        candidate: candidate(),
        confirmationObservedAtMs: 2_000,
      },
      {
        ...building(),
        _tag: "Invalidated",
        reason: "T1",
        invalidatedAtMs: 2_000,
      },
      {
        ...building(),
        _tag: "Degraded",
        reason: "T6",
        degradedAtMs: 2_000,
      },
    ];
    const events: readonly SpeculativeCommitEvent[] = [
      { _tag: "SubmittedBase", baseHeaderHash: "aa", atMs: 1 },
      { _tag: "CandidateReady", candidate: candidate() },
      { _tag: "ConfirmationObserved", confirmedHeaderHash: "aa", atMs: 2 },
      { _tag: "Invalidate", reason: "T1", atMs: 3 },
      { _tag: "RebuildStarted", atMs: 4 },
      { _tag: "SubmitSucceeded", submittedHeaderHash: "bb", atMs: 5 },
      { _tag: "SubmissionDeferred" },
      { _tag: "Clear" },
    ];
    for (const state of states) {
      for (const event of events) {
        expect(() =>
          reduceSpeculativeCommitState(state, event, 3),
        ).not.toThrow();
      }
    }
  });
});

describe("speculative invalidation decisions", () => {
  it.each([
    ["T1", { pendingBaseAbandoned: true }],
    ["T2", { confirmedHeaderHash: "a", candidateBaseHeaderHash: "b" }],
    ["T3", { userEventCountsMatch: false }],
    ["T4", { schedulerWindowFits: false }],
    ["T5", { resetInProgress: true }],
    ["T6", { confirmationExpired: true }],
    ["T7", { processRestarted: true }],
  ] satisfies ReadonlyArray<readonly [SpeculativeInvalidationReason, object]>)(
    "maps %s evidence",
    (reason, evidence) => {
      expect(decideSpeculativeInvalidation(evidence)).toBe(reason);
    },
  );

  it("accepts a matching fresh candidate", () => {
    expect(
      decideSpeculativeInvalidation({
        confirmedHeaderHash: "a",
        candidateBaseHeaderHash: "a",
        userEventCountsMatch: true,
        schedulerWindowFits: true,
      }),
    ).toBeUndefined();
  });
});

describe("speculative source and wake guards", () => {
  it("rejects same-count source replacement and duplicate expected IDs", () => {
    expect(sameSpeculativeSourceIdSet(["a", "b"], ["b", "a"])).toBe(true);
    expect(sameSpeculativeSourceIdSet(["a", "c"], ["a", "b"])).toBe(false);
    expect(sameSpeculativeSourceIdSet(["a", "a"], ["a", "a"])).toBe(false);
  });

  it("retries confirmation wakes while the matching base is rebuilding", () => {
    expect(
      shouldRetrySpeculativeConfirmationWake({
        state: building(),
        confirmedHeaderHash: building().baseHeaderHash,
      }),
    ).toBe(true);
    expect(
      shouldRetrySpeculativeConfirmationWake({
        state: {
          _tag: "Invalidated",
          baseHeaderHash: building().baseHeaderHash,
          rebuildAttempts: 1,
          reason: "T3",
          invalidatedAtMs: 2_000,
        },
        confirmedHeaderHash: building().baseHeaderHash,
      }),
    ).toBe(true);
    expect(
      shouldRetrySpeculativeConfirmationWake({
        state: building(),
        confirmedHeaderHash: "bb".repeat(28),
      }),
    ).toBe(false);
    expect(
      shouldRetrySpeculativeConfirmationWake({
        state: reduceSpeculativeCommitState(
          building(),
          { _tag: "CandidateReady", candidate: candidate() },
          3,
        ),
        confirmedHeaderHash: building().baseHeaderHash,
      }),
    ).toBe(false);
  });
});

describe("barrier watermark freshness", () => {
  it("uses the minimum watermark and rejects missing/stale data", () => {
    const watermarks = candidate().watermarks;
    expect(minimumBarrierWatermarkMs(watermarks)).toBe(1_000);
    expect(
      barrierWatermarksAreFresh({
        watermarks,
        nowMs: 2_000,
        maxStalenessMs: 1_000,
      }),
    ).toBe(true);
    expect(
      barrierWatermarksAreFresh({
        watermarks,
        nowMs: 2_001,
        maxStalenessMs: 1_000,
      }),
    ).toBe(false);
    expect(
      barrierWatermarksAreFresh({
        watermarks: { ...watermarks, depositMs: 0 },
        nowMs: 1,
        maxStalenessMs: 1_000,
      }),
    ).toBe(false);
  });

  it("publishes every source watermark monotonically", () => {
    expect(
      mergeBarrierWatermarks(
        {
          depositMs: 100,
          withdrawalMs: 200,
          txOrderMs: 300,
          refreshedAtMs: 400,
        },
        {
          depositMs: 150,
          withdrawalMs: 150,
          txOrderMs: 350,
          refreshedAtMs: 450,
        },
      ),
    ).toEqual({
      depositMs: 150,
      withdrawalMs: 200,
      txOrderMs: 350,
      refreshedAtMs: 450,
    });
  });
});

describe("speculation overlap efficiency", () => {
  it("measures the fraction of build work hidden by confirmation wait", () => {
    expect(
      speculationOverlapEfficiency({
        buildDurationMs: 3_000,
        confirmationWaitMs: 20_000,
      }),
    ).toBe(1);
    expect(
      speculationOverlapEfficiency({
        buildDurationMs: 20_000,
        confirmationWaitMs: 3_000,
      }),
    ).toBe(0.15);
    expect(
      speculationOverlapEfficiency({
        buildDurationMs: 0,
        confirmationWaitMs: 0,
      }),
    ).toBe(1);
  });
});
