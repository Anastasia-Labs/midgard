import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  alignedUnixTimeStrictlyAfter,
  alignUnixTimeToSlotBoundary,
  COMMIT_MIN_PRE_SUBMIT_BUDGET_MS,
  COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
  COMMIT_VALIDITY_BACKDATE_MS,
  COMMIT_VALIDITY_MAX_RANGE_MS,
  commitTimingBudget,
  EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS,
  makeSubmitSlotAnchoredClock,
  resolveAlignedCommitEndTime,
  resolveCommitEndTimeFit,
  resolveCommitValidityInterval,
  resolveExplicitCommitCandidateEndTimeMs,
} from "../src/workers/utils/commit-end-time.js";

/**
 * Builds a Lucid instance for commit-end-time tests.
 */
const makeLucid = async () => {
  const operator = generateEmulatorAccount({ lovelace: 50_000_000n });
  const emulator = new Emulator([operator]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  return lucid;
};

const currentSlotStartMs = (lucid: Awaited<ReturnType<typeof makeLucid>>) =>
  lucid.slotToUnixTime(lucid.currentSlot());

describe("commit end-time resolver", () => {
  it("keeps submit-slot time monotonic across wall-clock jumps", () => {
    let monotonicNow = 10_000;
    const anchoredNow = makeSubmitSlotAnchoredClock(
      1_779_150_000_000,
      () => monotonicNow,
    );
    const wallNow = vi.spyOn(Date, "now");
    try {
      wallNow.mockReturnValue(1_999_999_999_999);
      expect(anchoredNow()).toBe(1_779_150_000_000);

      wallNow.mockReturnValue(1);
      monotonicNow += 12_345.75;
      expect(anchoredNow()).toBe(1_779_150_012_345);

      monotonicNow -= 5_000;
      expect(anchoredNow()).toBe(1_779_150_012_345);
    } finally {
      wallNow.mockRestore();
    }
  });

  it("advances refreshed commit windows from the slot-backed clock", async () => {
    const lucid = await makeLucid();
    const slotStartMs = currentSlotStartMs(lucid);
    let monotonicNow = 5_000;
    const anchoredNow = makeSubmitSlotAnchoredClock(
      slotStartMs,
      () => monotonicNow,
    );
    const first = resolveAlignedCommitEndTime({
      lucid,
      latestEndTime: slotStartMs,
      candidateEndTime: slotStartMs,
      nowMs: anchoredNow(),
      minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
    });

    vi.spyOn(Date, "now").mockReturnValue(slotStartMs + 86_400_000);
    try {
      monotonicNow += 45_000;
      const refreshed = resolveAlignedCommitEndTime({
        lucid,
        latestEndTime: slotStartMs,
        candidateEndTime: slotStartMs,
        nowMs: anchoredNow(),
        minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
      });
      expect(refreshed.minimumCurrentTimeEndTime).toBeGreaterThan(
        first.minimumCurrentTimeEndTime,
      );
      expect(refreshed.resolvedEndTime).toBeGreaterThan(first.resolvedEndTime);
    } finally {
      vi.restoreAllMocks();
    }
  });

  it("keeps Custom end-time floors forward after the emulator advances beyond the Lucid creation anchor", async () => {
    const operator = generateEmulatorAccount({ lovelace: 50_000_000n });
    const emulator = new Emulator([operator]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(operator.seedPhrase);

    emulator.awaitSlot(16 * 60);
    const firstNowMs = emulator.now();
    const firstFloorMs =
      firstNowMs + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS;
    expect(
      alignUnixTimeToSlotBoundary(lucid, firstFloorMs),
    ).toBeGreaterThanOrEqual(firstFloorMs - 999);
    expect(alignedUnixTimeStrictlyAfter(lucid, firstFloorMs)).toBeGreaterThan(
      firstFloorMs,
    );
    const first = resolveAlignedCommitEndTime({
      lucid,
      latestEndTime: firstNowMs - 1,
      candidateEndTime: firstNowMs,
      nowMs: firstNowMs,
      minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
    });
    expect(first.minimumCurrentTimeEndTime).toBeGreaterThan(firstFloorMs);
    expect(first.resolvedEndTime).toBeGreaterThanOrEqual(
      first.minimumCurrentTimeEndTime,
    );

    emulator.awaitSlot(60);
    const refreshedNowMs = emulator.now();
    const refreshed = resolveAlignedCommitEndTime({
      lucid,
      latestEndTime: firstNowMs - 1,
      candidateEndTime: firstNowMs,
      nowMs: refreshedNowMs,
      minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
    });
    expect(refreshed.minimumCurrentTimeEndTime).toBeGreaterThan(
      first.minimumCurrentTimeEndTime,
    );
    expect(refreshed.resolvedEndTime).toBeGreaterThan(first.resolvedEndTime);
  });

  it("evaluates budgets against slot-backed time when wall time is ahead or behind", () => {
    let monotonicNow = 0;
    const observedAtMs = 1_779_150_000_000;
    const anchoredNow = makeSubmitSlotAnchoredClock(
      observedAtMs,
      () => monotonicNow,
    );
    const resolvedEndTimeMs = observedAtMs + COMMIT_MIN_PRE_SUBMIT_BUDGET_MS;
    const wallNow = vi.spyOn(Date, "now");
    try {
      wallNow.mockReturnValue(observedAtMs + 86_400_000);
      expect(
        commitTimingBudget({
          checkpoint: "pre_submit",
          resolvedEndTimeMs,
          nowMs: anchoredNow(),
        }).satisfied,
      ).toBe(true);

      wallNow.mockReturnValue(observedAtMs - 86_400_000);
      monotonicNow += 1;
      expect(
        commitTimingBudget({
          checkpoint: "pre_submit",
          resolvedEndTimeMs,
          nowMs: anchoredNow(),
        }).satisfied,
      ).toBe(false);
    } finally {
      wallNow.mockRestore();
    }
  });

  it("defaults explicit commit candidate end-time five minutes into the future", () => {
    const nowMs = 1_779_150_000_000;

    expect(resolveExplicitCommitCandidateEndTimeMs(undefined, nowMs)).toBe(
      nowMs + EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS,
    );
    expect(resolveExplicitCommitCandidateEndTimeMs(nowMs + 42_000, nowMs)).toBe(
      nowMs + 42_000,
    );
  });

  it("forces end-time to advance when candidate is stale", async () => {
    const lucid = await makeLucid();
    const latestEndTime = currentSlotStartMs(lucid);

    const {
      alignedCandidateEndTime,
      minimumMonotonicEndTime,
      resolvedEndTime,
    } = resolveAlignedCommitEndTime({
      lucid,
      latestEndTime,
      candidateEndTime: latestEndTime - 5_000,
    });

    expect(alignedCandidateEndTime).toBeLessThanOrEqual(latestEndTime);
    expect(minimumMonotonicEndTime).toBeGreaterThan(latestEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(minimumMonotonicEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(alignedCandidateEndTime);
  });

  it("can raise stale candidates to the production live-chain safety floor", async () => {
    const lucid = await makeLucid();
    const latestEndTime = currentSlotStartMs(lucid);
    const nowMs = latestEndTime + 10_000;

    const { resolvedEndTime } = resolveAlignedCommitEndTime({
      lucid,
      latestEndTime,
      candidateEndTime: latestEndTime,
      nowMs,
      minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
    });

    expect(resolvedEndTime).toBeGreaterThanOrEqual(
      nowMs + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
    );
  });

  it("builds a backdated bounded validity interval and exposes its inclusive upper bound", async () => {
    const lucid = await makeLucid();
    const currentSlot = lucid.currentSlot();
    const slotStartMs = lucid.slotToUnixTime(currentSlot);
    const validToMs = alignedUnixTimeStrictlyAfter(
      lucid,
      slotStartMs + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
    );

    const interval = resolveCommitValidityInterval({
      lucid,
      submitSlotSnapshot: {
        source: "test",
        currentSlot,
        observedAtMs: slotStartMs,
        slotLengthMs: 1_000,
      },
      validToMs,
    });

    expect(interval.validFromMs).toBe(
      slotStartMs - COMMIT_VALIDITY_BACKDATE_MS,
    );
    expect(interval.validToMs - interval.validFromMs).toBeLessThanOrEqual(
      COMMIT_VALIDITY_MAX_RANGE_MS,
    );
    expect(interval.inclusiveUpperBoundMs).toBe(validToMs - 1);
  });

  it("moves an old lower bound forward when a later validTo would exceed the range limit", async () => {
    const lucid = await makeLucid();
    const currentSlot = lucid.currentSlot();
    const slotStartMs = lucid.slotToUnixTime(currentSlot);
    const validToMs =
      slotStartMs + COMMIT_VALIDITY_MAX_RANGE_MS + COMMIT_VALIDITY_BACKDATE_MS;

    const interval = resolveCommitValidityInterval({
      lucid,
      submitSlotSnapshot: {
        source: "test",
        currentSlot,
        observedAtMs: slotStartMs,
        slotLengthMs: 1_000,
      },
      validToMs,
    });

    expect(interval.validFromMs).toBeGreaterThan(
      slotStartMs - COMMIT_VALIDITY_BACKDATE_MS,
    );
    expect(interval.validToMs - interval.validFromMs).toBeLessThanOrEqual(
      COMMIT_VALIDITY_MAX_RANGE_MS,
    );
  });

  it("reports a cap-aware fit when the production resolved end-time stays inside the scheduler window", async () => {
    const lucid = await makeLucid();
    const latestEndTime = currentSlotStartMs(lucid);
    const nowMs = latestEndTime + 10_000;
    const maximumEndTimeMs =
      nowMs + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS + 5_000;

    const fit = resolveCommitEndTimeFit({
      lucid,
      latestEndTime,
      candidateEndTime: latestEndTime,
      nowMs,
      minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
      maximumEndTimeMs,
    });

    expect(fit.status).toBe("fits");
    expect(fit.maximumEndTimeMs).toBe(maximumEndTimeMs);
    expect(fit.resolvedEndTime).toBeLessThanOrEqual(maximumEndTimeMs);
    expect(fit.minimumCurrentTimeEndTime).toBeGreaterThanOrEqual(
      nowMs + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
    );
  });

  it("reports cap overflow when the production current-time floor crosses the scheduler window", async () => {
    const lucid = await makeLucid();
    const latestEndTime = currentSlotStartMs(lucid);
    const nowMs = latestEndTime + 10_000;
    const maximumEndTimeMs =
      nowMs + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS - 60_000;

    const fit = resolveCommitEndTimeFit({
      lucid,
      latestEndTime,
      candidateEndTime: latestEndTime,
      nowMs,
      minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
      maximumEndTimeMs,
    });

    expect(fit.status).toBe("exceeds_cap");
    if (fit.status !== "exceeds_cap") {
      throw new Error("expected production current-time floor to exceed cap");
    }
    expect(fit.resolvedEndTime).toBeGreaterThan(maximumEndTimeMs);
    expect(fit.reason).toContain("minimum_current_time_valid_to_ms=");
  });

  it("keeps the monotonic latest-block lower bound when it is later than the production current-time floor", async () => {
    const lucid = await makeLucid();
    const nowMs = currentSlotStartMs(lucid);
    const latestEndTime =
      nowMs + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS + 30_000;

    const fit = resolveCommitEndTimeFit({
      lucid,
      latestEndTime,
      candidateEndTime: nowMs,
      nowMs,
      minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
      maximumEndTimeMs: latestEndTime + 5_000,
    });

    expect(fit.status).toBe("fits");
    expect(fit.resolvedEndTime).toBe(fit.minimumMonotonicEndTime);
    expect(fit.minimumMonotonicEndTime).toBeGreaterThan(
      fit.minimumCurrentTimeEndTime,
    );
  });

  it("reports slot-aligned current-time floors that exceed a cap by one slot", async () => {
    const lucid = await makeLucid();
    const latestEndTime = currentSlotStartMs(lucid);
    const nowMs = latestEndTime + 10_000;
    const maximumEndTimeMs = nowMs + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS;

    const fit = resolveCommitEndTimeFit({
      lucid,
      latestEndTime,
      candidateEndTime: latestEndTime,
      nowMs,
      minimumFutureBufferMs: COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
      maximumEndTimeMs,
    });

    expect(fit.status).toBe("exceeds_cap");
    expect(fit.minimumCurrentTimeEndTime).toBe(maximumEndTimeMs + 1_000);
    expect(fit.resolvedEndTime).toBe(fit.minimumCurrentTimeEndTime);
  });

  it("reports named timing checkpoint budgets", () => {
    const nowMs = 1_779_150_000_000;
    const satisfied = commitTimingBudget({
      checkpoint: "pre_submit",
      resolvedEndTimeMs: nowMs + COMMIT_MIN_PRE_SUBMIT_BUDGET_MS,
      nowMs,
    });
    const tooLate = commitTimingBudget({
      checkpoint: "pre_submit",
      resolvedEndTimeMs: nowMs + COMMIT_MIN_PRE_SUBMIT_BUDGET_MS - 1,
      nowMs,
    });

    expect(satisfied.satisfied).toBe(true);
    expect(tooLate.satisfied).toBe(false);
    expect(tooLate.remainingBudgetMs).toBe(COMMIT_MIN_PRE_SUBMIT_BUDGET_MS - 1);
  });

  it("keeps a forward candidate end-time when already monotonic", async () => {
    const lucid = await makeLucid();
    const latestEndTime = currentSlotStartMs(lucid);
    const candidateEndTime = latestEndTime + 2_500;

    const {
      alignedCandidateEndTime,
      minimumMonotonicEndTime,
      resolvedEndTime,
    } = resolveAlignedCommitEndTime({
      lucid,
      latestEndTime,
      candidateEndTime,
    });

    expect(alignedCandidateEndTime).toBe(latestEndTime + 3_000);
    expect(minimumMonotonicEndTime).toBeGreaterThan(latestEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(alignedCandidateEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(minimumMonotonicEndTime);
  });
});
