import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  COMMIT_MIN_PRE_SUBMIT_BUDGET_MS,
  COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
  commitTimingBudget,
  EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS,
  resolveAlignedCommitEndTime,
  resolveCommitEndTimeFit,
  resolveExplicitCommitCandidateEndTimeMs,
} from "@/workers/utils/commit-end-time.js";

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

describe("commit end-time resolver", () => {
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
    const provider = lucid.config().provider as unknown as {
      time: number;
      slot: number;
    };
    const zeroTime = provider.time - provider.slot * 1000;
    const latestEndTime = zeroTime + provider.slot * 1000;

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
    const provider = lucid.config().provider as unknown as {
      time: number;
      slot: number;
    };
    const zeroTime = provider.time - provider.slot * 1000;
    const latestEndTime = zeroTime + provider.slot * 1000;
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

  it("reports a cap-aware fit when the production resolved end-time stays inside the scheduler window", async () => {
    const lucid = await makeLucid();
    const provider = lucid.config().provider as unknown as {
      time: number;
      slot: number;
    };
    const zeroTime = provider.time - provider.slot * 1000;
    const latestEndTime = zeroTime + provider.slot * 1000;
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
    const provider = lucid.config().provider as unknown as {
      time: number;
      slot: number;
    };
    const zeroTime = provider.time - provider.slot * 1000;
    const latestEndTime = zeroTime + provider.slot * 1000;
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
    expect(fit.reason).toContain("minimum_current_time_end_time_ms=");
  });

  it("keeps the monotonic latest-block lower bound when it is later than the production current-time floor", async () => {
    const lucid = await makeLucid();
    const provider = lucid.config().provider as unknown as {
      time: number;
      slot: number;
    };
    const zeroTime = provider.time - provider.slot * 1000;
    const nowMs = zeroTime + provider.slot * 1000;
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
    const provider = lucid.config().provider as unknown as {
      time: number;
      slot: number;
    };
    const zeroTime = provider.time - provider.slot * 1000;
    const latestEndTime = zeroTime + provider.slot * 1000;
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
    const provider = lucid.config().provider as unknown as {
      time: number;
      slot: number;
    };
    const zeroTime = provider.time - provider.slot * 1000;
    const latestEndTime = zeroTime + provider.slot * 1000;
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

    expect(alignedCandidateEndTime).toBe(latestEndTime + 2_000);
    expect(minimumMonotonicEndTime).toBeGreaterThan(latestEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(alignedCandidateEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(minimumMonotonicEndTime);
  });
});
