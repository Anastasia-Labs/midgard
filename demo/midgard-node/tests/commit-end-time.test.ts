import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS,
  resolveAlignedCommitEndTime,
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

  it("is deterministic across calls for a fixed now reference", async () => {
    const lucid = await makeLucid();
    const provider = lucid.config().provider as unknown as {
      time: number;
      slot: number;
    };
    const zeroTime = provider.time - provider.slot * 1000;
    const latestEndTime = zeroTime + provider.slot * 1000;
    // Stale candidate: the wall-clock future-buffer floor decides the result,
    // which is exactly the case that looped forever on preprod.
    const candidateEndTime = latestEndTime - 5_000;
    const nowMs = latestEndTime + 1_000;

    const first = resolveAlignedCommitEndTime({
      lucid,
      latestEndTime,
      candidateEndTime,
      nowMs,
    });
    // Same now snapshot -> identical resolved end time, so the stabilization
    // loop's witnessed end time stays valid no matter how long the witness
    // fetch takes between iterations.
    const second = resolveAlignedCommitEndTime({
      lucid,
      latestEndTime,
      candidateEndTime,
      nowMs,
    });
    expect(second.resolvedEndTime).toBe(first.resolvedEndTime);

    // The floor tracks the provided now reference, not the live wall clock.
    const later = resolveAlignedCommitEndTime({
      lucid,
      latestEndTime,
      candidateEndTime,
      nowMs: nowMs + 60_000,
    });
    expect(later.resolvedEndTime).toBeGreaterThan(first.resolvedEndTime);
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
