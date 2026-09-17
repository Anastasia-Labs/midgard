import { LucidEvolution } from "@lucid-evolution/lucid";

import type { SubmitSlotSnapshot } from "../../local-ledger-slot.js";

export const EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS = 5 * 60 * 1000;

// Commit construction may include a scheduler refresh and reference-script
// lookups under provider latency; keep enough validity for submission after
// the witness context is assembled without exceeding on-chain range limits.
export const COMMIT_DEFAULT_MINIMUM_FUTURE_BUFFER_MS = 240_000;
export const COMMIT_VALIDITY_MAX_RANGE_MS = 8 * 60 * 1_000;
export const COMMIT_VALIDITY_BACKDATE_MS = 60 * 1_000;
const COMMIT_SLOT_ALIGNMENT_MARGIN_MS = 1_000;
export const COMMIT_MINIMUM_FUTURE_BUFFER_MS =
  COMMIT_VALIDITY_MAX_RANGE_MS -
  COMMIT_VALIDITY_BACKDATE_MS -
  COMMIT_SLOT_ALIGNMENT_MARGIN_MS;
export const COMMIT_MIN_PRE_WITNESS_BUDGET_MS = 6 * 60 * 1_000;
export const COMMIT_MIN_PRE_BUILD_BUDGET_MS = 3 * 60 * 1_000;
export const COMMIT_MIN_PRE_SUBMIT_BUDGET_MS = 2 * 60 * 1_000;

export type CommitTimingCheckpoint = "pre_witness" | "pre_build" | "pre_submit";

export type CommitTimingBudget = {
  readonly checkpoint: CommitTimingCheckpoint;
  readonly resolvedEndTimeMs: number;
  readonly nowMs: number;
  readonly remainingBudgetMs: number;
  readonly minimumBudgetMs: number;
  readonly satisfied: boolean;
};

export type CommitValidityInterval = {
  readonly validFromMs: number;
  readonly validToMs: number;
  readonly inclusiveUpperBoundMs: number;
};

/**
 * Anchors a build attempt to one provider-observed submit-slot time while
 * allowing time to advance monotonically during witness assembly and submit.
 * Wall-clock changes after the anchor cannot move the build backward or jump
 * it into a different clock domain.
 */
export const makeSubmitSlotAnchoredClock = (
  observedAtMs: number,
  monotonicNow: () => number = () => performance.now(),
): (() => number) => {
  const startedAt = monotonicNow();
  let lastNowMs = observedAtMs;
  return () => {
    const elapsedMs = Math.max(0, Math.floor(monotonicNow() - startedAt));
    lastNowMs = Math.max(lastNowMs, observedAtMs + elapsedMs);
    return lastNowMs;
  };
};

type CommitEndTimeResolution = {
  readonly alignedCandidateEndTime: number;
  readonly minimumMonotonicEndTime: number;
  readonly minimumCurrentTimeEndTime: number;
  readonly resolvedEndTime: number;
};

export type CommitEndTimeFit =
  | (CommitEndTimeResolution & {
      readonly status: "fits";
      readonly maximumEndTimeMs?: number;
    })
  | (CommitEndTimeResolution & {
      readonly status: "exceeds_cap";
      readonly maximumEndTimeMs: number;
      readonly reason: string;
    });

export const alignUnixTimeToSlotBoundary = (
  lucid: LucidEvolution,
  unixTime: number,
): number => {
  if (!Number.isSafeInteger(unixTime)) {
    throw new Error(`Cannot align invalid unix time ${String(unixTime)}`);
  }
  const slot = Number(lucid.unixTimeToSlot(unixTime));
  const aligned = lucid.slotToUnixTime(slot);
  if (!Number.isSafeInteger(slot) || !Number.isSafeInteger(aligned)) {
    throw new Error(
      `Lucid returned an invalid slot boundary for unix time ${unixTime.toString()}`,
    );
  }
  return aligned;
};

export const alignedUnixTimeStrictlyAfter = (
  lucid: LucidEvolution,
  unixTimeExclusive: number,
): number => {
  const aligned = alignUnixTimeToSlotBoundary(lucid, unixTimeExclusive);
  if (aligned > unixTimeExclusive) {
    return aligned;
  }
  const slot = Number(lucid.unixTimeToSlot(unixTimeExclusive));
  const strictlyAfter = lucid.slotToUnixTime(slot + 1);
  if (!Number.isSafeInteger(strictlyAfter)) {
    throw new Error(
      `Lucid returned an invalid slot boundary after unix time ${unixTimeExclusive.toString()}`,
    );
  }
  return strictlyAfter;
};

export const resolveCommitValidityInterval = ({
  lucid,
  submitSlotSnapshot,
  validToMs,
}: {
  readonly lucid: LucidEvolution;
  readonly submitSlotSnapshot: SubmitSlotSnapshot;
  readonly validToMs: number;
}): CommitValidityInterval => {
  if (!Number.isSafeInteger(validToMs)) {
    throw new Error(`Commit validTo is invalid: ${String(validToMs)}`);
  }
  const currentSlotStartMs = lucid.slotToUnixTime(
    submitSlotSnapshot.currentSlot,
  );
  if (!Number.isSafeInteger(currentSlotStartMs)) {
    throw new Error(
      `Commit submit-slot start is invalid: ${String(currentSlotStartMs)}`,
    );
  }
  const backdatedCurrentSlotStartMs = Math.max(
    0,
    currentSlotStartMs - COMMIT_VALIDITY_BACKDATE_MS,
  );
  const minimumRangeBoundedValidFromMs =
    validToMs - COMMIT_VALIDITY_MAX_RANGE_MS;
  let validFromMs = alignUnixTimeToSlotBoundary(
    lucid,
    Math.max(backdatedCurrentSlotStartMs, minimumRangeBoundedValidFromMs),
  );
  if (validToMs - validFromMs > COMMIT_VALIDITY_MAX_RANGE_MS) {
    validFromMs = alignedUnixTimeStrictlyAfter(
      lucid,
      minimumRangeBoundedValidFromMs,
    );
  }
  const inclusiveUpperBoundMs = validToMs - 1;
  if (
    !Number.isSafeInteger(validFromMs) ||
    validFromMs >= validToMs ||
    inclusiveUpperBoundMs - validFromMs > COMMIT_VALIDITY_MAX_RANGE_MS
  ) {
    throw new Error(
      `Commit validity interval is invalid: valid_from_ms=${validFromMs.toString()},valid_to_ms=${validToMs.toString()},inclusive_upper_bound_ms=${inclusiveUpperBoundMs.toString()}`,
    );
  }
  return {
    validFromMs,
    validToMs,
    inclusiveUpperBoundMs,
  };
};

export const resolveAlignedCommitEndTime = ({
  lucid,
  latestEndTime,
  candidateEndTime,
  nowMs = Date.now(),
  minimumFutureBufferMs = COMMIT_DEFAULT_MINIMUM_FUTURE_BUFFER_MS,
}: {
  readonly lucid: LucidEvolution;
  readonly latestEndTime: number;
  readonly candidateEndTime: number;
  readonly nowMs?: number;
  readonly minimumFutureBufferMs?: number;
}): {
  readonly alignedCandidateEndTime: number;
  readonly minimumMonotonicEndTime: number;
  readonly minimumCurrentTimeEndTime: number;
  readonly resolvedEndTime: number;
} => {
  const resolution = resolveCommitEndTimeFit({
    lucid,
    latestEndTime,
    candidateEndTime,
    nowMs,
    minimumFutureBufferMs,
  });
  return {
    alignedCandidateEndTime: resolution.alignedCandidateEndTime,
    minimumMonotonicEndTime: resolution.minimumMonotonicEndTime,
    minimumCurrentTimeEndTime: resolution.minimumCurrentTimeEndTime,
    resolvedEndTime: resolution.resolvedEndTime,
  };
};

export const resolveCommitEndTimeFit = ({
  lucid,
  latestEndTime,
  candidateEndTime,
  nowMs = Date.now(),
  minimumFutureBufferMs = COMMIT_DEFAULT_MINIMUM_FUTURE_BUFFER_MS,
  maximumEndTimeMs,
}: {
  readonly lucid: LucidEvolution;
  readonly latestEndTime: number;
  readonly candidateEndTime: number;
  readonly nowMs?: number;
  readonly minimumFutureBufferMs?: number;
  readonly maximumEndTimeMs?: number;
}): CommitEndTimeFit => {
  const alignedCandidateEndTime = alignedUnixTimeStrictlyAfter(
    lucid,
    candidateEndTime,
  );
  const minimumMonotonicEndTime = alignedUnixTimeStrictlyAfter(
    lucid,
    latestEndTime,
  );
  const minimumCurrentTimeEndTime = alignedUnixTimeStrictlyAfter(
    lucid,
    nowMs + minimumFutureBufferMs,
  );
  const resolvedEndTime = Math.max(
    alignedCandidateEndTime,
    minimumMonotonicEndTime,
    minimumCurrentTimeEndTime,
  );
  const resolution = {
    alignedCandidateEndTime,
    minimumMonotonicEndTime,
    minimumCurrentTimeEndTime,
    resolvedEndTime,
  };
  if (
    maximumEndTimeMs !== undefined &&
    resolvedEndTime - 1 > maximumEndTimeMs
  ) {
    return {
      ...resolution,
      status: "exceeds_cap",
      maximumEndTimeMs,
      reason: `resolved_valid_to_ms=${resolvedEndTime.toString()},resolved_inclusive_end_time_ms=${(resolvedEndTime - 1).toString()},maximum_end_time_ms=${maximumEndTimeMs.toString()},aligned_candidate_valid_to_ms=${alignedCandidateEndTime.toString()},minimum_monotonic_valid_to_ms=${minimumMonotonicEndTime.toString()},minimum_current_time_valid_to_ms=${minimumCurrentTimeEndTime.toString()}`,
    };
  }
  return {
    ...resolution,
    status: "fits",
    maximumEndTimeMs,
  };
};

export const minimumCommitBudgetMs = (
  checkpoint: CommitTimingCheckpoint,
): number => {
  switch (checkpoint) {
    case "pre_witness":
      return COMMIT_MIN_PRE_WITNESS_BUDGET_MS;
    case "pre_build":
      return COMMIT_MIN_PRE_BUILD_BUDGET_MS;
    case "pre_submit":
      return COMMIT_MIN_PRE_SUBMIT_BUDGET_MS;
  }
};

export const commitTimingBudget = ({
  checkpoint,
  resolvedEndTimeMs,
  nowMs = Date.now(),
}: {
  readonly checkpoint: CommitTimingCheckpoint;
  readonly resolvedEndTimeMs: number;
  readonly nowMs?: number;
}): CommitTimingBudget => {
  const minimumBudgetMs = minimumCommitBudgetMs(checkpoint);
  const remainingBudgetMs = resolvedEndTimeMs - nowMs;
  return {
    checkpoint,
    resolvedEndTimeMs,
    nowMs,
    remainingBudgetMs,
    minimumBudgetMs,
    satisfied: remainingBudgetMs >= minimumBudgetMs,
  };
};

export const formatCommitTimingBudget = (budget: CommitTimingBudget): string =>
  `checkpoint=${budget.checkpoint},resolvedEndTimeMs=${budget.resolvedEndTimeMs.toString()},nowMs=${budget.nowMs.toString()},remainingBudgetMs=${budget.remainingBudgetMs.toString()},minimumBudgetMs=${budget.minimumBudgetMs.toString()},satisfied=${String(budget.satisfied)}`;

export const resolveExplicitCommitCandidateEndTimeMs = (
  candidateEndTimeMs: number | undefined,
  nowMs: number = Date.now(),
): number =>
  candidateEndTimeMs ??
  nowMs + EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS;
