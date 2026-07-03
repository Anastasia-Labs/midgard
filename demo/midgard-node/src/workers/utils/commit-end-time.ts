import {
  LucidEvolution,
  Network,
  slotToUnixTime,
  unixTimeToSlot,
} from "@lucid-evolution/lucid";

export const EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS = 5 * 60 * 1000;

// Commit construction may include a scheduler refresh and reference-script
// lookups under provider latency; keep enough validity for submission after
// the witness context is assembled without exceeding on-chain range limits.
export const COMMIT_DEFAULT_MINIMUM_FUTURE_BUFFER_MS = 240_000;
export const COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS = 8 * 60 * 1_000;
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
  const network = lucid.config().network;
  if (network === "Custom") {
    const provider = lucid.config().provider as {
      time?: number;
      slot?: number;
    };
    if (
      typeof provider.time === "number" &&
      typeof provider.slot === "number"
    ) {
      const slotLength = 1000;
      const zeroTime = provider.time - provider.slot * slotLength;
      const slot = lucid.unixTimeToSlot(unixTime);
      return zeroTime + slot * slotLength;
    }
    return unixTime;
  }
  const slot = unixTimeToSlot(network as Exclude<Network, "Custom">, unixTime);
  return slotToUnixTime(network as Exclude<Network, "Custom">, slot);
};

export const alignedUnixTimeStrictlyAfter = (
  lucid: LucidEvolution,
  unixTimeExclusive: number,
): number => {
  const network = lucid.config().network;
  if (network === "Custom") {
    const provider = lucid.config().provider as {
      time?: number;
      slot?: number;
    };
    if (
      typeof provider.time === "number" &&
      typeof provider.slot === "number"
    ) {
      const slotLength = 1000;
      const zeroTime = provider.time - provider.slot * slotLength;
      const slot = lucid.unixTimeToSlot(unixTimeExclusive);
      return zeroTime + (slot + 1) * slotLength;
    }
    return unixTimeExclusive + 1;
  }
  const slot = unixTimeToSlot(
    network as Exclude<Network, "Custom">,
    unixTimeExclusive,
  );
  return slotToUnixTime(network as Exclude<Network, "Custom">, slot + 1);
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
  const alignedCandidateEndTime = alignUnixTimeToSlotBoundary(
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
  if (maximumEndTimeMs !== undefined && resolvedEndTime > maximumEndTimeMs) {
    return {
      ...resolution,
      status: "exceeds_cap",
      maximumEndTimeMs,
      reason: `resolved_end_time_ms=${resolvedEndTime.toString()},maximum_end_time_ms=${maximumEndTimeMs.toString()},aligned_candidate_end_time_ms=${alignedCandidateEndTime.toString()},minimum_monotonic_end_time_ms=${minimumMonotonicEndTime.toString()},minimum_current_time_end_time_ms=${minimumCurrentTimeEndTime.toString()}`,
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
