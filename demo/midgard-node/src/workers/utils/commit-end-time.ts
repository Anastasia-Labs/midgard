import {
  LucidEvolution,
  Network,
  slotToUnixTime,
  unixTimeToSlot,
} from "@lucid-evolution/lucid";

export const EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS =
  5 * 60 * 1000;

// Commit construction may include scheduler refresh + layout retries, which can
// exceed one minute under provider latency; keep a safety margin without
// pushing header end-times too far into the future.
const COMMIT_VALIDITY_FUTURE_BUFFER_MS = 120_000;

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
    if (typeof provider.time === "number" && typeof provider.slot === "number") {
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
    if (typeof provider.time === "number" && typeof provider.slot === "number") {
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
}: {
  readonly lucid: LucidEvolution;
  readonly latestEndTime: number;
  readonly candidateEndTime: number;
  // Wall-clock reference for the future-buffer floor. Callers that resolve the
  // window repeatedly (e.g. the commit-window stabilization loop) must pass a
  // single snapshot so the floor does not advance between iterations; otherwise
  // on short-slot networks (preprod: 1s) provider latency pushes the floor past
  // the witnessed end time every attempt and stabilization can never converge.
  readonly nowMs?: number;
}): {
  readonly alignedCandidateEndTime: number;
  readonly minimumMonotonicEndTime: number;
  readonly resolvedEndTime: number;
} => {
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
    nowMs + COMMIT_VALIDITY_FUTURE_BUFFER_MS,
  );
  return {
    alignedCandidateEndTime,
    minimumMonotonicEndTime,
    resolvedEndTime: Math.max(
      alignedCandidateEndTime,
      minimumMonotonicEndTime,
      minimumCurrentTimeEndTime,
    ),
  };
};

export const resolveExplicitCommitCandidateEndTimeMs = (
  candidateEndTimeMs: number | undefined,
  nowMs: number = Date.now(),
): number =>
  candidateEndTimeMs ??
  nowMs + EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS;
