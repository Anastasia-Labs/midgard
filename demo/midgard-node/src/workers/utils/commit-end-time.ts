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

// Once an end-time has been selected and a scheduler-aligned witness has been
// built for it, the end-time stays usable as long as it remains at least this
// far in the future. This floor is intentionally smaller than
// COMMIT_VALIDITY_FUTURE_BUFFER_MS: the difference is the latency budget the
// witness build (several sequential provider round-trips) is allowed to consume
// without forcing the commit window to be re-resolved against a now-advanced
// clock. Without this hysteresis the stabilization loop could never converge,
// because the current-time floor rises with every witness fetch.
const COMMIT_VALIDITY_MIN_FUTURE_BUFFER_MS = 60_000;

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
}: {
  readonly lucid: LucidEvolution;
  readonly latestEndTime: number;
  readonly candidateEndTime: number;
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
    Date.now() + COMMIT_VALIDITY_FUTURE_BUFFER_MS,
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

/**
 * Reports whether a previously selected commit end-time is still safe to submit.
 *
 * The end-time is chosen with a generous future buffer
 * ({@link COMMIT_VALIDITY_FUTURE_BUFFER_MS}); building the scheduler-aligned
 * witness for it then consumes wall-clock time. As long as the end-time is still
 * at least {@link COMMIT_VALIDITY_MIN_FUTURE_BUFFER_MS} in the future, the
 * already-built witness remains aligned with it and the commit can proceed
 * without re-resolving the window.
 */
export const isCommitEndTimeStillSubmittable = (
  endTime: number,
  nowMs: number = Date.now(),
): boolean => endTime >= nowMs + COMMIT_VALIDITY_MIN_FUTURE_BUFFER_MS;

export const resolveExplicitCommitCandidateEndTimeMs = (
  candidateEndTimeMs: number | undefined,
  nowMs: number = Date.now(),
): number =>
  candidateEndTimeMs ??
  nowMs + EXPLICIT_COMMIT_DEFAULT_CANDIDATE_FUTURE_BUFFER_MS;
