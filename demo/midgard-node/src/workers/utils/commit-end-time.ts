import { LucidEvolution, Network, slotToUnixTime, unixTimeToSlot } from "@lucid-evolution/lucid";

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
