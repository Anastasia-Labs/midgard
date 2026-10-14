import type {
  CommitEventCounts,
  CommitEventWindow,
} from "./commit-block-planner.event-window.js";

export type CommitFrameEventContribution = Readonly<{
  kind: keyof CommitEventCounts;
  inclusionTimeMs: number;
  bytes: bigint;
}>;
export type AdditiveCommitFramePlan =
  | Readonly<{ kind: "frame_hold"; reason: string }>
  | Readonly<{
      kind: "selected";
      window: Extract<CommitEventWindow, { endTimeMs: number }>;
      candidateCount: number;
      upperBoundBytes: bigint;
      rescue?: "whole_instant" | "ordinary";
    }>;

/** Linear source scan plus ordered complete instants. Withdrawal effects never
 * subtract bytes, nor can a shared program or an earlier creator lower this
 * budget. Under-fill is the explicit C2 technical ruling; no exact maximal
 * prefix or monotonic actual-serialization assumption is claimed. */
export const selectAdditiveCommitFrame = ({
  initialWindow,
  events,
  ordinaryBytes,
  fixedBytes,
  maxInnerBytes,
}: {
  readonly initialWindow: Extract<CommitEventWindow, { endTimeMs: number }>;
  readonly events: readonly CommitFrameEventContribution[];
  readonly ordinaryBytes: readonly bigint[];
  readonly fixedBytes: bigint;
  readonly maxInnerBytes: number;
}): AdditiveCommitFramePlan => {
  if (
    !Number.isSafeInteger(maxInnerBytes) ||
    maxInnerBytes < 0 ||
    fixedBytes < 0n
  )
    throw new Error("invalid additive DA frame authority");
  const groups = new Map<
    number,
    { bytes: bigint; counts: CommitEventCounts }
  >();
  for (const event of events) {
    if (
      !Number.isSafeInteger(event.inclusionTimeMs) ||
      event.inclusionTimeMs < 0 ||
      event.bytes < 0n ||
      event.inclusionTimeMs > initialWindow.endTimeMs
    )
      throw new Error("invalid additive DA event contribution");
    const group = groups.get(event.inclusionTimeMs) ?? {
      bytes: 0n,
      counts: { deposits: 0, withdrawals: 0, forcedTransactions: 0 },
    };
    group.bytes += event.bytes;
    group.counts = {
      ...group.counts,
      [event.kind]: group.counts[event.kind] + 1,
    };
    groups.set(event.inclusionTimeMs, group);
  }
  if (groups.size !== initialWindow.includedInstantCount)
    throw new Error("additive DA grouping differs from count authority");
  const limit = BigInt(maxInnerBytes);
  let bytes = fixedBytes;
  let includedInstantCount = 0;
  let endTimeMs = initialWindow.endTimeMs;
  let counts: CommitEventCounts = {
    deposits: 0,
    withdrawals: 0,
    forcedTransactions: 0,
  };
  let overflow = bytes > limit;
  for (const [time, group] of [...groups].sort(([a], [b]) => a - b)) {
    if (bytes + group.bytes > limit) {
      overflow = true;
      break;
    }
    bytes += group.bytes;
    endTimeMs = time;
    includedInstantCount += 1;
    counts = {
      deposits: counts.deposits + group.counts.deposits,
      withdrawals: counts.withdrawals + group.counts.withdrawals,
      forcedTransactions:
        counts.forcedTransactions + group.counts.forcedTransactions,
    };
  }
  if (overflow) {
    if (includedInstantCount > 0)
      return {
        kind: "selected",
        window: { mode: "catch_up", endTimeMs, counts, includedInstantCount },
        candidateCount: 0,
        upperBoundBytes: bytes,
      };
    const first = [...groups].sort(([a], [b]) => a - b)[0];
    if (first !== undefined)
      return {
        kind: "selected",
        window: {
          mode: "catch_up",
          endTimeMs: first[0],
          counts: first[1].counts,
          includedInstantCount: 1,
        },
        candidateCount: 0,
        upperBoundBytes: fixedBytes + first[1].bytes,
        rescue: "whole_instant",
      };
    if (initialWindow.mode === "normal" && ordinaryBytes.length > 0)
      return {
        kind: "selected",
        window: initialWindow,
        candidateCount: 1,
        upperBoundBytes: fixedBytes + ordinaryBytes[0]!,
        rescue: "ordinary",
      };
    return {
      kind: "frame_hold",
      reason: "fixed DA frame budget excludes every eligible source",
    };
  }
  let candidateCount = 0;
  if (initialWindow.mode === "normal")
    for (const contribution of ordinaryBytes) {
      if (contribution < 0n)
        throw new Error("invalid ordinary DA contribution");
      if (bytes + contribution > limit) break;
      bytes += contribution;
      candidateCount += 1;
    }
  if (groups.size === 0 && candidateCount === 0 && ordinaryBytes.length > 0)
    return {
      kind: "selected",
      window: initialWindow,
      candidateCount: 1,
      upperBoundBytes: fixedBytes + ordinaryBytes[0]!,
      rescue: "ordinary",
    };
  return {
    kind: "selected",
    window: initialWindow,
    candidateCount,
    upperBoundBytes: bytes,
  };
};
