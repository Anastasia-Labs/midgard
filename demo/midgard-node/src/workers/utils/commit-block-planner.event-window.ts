/** An authenticated due-event instant. All kinds at this instant are indivisible. */
export type CommitEventInstant = Readonly<{
  inclusionTimeMs: number;
  deposits: number;
  forcedTransactions: number;
  withdrawals: number;
}>;
export type CommitEventCounts = Omit<CommitEventInstant, "inclusionTimeMs">;
export type CommitEventLimits = CommitEventCounts &
  Readonly<{ totalEvents: number }>;
export type CommitEventWindow =
  | Readonly<{
      mode: "normal" | "catch_up";
      endTimeMs: number;
      counts: CommitEventCounts;
      includedInstantCount: number;
    }>
  | Readonly<{ mode: "empty" }>
  | Readonly<{ mode: "instant_exceeds_cap"; inclusionTimeMs: number }>;

const kinds = ["deposits", "forcedTransactions", "withdrawals"] as const;
const natural = (value: number): boolean =>
  Number.isSafeInteger(value) && value >= 0;
const total = (counts: CommitEventCounts): number =>
  kinds.reduce((sum, kind) => sum + counts[kind], 0);
const fits = (counts: CommitEventCounts, limits: CommitEventLimits): boolean =>
  kinds.every((kind) => counts[kind] <= limits[kind]) &&
  total(counts) <= limits.totalEvents;

/**
 * Selects the largest complete prefix the protocol counts admit. The input is
 * the ordered GROUP BY result, bounded to cap + 1 instants per kind; even a
 * truncated result must overflow that kind's cap, so it cannot claim normal
 * coverage. A catch-up slice carries no normal transactions and ends at its
 * last real event instant. Unknown events beyond the safe horizon remain due.
 */
export const planCommitEventWindow = ({
  groups,
  startTimeMs,
  horizonMs,
  blockTimeMs,
  limits,
}: {
  readonly groups: readonly CommitEventInstant[];
  readonly startTimeMs: number;
  readonly horizonMs: number;
  readonly blockTimeMs: number;
  readonly limits: CommitEventLimits;
}): CommitEventWindow => {
  if (
    ![
      startTimeMs,
      horizonMs,
      blockTimeMs,
      limits.totalEvents,
      ...kinds.map((kind) => limits[kind]),
    ].every(natural) ||
    blockTimeMs <= startTimeMs
  )
    throw new Error("invalid commit event window authority or limits");
  let counts: CommitEventCounts = {
    deposits: 0,
    forcedTransactions: 0,
    withdrawals: 0,
  };
  let includedInstantCount = 0;
  let endTimeMs = startTimeMs;
  let previous = -1;
  let overflow = false;
  for (const group of groups) {
    if (
      !natural(group.inclusionTimeMs) ||
      !kinds.every((kind) => natural(group[kind])) ||
      !natural(total(group)) ||
      total(group) === 0 ||
      group.inclusionTimeMs <= previous
    )
      throw new Error(
        "commit event instants are not canonical ordered positive groups",
      );
    previous = group.inclusionTimeMs;
    if (group.inclusionTimeMs <= startTimeMs)
      throw new Error(
        "unassigned event does not follow the previous coverage end",
      );
    if (group.inclusionTimeMs > Math.min(horizonMs, blockTimeMs)) break;
    const next = {
      deposits: counts.deposits + group.deposits,
      forcedTransactions: counts.forcedTransactions + group.forcedTransactions,
      withdrawals: counts.withdrawals + group.withdrawals,
    };
    if (!fits(next, limits)) {
      if (includedInstantCount === 0)
        return {
          mode: "instant_exceeds_cap",
          inclusionTimeMs: group.inclusionTimeMs,
        };
      overflow = true;
      break;
    }
    counts = next;
    includedInstantCount += 1;
    endTimeMs = group.inclusionTimeMs;
  }
  if (!overflow && horizonMs >= blockTimeMs)
    return {
      mode: "normal",
      endTimeMs: blockTimeMs,
      counts,
      includedInstantCount,
    };
  return includedInstantCount === 0
    ? { mode: "empty" }
    : { mode: "catch_up", endTimeMs, counts, includedInstantCount };
};
