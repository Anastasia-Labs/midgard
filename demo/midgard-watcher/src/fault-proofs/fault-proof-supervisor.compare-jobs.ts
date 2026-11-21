import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import type { WatcherFaultProofJob } from "./fault-proof-supervisor.validate-job.js";

/**
 * The order the supervisor's pump starts jobs in: deadline work first,
 * earliest latest-safe-start, then earliest maturity, then `categories`
 * order and header hash; deadline-free work keeps arrival order behind it.
 */
export const watcherFaultProofJobOrder =
  (categories: readonly WatcherInstalledWorkflowCategory[]) =>
  (left: WatcherFaultProofJob, right: WatcherFaultProofJob): number => {
    if (left.deadline === null || right.deadline === null)
      return left.deadline === right.deadline ? 0 : left.deadline ? -1 : 1;
    const leftDeadline = BigInt(left.deadline.latestSafeStartAtMs);
    const rightDeadline = BigInt(right.deadline.latestSafeStartAtMs);
    if (leftDeadline !== rightDeadline)
      return leftDeadline < rightDeadline ? -1 : 1;
    const leftMaturity = BigInt(left.deadline.maturityAtMs);
    const rightMaturity = BigInt(right.deadline.maturityAtMs);
    if (leftMaturity !== rightMaturity)
      return leftMaturity < rightMaturity ? -1 : 1;
    const leftCategory = categories.indexOf(left.category);
    const rightCategory = categories.indexOf(right.category);
    if (leftCategory !== rightCategory) return leftCategory - rightCategory;
    return left.headerHash.localeCompare(right.headerHash);
  };
