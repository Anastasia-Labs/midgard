import { EVENT_WAIT_DURATION_MS } from "@al-ft/midgard-sdk";

import type {
  CommitTimingBudget,
  CommitTimingCheckpoint,
} from "../workers/utils/commit-end-time.js";
import type { HistoryOwnerCoverage } from "./event-history-owner.js";

/** Both admitted validator environments enforce inclusion = inclusive validTo
 * + this delay. An admission after the whole canonical point cannot be eligible
 * at or before this conservative horizon. This does not move the source point,
 * and known admissions in the lookahead interval still have to be selected. */
export const historyEligibilityHorizon = (
  coverage: HistoryOwnerCoverage,
): number => {
  const end = coverage.includedThroughMs + EVENT_WAIT_DURATION_MS - 1;
  if (
    !Number.isSafeInteger(coverage.includedThroughMs) ||
    !Number.isSafeInteger(end)
  )
    throw new Error(
      "Authenticated history time cannot form a safe commit horizon",
    );
  return end;
};

/** The fixed header interval cannot be extended to rescue a slow build. These
 * stage reserves apply only to a source-owned short-window attempt; expiration
 * causes a new selection/build against fresh coverage. The ordinary long-window
 * fixture policy and on-chain range/size/execution limits remain independent. */
export const HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS = 30_000;
const reserves: Readonly<Record<CommitTimingCheckpoint, number>> = {
  pre_witness: 30_000,
  pre_build: 20_000,
  pre_submit: 10_000,
};
export const historyCommitTimingBudget = (input: {
  checkpoint: CommitTimingCheckpoint;
  resolvedEndTimeMs: number;
  nowMs: number;
}): CommitTimingBudget => {
  const minimumBudgetMs = reserves[input.checkpoint];
  const remainingBudgetMs = input.resolvedEndTimeMs - input.nowMs;
  return {
    ...input,
    remainingBudgetMs,
    minimumBudgetMs,
    satisfied:
      Number.isSafeInteger(input.nowMs) &&
      Number.isSafeInteger(input.resolvedEndTimeMs) &&
      remainingBudgetMs >= minimumBudgetMs,
  };
};
