import type { CommitPlanningSelection } from "../../src/workers/commit-planning-notice.js";

/**
 * A plan the commit-planning boundary accepts (`makeCommitPlanningNotice`):
 * normal mode commits up to the block time, catch-up stops short of it.
 */
export const commitPlanningSelection = (
  mode: CommitPlanningSelection["mode"],
): CommitPlanningSelection => ({
  mode,
  observedAtMs: 5_000,
  authority: {
    view: { generation: 7, slot: 101, height: 90, hash: "aa".repeat(32) },
    epoch: "1",
    baseHeaderHash: "bb".repeat(28),
    baseUtxosRoot: "cc".repeat(32),
    deploymentFingerprint: "dd".repeat(32),
  },
  coverageStartTimeMs: 100,
  selectedEndTimeMs: mode === "normal" ? 1_000 : 200,
  blockTimeMs: 1_000,
  canonicalHorizonMs: mode === "normal" ? 1_000 : 900,
  eligibleCutoffMs: mode === "normal" ? 1_000 : 900,
  eligibleEventCountLowerBounds: {
    deposits: 4,
    withdrawals: 0,
    forcedTransactions: 0,
  },
  selectedEventCounts: { deposits: 2, withdrawals: 0, forcedTransactions: 0 },
  includedInstantCount: 1,
  selectedOrdinaryTxCount: 0,
  rescue: null,
});
