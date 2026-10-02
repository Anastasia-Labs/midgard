import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { Effect, Ref } from "effect";

import type { Globals } from "../services/globals.globals.js";
import {
  extendL1ControlPlaneHold,
  l1ControlPlaneHoldTimeoutStreak,
} from "../services/globals.l1-control-plane.js";

/**
 * The L1 control-plane hold a commitment needs, derived from the work queued
 * for it rather than fixed.
 *
 * The worker's batch planner already caps one block at `COMMIT_MAX_L2_TX_COUNT`
 * transactions, so the transaction term counts at most that many: a backlog
 * larger than one block makes several blocks. User events are counted up to
 * the most one block can carry, the consensus caps on its deposits, forced
 * transactions and withdrawals together. A scope whose holds keep timing out
 * doubles its budget, twice at most, and `extendL1ControlPlaneHold` caps the
 * result at the control-plane ceiling.
 *
 * The per-entry allowances are estimates, not measurements, and a block at
 * the user-event cap asks for more than the ceiling grants.
 */
export const COMMIT_HOLD_BASE_MS = 180_000;
export const COMMIT_HOLD_PER_TX_MS = 20;
export const COMMIT_HOLD_PER_USER_EVENT_MS = 50;
export const COMMIT_HOLD_MAX_COUNTED_USER_EVENTS =
  MIDGARD_CONSENSUS_LIMITS.maxDepositCount +
  MIDGARD_CONSENSUS_LIMITS.maxForcedTransactionCount +
  MIDGARD_CONSENSUS_LIMITS.maxWithdrawalCount;
export const COMMIT_HOLD_MAX_TIMEOUT_DOUBLINGS = 2;

export const commitHoldBudgetMs = ({
  mempoolTxCount,
  maxL2TxCount,
  pendingUserEventCount,
  consecutiveHoldTimeouts,
}: {
  readonly mempoolTxCount: number;
  readonly maxL2TxCount: number;
  readonly pendingUserEventCount: number;
  readonly consecutiveHoldTimeouts: number;
}): number => {
  const countedTxs = Math.min(Math.max(0, mempoolTxCount), maxL2TxCount);
  const countedEvents = Math.min(
    Math.max(0, pendingUserEventCount),
    COMMIT_HOLD_MAX_COUNTED_USER_EVENTS,
  );
  const workMs =
    COMMIT_HOLD_BASE_MS +
    COMMIT_HOLD_PER_TX_MS * countedTxs +
    COMMIT_HOLD_PER_USER_EVENT_MS * countedEvents;
  return (
    workMs *
    2 **
      Math.min(
        Math.max(0, consecutiveHoldTimeouts),
        COMMIT_HOLD_MAX_TIMEOUT_DOUBLINGS,
      )
  );
};

/**
 * Raises the current `block_commitment` hold to the budget of the backlog
 * this tick's pre-permit check counted, before the worker starts. Returns
 * the budget asked for and the one granted (undefined outside a hold).
 */
export const extendCommitmentHoldForBacklog = (
  globals: Globals,
  maxL2TxCount: number,
): Effect.Effect<{
  readonly holdBudgetMs: number;
  readonly grantedHoldMs: number | undefined;
}> =>
  Effect.gen(function* () {
    const backlog = yield* Ref.get(globals.COMMIT_PIPELINE_BACKLOG);
    const holdBudgetMs = commitHoldBudgetMs({
      mempoolTxCount: backlog.mempoolTxCount,
      maxL2TxCount,
      pendingUserEventCount: backlog.pendingUserEventCount,
      consecutiveHoldTimeouts: yield* l1ControlPlaneHoldTimeoutStreak(
        globals,
        "block_commitment",
      ),
    });
    const grantedHoldMs = yield* extendL1ControlPlaneHold(holdBudgetMs);
    if (grantedHoldMs !== undefined && grantedHoldMs < holdBudgetMs) {
      yield* Effect.logWarning(
        `🔹 Commitment hold budget ${holdBudgetMs.toString()} ms exceeds the L1 control-plane ceiling; holding for ${grantedHoldMs.toString()} ms.`,
      );
    }
    return { holdBudgetMs, grantedHoldMs };
  });
