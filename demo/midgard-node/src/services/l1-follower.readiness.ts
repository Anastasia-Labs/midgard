/**
 * What the node's L1 follower contributes to `/readyz` (N1): every named
 * reason the shared follow loop reports (`FollowStatus.readiness`: the
 * interventions R1 to R5 and `origin_mismatch`, `l1_follower_catching_up`,
 * `l1_follower_waiting`, `l1_follower_apply_stuck`), every hold of the
 * follower-change driver (`l1_events_*`; the forced-order hook's
 * `forced_order_carriage_pending`, `forced_order_admission_stopped` and
 * `forced_order_ingestion_failed`; and landed-block processing's, from
 * `landed-blocks/holds.ts`: `landed_block_invalid`,
 * `landed_block_own_journal_abandoned`, `landed_block_event_unknown`,
 * `landed_block_forced_order_pending`, `landed_block_awaiting_da`,
 * `landed_block_rebase_failed`, `landed_block_replay_failed`,
 * `landed_blocks_waiting`, `confirmed_ledger_behind` and
 * `landed_block_rebase_pending`, the first by priority named as the reason
 * and the rest in its detail), the intent stage's (`wallet_seed_pending`,
 * `intent_reconcile_failed`, `intent_reconcile_transient`) and the intent
 * journal's refusals (`intent_journal_*`, `intent_input_untracked`,
 * `intent_bytes_mismatch`, `intent_undecodable`), and
 * `l1_follower_unconfigured` while the node has no follower. Each fails
 * readiness by name; none stops the process, and `/healthz` stays live.
 *
 * One degradation is a detail, leaving the node ready:
 * `landed_frontier_prune_floor:<slots>` while the landed frontier's prune
 * floor holds the follower's prune boundary that many slots back (the facts
 * the frontier still needs are kept; the store grows until it moves).
 */
import type { FollowStatus } from "@al-ft/midgard-l1-follower";

import type { DriverHold, IngestionPlan } from "../l1-events/driver.js";

/** The node has no follower: its configuration is missing a piece (named in the detail). */
export const L1_FOLLOWER_UNCONFIGURED = "l1_follower_unconfigured";

/** The landed frontier's prune floor holds the follower's prune boundary back (detail, with the lag in slots). */
export const LANDED_FRONTIER_PRUNE_FLOOR = "landed_frontier_prune_floor";

/** The projection at the follower's current view, or why there is none. */
export type FollowerPlanRead =
  | Readonly<{ kind: "ok"; plan: IngestionPlan }>
  | Readonly<{ kind: "none"; detail: string }>;

/** The running follower, as the rest of the node reads it. */
export type L1FollowerHandle = Readonly<{
  kind: "running";
  /** The follow loop's latest status. */
  status: () => FollowStatus;
  /** The driver's and the intent stage's holds from their latest run, and the journal's refusals. */
  holds: () => readonly DriverHold[];
  /** Reads the event projection at the follower's current view. */
  planCurrent: () => Promise<FollowerPlanRead>;
}>;

export type L1FollowerState =
  | Readonly<{ kind: "unconfigured"; detail: string }>
  | L1FollowerHandle;

/**
 * The follower has applied the chain through the node's tip with no
 * intervention or stuck point: every admission since its origin is in its
 * key set (keys are never pruned), so a key it lacks is an orphan, not one
 * it has not reached yet.
 */
export const followerCaughtUp = (status: FollowStatus): boolean =>
  status.atTip && status.interventions.length === 0 && status.stuck === null;

/** The running follower, when it is caught up. */
export const caughtUpFollower = (
  state: L1FollowerState,
): L1FollowerHandle | undefined =>
  state.kind === "running" && followerCaughtUp(state.status())
    ? state
    : undefined;

export const L1_FOLLOWER_NOT_STARTED: L1FollowerState = {
  kind: "unconfigured",
  detail: "the L1 follower has not started",
};

export type L1FollowerReadiness = Readonly<{
  /** Named reasons, each one failing `/readyz`. */
  reasons: readonly string[];
  /** Named degradations that leave the node ready. */
  details: readonly string[];
  /** The report `/readyz` carries for them. */
  report: Readonly<Record<string, unknown>>;
}>;

/** The `/readyz` reasons and report of the follower `state`. */
export const l1FollowerReadiness = (
  state: L1FollowerState,
): L1FollowerReadiness => {
  if (state.kind === "unconfigured")
    return {
      reasons: [L1_FOLLOWER_UNCONFIGURED],
      details: [],
      report: {
        state: "unconfigured",
        readiness: [{ reason: L1_FOLLOWER_UNCONFIGURED, detail: state.detail }],
      },
    };
  const status = state.status();
  const holds = state.holds();
  const readiness = [...status.readiness, ...holds];
  const reasons: string[] = [];
  for (const { reason } of readiness)
    if (!reasons.includes(reason)) reasons.push(reason);
  return {
    reasons,
    details:
      status.prune.floorLagSlots === null
        ? []
        : [
            `${LANDED_FRONTIER_PRUNE_FLOOR}:${status.prune.floorLagSlots.toString()}`,
          ],
    report: {
      state: status.state,
      readiness,
      cursor: status.cursor,
      tip: status.tip,
      atTip: status.atTip,
      protocolInit: status.protocolInit,
      events: status.events,
      lastError: status.lastError,
      prune: status.prune,
    },
  };
};
