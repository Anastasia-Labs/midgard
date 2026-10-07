/**
 * What the node's L1 follower contributes to `/readyz` (N1): every named
 * reason the shared follow loop reports (`FollowStatus.readiness`: the
 * interventions R1 to R5 and `origin_mismatch`, `l1_follower_catching_up`,
 * `l1_follower_waiting`, `l1_follower_apply_stuck`), every hold of the
 * follower-change driver (`l1_events_*`), and `l1_follower_unconfigured`
 * while the node has no follower. Each fails readiness by name; none stops
 * the process, and `/healthz` stays live.
 */
import type { FollowStatus } from "@al-ft/midgard-l1-follower";

import type { DriverHold, IngestionPlan } from "../l1-events/driver.js";

/** The node has no follower: its configuration is missing a piece (named in the detail). */
export const L1_FOLLOWER_UNCONFIGURED = "l1_follower_unconfigured";

/** The projection at the follower's current view, or why there is none. */
export type FollowerPlanRead =
  | Readonly<{ kind: "ok"; plan: IngestionPlan }>
  | Readonly<{ kind: "none"; detail: string }>;

/** The running follower, as the rest of the node reads it. */
export type L1FollowerHandle = Readonly<{
  kind: "running";
  /** The follow loop's latest status. */
  status: () => FollowStatus;
  /** The driver's holds from its latest run. */
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
