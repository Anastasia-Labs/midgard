/**
 * What the node's L1 follower contributes to `/readyz` (N1): every named
 * reason the shared follow loop reports (`FollowStatus.readiness`: the
 * interventions R1 to R5 and `origin_mismatch`, `l1_follower_catching_up`,
 * `l1_follower_waiting`, `l1_follower_apply_stuck`,
 * `l1_follower_prune_failing`), every hold of the
 * follower-change driver (`l1_events_*`, and the forced-order hook's
 * `forced_order_carriage_pending`, `forced_order_admission_stopped` and
 * `forced_order_ingestion_failed`), the intent stage's (`wallet_seed_pending`,
 * `intent_reconcile_failed`, `intent_reconcile_transient`) and the intent
 * journal's refusals, the worker threads' included (`intent_journal_*`,
 * `intent_input_untracked`, `intent_bytes_mismatch`, `intent_undecodable`,
 * `intent_content_ref_missing`), and
 * `l1_follower_unconfigured` while the node has no follower. Each fails
 * readiness by name; none stops the process, and `/healthz` stays live.
 */
import type { FactStore, FollowStatus } from "@al-ft/midgard-l1-follower";

import {
  type DriverHold,
  type IngestionPlan,
  planIngestion,
} from "../l1-events/driver.js";
import type { EventProjectionConfig } from "../l1-events/index.js";

/** The node has no follower: its configuration is missing a piece (named in the detail). */
export const L1_FOLLOWER_UNCONFIGURED = "l1_follower_unconfigured";

/** The projection at the follower's current view, or why there is none. */
export type FollowerPlanRead =
  | Readonly<{ kind: "ok"; plan: IngestionPlan }>
  | Readonly<{ kind: "none"; detail: string }>;

/** The projection at the store's current view. */
export const planCurrentView = async (
  store: FactStore,
  config: EventProjectionConfig,
): Promise<FollowerPlanRead> => {
  const view = await store.currentView();
  if (view === null)
    return { kind: "none", detail: "the follower store has no view yet" };
  const planned = await planIngestion(store, config, view);
  return planned.kind === "ok"
    ? planned
    : { kind: "none", detail: planned.detail };
};

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
