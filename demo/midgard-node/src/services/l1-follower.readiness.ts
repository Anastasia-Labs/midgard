/**
 * What the node's L1 follower contributes to `/readyz` (N1): every named
 * reason the shared follow loop reports (`FollowStatus.readiness`: the
 * interventions R1 to R5 and `origin_mismatch`, `l1_follower_catching_up`,
 * `l1_follower_waiting`, `l1_follower_apply_stuck`,
 * `l1_follower_prune_failing`), every hold of the
 * follower-change driver (`l1_events_*`; the forced-order hook's
 * `forced_order_carriage_pending`, `forced_order_admission_stopped` and
 * `forced_order_ingestion_failed`; and landed-block processing's, from
 * `landed-blocks/holds.ts`: `landed_block_invalid`,
 * `landed_block_own_journal_abandoned`, `landed_block_event_unknown`,
 * `landed_block_forced_order_pending`, `landed_block_awaiting_da`,
 * `landed_block_batch_undecided`, `landed_block_rebase_failed`,
 * `landed_block_replay_failed`, `confirmed_ledger_base_mismatch`,
 * `landed_blocks_waiting`, `confirmed_ledger_behind`,
 * `confirmed_ledger_own_block_pending` and `landed_block_rebase_pending`,
 * the first by priority named as the reason and the rest in its detail),
 * the intent stage's (`wallet_seed_pending`, `intent_reconcile_failed`,
 * `intent_reconcile_transient`, `intent_resubmit_rejected`, a predicate's
 * wait such as `intent_included_events_not_deep`, and `tracked_set_changed`
 * while the store replays a tracked-set reset) and the intent
 * journal's refusals, the worker threads' included (`intent_journal_*`,
 * `intent_input_untracked`, `intent_bytes_mismatch`, `intent_undecodable`,
 * `intent_content_ref_missing`), and
 * `l1_follower_unconfigured` while the node has no follower, and
 * `l1_node_config_unreadable` while its network magic is retried. Each fails
 * readiness by name; none stops the process, and `/healthz` stays live.
 *
 * One degradation is a detail, leaving the node ready:
 * `<floor>_prune_floor:<slots>` for each role prune floor that holds the
 * follower's prune boundary that many slots back, named by the declaring
 * floor (`landed_frontier_prune_floor` for the landed frontier,
 * `intent_journal_replay_prune_floor` for the intent journal's replay,
 * `operator_activity_record_prune_floor` for the operator-activity record).
 * The facts the floor still needs are kept; the store grows until it moves.
 *
 * The report also carries `confirmedLedger`: the confirmed-ledger frontier,
 * the slot of its merge and that merge's level (`landed`, `safe`, `final`).
 */
import {
  type FactStore,
  type FollowStatus,
  readinessOf,
} from "@al-ft/midgard-l1-follower";
import type { EventProjectionConfig } from "@al-ft/midgard-l1-follower/events";

import {
  type DriverHold,
  type IngestionPlan,
  planIngestion,
} from "../l1-events/driver.js";
import type { ConfirmedLedgerPosition } from "../landed-blocks/position.js";

/** The node has no follower: its configuration is missing a piece (named in the detail). */
export const L1_FOLLOWER_UNCONFIGURED = "l1_follower_unconfigured";
/**
 * The node's config files do not yield its network magic yet (the detail says
 * why); the node reads them again with capped backoff and starts its follower
 * once they do.
 */
export const L1_NODE_CONFIG_UNREADABLE = "l1_node_config_unreadable";

/** The follower cursor's identity, to tell a moved cursor from a repeat. */
export const cursorKey = (status: FollowStatus): string | null =>
  status.cursor === null
    ? null
    : `${status.cursor.generation.toString()}:${status.cursor.slot.toString()}`;

const starting = {
  state: "starting",
  interventions: [],
  waiting: null,
  stuck: null,
  protocolInit: "unknown",
  cursor: null,
  node: null,
  nodeBehind: null,
  tip: null,
  atTip: false,
  replaying: false,
  events: 0,
  lastError: null,
  prune: {
    steps: 0,
    prunedThroughSlot: null,
    lastError: null,
    floorLags: [],
    failures: 0,
  },
} as const;
/** The follower's status before its loop publishes one. */
export const startingStatus: FollowStatus = {
  ...starting,
  readiness: readinessOf(starting),
};

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
  /** Where `confirmed_ledger` stands, with its merge's level (P10); null before the first run. */
  confirmedLedger?: () => ConfirmedLedgerPosition | null;
}>;

export type L1FollowerState =
  | Readonly<{ kind: "unconfigured"; detail: string }>
  | Readonly<{
      kind: "waiting";
      reason: typeof L1_NODE_CONFIG_UNREADABLE;
      detail: string;
    }>
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
  if (state.kind === "waiting")
    return {
      reasons: [state.reason],
      details: [],
      report: {
        state: "waiting",
        readiness: [{ reason: state.reason, detail: state.detail }],
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
    details: status.prune.floorLags.map(
      ({ floor, lagSlots }) => `${floor}_prune_floor:${lagSlots.toString()}`,
    ),
    report: {
      state: status.state,
      readiness,
      cursor: status.cursor,
      tip: status.tip,
      atTip: status.atTip,
      node: status.node,
      nodeBehind: status.nodeBehind,
      protocolInit: status.protocolInit,
      events: status.events,
      lastError: status.lastError,
      prune: status.prune,
      confirmedLedger: state.confirmedLedger?.() ?? null,
    },
  };
};
