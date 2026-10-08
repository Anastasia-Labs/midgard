import type {
  L1NodeTransport,
  TransportUnreadyReason,
} from "@al-ft/l1-node-transport";

import type { OriginConfig } from "../origin.js";
import type { FactStore } from "../store/fact-store.js";
import type { PruneFloorLag } from "../store/prune.js";
import type { InterventionReason } from "../types.js";

/** The cursor is not known to be at the node's tip (before the first event too). */
export const FOLLOWER_CATCHING_UP = "l1_follower_catching_up";
/** A transient failure is being backed off from; the detail names its cause. */
export const FOLLOWER_WAITING = "l1_follower_waiting";
/** One point keeps failing to apply; an operator has to look. */
export const FOLLOWER_APPLY_STUCK = "l1_follower_apply_stuck";
/**
 * The store refused its migrations at start (an applied migration changed
 * or is unknown): every start fails the same way, so an operator has to
 * look; the process stays up and keeps retrying.
 */
export const FOLLOWER_MIGRATION_FAILED = "l1_follower_migration_failed";
/** The L1 node transport is not ready; the detail carries its reason. */
export const FOLLOWER_NODE_UNAVAILABLE = "l1_node_unavailable";
/**
 * The local node's tip is behind wall-clock time by more than the bound
 * (`FollowChainOptions.nodeBehind`); the detail carries the lag. The node is
 * reachable but not producing or receiving blocks (it is syncing, or cut off
 * from its peers), so every decision taken now would rest on a stale chain:
 * a role does not submit while it holds. Transient: cleared by the first
 * check (each event, and a timer while none arrive) that finds the tip
 * within the bound. Never an exit.
 */
export const FOLLOWER_NODE_BEHIND = "l1_node_behind";
/**
 * A store reset is replaying from the origin: the start reset the store
 * because the configured protocol tracked set added an item its record did
 * not hold or the store had no record, or an operator ran
 * `reset --to-origin`. Transient: cleared, in the store too, the first time
 * the loop reports the cursor at the tip of an available node with the
 * cursor at least as high as before the reset.
 */
export const FOLLOWER_TRACKED_SET_CHANGED = "tracked_set_changed";
/**
 * `PRUNE_FAILING_AFTER` prune passes in a row failed (a prune hook threw, or
 * the store did): every fact past retention stays until one succeeds.
 */
export const FOLLOWER_PRUNE_FAILING = "l1_follower_prune_failing";

/** A named reason a role's `/readyz` reports while the follower holds it unready. */
export type FollowReadinessReason =
  | InterventionReason
  | typeof FOLLOWER_CATCHING_UP
  | typeof FOLLOWER_WAITING
  | typeof FOLLOWER_APPLY_STUCK
  | typeof FOLLOWER_MIGRATION_FAILED
  | typeof FOLLOWER_NODE_UNAVAILABLE
  | typeof FOLLOWER_NODE_BEHIND
  | typeof FOLLOWER_TRACKED_SET_CHANGED
  | typeof FOLLOWER_PRUNE_FAILING;

export type FollowReadiness = Readonly<{
  reason: FollowReadinessReason;
  detail: string;
}>;

/**
 * What a `waiting` loop backs off from: `store_locked` (the writer lease is
 * held elsewhere or was lost), `stream` (the chain-sync stream failed or
 * ended), `store` (starting or reading the store failed), `apply` (an
 * event failed to apply; see `stuck`).
 */
export type FollowWaitCause = "store_locked" | "stream" | "store" | "apply";

export type FollowStatus = Readonly<{
  /**
   * `following`: applying chain-sync events. `waiting`: backing off from a
   * failure (`waiting` says which). `intervention`: stopped on a condition
   * only an operator clears; the process stays up. `stopped`: aborted.
   */
  state: "starting" | "following" | "waiting" | "intervention" | "stopped";
  /** Every reason the role is not ready; empty when it is. */
  readiness: readonly FollowReadiness[];
  /** The interventions in force (R1 to R5, `origin_mismatch`). */
  interventions: readonly Readonly<{
    reason: InterventionReason;
    detail: string;
  }>[];
  waiting: Readonly<{ cause: FollowWaitCause; detail: string }> | null;
  /** Set once one point failed `stuckAfter` times, or once deterministically. */
  stuck: Readonly<{ at: string; failures: number; detail: string }> | null;
  /** Whether the protocol-init tx (the hubOracleOneShot spend) is in the facts. */
  protocolInit: "seen" | "pending" | "unknown";
  cursor: Readonly<{ slot: number; height: number; generation: number }> | null;
  /**
   * The transport's unready reason and detail while it is not ready (the
   * sidecar or the node is down or restarting); null while it is ready.
   */
  node: Readonly<{ reason: TransportUnreadyReason; detail: string }> | null;
  /** The node tip the latest applied event reported. */
  tip: Readonly<{ slot: number; height: number }> | null;
  /**
   * Set while that tip's slot time is more than `boundMs` behind wall-clock
   * time (`FOLLOWER_NODE_BEHIND`); null otherwise, before the first tip, and
   * when the role passed no `nodeBehind` option.
   */
  nodeBehind: Readonly<{
    tipSlot: number;
    lagMs: number;
    boundMs: number;
  }> | null;
  /** The cursor equals that tip; false while `node` is set. */
  atTip: boolean;
  /** A store reset is replaying from the origin; cleared at the first `atTip` at or above the height before it. */
  replaying: boolean;
  /** Events applied by this loop. */
  events: number;
  /** The latest failure, cleared by the next applied event. */
  lastError: string | null;
  prune: Readonly<{
    steps: number;
    prunedThroughSlot: number | null;
    lastError: string | null;
    /** Prune passes failed in a row; the next successful one resets it. */
    failures: number;
    /** Each role prune floor holding the boundary back, by name, with its lag in slots (empty: none). */
    floorLags: readonly PruneFloorLag[];
  }>;
}>;

export type FollowChainOptions = Readonly<{
  store: FactStore;
  transport: Pick<
    L1NodeTransport,
    "openChainSync" | "readiness" | "onReadiness"
  >;
  origin: OriginConfig;
  signal: AbortSignal;
  /** The chain-sync stream's credit (default 64). */
  credit?: number;
  /** Capped exponential backoff (default 500 ms to 30 s). */
  backoffMs?: Readonly<{ initial: number; max: number }>;
  log?: (line: string) => void;
  /** Hears every status change; its failure is logged, never thrown. */
  onStatus?: (status: FollowStatus) => void | Promise<void>;
  /** Consecutive failures on one point before `l1_follower_apply_stuck` (default 5). */
  stuckAfter?: number;
  /**
   * One budgeted `store.prune(budget)` step at the tip after each applied
   * event, and every `everyEvents` applied events while catching up (then
   * after every event until a step reports `done`). Defaults: 500 rows per
   * table per step, every 100 events.
   */
  prune?: Readonly<{ budget?: number; everyEvents?: number }>;
  /** The `l1_node_behind` check (`NodeBehindOptions`); off when omitted. */
  nodeBehind?: NodeBehindOptions;
}>;

/**
 * How the loop tells that the node's tip is behind wall-clock time (plan §9
 * Time: the wall clock may hold, never decide). Checked after every applied
 * event and every `checkEveryMs` while none arrive.
 */
export type NodeBehindOptions = Readonly<{
  /**
   * POSIX ms at the start of `slot`, from the network's slot configuration.
   * A failure skips that check (the last result stands) and is logged.
   */
  slotTime: (slot: number) => number | Promise<number>;
  /** The largest lag that is not behind (default `DEFAULT_NODE_BEHIND_MS`). */
  boundMs?: number;
  /** The timer's period while no event arrives (default 10 s). */
  checkEveryMs?: number;
  /** Wall-clock ms (default `Date.now`). */
  now?: () => number;
}>;

/**
 * The default `l1_node_behind` bound: 5 minutes. With Cardano's active
 * slot coefficient (f = 0.05, 1 s slots) a gap of 300 slots between honest
 * blocks has probability 0.95^300 ≈ 2·10⁻⁷, about once in three years at
 * ~4,300 blocks a day, so the reason does not flap on honest gaps; a node
 * that far behind is not following the chain. It is small against k (2,160
 * blocks, about 12 h).
 */
export const DEFAULT_NODE_BEHIND_MS = 300_000;
export const NODE_BEHIND_CHECK_EVERY_MS = 10_000;

export const DEFAULT_STUCK_AFTER = 5;
export const LOOP_PRUNE_BUDGET = 500;
export const LOOP_PRUNE_EVERY = 100;
/**
 * Consecutive failed prune passes before `l1_follower_prune_failing`. One
 * failure alone is not a reason: the next pass retries it.
 */
export const PRUNE_FAILING_AFTER = 3;

/** The readiness reasons a status implies. */
export const readinessOf = (
  status: Omit<FollowStatus, "readiness">,
): FollowReadiness[] => {
  const reasons: FollowReadiness[] = status.interventions.map((i) => ({
    reason: i.reason,
    detail: i.detail,
  }));
  if (status.node !== null)
    reasons.push({
      reason: FOLLOWER_NODE_UNAVAILABLE,
      detail: `${status.node.reason}: ${status.node.detail}`,
    });
  // An unavailable node's tip is not known; that reason covers it.
  if (status.node === null && status.nodeBehind !== null)
    reasons.push({
      reason: FOLLOWER_NODE_BEHIND,
      detail: `node tip slot ${status.nodeBehind.tipSlot} is ${Math.round(status.nodeBehind.lagMs / 1000)} s behind wall-clock time (bound ${Math.round(status.nodeBehind.boundMs / 1000)} s)`,
    });
  if (status.stuck?.at === "migration")
    reasons.push({
      reason: FOLLOWER_MIGRATION_FAILED,
      detail: `the store refused its migrations: ${status.stuck.detail}`,
    });
  else if (status.stuck !== null)
    reasons.push({
      reason: FOLLOWER_APPLY_STUCK,
      detail: `${status.stuck.at} failed ${status.stuck.failures} times: ${status.stuck.detail}`,
    });
  else if (status.state === "waiting" && status.waiting !== null)
    reasons.push({
      reason: FOLLOWER_WAITING,
      detail: `${status.waiting.cause}: ${status.waiting.detail}`,
    });
  if (status.replaying)
    reasons.push({
      reason: FOLLOWER_TRACKED_SET_CHANGED,
      detail:
        "the store was reset (a tracked-set change, a missing tracked-set record, or reset --to-origin); replaying from the origin until the cursor reaches the node tip and the height it held before the reset",
    });
  if (status.prune.failures >= PRUNE_FAILING_AFTER)
    reasons.push({
      reason: FOLLOWER_PRUNE_FAILING,
      detail: `${status.prune.failures} prune passes in a row failed: ${status.prune.lastError ?? "unknown"}`,
    });
  if (!status.atTip)
    reasons.push({
      reason: FOLLOWER_CATCHING_UP,
      detail:
        status.tip === null
          ? "no node tip seen yet"
          : `cursor height ${status.cursor?.height ?? "none"}, node tip height ${status.tip.height}`,
    });
  return reasons;
};
