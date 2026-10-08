import type { L1NodeTransport } from "@al-ft/l1-node-transport";

import type { OriginConfig } from "../origin.js";
import type { FactStore } from "../store/fact-store.js";
import type { InterventionReason } from "../types.js";

/** The cursor is not known to be at the node's tip (before the first event too). */
export const FOLLOWER_CATCHING_UP = "l1_follower_catching_up";
/** A transient failure is being backed off from; the detail names its cause. */
export const FOLLOWER_WAITING = "l1_follower_waiting";
/** One point keeps failing to apply; an operator has to look. */
export const FOLLOWER_APPLY_STUCK = "l1_follower_apply_stuck";

/** A named reason a role's `/readyz` reports while the follower holds it unready. */
export type FollowReadinessReason =
  | InterventionReason
  | typeof FOLLOWER_CATCHING_UP
  | typeof FOLLOWER_WAITING
  | typeof FOLLOWER_APPLY_STUCK;

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
  /** The node tip the latest applied event reported. */
  tip: Readonly<{ slot: number; height: number }> | null;
  /** The cursor equals that tip. */
  atTip: boolean;
  /** Events applied by this loop. */
  events: number;
  /** The latest failure, cleared by the next applied event. */
  lastError: string | null;
  prune: Readonly<{
    steps: number;
    prunedThroughSlot: number | null;
    lastError: string | null;
    /** Slots a role's prune floor holds the boundary back (null: none). */
    floorLagSlots: number | null;
  }>;
}>;

export type FollowChainOptions = Readonly<{
  store: FactStore;
  transport: Pick<L1NodeTransport, "openChainSync">;
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
}>;

export const DEFAULT_STUCK_AFTER = 5;
export const LOOP_PRUNE_BUDGET = 500;
export const LOOP_PRUNE_EVERY = 100;

/** The readiness reasons a status implies. */
export const readinessOf = (
  status: Omit<FollowStatus, "readiness">,
): FollowReadiness[] => {
  const reasons: FollowReadiness[] = status.interventions.map((i) => ({
    reason: i.reason,
    detail: i.detail,
  }));
  if (status.stuck !== null)
    reasons.push({
      reason: FOLLOWER_APPLY_STUCK,
      detail: `${status.stuck.at} failed ${status.stuck.failures} times: ${status.stuck.detail}`,
    });
  else if (status.state === "waiting" && status.waiting !== null)
    reasons.push({
      reason: FOLLOWER_WAITING,
      detail: `${status.waiting.cause}: ${status.waiting.detail}`,
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
