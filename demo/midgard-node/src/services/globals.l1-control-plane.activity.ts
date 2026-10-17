import { Effect, Metric, Ref } from "effect";

import type { Globals } from "./globals.globals.js";
import { DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS } from "./globals.next-l1-provider-health-evidence.js";

/** No hold, however it was extended, outlives this. */
export const L1_CONTROL_PLANE_HOLD_CEILING_MS = 900_000;
/** A holder still running this long past its deadline is wedged: its
 * interruption cannot complete, so the permit is never released. */
export const L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS = 60_000;
/** A waiter blocked for this many multiples of the largest hold in force
 * since it began waiting is wedged. */
export const L1_CONTROL_PLANE_WEDGED_WAIT_FACTOR = 3;
/** Consecutive hold timeouts in one scope that make it a liveness reason. */
export const L1_CONTROL_PLANE_HOLD_TIMEOUT_STREAK = 3;
/** A scope's hold-timeout streak stops being a liveness reason this many
 * multiples of its timed-out hold budget after its latest timeout, unless the
 * scope holds or waits for the permit. A scope entered only conditionally may
 * never hold again to reset its streak. */
export const L1_CONTROL_PLANE_HOLD_TIMEOUT_QUIET_FACTOR = 3;

export type L1ControlPlaneActivity = {
  readonly holder: {
    readonly scope: string;
    readonly sinceMs: number;
    readonly deadlineMs: number;
  } | null;
  readonly waiters: ReadonlyMap<number, L1ControlPlaneWaiter>;
  /**
   * The budget assumed for a hold that registers none, such as a raw
   * `L1_CONTROL_PLANE` permit. Such a hold can come at any time, so no
   * waiter's limit is sized below it.
   */
  readonly unregisteredHoldBudgetMs: number;
  readonly consecutiveHoldTimeouts: ReadonlyMap<string, number>;
  /** Per scope with a streak: until when it is reported with the scope
   * neither holding nor waiting. */
  readonly holdTimeoutQuietUntilMs: ReadonlyMap<string, number>;
};

export type L1ControlPlaneWaiter = {
  readonly scope: string;
  readonly sinceMs: number;
  /** The largest hold budget in force since this waiter began waiting. */
  readonly largestHoldMs: number;
};

export const initialL1ControlPlaneActivity = (): L1ControlPlaneActivity => ({
  holder: null,
  waiters: new Map(),
  unregisteredHoldBudgetMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  consecutiveHoldTimeouts: new Map(),
  holdTimeoutQuietUntilMs: new Map(),
});

/**
 * How long `waiter` may wait before it is wedged: several multiples of the
 * largest hold it has had to wait out. Sized per waiter, so one long hold
 * raises the limit only for the waiters queued behind it.
 */
export const l1ControlPlaneWaiterWedgeLimitMs = (
  activity: L1ControlPlaneActivity,
  waiter: L1ControlPlaneWaiter,
): number =>
  L1_CONTROL_PLANE_WEDGED_WAIT_FACTOR *
  Math.max(activity.unregisteredHoldBudgetMs, waiter.largestHoldMs);

/** Records a hold budget now in force against every current waiter. */
export const noteHoldBudget = (
  activity: L1ControlPlaneActivity,
  budgetMs: number,
): L1ControlPlaneActivity => {
  const waiters = new Map<number, L1ControlPlaneWaiter>();
  for (const [id, waiter] of activity.waiters) {
    waiters.set(
      id,
      budgetMs > waiter.largestHoldMs
        ? { ...waiter, largestHoldMs: budgetMs }
        : waiter,
    );
  }
  return { ...activity, waiters };
};

export const l1ControlPlaneHoldBudgetGauge = Metric.gauge(
  "l1_control_plane_hold_budget_ms",
  { description: "Hold budget of the latest L1 control-plane hold, by scope" },
);

export const l1ControlPlaneWedgedGauge = Metric.gauge(
  "l1_control_plane_wedged",
  {
    description:
      "1 while an L1 control-plane holder runs past its deadline plus grace, or a waiter waits past its wedge limit",
  },
);

/**
 * The liveness reasons the L1 control plane raises at `nowMs`: a holder whose
 * interruption has not completed long after its deadline, a waiter blocked
 * for several multiples of the largest hold, and a scope whose holds keep
 * timing out, while it holds, waits, or timed out recently.
 */
export const l1ControlPlaneLivenessReasons = (
  activity: L1ControlPlaneActivity,
  nowMs: number,
): readonly string[] => {
  const reasons: string[] = [];
  const holder = activity.holder;
  if (
    holder !== null &&
    nowMs > holder.deadlineMs + L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS
  ) {
    reasons.push(
      `l1_control_plane_wedged:holder=${holder.scope}:overrun_ms=${(nowMs - holder.deadlineMs).toString()}`,
    );
  }
  let oldest: L1ControlPlaneWaiter | undefined;
  for (const waiter of activity.waiters.values()) {
    if (
      nowMs - waiter.sinceMs >
        l1ControlPlaneWaiterWedgeLimitMs(activity, waiter) &&
      (oldest === undefined || waiter.sinceMs < oldest.sinceMs)
    ) {
      oldest = waiter;
    }
  }
  if (oldest !== undefined) {
    reasons.push(
      `l1_control_plane_wedged:waiter=${oldest.scope}:wait_ms=${(nowMs - oldest.sinceMs).toString()}`,
    );
  }
  const active = new Set<string>(
    [...activity.waiters.values()].map((waiter) => waiter.scope),
  );
  if (holder !== null) active.add(holder.scope);
  for (const [scope, count] of activity.consecutiveHoldTimeouts) {
    if (
      count >= L1_CONTROL_PLANE_HOLD_TIMEOUT_STREAK &&
      (active.has(scope) ||
        nowMs <= (activity.holdTimeoutQuietUntilMs.get(scope) ?? -Infinity))
    ) {
      reasons.push(
        `l1_control_plane_hold_timeouts:${scope}:${count.toString()}`,
      );
    }
  }
  return reasons;
};

export const l1ControlPlaneHoldTimeoutStreak = (
  globals: Globals,
  scope: string,
): Effect.Effect<number> =>
  Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY).pipe(
    Effect.map((activity) => activity.consecutiveHoldTimeouts.get(scope) ?? 0),
  );
