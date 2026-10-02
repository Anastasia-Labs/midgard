import {
  Cause,
  Duration,
  Effect,
  Exit,
  Fiber,
  FiberRef,
  Metric,
  Option,
  Ref,
} from "effect";

import type { Globals } from "./globals.globals.js";
import {
  DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  l1ControlPlaneAcquisitionCounter,
  l1ControlPlaneHoldTimer,
  l1ControlPlaneTimeoutCounter,
  L1ControlPlaneTimeoutError,
  l1ControlPlaneWaitTimer,
} from "./globals.next-l1-provider-health-evidence.js";

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

export type L1ControlPlaneActivity = {
  readonly holder: {
    readonly scope: string;
    readonly sinceMs: number;
    readonly deadlineMs: number;
  } | null;
  readonly waiters: ReadonlyMap<number, L1ControlPlaneWaiter>;
  /**
   * The budget assumed for a hold that registers none: those
   * `withL1ControlPlaneWaitTimeout` and `withL1ControlPlaneIfAvailable` take.
   * Such a hold can come at any time, so no waiter's limit is sized below it.
   */
  readonly unregisteredHoldBudgetMs: number;
  readonly consecutiveHoldTimeouts: ReadonlyMap<string, number>;
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
const noteHoldBudget = (
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

const l1ControlPlaneHoldBudgetGauge = Metric.gauge(
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
 * timing out.
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
  for (const [scope, count] of activity.consecutiveHoldTimeouts) {
    if (count >= L1_CONTROL_PLANE_HOLD_TIMEOUT_STREAK) {
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

type HoldHandle = {
  readonly globals: Globals;
  readonly scope: string;
  readonly startedAtMs: number;
  readonly deadlineMs: Ref.Ref<number>;
};

const CurrentL1ControlPlaneHold = FiberRef.unsafeMake<HoldHandle | undefined>(
  undefined,
);

/**
 * Raises the current hold's budget to `totalHoldMs` from its start, capped at
 * `L1_CONTROL_PLANE_HOLD_CEILING_MS`; it never shortens it. Work that learns
 * its own size once it holds the permit calls this before starting, so its
 * cap fits the work instead of a constant. Returns the resulting budget, or
 * undefined outside a `withL1ControlPlane` hold.
 */
export const extendL1ControlPlaneHold = (
  totalHoldMs: number,
): Effect.Effect<number | undefined> =>
  Effect.gen(function* () {
    const hold = yield* FiberRef.get(CurrentL1ControlPlaneHold);
    if (hold === undefined) return undefined;
    const requested =
      hold.startedAtMs +
      Math.min(Math.max(0, totalHoldMs), L1_CONTROL_PLANE_HOLD_CEILING_MS);
    const deadlineMs = yield* Ref.updateAndGet(hold.deadlineMs, (current) =>
      Math.max(current, requested),
    );
    yield* Ref.update(hold.globals.L1_CONTROL_PLANE_ACTIVITY, (activity) => ({
      ...noteHoldBudget(activity, deadlineMs - hold.startedAtMs),
      holder:
        activity.holder?.sinceMs === hold.startedAtMs
          ? { ...activity.holder, deadlineMs }
          : activity.holder,
    }));
    const budgetMs = deadlineMs - hold.startedAtMs;
    yield* Metric.tagged(
      l1ControlPlaneHoldBudgetGauge,
      "scope",
      hold.scope,
    )(Effect.succeed(budgetMs));
    return budgetMs;
  });

const awaitDeadline = (deadlineMs: Ref.Ref<number>): Effect.Effect<void> =>
  Effect.gen(function* () {
    while (true) {
      const remainingMs = (yield* Ref.get(deadlineMs)) - Date.now();
      if (remainingMs <= 0) return;
      yield* Effect.sleep(Duration.millis(remainingMs));
    }
  });

let nextWaiterId = 0;

const updateActivity = (
  globals: Globals,
  f: (activity: L1ControlPlaneActivity) => L1ControlPlaneActivity,
) => Ref.update(globals.L1_CONTROL_PLANE_ACTIVITY, f);

const withoutWaiter = (activity: L1ControlPlaneActivity, waiterId: number) => {
  if (!activity.waiters.has(waiterId)) return activity;
  const waiters = new Map(activity.waiters);
  waiters.delete(waiterId);
  return { ...activity, waiters };
};

const recordHoldExit = (
  current: L1ControlPlaneActivity,
  scope: string,
  holdStartedAtMs: number,
  exit: Exit.Exit<unknown, unknown>,
): L1ControlPlaneActivity => {
  const activity =
    current.holder?.sinceMs === holdStartedAtMs
      ? { ...current, holder: null }
      : current;
  const timedOut =
    Exit.isFailure(exit) &&
    Option.exists(
      Cause.failureOption(exit.cause),
      (error) => error instanceof L1ControlPlaneTimeoutError,
    );
  // Interrupted from outside, the hold says nothing about its own budget.
  if (!timedOut && Exit.isInterrupted(exit)) return activity;
  const consecutiveHoldTimeouts = new Map(activity.consecutiveHoldTimeouts);
  if (timedOut) {
    consecutiveHoldTimeouts.set(
      scope,
      (consecutiveHoldTimeouts.get(scope) ?? 0) + 1,
    );
  } else {
    consecutiveHoldTimeouts.delete(scope);
  }
  return { ...activity, consecutiveHoldTimeouts };
};

/**
 * Logs once, and raises the wedged gauge, when the holder is still running
 * `L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS` past its (possibly extended)
 * deadline. The hold itself keeps waiting for its interruption to finish:
 * the permit is never released under a holder that may still be running.
 */
const holderOverrunWatchdog = (scope: string, deadlineMs: Ref.Ref<number>) =>
  Effect.gen(function* () {
    while (true) {
      yield* awaitDeadline(deadlineMs);
      yield* Effect.sleep(
        Duration.millis(L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS),
      );
      const deadline = yield* Ref.get(deadlineMs);
      if (Date.now() >= deadline + L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS) {
        break;
      }
    }
    yield* l1ControlPlaneWedgedGauge(Effect.succeed(1));
    yield* Effect.logError(
      `L1 control plane wedged: scope ${scope} is still running ${L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS.toString()} ms past its hold deadline; its interruption has not completed, so every other L1 scope is blocked.`,
    );
  });

/**
 * Raises the wedged gauge and logs once when this waiter has waited past its
 * wedge limit, naming the holder, or an unregistered one. It catches the
 * wedges the holder watchdog cannot see: a hold taken without registering,
 * whose interruption never completes. It resets the gauge when interrupted,
 * which happens once the waiter acquires the permit or stops waiting.
 */
const waiterWedgeWatchdog = (
  globals: Globals,
  waiterId: number,
  scope: string,
) =>
  Effect.gen(function* () {
    while (true) {
      const activity = yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY);
      const waiter = activity.waiters.get(waiterId);
      if (waiter === undefined) return;
      const waitedMs = Date.now() - waiter.sinceMs;
      const limitMs = l1ControlPlaneWaiterWedgeLimitMs(activity, waiter);
      if (waitedMs > limitMs) {
        const holder =
          activity.holder === null
            ? "an unregistered holder"
            : `scope ${activity.holder.scope}`;
        yield* l1ControlPlaneWedgedGauge(Effect.succeed(1));
        yield* Effect.logError(
          `L1 control plane wedged: scope ${scope} has waited ${waitedMs.toString()} ms for the permit, past its ${limitMs.toString()} ms wedge limit; it is held by ${holder}.`,
        );
        return yield* Effect.never.pipe(
          Effect.onInterrupt(() =>
            l1ControlPlaneWedgedGauge(Effect.succeed(0)),
          ),
        );
      }
      yield* Effect.sleep(Duration.millis(limitMs - waitedMs + 1));
    }
  });

export const withL1ControlPlane = <A, E, R>(
  globals: Globals,
  options: {
    readonly scope: string;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | Error, R> => {
  const maxHoldMs = Math.min(
    options.maxHoldMs ?? DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
    L1_CONTROL_PLANE_HOLD_CEILING_MS,
  );
  const scope = options.scope;
  const waitTimer = Metric.tagged(l1ControlPlaneWaitTimer, "scope", scope);
  const holdTimer = Metric.tagged(l1ControlPlaneHoldTimer, "scope", scope);
  const acquisitionCounter = Metric.tagged(
    l1ControlPlaneAcquisitionCounter,
    "scope",
    scope,
  );
  const timeoutCounter = Metric.tagged(
    l1ControlPlaneTimeoutCounter,
    "scope",
    scope,
  );
  const held = (
    waiterId: number,
    waitStartedAtMs: number,
    waiterWatchdog: Fiber.RuntimeFiber<void>,
  ) =>
    Effect.uninterruptibleMask((restore) =>
      Effect.gen(function* () {
        yield* Fiber.interrupt(waiterWatchdog);
        const holdStartedAtMs = Date.now();
        const deadlineMs = yield* Ref.make(holdStartedAtMs + maxHoldMs);
        yield* updateActivity(globals, (activity) => ({
          ...noteHoldBudget(withoutWaiter(activity, waiterId), maxHoldMs),
          holder: {
            scope,
            sinceMs: holdStartedAtMs,
            deadlineMs: holdStartedAtMs + maxHoldMs,
          },
        }));
        // Forked here it would inherit the mask, and interrupting it would
        // wait out its whole grace period.
        const watchdog = yield* Effect.forkDaemon(
          Effect.interruptible(holderOverrunWatchdog(scope, deadlineMs)),
        );
        const body = Effect.gen(function* () {
          yield* waitTimer(
            Effect.succeed(Duration.millis(holdStartedAtMs - waitStartedAtMs)),
          );
          yield* Metric.increment(acquisitionCounter);
          yield* Metric.tagged(
            l1ControlPlaneHoldBudgetGauge,
            "scope",
            scope,
          )(Effect.succeed(maxHoldMs));
          return yield* Effect.raceFirst(
            effect,
            awaitDeadline(deadlineMs).pipe(
              Effect.zipRight(
                Effect.suspend(() =>
                  Ref.get(deadlineMs).pipe(
                    Effect.flatMap((deadline) =>
                      Effect.fail(
                        new L1ControlPlaneTimeoutError(
                          scope,
                          deadline - holdStartedAtMs,
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          );
        }).pipe(
          Effect.locally(CurrentL1ControlPlaneHold, {
            globals,
            scope,
            startedAtMs: holdStartedAtMs,
            deadlineMs,
          }),
        );
        return yield* restore(body).pipe(
          Effect.tapError((error) =>
            error instanceof L1ControlPlaneTimeoutError
              ? Metric.increment(timeoutCounter)
              : Effect.void,
          ),
          Effect.onExit((exit) =>
            Effect.gen(function* () {
              yield* Fiber.interrupt(watchdog);
              yield* l1ControlPlaneWedgedGauge(Effect.succeed(0));
              yield* updateActivity(globals, (activity) =>
                recordHoldExit(activity, scope, holdStartedAtMs, exit),
              );
              yield* holdTimer(
                Effect.succeed(Duration.millis(Date.now() - holdStartedAtMs)),
              );
            }),
          ),
        );
      }),
    );
  return Effect.uninterruptibleMask((restore) =>
    Effect.gen(function* () {
      const waitStartedAtMs = Date.now();
      const waiterId = nextWaiterId++;
      yield* updateActivity(globals, (activity) => {
        const waiters = new Map(activity.waiters);
        waiters.set(waiterId, {
          scope,
          sinceMs: waitStartedAtMs,
          largestHoldMs:
            activity.holder === null
              ? 0
              : activity.holder.deadlineMs - activity.holder.sinceMs,
        });
        return { ...activity, waiters };
      });
      // Forked here it would inherit the mask; see the holder watchdog.
      const waiterWatchdog = yield* Effect.forkDaemon(
        Effect.interruptible(waiterWedgeWatchdog(globals, waiterId, scope)),
      );
      return yield* restore(
        globals.L1_CONTROL_PLANE.withPermits(1)(
          held(waiterId, waitStartedAtMs, waiterWatchdog),
        ),
      ).pipe(
        Effect.ensuring(
          Fiber.interrupt(waiterWatchdog).pipe(
            Effect.zipRight(
              updateActivity(globals, (activity) =>
                withoutWaiter(activity, waiterId),
              ),
            ),
          ),
        ),
      );
    }),
  );
};
