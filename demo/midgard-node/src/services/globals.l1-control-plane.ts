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
  L1_CONTROL_PLANE_HOLD_CEILING_MS,
  L1_CONTROL_PLANE_HOLD_TIMEOUT_QUIET_FACTOR,
  L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS,
  type L1ControlPlaneActivity,
  l1ControlPlaneHoldBudgetGauge,
  l1ControlPlaneWaiterWedgeLimitMs,
  l1ControlPlaneWedgedGauge,
  noteHoldBudget,
} from "./globals.l1-control-plane.activity.js";
import {
  DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  l1ControlPlaneAcquisitionCounter,
  l1ControlPlaneHoldTimer,
  l1ControlPlaneTimeoutCounter,
  L1ControlPlaneTimeoutError,
  l1ControlPlaneWaitTimer,
} from "./globals.next-l1-provider-health-evidence.js";

export {
  initialL1ControlPlaneActivity,
  L1_CONTROL_PLANE_HOLD_CEILING_MS,
  L1_CONTROL_PLANE_HOLD_TIMEOUT_QUIET_FACTOR,
  L1_CONTROL_PLANE_HOLD_TIMEOUT_STREAK,
  L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS,
  L1_CONTROL_PLANE_WEDGED_WAIT_FACTOR,
  type L1ControlPlaneActivity,
  l1ControlPlaneHoldTimeoutStreak,
  l1ControlPlaneLivenessReasons,
  type L1ControlPlaneWaiter,
  l1ControlPlaneWaiterWedgeLimitMs,
  l1ControlPlaneWedgedGauge,
} from "./globals.l1-control-plane.activity.js";

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
  holdBudgetMs: number,
  exit: Exit.Exit<unknown, unknown>,
  nowMs: number,
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
  const holdTimeoutQuietUntilMs = new Map(activity.holdTimeoutQuietUntilMs);
  if (timedOut) {
    consecutiveHoldTimeouts.set(
      scope,
      (consecutiveHoldTimeouts.get(scope) ?? 0) + 1,
    );
    holdTimeoutQuietUntilMs.set(
      scope,
      nowMs + L1_CONTROL_PLANE_HOLD_TIMEOUT_QUIET_FACTOR * holdBudgetMs,
    );
  } else {
    consecutiveHoldTimeouts.delete(scope);
    holdTimeoutQuietUntilMs.delete(scope);
  }
  return { ...activity, consecutiveHoldTimeouts, holdTimeoutQuietUntilMs };
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

/**
 * Runs `effect` as the registered holder of the permit, which the caller
 * already holds: the holder entry and overrun watchdog the wedge reasons
 * read, the deadline `extendL1ControlPlaneHold` raises, and the hold-timeout
 * streak. `waiter` is the caller's own waiter entry, retired on acquisition.
 */
export const runRegisteredL1ControlPlaneHold = <A, E, R>(
  globals: Globals,
  options: {
    readonly scope: string;
    readonly maxHoldMs?: number;
  },
  waitStartedAtMs: number,
  effect: Effect.Effect<A, E, R>,
  waiter?: {
    readonly id: number;
    readonly watchdog: Fiber.RuntimeFiber<void>;
  },
): Effect.Effect<A, E | L1ControlPlaneTimeoutError, R> => {
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
  return Effect.uninterruptibleMask((restore) =>
    Effect.gen(function* () {
      if (waiter !== undefined) yield* Fiber.interrupt(waiter.watchdog);
      const holdStartedAtMs = Date.now();
      const deadlineMs = yield* Ref.make(holdStartedAtMs + maxHoldMs);
      yield* updateActivity(globals, (activity) => ({
        ...noteHoldBudget(
          waiter === undefined ? activity : withoutWaiter(activity, waiter.id),
          maxHoldMs,
        ),
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
            const holdBudgetMs = (yield* Ref.get(deadlineMs)) - holdStartedAtMs;
            yield* updateActivity(globals, (activity) =>
              recordHoldExit(
                activity,
                scope,
                holdStartedAtMs,
                holdBudgetMs,
                exit,
                Date.now(),
              ),
            );
            yield* holdTimer(
              Effect.succeed(Duration.millis(Date.now() - holdStartedAtMs)),
            );
          }),
        ),
      );
    }),
  );
};

export const withL1ControlPlane = <A, E, R>(
  globals: Globals,
  options: {
    readonly scope: string;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | Error, R> => {
  const scope = options.scope;
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
          runRegisteredL1ControlPlaneHold(
            globals,
            options,
            waitStartedAtMs,
            effect,
            { id: waiterId, watchdog: waiterWatchdog },
          ),
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
