import { performance } from "node:perf_hooks";

/**
 * The scheduled merge (`withL1ControlPlaneWaitTimeout`) and the signed-intent
 * rebroadcast (`withL1ControlPlaneIfAvailable`) hold the same permit as every
 * other L1 scope. Their holds must be as visible to the control-plane activity
 * as a `withL1ControlPlane` hold: registered as the holder, wedged when they
 * overrun their deadline, counted in the hold-timeout streak, and able to
 * extend their own budget.
 */
import { Deferred, Effect, Exit, Fiber, Option, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  Globals,
  withL1ControlPlane,
  withL1ControlPlaneIfAvailable,
  withL1ControlPlaneWaitTimeout,
} from "../src/services/globals.js";
import {
  extendL1ControlPlaneHold,
  L1_CONTROL_PLANE_HOLD_TIMEOUT_STREAK,
  L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS,
} from "../src/services/globals.l1-control-plane.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";

const runWithGlobals = <A, E>(effect: Effect.Effect<A, E, Globals>) =>
  Effect.runPromise(effect.pipe(Effect.provide(Globals.Default)));

type Hold = <A, E>(
  globals: Globals,
  maxHoldMs: number,
  effect: Effect.Effect<A, E>,
) => Effect.Effect<unknown, unknown>;

const holds: ReadonlyArray<readonly [string, Hold]> = [
  [
    "signed_intent_rebroadcast",
    (globals, maxHoldMs, effect) =>
      withL1ControlPlaneIfAvailable(
        globals,
        { scope: "signed_intent_rebroadcast", maxHoldMs },
        effect,
      ),
  ],
  [
    "state_queue_merge",
    (globals, maxHoldMs, effect) =>
      withL1ControlPlaneWaitTimeout(
        globals,
        { scope: "state_queue_merge", waitTimeoutMs: 1_000, maxHoldMs },
        effect,
      ),
  ],
];

describe.each(holds)("the %s hold", (scope, hold) => {
  it("registers as the holder, and is a wedge reason once it overruns its deadline", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const holding = yield* Effect.fork(
          hold(
            globals,
            10_000,
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Deferred.await(release)),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const activity = yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY);
        const overrun = yield* currentLivenessReasons(
          globals,
          Date.now() +
            10_000 +
            L1_CONTROL_PLANE_HOLDER_OVERRUN_GRACE_MS +
            1_000,
        );
        yield* Deferred.succeed(release, undefined);
        yield* Fiber.join(holding);
        return {
          holder: activity.holder?.scope,
          overrun,
          after: (yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY)).holder,
        };
      }),
    );
    expect(outcome.holder).toBe(scope);
    expect(
      outcome.overrun.some((reason) =>
        reason.startsWith(`l1_control_plane_wedged:holder=${scope}:`),
      ),
    ).toBe(true);
    expect(outcome.after).toBeNull();
  });

  it("counts its hold timeouts in the streak readiness reports, and a clean hold clears it", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const exits: Exit.Exit<unknown, unknown>[] = [];
        for (let n = 0; n < L1_CONTROL_PLANE_HOLD_TIMEOUT_STREAK; n += 1)
          exits.push(
            yield* Effect.exit(hold(globals, 20, Effect.sleep("2 seconds"))),
          );
        const streak = yield* currentLivenessReasons(globals);
        yield* hold(globals, 1_000, Effect.void);
        return {
          exits,
          streak,
          cleared: yield* currentLivenessReasons(globals),
        };
      }),
    );
    expect(outcome.exits.every(Exit.isFailure)).toBe(true);
    expect(outcome.streak).toContain(
      `l1_control_plane_hold_timeouts:${scope}:${L1_CONTROL_PLANE_HOLD_TIMEOUT_STREAK.toString()}`,
    );
    expect(outcome.cleared).toEqual([]);
  });

  it("lets its work extend its own hold budget", async () => {
    const granted = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        let budget: number | undefined;
        yield* hold(
          globals,
          1_000,
          extendL1ControlPlaneHold(5_000).pipe(
            Effect.map((ms) => {
              budget = ms;
            }),
          ),
        );
        return budget;
      }),
    );
    expect(granted).toBe(5_000);
  });
});

const withWallStep = async <A>(
  offset: number,
  run: (started: () => void) => Promise<A>,
): Promise<A> => {
  const realNow = Date.now;
  let startedAt = Infinity;
  const wall = vi
    .spyOn(Date, "now")
    .mockImplementation(
      () => realNow() + (performance.now() - startedAt >= 5 ? offset : 0),
    );
  try {
    return await run(() => {
      startedAt = performance.now();
    });
  } finally {
    wall.mockRestore();
  }
};

const clockHolds: ReadonlyArray<readonly [string, Hold]> = [
  [
    "ordinary",
    (globals, maxHoldMs, work) =>
      withL1ControlPlane(globals, { scope: "ordinary", maxHoldMs }, work),
  ],
  ...holds,
];

describe.each(clockHolds)("monotonic %s hold", (_scope, hold) => {
  it.each([-60_000, 0, 60_000])(
    "times out its actual20ms hold and admits the waiter despite wall step%s",
    async (offset) => {
      const result = await withWallStep(offset, (started) =>
        runWithGlobals(
          Effect.gen(function* () {
            const globals = yield* Globals;
            const entered = yield* Deferred.make<void>();
            let ended = false,
              competitorEntered = false;
            const start = performance.now();
            const holder = yield* Effect.fork(
              hold(
                globals,
                20,
                Effect.sync(started).pipe(
                  Effect.zipRight(Deferred.succeed(entered, undefined)),
                  Effect.zipRight(Effect.never),
                ),
              ).pipe(
                Effect.exit,
                Effect.tap(() =>
                  Effect.sync(() => {
                    ended = true;
                  }),
                ),
              ),
            );
            yield* Deferred.await(entered);
            const registered = (yield* Ref.get(
              globals.L1_CONTROL_PLANE_ACTIVITY,
            )).holder;
            const waiter = yield* Effect.fork(
              withL1ControlPlane(
                globals,
                { scope: "clock_waiter", maxHoldMs: 1000 },
                Effect.sync(() => {
                  competitorEntered = true;
                }),
              ),
            );
            yield* Effect.sleep(150);
            const trace = {
              ended,
              competitorEntered,
              elapsed: performance.now() - start,
              diagnosticBudget:
                registered === null
                  ? undefined
                  : registered.deadlineMs - registered.sinceMs,
            };
            yield* Fiber.interrupt(waiter);
            yield* Fiber.interrupt(holder);
            const reacquired = yield* withL1ControlPlane(
              globals,
              { scope: "clock_retry", maxHoldMs: 1000 },
              Effect.succeed(true),
            );
            return {
              ...trace,
              reacquired,
              after: (yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY)).holder,
            };
          }),
        ),
      );
      expect(result.ended).toBe(true);
      expect(result.competitorEntered).toBe(true);
      expect(result.elapsed).toBeLessThan(700);
      expect(result.diagnosticBudget).toBe(20);
      expect(result.reacquired).toBe(true);
      expect(result.after).toBeNull();
    },
  );

  it.each([-60_000, 60_000])(
    "retains the extended budget under wall step%s",
    async (offset) => {
      const result = await withWallStep(offset, (started) =>
        runWithGlobals(
          Effect.gen(function* () {
            const globals = yield* Globals;
            let granted: number | undefined,
              finished = false;
            const start = performance.now();
            const exit = yield* Effect.exit(
              hold(
                globals,
                20,
                Effect.gen(function* () {
                  started();
                  yield* Effect.sleep(10);
                  granted = yield* extendL1ControlPlaneHold(100);
                  yield* Effect.sleep(50);
                  finished = true;
                }),
              ),
            );
            return {
              exit,
              granted,
              finished,
              elapsed: performance.now() - start,
            };
          }),
        ),
      );
      expect(Exit.isSuccess(result.exit)).toBe(true);
      expect(result.granted).toBe(100);
      expect(result.finished).toBe(true);
      expect(result.elapsed).toBeGreaterThanOrEqual(45);
    },
  );
});

it("keeps the waiter excluded until timed-out work finishes its cleanup", async () => {
  const result = await withWallStep(-60_000, (started) =>
    runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const entered = yield* Deferred.make<void>();
        const cleanupEntered = yield* Deferred.make<void>();
        const releaseCleanup = yield* Deferred.make<void>();
        let cleanupFinished = false,
          waiterEntered = false;
        const holder = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "cleanup_holder", maxHoldMs: 20 },
            Effect.sync(started).pipe(
              Effect.zipRight(Deferred.succeed(entered, undefined)),
              Effect.zipRight(Effect.never),
              Effect.ensuring(
                Deferred.succeed(cleanupEntered, undefined).pipe(
                  Effect.zipRight(Deferred.await(releaseCleanup)),
                  Effect.tap(() =>
                    Effect.sync(() => {
                      cleanupFinished = true;
                    }),
                  ),
                ),
              ),
            ),
          ).pipe(Effect.exit),
        );
        yield* Deferred.await(entered);
        const waiter = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "cleanup_waiter", maxHoldMs: 1000 },
            Effect.sync(() => {
              waiterEntered = true;
            }),
          ),
        );
        const cleanupStarted = yield* Deferred.await(cleanupEntered).pipe(
          Effect.timeoutOption(150),
        );
        const during = {
          cleanupFinished,
          waiterEntered,
          holder: (yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY)).holder
            ?.scope,
        };
        // Always release our own cleanup before joining/interruption, including RED.
        yield* Deferred.succeed(releaseCleanup, undefined);
        yield* Fiber.interrupt(holder);
        yield* Fiber.join(waiter);
        return { cleanupStarted, during, cleanupFinished, waiterEntered };
      }),
    ),
  );
  expect(Option.isSome(result.cleanupStarted)).toBe(true);
  expect(result.during).toMatchObject({
    cleanupFinished: false,
    waiterEntered: false,
    holder: "cleanup_holder",
  });
  expect(result.cleanupFinished).toBe(true);
  expect(result.waiterEntered).toBe(true);
});
