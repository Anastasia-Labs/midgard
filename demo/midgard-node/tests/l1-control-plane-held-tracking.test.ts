/**
 * The scheduled merge (`withL1ControlPlaneWaitTimeout`) and the signed-intent
 * rebroadcast (`withL1ControlPlaneIfAvailable`) hold the same permit as every
 * other L1 scope. Their holds must be as visible to the control-plane activity
 * as a `withL1ControlPlane` hold: registered as the holder, wedged when they
 * overrun their deadline, counted in the hold-timeout streak, and able to
 * extend their own budget.
 */
import { Deferred, Effect, Exit, Fiber, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  Globals,
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
