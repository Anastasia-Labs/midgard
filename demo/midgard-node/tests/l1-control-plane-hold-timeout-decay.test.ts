/**
 * The derived `l1_control_plane_hold_timeouts:<scope>` reason must not stay on
 * /readyz for a scope that is never entered again: only a later clean hold of
 * the same scope resets its streak, and a scope entered only conditionally
 * (the operator watchdog's strike) may never hold again. A scope that keeps
 * timing out, or that is holding or waiting for the permit, keeps it.
 */
import { Deferred, Effect, Fiber } from "effect";
import { describe, expect, it } from "vitest";

import { Globals, withL1ControlPlane } from "../src/services/globals.js";
import {
  L1_CONTROL_PLANE_HOLD_TIMEOUT_QUIET_FACTOR,
  l1ControlPlaneHoldTimeoutStreak,
} from "../src/services/globals.l1-control-plane.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";

const runWithGlobals = <A, E>(effect: Effect.Effect<A, E, Globals>) =>
  Effect.runPromise(effect.pipe(Effect.provide(Globals.Default)));

const HOLD_MS = 50;
const QUIET_MS = L1_CONTROL_PLANE_HOLD_TIMEOUT_QUIET_FACTOR * HOLD_MS;

const timeoutReasons = (reasons: readonly string[]) =>
  reasons.filter((reason) =>
    reason.startsWith("l1_control_plane_hold_timeouts:slow_scope:"),
  );

const timedOutHold = (globals: Globals) =>
  withL1ControlPlane(
    globals,
    { scope: "slow_scope", maxHoldMs: HOLD_MS },
    Effect.sleep(HOLD_MS * 10),
  ).pipe(Effect.exit);

describe("L1 control-plane hold-timeout streak decay", () => {
  it("stops publishing the streak after a quiet period with the scope never entered again, and publishes it again on the next timeout", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* timedOutHold(globals);
        yield* timedOutHold(globals);
        yield* timedOutHold(globals);
        const thirdAt = Date.now();
        const recent = yield* currentLivenessReasons(globals, thirdAt);
        const quiet = yield* currentLivenessReasons(
          globals,
          thirdAt + QUIET_MS + 1_000,
        );
        // The streak itself, which sizes the commit hold budget, is kept.
        const streakWhileQuiet = yield* l1ControlPlaneHoldTimeoutStreak(
          globals,
          "slow_scope",
        );
        yield* timedOutHold(globals);
        const again = yield* currentLivenessReasons(globals, Date.now());
        return { recent, quiet, streakWhileQuiet, again };
      }),
    );
    expect(timeoutReasons(outcome.recent)).toEqual([
      "l1_control_plane_hold_timeouts:slow_scope:3",
    ]);
    expect(timeoutReasons(outcome.quiet)).toEqual([]);
    expect(outcome.streakWhileQuiet).toBe(3);
    expect(timeoutReasons(outcome.again)).toEqual([
      "l1_control_plane_hold_timeouts:slow_scope:4",
    ]);
  });

  it("keeps publishing the streak while the scope keeps timing out", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const samples: string[][] = [];
        for (let hold = 0; hold < 6; hold += 1) {
          yield* timedOutHold(globals);
          samples.push([
            ...timeoutReasons(yield* currentLivenessReasons(globals)),
          ]);
        }
        return samples;
      }),
    );
    expect(outcome).toEqual([
      [],
      [],
      ["l1_control_plane_hold_timeouts:slow_scope:3"],
      ["l1_control_plane_hold_timeouts:slow_scope:4"],
      ["l1_control_plane_hold_timeouts:slow_scope:5"],
      ["l1_control_plane_hold_timeouts:slow_scope:6"],
    ]);
  });

  it("keeps publishing the streak past the quiet period while the scope holds or waits for the permit", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* timedOutHold(globals);
        yield* timedOutHold(globals);
        yield* timedOutHold(globals);
        const farFuture = Date.now() + QUIET_MS + 1_000;

        const entered = yield* Deferred.make<void>();
        const holding = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "slow_scope", maxHoldMs: 600_000 },
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Effect.never),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const whileHolding = yield* currentLivenessReasons(globals, farFuture);
        yield* Fiber.interrupt(holding);

        const otherEntered = yield* Deferred.make<void>();
        const other = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "other_scope", maxHoldMs: 600_000 },
            Deferred.succeed(otherEntered, undefined).pipe(
              Effect.zipRight(Effect.never),
            ),
          ),
        );
        yield* Deferred.await(otherEntered);
        const waiting = yield* Effect.fork(
          withL1ControlPlane(globals, { scope: "slow_scope" }, Effect.void),
        );
        yield* Effect.sleep(20);
        const whileWaiting = yield* currentLivenessReasons(globals, farFuture);
        yield* Fiber.interrupt(waiting);
        yield* Fiber.interrupt(other);
        const idle = yield* currentLivenessReasons(globals, farFuture);
        return { whileHolding, whileWaiting, idle };
      }),
    );
    expect(timeoutReasons(outcome.whileHolding)).toEqual([
      "l1_control_plane_hold_timeouts:slow_scope:3",
    ]);
    expect(timeoutReasons(outcome.whileWaiting)).toEqual([
      "l1_control_plane_hold_timeouts:slow_scope:3",
    ]);
    expect(timeoutReasons(outcome.idle)).toEqual([]);
  });
});
