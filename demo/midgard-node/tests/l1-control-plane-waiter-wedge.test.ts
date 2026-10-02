import { Deferred, Effect, Fiber, Logger, Metric, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  Globals,
  withL1ControlPlane,
  withL1ControlPlaneWaitTimeout,
} from "../src/services/globals.js";
import { l1ControlPlaneWedgedGauge } from "../src/services/globals.l1-control-plane.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";

/** Runs `effect` with error logs captured into `errors`. */
const runCapturingErrors = <A, E>(
  errors: string[],
  effect: Effect.Effect<A, E, Globals>,
) =>
  Effect.runPromise(
    effect.pipe(
      Effect.provide(Globals.Default),
      Effect.provide(
        Logger.replace(
          Logger.defaultLogger,
          Logger.make(({ logLevel, message }) => {
            if (logLevel.label === "ERROR") errors.push(String(message));
          }),
        ),
      ),
    ),
  );

const wedgedGauge = Metric.value(l1ControlPlaneWedgedGauge).pipe(
  Effect.map((state) => state.value),
);

/**
 * A hold taken without registering, like a scheduled merge's, whose
 * uninterruptible body blocks until `release` (a local finalization stuck on
 * a database acquisition): its hold timeout cannot complete.
 */
const hungUnregisteredHold = (
  globals: Globals,
  entered: Deferred.Deferred<void>,
  release: Deferred.Deferred<void>,
) =>
  withL1ControlPlaneWaitTimeout(
    globals,
    { scope: "scheduled_merge", waitTimeoutMs: 1_000, maxHoldMs: 20 },
    Effect.uninterruptible(
      Deferred.succeed(entered, undefined).pipe(
        Effect.zipRight(Deferred.await(release)),
      ),
    ),
  );

describe("L1 control-plane waiter wedge watchdog", () => {
  it("raises the gauge and logs once when a waiter is blocked behind an unregistered hung holder, then clears once it acquires", async () => {
    const errors: string[] = [];
    const outcome = await runCapturingErrors(
      errors,
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.update(globals.L1_CONTROL_PLANE_ACTIVITY, (activity) => ({
          ...activity,
          unregisteredHoldBudgetMs: 30,
        }));
        // A long registered hold earlier in the process must not size the
        // limit of a waiter that never waited behind it.
        yield* withL1ControlPlane(
          globals,
          { scope: "block_commitment", maxHoldMs: 60_000 },
          Effect.void,
        );
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const holder = yield* Effect.forkDaemon(
          hungUnregisteredHold(globals, entered, release),
        );
        yield* Deferred.await(entered);
        const waiter = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "block_confirmation", maxHoldMs: 1_000 },
            Effect.succeed("ran"),
          ),
        );
        yield* Effect.sleep(400);
        const gaugeWhileWedged = yield* wedgedGauge;
        const reasonsWhileWedged = yield* currentLivenessReasons(globals);
        const errorsWhileWedged = errors.length;
        yield* Deferred.succeed(release, undefined);
        const ran = yield* Fiber.join(waiter);
        // Its hold timed out long ago; it fails once its body lets it.
        yield* Fiber.await(holder);
        return {
          gaugeWhileWedged,
          reasonsWhileWedged,
          errorsWhileWedged,
          ran,
          gaugeAfter: yield* wedgedGauge,
          reasonsAfter: yield* currentLivenessReasons(globals),
        };
      }),
    );
    expect(outcome.gaugeWhileWedged).toBe(1);
    expect(outcome.errorsWhileWedged).toBe(1);
    expect(errors).toHaveLength(1);
    expect(errors[0]).toContain("scope block_confirmation has waited");
    expect(errors[0]).toContain("held by an unregistered holder");
    expect(
      outcome.reasonsWhileWedged.some((r) =>
        r.startsWith("l1_control_plane_wedged:waiter=block_confirmation"),
      ),
    ).toBe(true);
    expect(outcome.ran).toBe("ran");
    expect(outcome.gaugeAfter).toBe(0);
    expect(outcome.reasonsAfter).toEqual([]);
  });

  it("stays silent for a waiter queued behind a registered hold that finishes within its budget", async () => {
    const errors: string[] = [];
    const outcome = await runCapturingErrors(
      errors,
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.update(globals.L1_CONTROL_PLANE_ACTIVITY, (activity) => ({
          ...activity,
          unregisteredHoldBudgetMs: 30,
        }));
        const entered = yield* Deferred.make<void>();
        // Takes 250 ms, longer than the 90 ms the unregistered budget alone
        // would allow a waiter, but inside its own 500 ms budget.
        const holder = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "block_commitment", maxHoldMs: 500 },
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Effect.sleep(250)),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const ran = yield* withL1ControlPlane(
          globals,
          { scope: "block_confirmation", maxHoldMs: 1_000 },
          Effect.succeed("ran"),
        );
        yield* Fiber.join(holder);
        return { ran, gauge: yield* wedgedGauge };
      }),
    );
    expect(outcome.ran).toBe("ran");
    expect(outcome.gauge).toBe(0);
    expect(errors).toEqual([]);
  });

  it("sizes a waiter's limit by a longer hold that acquires ahead of it", async () => {
    const errors: string[] = [];
    const outcome = await runCapturingErrors(
      errors,
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.update(globals.L1_CONTROL_PLANE_ACTIVITY, (activity) => ({
          ...activity,
          unregisteredHoldBudgetMs: 30,
        }));
        const entered = yield* Deferred.make<void>();
        // A short hold first: the waiter registers behind a 50 ms budget.
        const first = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "short", maxHoldMs: 50 },
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Effect.sleep(20)),
            ),
          ),
        );
        yield* Deferred.await(entered);
        // Queued before the waiter, then holds 250 ms of its 500 ms budget.
        const second = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "long", maxHoldMs: 500 },
            Effect.sleep(250),
          ),
        );
        yield* Effect.sleep(5);
        const ran = yield* withL1ControlPlane(
          globals,
          { scope: "block_confirmation", maxHoldMs: 1_000 },
          Effect.succeed("ran"),
        );
        yield* Fiber.join(first);
        yield* Fiber.join(second);
        return { ran, gauge: yield* wedgedGauge };
      }),
    );
    expect(outcome.ran).toBe("ran");
    expect(outcome.gauge).toBe(0);
    expect(errors).toEqual([]);
  });
});
