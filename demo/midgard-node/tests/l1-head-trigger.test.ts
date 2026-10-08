/**
 * Head-change triggering of the planner fibers (plan §4.1, N2): a fiber on
 * `onL1HeadChange` ticks when the follower driver publishes a new view, at
 * most one tick per `minSpacing`, and otherwise only once `maxIdle` passed.
 */
import { Duration, Effect, Fiber, Ref, SubscriptionRef } from "effect";
import { describe, expect, it } from "vitest";

import {
  awaitL1HeadChange,
  onL1HeadChange,
  publishL1HeadChange,
} from "../src/services/l1-head-trigger.js";

const heads = () =>
  Effect.map(SubscriptionRef.make(0), (L1_HEAD_SEQUENCE) => ({
    L1_HEAD_SEQUENCE,
  }));

describe("L1 head-change triggering", () => {
  it("wakes a waiter on the next head change, or after the idle bound with the head then current", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* heads();
        const waiter = yield* Effect.fork(
          awaitL1HeadChange(globals, 0, Duration.seconds(30)),
        );
        yield* Effect.sleep("20 millis");
        yield* publishL1HeadChange(globals);
        expect(yield* Fiber.join(waiter)).toBe(1);
        // A head already past `seen` answers at once.
        expect(yield* awaitL1HeadChange(globals, 0, Duration.seconds(30))).toBe(
          1,
        );
        // No change: the idle bound passes and the current head is returned.
        expect(yield* awaitL1HeadChange(globals, 1, Duration.millis(30))).toBe(
          1,
        );
      }),
    );
  });

  it("ticks a fiber on head changes, at most one per spacing, and not on a timer shorter than its idle bound", async () => {
    await Effect.runPromise(
      Effect.scoped(
        Effect.gen(function* () {
          const globals = yield* heads();
          const ticks = yield* Ref.make(0);
          const schedule = yield* onL1HeadChange(
            globals,
            Duration.seconds(30),
            Duration.millis(50),
          );
          yield* Effect.forkScoped(
            Effect.repeat(
              Ref.update(ticks, (n) => n + 1),
              schedule,
            ),
          );
          // The first tick runs at once; with no head change none follows.
          yield* Effect.sleep("300 millis");
          expect(yield* Ref.get(ticks)).toBe(1);
          // One head change: one more tick, after the spacing.
          yield* publishL1HeadChange(globals);
          yield* Effect.sleep("200 millis");
          expect(yield* Ref.get(ticks)).toBe(2);
          // A burst of five changes within one spacing: the waiter wakes on
          // the first, and the rest coalesce into one tick after the spacing.
          for (let i = 0; i < 5; i += 1) yield* publishL1HeadChange(globals);
          yield* Effect.sleep("200 millis");
          const afterBurst = (yield* Ref.get(ticks)) - 2;
          expect(afterBurst).toBeGreaterThanOrEqual(1);
          expect(afterBurst).toBeLessThanOrEqual(2);
          // Then quiet again.
          yield* Effect.sleep("200 millis");
          expect(yield* Ref.get(ticks)).toBe(2 + afterBurst);
        }),
      ),
    );
  });

  it("still ticks after the idle bound when the head never moves", async () => {
    await Effect.runPromise(
      Effect.scoped(
        Effect.gen(function* () {
          const globals = yield* heads();
          const ticks = yield* Ref.make(0);
          const schedule = yield* onL1HeadChange(
            globals,
            Duration.millis(100),
            Duration.millis(10),
          );
          yield* Effect.forkScoped(
            Effect.repeat(
              Ref.update(ticks, (n) => n + 1),
              schedule,
            ),
          );
          yield* Effect.sleep("450 millis");
          const count = yield* Ref.get(ticks);
          expect(count).toBeGreaterThanOrEqual(3);
          expect(count).toBeLessThanOrEqual(6);
        }),
      ),
    );
  });
});
