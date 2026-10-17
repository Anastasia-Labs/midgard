import { describe, expect, it } from "@effect/vitest";
import {
  Duration,
  Effect,
  Fiber,
  Option,
  Ref,
  Schedule,
  TestClock,
} from "effect";

import {
  OPERATOR_REMOVED,
  type OperatorMembership,
  publishOperatorMembership,
} from "../src/l1-operator-set/index.js";
import { withL1ControlPlane } from "../src/services/globals.globals.js";
import { Globals } from "../src/services/globals.js";
import {
  activeLivenessReasons,
  FIBER_HALT_SOURCES,
  HALT_POLL_MS,
  HaltSource,
  pausedWhileHalted,
} from "../src/services/liveness-halt.js";

const membership = (
  state: OperatorMembership["state"],
): OperatorMembership => ({ state, detail: `operator k is ${state}` });

const haltOf = (globals: Globals) =>
  Effect.map(Ref.get(globals.LIVENESS_REASONS), (reasons) =>
    reasons.get(HaltSource.operatorMembership),
  );

describe("operator membership holds duties", () => {
  it.effect(
    "raises operator_removed on removal and clears it on every other state",
    () =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        // The startup default holds nothing: duties run until the first
        // operator-set read.
        expect(yield* Ref.get(globals.OPERATOR_MEMBERSHIP)).toBe("unknown");
        expect(yield* haltOf(globals)).toBeUndefined();

        yield* publishOperatorMembership(membership("removed"));
        expect(yield* Ref.get(globals.OPERATOR_MEMBERSHIP)).toBe("removed");
        expect(yield* haltOf(globals)).toBe(OPERATOR_REMOVED);
        // /readyz fails on every active liveness reason.
        const reasons = yield* activeLivenessReasons(globals);
        expect(reasons.map(({ reason }) => reason)).toContain(OPERATOR_REMOVED);

        for (const state of [
          "active",
          "awaiting_activation",
          "unknown",
        ] as const) {
          yield* publishOperatorMembership(membership("removed"));
          yield* publishOperatorMembership(membership(state));
          expect(yield* Ref.get(globals.OPERATOR_MEMBERSHIP)).toBe(state);
          expect(yield* haltOf(globals)).toBeUndefined();
        }
      }).pipe(Effect.provide(Globals.Default)),
  );

  it.effect(
    "holds every duty while removed, exits nothing, and resumes when a rollback undoes it",
    () =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        let ticks = 0;
        // Each held fiber's schedule, as `nodeFibers` runs it.
        const fibers = yield* Effect.forEach(
          Object.values(FIBER_HALT_SOURCES),
          (sources) =>
            Effect.fork(
              Effect.repeat(
                Effect.sync(() => {
                  ticks += 1;
                }),
                pausedWhileHalted(
                  Schedule.spaced("10 millis"),
                  globals,
                  sources,
                ),
              ),
            ),
        );
        yield* TestClock.adjust("10 millis");
        expect(ticks).toBeGreaterThan(0);

        yield* publishOperatorMembership(membership("removed"));
        const held = ticks;
        yield* TestClock.adjust("10 seconds");
        expect(ticks).toBe(held);
        // Nothing exited: every duty fiber is still there, held.
        for (const fiber of fibers)
          expect(Option.isNone(yield* Fiber.poll(fiber))).toBe(true);

        // A rollback that undoes the removal: the next read is active.
        yield* publishOperatorMembership(membership("active"));
        yield* TestClock.adjust(Duration.millis(2 * HALT_POLL_MS));
        expect(ticks).toBeGreaterThan(held);
        for (const fiber of fibers) yield* Fiber.interrupt(fiber);
      }).pipe(Effect.provide(Globals.Default)),
  );

  it.effect("leaves non-duty L1 work to run while removed", () =>
    // The halt source holds the duties (see `FIBER_HALT_SOURCES`); block
    // confirmation, ingestion and the readiness refreshers keep the permit.
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* publishOperatorMembership(membership("removed"));
      let ran = false;
      yield* withL1ControlPlane(
        globals,
        { scope: "block_confirmation" },
        Effect.sync(() => {
          ran = true;
        }),
      );
      expect(ran).toBe(true);
    }).pipe(Effect.provide(Globals.Default)),
  );
});
