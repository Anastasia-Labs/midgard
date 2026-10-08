import { it } from "@effect/vitest";
import {
  Duration,
  Effect,
  Fiber,
  Metric,
  Ref,
  Schedule,
  TestClock,
} from "effect";
import { describe, expect } from "vitest";

import { setLivenessReason } from "../src/services/globals.liveness-reasons.js";
import {
  clearLivenessIncident,
  FIBER_HALT_SOURCES,
  HALT_POLL_MS,
  HaltSource,
  livenessIncidentCounter,
  pausedWhileHalted,
  raiseLivenessIncident,
  restartedAcrossHalts,
} from "../src/services/liveness-halt.js";

const SOURCE = "test_halt_source";

const liveness = () => ({
  LIVENESS_REASONS: Ref.unsafeMake<ReadonlyMap<string, string>>(new Map()),
});

const incidents = (reason: string) =>
  Effect.map(
    Metric.value(Metric.tagged(livenessIncidentCounter, "reason", reason)),
    (state) => state.count,
  );

describe("liveness halts", () => {
  it.effect(
    "holds a scheduled fiber between ticks while halted and runs exactly one tick once it clears",
    () =>
      Effect.gen(function* () {
        const globals = liveness();
        let ticks = 0;
        const fiber = yield* Effect.fork(
          Effect.repeat(
            Effect.sync(() => {
              ticks += 1;
            }),
            pausedWhileHalted(Schedule.spaced("10 millis"), globals, [SOURCE]),
          ),
        );
        yield* TestClock.adjust("10 millis");
        expect(ticks).toBe(2);

        yield* setLivenessReason(globals, SOURCE, "halted");
        yield* TestClock.adjust("10 seconds");
        expect(ticks).toBe(2);

        // The held tick runs at the next halt poll after the clear, once,
        // and the schedule's own spacing resumes from there.
        yield* clearLivenessIncident(globals, SOURCE);
        let waitedMs = 0;
        while (ticks === 2 && waitedMs <= HALT_POLL_MS) {
          yield* TestClock.adjust("1 millis");
          waitedMs += 1;
        }
        expect(ticks).toBe(3);
        yield* TestClock.adjust("9 millis");
        expect(ticks).toBe(3);
        yield* TestClock.adjust("1 millis");
        expect(ticks).toBe(4);
        yield* Fiber.interrupt(fiber);
      }),
  );

  it.effect("a source outside the held set never holds the fiber", () =>
    Effect.gen(function* () {
      const globals = liveness();
      let ticks = 0;
      const fiber = yield* Effect.fork(
        Effect.repeat(
          Effect.sync(() => {
            ticks += 1;
          }),
          pausedWhileHalted(Schedule.spaced("10 millis"), globals, [SOURCE]),
        ),
      );
      yield* setLivenessReason(globals, "another_source", "halted");
      yield* TestClock.adjust("50 millis");
      expect(ticks).toBe(6);
      yield* Fiber.interrupt(fiber);
    }),
  );

  it.effect(
    "stops a restartable fiber on a halt and starts it once when the halt clears",
    () =>
      Effect.gen(function* () {
        const globals = liveness();
        let starts = 0;
        let interruptions = 0;
        const worker = Effect.sync(() => {
          starts += 1;
        }).pipe(
          Effect.zipRight(Effect.never),
          Effect.onInterrupt(() =>
            Effect.sync(() => {
              interruptions += 1;
            }),
          ),
        );
        const fiber = yield* Effect.fork(
          restartedAcrossHalts(globals, [SOURCE], worker),
        );
        yield* TestClock.adjust("1 millis");
        expect(starts).toBe(1);

        yield* setLivenessReason(globals, SOURCE, "halted");
        yield* TestClock.adjust(Duration.millis(HALT_POLL_MS));
        expect(interruptions).toBe(1);
        yield* TestClock.adjust("10 seconds");
        expect(starts).toBe(1);

        yield* clearLivenessIncident(globals, SOURCE);
        yield* TestClock.adjust(Duration.millis(HALT_POLL_MS));
        expect(starts).toBe(2);
        yield* TestClock.adjust("10 seconds");
        expect(starts).toBe(2);
        yield* Fiber.interrupt(fiber);
      }),
  );

  it.effect("returns a restartable fiber's own outcome unchanged", () =>
    Effect.gen(function* () {
      const globals = liveness();
      expect(
        yield* restartedAcrossHalts(globals, [SOURCE], Effect.succeed(7)),
      ).toBe(7);
      const failure = yield* Effect.flip(
        restartedAcrossHalts(globals, [SOURCE], Effect.fail("boom")),
      );
      expect(failure).toBe("boom");
    }),
  );

  it.effect(
    "counts an incident once per raised reason and clears it from readiness",
    () =>
      Effect.gen(function* () {
        const globals = liveness();
        const reason = "test_liveness_incident_reason";
        const other = "test_liveness_incident_other_reason";
        const before = yield* incidents(reason);
        for (let tick = 0; tick < 3; tick += 1)
          yield* raiseLivenessIncident(globals, SOURCE, reason, "detail");
        expect((yield* incidents(reason)) - before).toBe(1);
        expect((yield* Ref.get(globals.LIVENESS_REASONS)).get(SOURCE)).toBe(
          reason,
        );

        const otherBefore = yield* incidents(other);
        yield* raiseLivenessIncident(globals, SOURCE, other, "detail");
        expect((yield* incidents(other)) - otherBefore).toBe(1);
        expect((yield* Ref.get(globals.LIVENESS_REASONS)).get(SOURCE)).toBe(
          other,
        );

        yield* clearLivenessIncident(globals, SOURCE);
        expect((yield* Ref.get(globals.LIVENESS_REASONS)).has(SOURCE)).toBe(
          false,
        );
        // Raised again after clearing, it is a new incident.
        yield* raiseLivenessIncident(globals, SOURCE, reason, "detail");
        expect((yield* incidents(reason)) - before).toBe(2);
      }),
  );

  it("holds every operator duty on removal", () => {
    const held = (source: HaltSource) =>
      Object.entries(FIBER_HALT_SOURCES)
        .filter(([, sources]) => sources.includes(source))
        .map(([name]) => name)
        .sort();
    expect(held(HaltSource.operatorMembership)).toEqual([
      "blockCommitment",
      "merge",
      "operatorWatchdog",
      "settlement",
    ]);
  });
});
