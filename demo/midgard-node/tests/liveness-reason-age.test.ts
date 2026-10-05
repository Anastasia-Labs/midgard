import { Effect, Metric, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { initialL1ControlPlaneActivity } from "../src/services/globals.l1-control-plane.js";
import {
  clearLivenessReason,
  livenessReasonAgeGauge,
  setLivenessReason,
} from "../src/services/globals.liveness-reasons.js";
import {
  activeLivenessReasons,
  clearLivenessIncident,
  L1_CONTROL_PLANE_LIVENESS_SOURCE,
  raiseLivenessIncident,
} from "../src/services/liveness-halt.js";

/**
 * Each raised liveness reason carries how long its source has been raised
 * (in readiness, and as a gauge by source that returns to 0 once it clears),
 * and an escalated flag once its source's bound has passed. Escalation is
 * only reported.
 */

const node = () => ({
  LIVENESS_REASONS: Ref.unsafeMake<ReadonlyMap<string, string>>(new Map()),
  L1_CONTROL_PLANE_ACTIVITY: Ref.unsafeMake(initialL1ControlPlaneActivity()),
});

const gauge = (source: string) =>
  Effect.runSync(
    Metric.value(Metric.tagged(livenessReasonAgeGauge, "source", source)),
  ).value;

describe("liveness reason age and escalation", () => {
  it("ages a raised reason, flags it once past its bound, and resets on clear", async () => {
    const globals = node();
    const source = "age-test:escalating";
    const startMs = Date.now();
    const raise = raiseLivenessIncident(globals, source, "held", "detail", {
      escalateAfterMs: 60_000,
    });
    const [inside, past, cleared] = await Effect.runPromise(
      Effect.gen(function* () {
        yield* raise;
        const inside = yield* activeLivenessReasons(globals, startMs + 30_000);
        const past = yield* activeLivenessReasons(globals, startMs + 120_000);
        yield* clearLivenessIncident(globals, source);
        return [inside, past, yield* activeLivenessReasons(globals)] as const;
      }),
    );
    expect(inside).toEqual([
      {
        source,
        reason: "held",
        ageMs: expect.any(Number),
        escalateAfterMs: 60_000,
        escalated: false,
      },
    ]);
    expect(inside[0]!.ageMs).toBeGreaterThan(29_000);
    expect(past[0]).toMatchObject({ escalated: true });
    expect(past[0]!.ageMs).toBeGreaterThan(119_000);
    expect(cleared).toEqual([]);
    expect(gauge(source)).toBe(0);
  });

  it("publishes the age gauge while raised, keeping it across a replaced reason", async () => {
    const globals = node();
    const source = "age-test:gauge";
    await Effect.runPromise(setLivenessReason(globals, source, "stalled:3"));
    await new Promise((resolve) => setTimeout(resolve, 25));
    await Effect.runPromise(setLivenessReason(globals, source, "stalled:4"));
    expect(gauge(source)).toBeGreaterThanOrEqual(20);
    const [entry] = await Effect.runPromise(activeLivenessReasons(globals));
    expect(entry).toMatchObject({
      source,
      reason: "stalled:4",
      escalateAfterMs: null,
      escalated: false,
    });
    await Effect.runPromise(clearLivenessReason(globals, source));
    expect(gauge(source)).toBe(0);
  });

  it("never escalates a reason whose source gave no bound, however old", async () => {
    const globals = node();
    const source = "age-test:unbounded";
    const reasons = await Effect.runPromise(
      Effect.zipRight(
        raiseLivenessIncident(globals, source, "held", "detail"),
        activeLivenessReasons(globals, Date.now() + 365 * 86_400_000),
      ),
    );
    expect(reasons).toEqual([
      expect.objectContaining({ source, escalated: false }),
    ]);
  });

  it("lists the control plane's derived reasons, and a reason with no recorded start without an age", async () => {
    const globals = node();
    const nowMs = Date.now();
    Effect.runSync(
      Ref.update(globals.L1_CONTROL_PLANE_ACTIVITY, (activity) => ({
        ...activity,
        holder: {
          scope: "settlement",
          sinceMs: nowMs - 600_000,
          deadlineMs: nowMs - 300_000,
        },
      })),
    );
    Effect.runSync(
      Ref.set(globals.LIVENESS_REASONS, new Map([["age-test:raw", "raw"]])),
    );
    const reasons = await Effect.runPromise(
      activeLivenessReasons(globals, nowMs),
    );
    expect(reasons).toEqual([
      {
        source: L1_CONTROL_PLANE_LIVENESS_SOURCE,
        reason: "l1_control_plane_wedged:holder=settlement:overrun_ms=300000",
        ageMs: null,
        escalateAfterMs: null,
        escalated: false,
      },
      {
        source: "age-test:raw",
        reason: "raw",
        ageMs: null,
        escalateAfterMs: null,
        escalated: false,
      },
    ]);
  });
});
