import { Effect, Metric, Ref } from "effect";

import type { Globals } from "./globals.globals.js";
import { l1ControlPlaneLivenessReasons } from "./globals.l1-control-plane.js";

export const livenessReasonAgeGauge = Metric.gauge(
  "midgard_liveness_reason_age_ms",
  {
    description:
      "How long each source has had a liveness reason raised, by source; 0 once it clears.",
  },
);

/** When each source last went from no raised reason to one, per Globals
 * instance (keyed by its `LIVENESS_REASONS`), so each node has its own. A
 * source that replaces its reason keeps its age. */
const raisedSince = new WeakMap<object, Map<string, number>>();

const raisedSinceOf = (globals: Pick<Globals, "LIVENESS_REASONS">) => {
  let map = raisedSince.get(globals.LIVENESS_REASONS);
  if (map === undefined) {
    map = new Map();
    raisedSince.set(globals.LIVENESS_REASONS, map);
  }
  return map;
};

/** Publishes `ageMs` as how long `source` has had a reason raised. */
export const publishLivenessReasonAge = (
  source: string,
  ageMs: number,
): Effect.Effect<void> =>
  Metric.set(Metric.tagged(livenessReasonAgeGauge, "source", source), ageMs);

/** Since when `source` has had a reason raised, if it has one this module
 * recorded (a reason written straight into the Ref has none). */
export const livenessReasonRaisedSinceMs = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  source: string,
): number | undefined => raisedSinceOf(globals).get(source);

/** Raises (or replaces) the liveness reason of `source`. */
export const setLivenessReason = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  source: string,
  reason: string,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    yield* Ref.update(globals.LIVENESS_REASONS, (reasons) => {
      if (reasons.get(source) === reason) return reasons;
      const updated = new Map(reasons);
      updated.set(source, reason);
      return updated;
    });
    const nowMs = Date.now();
    const since = raisedSinceOf(globals);
    const sinceMs = since.get(source) ?? nowMs;
    since.set(source, sinceMs);
    yield* publishLivenessReasonAge(source, nowMs - sinceMs);
  });

/** Clears the liveness reason of `source`, if it raised one. */
export const clearLivenessReason = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  source: string,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    yield* Ref.update(globals.LIVENESS_REASONS, (reasons) => {
      if (!reasons.has(source)) return reasons;
      const updated = new Map(reasons);
      updated.delete(source);
      return updated;
    });
    if (raisedSinceOf(globals).delete(source))
      yield* publishLivenessReasonAge(source, 0);
  });

/**
 * Every liveness reason the node raises right now: those fibers raised and
 * have not cleared, and those the L1 control plane derives from who holds and
 * who waits for its permit. Sorted, so a readiness body is stable.
 */
export const currentLivenessReasons = (
  globals: Pick<Globals, "LIVENESS_REASONS" | "L1_CONTROL_PLANE_ACTIVITY">,
  nowMs: number = Date.now(),
): Effect.Effect<readonly string[]> =>
  Effect.gen(function* () {
    const raised = yield* Ref.get(globals.LIVENESS_REASONS);
    const activity = yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY);
    return [
      ...raised.values(),
      ...l1ControlPlaneLivenessReasons(activity, nowMs),
    ].sort();
  });

/**
 * Logs `message` at info when the state under `key` changes and at debug
 * otherwise, so a status that repeats every tick appears once per change.
 */
export const logOnStateChange = (
  globals: Pick<Globals, "LOGGED_STATES">,
  key: string,
  state: string,
  message: string,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    const changed = yield* Ref.modify(globals.LOGGED_STATES, (states) => {
      if (states.get(key) === state) return [false, states];
      const updated = new Map(states);
      updated.set(key, state);
      return [true, updated];
    });
    yield* changed ? Effect.logInfo(message) : Effect.logDebug(message);
  });

/** Forgets the state logged under `key`, so its next state logs at info. */
export const forgetLoggedState = (
  globals: Pick<Globals, "LOGGED_STATES">,
  key: string,
): Effect.Effect<void> =>
  Ref.update(globals.LOGGED_STATES, (states) => {
    if (!states.has(key)) return states;
    const updated = new Map(states);
    updated.delete(key);
    return updated;
  });
