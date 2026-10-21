/**
 * The commit fiber's pause for a due validation-trace Defend: a Defend
 * spends our active-operator node, as a commit does, so while one is due
 * the commit fiber starts no new commit, and the Defend takes the L1
 * control plane and the state-queue lease as soon as no signed commit is
 * unresolved.
 *
 * The pause is bounded, so commits never stop for good: it holds while the
 * defender's last decision is fresh, and for at most the Defend's own hold
 * (`validationTraceDefendHoldMs`) and a validity range past the moment the
 * Defend could first be played. Once that bound passes, commits resume
 * whatever the defender does; a Defend that then loses a race to a commit
 * is a dead intent, rebuilt at its next tick.
 */
import { MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS } from "@al-ft/midgard-core";
import { Effect, Ref } from "effect";

import { Globals } from "../services/index.js";
import type { ValidationTraceDefenseHealth } from "../validation-trace-defense/health.js";

/** How long a due Defend pauses commits, past when it could first be played. */
export const VALIDATION_TRACE_DEFENSE_COMMIT_PAUSE_MS =
  180_000 + 2 * MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS;

export const SKIPPED_DUE_VALIDATION_TRACE_DEFENSE =
  "skipped_due_validation_trace_defense";

/** Whether `health` pauses commits at wall-clock `nowMs`. */
export const validationTraceDefensePausesCommits = (
  health: ValidationTraceDefenseHealth,
  nowMs: number,
): boolean =>
  health.following &&
  health.dueDefends.length > 0 &&
  nowMs - health.decidedAtMs <= VALIDATION_TRACE_DEFENSE_COMMIT_PAUSE_MS &&
  (health.defendDueSinceMs === null ||
    nowMs - health.defendDueSinceMs <=
      VALIDATION_TRACE_DEFENSE_COMMIT_PAUSE_MS);

let reportedPause = false;

/** True while a due Defend pauses commits; logged once per pause. */
export const shouldSkipForDueValidationTraceDefense = Effect.gen(function* () {
  const globals = yield* Globals;
  const health = yield* Ref.get(globals.VALIDATION_TRACE_DEFENSE_HEALTH);
  const paused = validationTraceDefensePausesCommits(health, Date.now());
  if (paused && !reportedPause)
    yield* Effect.logInfo(
      `🔹 Skipping block commitment ticks (${SKIPPED_DUE_VALIDATION_TRACE_DEFENSE}): ${health.dueDefends.length.toString()} validation-trace Defend(s) due.`,
    );
  else if (!paused && reportedPause)
    yield* Effect.logInfo(
      "🔹 Resuming block commitment: no validation-trace Defend pauses it.",
    );
  reportedPause = paused;
  return paused;
});
