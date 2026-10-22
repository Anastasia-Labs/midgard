/**
 * The defending operator's health, as its fiber last recorded it (a
 * Globals record), and what `/readyz` makes of it at a time.
 *
 * - Each tick records the named states it decided, the Defends it can
 *   build now (the commit fiber pauses for them, bounded:
 *   `block-commitment.validation-trace-defense-pause.ts`), and the L1 time
 *   it decided at.
 * - A step that fails keeps the last tick's states and due Defends, so a
 *   stalled fiber still reaches readiness: a due Defend whose deadline is
 *   less than the late margin away fails readiness as late at any time, and
 *   a material-unavailable, trace-disagrees or references-unresolved state
 *   fails once its deadline is that close.
 * - Underfunded, not-active and late fail readiness; the alarms that need
 *   no repair (operator fault, thread lost, history pruned) and the waiting
 *   states (funding pending among them) are named details.
 */
import {
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
  MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
} from "@al-ft/midgard-core";

import type { ValidationTraceDefenseState } from "./decide.js";

/** How long before a deadline a lack of the move fails readiness: a quarter of
 * the side clock. */
export const VALIDATION_TRACE_DEFENSE_LATE_MARGIN_MS = Math.floor(
  MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS / 4,
);

/** The state-queue mutation lease's holder, and the control plane's scope, of a Defend. */
export const VALIDATION_TRACE_DEFENSE_LEASE_HOLDER = "validation_trace_defense";

/** A Defend the node can build now, and its deadline (L1 time). */
export type ValidationTraceDefenseDue = Readonly<{
  subject: string;
  deadline: number;
}>;

/** When a Defend first became due (wall clock), and its deadline (L1 time). */
export type ValidationTraceDefenseFirstDue = Readonly<{
  firstDueMs: number;
  deadline: number;
}>;

export type ValidationTraceDefenseHealth = Readonly<{
  /** Whether the last tick ran under an L1 follower; with none it does nothing. */
  following: boolean;
  /** The named states of the last tick. */
  states: readonly ValidationTraceDefenseState[];
  /** The Defends the last tick could build. */
  dueDefends: readonly ValidationTraceDefenseDue[];
  /**
   * By subject, when (wall clock) each Defend first became due with no
   * signed commit unresolved, and its deadline. Kept for the process
   * whatever commits do meanwhile, and forgotten once its deadline has
   * passed: each Defend pauses commits once, for at most the bound.
   */
  defendFirstDue: ReadonlyMap<string, ValidationTraceDefenseFirstDue>;
  /** When (wall clock) the last tick decided these. */
  decidedAtMs: number;
  /** L1 time less wall-clock time at the last tick: deadlines are L1 times. */
  l1OffsetMs: number;
  lastProgressAtMs: number;
  lastFailureAtMs: number;
  lastError: string | null;
  consecutiveFailures: number;
}>;

export const initialValidationTraceDefenseHealth = (
  nowMs: number,
): ValidationTraceDefenseHealth => ({
  following: false,
  states: [],
  dueDefends: [],
  defendFirstDue: new Map(),
  decidedAtMs: nowMs,
  l1OffsetMs: 0,
  lastProgressAtMs: nowMs,
  lastFailureAtMs: 0,
  lastError: null,
  consecutiveFailures: 0,
});

/** How long a failing fiber may go without a good tick before it fails readiness. */
export const VALIDATION_TRACE_DEFENSE_STALL_MS =
  10 * MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS;

const FAILING_NEAR_DEADLINE: ReadonlySet<string> = new Set([
  "validation-trace-defense-material-unavailable",
  "validation-trace-defense-trace-disagrees",
  "validation-trace-defense-references-unresolved",
]);

/** `<state>:<subject>:<deadline|none>[:<shortfall>]`. */
const line = (state: ValidationTraceDefenseState): string =>
  [
    state.state,
    state.subject,
    state.deadline?.toString() ?? "none",
    ...(state.shortfallLovelace === undefined
      ? []
      : [state.shortfallLovelace.toString()]),
  ].join(":");

/** The readiness reasons and details of `health` at wall-clock `nowMs`. */
export const validationTraceDefenseReadiness = (
  health: ValidationTraceDefenseHealth,
  nowMs: number,
): Readonly<{ reasons: string[]; details: string[] }> => {
  const reasons: string[] = [];
  const details: string[] = [];
  if (!health.following) return { reasons, details };
  const l1Now = nowMs + health.l1OffsetMs;
  const near = (deadline: number | undefined) =>
    deadline !== undefined &&
    deadline - l1Now < VALIDATION_TRACE_DEFENSE_LATE_MARGIN_MS;
  const late = new Set<string>();
  for (const state of health.states) {
    const fails =
      state.failsReadiness ||
      (FAILING_NEAR_DEADLINE.has(state.state) && near(state.deadline));
    if (state.state === "validation-trace-defense-late")
      late.add(state.subject);
    (fails ? reasons : details).push(line(state));
  }
  for (const due of health.dueDefends)
    if (near(due.deadline) && !late.has(due.subject))
      reasons.push(
        line({
          state: "validation-trace-defense-late",
          subject: due.subject,
          deadline: due.deadline,
          detail: "the Defend has not landed",
          failsReadiness: true,
        }),
      );
  if (
    health.consecutiveFailures > 0 &&
    nowMs - health.lastProgressAtMs > VALIDATION_TRACE_DEFENSE_STALL_MS
  )
    reasons.push(
      `validation-trace-defense-stalled:${health.consecutiveFailures.toString()}`,
    );
  return { reasons, details };
};
