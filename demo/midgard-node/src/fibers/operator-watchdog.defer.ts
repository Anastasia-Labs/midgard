/**
 * The operator watchdog's deferral through the slot-aware due-work registry,
 * and its projection of the SDK planner's result onto the policy's view.
 */
import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  canonicalSlotConfigForLucid,
  unixTimeToSlotForConfig,
} from "../lucid-time.js";
import type { WatchdogTakeoverPlan } from "./operator-watchdog-policy.js";
import { registerSlotAwareDueWork } from "./slot-aware-due-work.js";

export const DUE_WORK_KIND = "operator_watchdog" as const;
export const DUE_WORK_KEY = "takeover";

/**
 * Maps a wall-clock target to the slot at which it becomes current. When Lucid
 * exposes no slot configuration (emulator), one-second slots are assumed.
 */
const unixTimeToSlotOrFallback = (
  lucid: Parameters<typeof canonicalSlotConfigForLucid>[0],
  unixTimeMs: number,
  currentSlot: number,
  waitMs: number,
): number => {
  try {
    return unixTimeToSlotForConfig(
      unixTimeMs,
      canonicalSlotConfigForLucid(lucid),
    );
  } catch {
    return currentSlot + Math.ceil(waitMs / 1000);
  }
};

const toSafeNumber = (value: bigint): number =>
  value > BigInt(Number.MAX_SAFE_INTEGER)
    ? Number.MAX_SAFE_INTEGER
    : Number(value);

/** Projects the SDK planner's result onto the policy's view of it. */
export const toWatchdogPlan = (
  plan: SDK.InactivityTakeoverPlan,
): WatchdogTakeoverPlan => {
  switch (plan.kind) {
    case "no-shift":
      return { kind: "no-shift" };
    case "no-neglected-event":
      return {
        kind: "no-neglected-event",
        currentOperator: plan.currentOperator,
      };
    case "not-yet":
      return {
        kind: "not-yet",
        currentOperator: plan.currentOperator,
        thresholdMs: toSafeNumber(plan.thresholdMs),
      };
    case "blocked":
      // A blocked plan (e.g. a registered operator may still activate, or
      // the successor node is missing) is treated as "no shift to take" this
      // tick; the next tick re-plans from a fresh snapshot.
      return { kind: "no-shift" };
    case "strikes-exhausted":
      return {
        kind: "strikes-exhausted",
        currentOperator: plan.currentOperator,
        shiftStartMs: toSafeNumber(plan.shiftStartMs),
      };
    case "ready":
      return {
        kind: "ready",
        currentOperator: plan.currentOperator,
        newOperatorKey: plan.newOperatorKey,
        thresholdMs: toSafeNumber(plan.thresholdMs),
      };
  }
};

/**
 * Defers the next plan until `untilMs`. The registry's dependency and
 * invalidation keys are descriptive here: the check runs before the operator
 * set is read, so there is nothing yet to compare them against.
 */
export const deferWatchdog = (input: {
  readonly lucid: Parameters<typeof canonicalSlotConfigForLucid>[0];
  readonly currentSlot: number;
  readonly nowMs: number;
  readonly untilMs: number;
  readonly reason: string;
  readonly dependencyKey: string;
}): Effect.Effect<void> => {
  const waitMs = Math.max(0, input.untilMs - input.nowMs);
  const dueSlot = unixTimeToSlotOrFallback(
    input.lucid,
    input.untilMs,
    input.currentSlot,
    waitMs,
  );
  registerSlotAwareDueWork({
    kind: DUE_WORK_KIND,
    key: DUE_WORK_KEY,
    callerLabel: "operator-watchdog",
    reason: input.reason,
    observedSlot: input.currentSlot,
    dueSlot,
    dueAtMs: input.untilMs,
    waitMs,
    slotSource: "l1_slot_now",
    dependencyKey: input.dependencyKey,
    invalidationKey: input.reason,
  });
  return Effect.logInfo(
    `🐕 Operator watchdog waiting (${input.reason}) until ${new Date(input.untilMs).toISOString()} (wait_ms=${waitMs.toString()}, due_slot=${dueSlot.toString()}).`,
  );
};
