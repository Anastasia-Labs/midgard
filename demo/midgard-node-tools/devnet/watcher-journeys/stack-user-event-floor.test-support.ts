import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import type { OperatorShiftAppointment } from "midgard-watcher/tests/support/published-block-actor.operator-shift";

import {
  stackUserEventGate,
  type StackUserEventTiming,
} from "./stack-user-event-floor.js";

export type ProfileName = "preprod-testing" | "preprod-rehearsal";

/** The user-event timing a deployment profile compiles into the SDK. */
export const eventTiming = (name: ProfileName): StackUserEventTiming => {
  const profile = DEPLOYMENT_PROFILES[name].timing;
  return {
    eventTtlMs: SDK.USER_EVENT_TX_TTL_MS,
    eventWaitMs: profile.event_wait_ms,
    negligenceTimeoutMs: profile.user_events_negligence_timeout_ms,
    graceMs: profile.new_shift_inactivity_grace_period_ms,
    shiftDurationMs: profile.operator_shift_ms,
  };
};

export const OPERATOR_2 = "02".repeat(28);
export const OUR_NODE = "01".repeat(28);
export const SHIFT_START_MS = 1_800_000_000_000;

/** The gate on a ledger clock the test moves, recording what it waits for. */
export const gateOn = (
  input: Readonly<{
    timing: StackUserEventTiming;
    /** The scheduler's appointment, or the one it holds at a ledger time. */
    appointment:
      | OperatorShiftAppointment
      | ((nowMs: number) => OperatorShiftAppointment);
    nowMs: number;
    /** How far the wall clock runs ahead of the ledger; none by default. */
    wallAheadMs?: number;
    commitAllowanceMs?: number;
    undelivered?: SDK.NeglectedUserEventClaim | null;
  }>,
) => {
  const ledger = {
    nowMs: input.nowMs,
    stages: [] as string[],
    tailsRead: [] as bigint[],
  };
  const { appointment } = input;
  const gate = stackUserEventGate({
    readAppointment: async () =>
      typeof appointment === "function"
        ? appointment(ledger.nowMs)
        : appointment,
    timing: input.timing,
    commitAllowanceMs: input.commitAllowanceMs ?? 190_000,
    readUndeliveredEvent: async (tailEndTimeMs) => {
      ledger.tailsRead.push(tailEndTimeMs);
      return input.undelivered ?? null;
    },
    now: () => ledger.nowMs,
    wallClock: () => ledger.nowMs + (input.wallAheadMs ?? 0),
    awaitLedgerTime: async (timeMs) => {
      ledger.nowMs = Math.max(ledger.nowMs, timeMs);
    },
    onStage: (name) => {
      ledger.stages.push(name);
    },
  });
  return { gate, ledger };
};

export const operator2Shift = {
  operator: OPERATOR_2,
  startTime: BigInt(SHIFT_START_MS),
};
