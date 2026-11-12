import {
  DEPLOYMENT_PROFILES,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import type {
  OperatorShiftAppointment,
  OperatorShiftPorts,
} from "midgard-watcher/tests/support/published-block-actor.operator-shift";

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

/** The scheduler's threshold for a user event due at `inclusionTimeMs`. */
export const strikeThreshold = (
  timing: StackUserEventTiming,
  shiftStartMs: number,
  inclusionTimeMs: number,
) =>
  SDK.computeInactivityThreshold({
    shiftStartMs: BigInt(shiftStartMs),
    stateQueueTailEndTimeMs: 0n,
    neglectedEvent: {
      kind: "Deposit",
      inclusionTimeMs: BigInt(inclusionTimeMs),
    },
    params: {
      ...SDK.DEFAULT_INACTIVITY_TIMING_PARAMETERS,
      shiftDurationMs: BigInt(timing.shiftDurationMs),
      newShiftInactivityGracePeriodMs: BigInt(timing.graceMs),
      userEventsNegligenceTimeoutMs: BigInt(timing.negligenceTimeoutMs),
    },
  });

/**
 * The soonest the SDK makes an event built at `builtMs` due on `lucid`'s slot
 * grid, over every
 * place in a slot the build can fall: its validity end
 * (`resolveUserEventValidTo`, at that instant on the emulator's slot grid),
 * then its inclusion (`resolveEventInclusionTime`). The SDK compiles one
 * profile's event wait, so another profile's replaces it.
 */
export const soonestDueOnGrid = (
  lucid: LucidEvolution,
  timing: StackUserEventTiming,
  builtMs: number,
) => {
  const slotStartMs = SDK.slotToUnixTimeForLucid(lucid, 1_000)!;
  const slotMs = SDK.slotToUnixTimeForLucid(lucid, 1_001)! - slotStartMs;
  let soonestMs = Number.POSITIVE_INFINITY;
  for (let intoSlotMs = 0; intoSlotMs < slotMs; intoSlotMs++) {
    const nowMs = slotStartMs + intoSlotMs;
    const validToMs = SDK.resolveUserEventValidTo(
      lucid,
      timing.eventTtlMs,
      () => nowMs,
    );
    const dueMs =
      SDK.resolveEventInclusionTime(
        validToMs,
        SELECTED_DEPLOYMENT_PROFILE.network,
      ) -
      SDK.EVENT_WAIT_DURATION_MS +
      timing.eventWaitMs;
    soonestMs = Math.min(soonestMs, builtMs + dueMs - nowMs);
  }
  return soonestMs;
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
    /**
     * The scheduler refresh at the ledger time it is asked at; none by
     * default. Like operator-2's guarded wallet, the gate's refresh is
     * refused until the gate holds operator-2's node stopped.
     */
    refresh?: (
      targetTime: bigint,
      nowMs: number,
    ) => ReturnType<OperatorShiftPorts["refresh"]>;
  }>,
) => {
  const ledger = {
    nowMs: input.nowMs,
    stages: [] as string[],
    tailsRead: [] as bigint[],
    operator2Held: false,
  };
  const { appointment } = input;
  const gate = stackUserEventGate({
    readAppointment: async () =>
      typeof appointment === "function"
        ? appointment(ledger.nowMs)
        : appointment,
    holdOperator2: async () => {
      ledger.operator2Held = true;
    },
    refresh: (targetTime) =>
      input.refresh === undefined
        ? Promise.reject(
            new Error("This case gives the gate no scheduler refresh"),
          )
        : !ledger.operator2Held
          ? Promise.reject(
              new Error(
                "Operator-2's node is not held stopped by this drill; refusing to sign as operator-2",
              ),
            )
          : input.refresh(targetTime, ledger.nowMs),
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
