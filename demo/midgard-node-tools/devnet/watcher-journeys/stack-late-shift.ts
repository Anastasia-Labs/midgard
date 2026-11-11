import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  type OperatorShiftAppointment,
  operatorShiftAppointment,
} from "midgard-watcher/tests/support/published-block-actor.operator-shift";

import { writeJourneyArtifact } from "./artifacts.js";
import type { JourneyCategory } from "./fixture.js";
import {
  lateShiftMinimumLeadMs,
  planLateShiftFaultCommit,
} from "./honest-successor.observables.js";
import type { StackJourneyContext } from "./stack-context.js";
import { stackDaExpectation } from "./stack-da-publication.js";
import { stackUserEventGate } from "./stack-user-event-floor.js";

/**
 * The stack node `.env` name of the late-shift lead: how long before the end
 * of operator-2's shift the drill commits a fault the committee attests, so
 * that our node's shift starts while the fault is still the tail. It is
 * measured on a live run (each family's `fault-tail.json`) and has no default.
 */
export const STACK_LATE_SHIFT_LEAD_ENV = "STACK_LATE_SHIFT_LEAD_MS";

/** The late-shift lead in the stack node environment, absent when unset. */
export const readStackLateShiftLeadMs = (
  env: Readonly<Record<string, string | undefined>>,
): number | undefined => {
  const value = env[STACK_LATE_SHIFT_LEAD_ENV]?.trim();
  if (value === undefined || value === "") return undefined;
  const leadMs = Number(value);
  if (!/^[1-9][0-9]*$/u.test(value) || !Number.isSafeInteger(leadMs))
    throw new Error(
      `${STACK_LATE_SHIFT_LEAD_ENV} must be a positive whole number of milliseconds`,
    );
  return leadMs;
};

/** When the fault's commit started, and the window it was planned in. */
export type StackLateShiftCommit = Readonly<{
  leadMs: number;
  commitAllowanceMs: number;
  shift: Readonly<{ startMs: number; endMs: number }>;
  notBeforeMs: number;
  notAfterMs: number;
  commitStartMs: number;
}>;

/**
 * Before each commit of an attested family's fault on an attached stack:
 * reads operator-2's shift from the scheduler, waits until the lead before
 * its end, and refuses once a commit and its fault header could no longer
 * end in it. A family the committee refuses is never convicted, so it
 * commits as soon as it may.
 * Constructing it for an attested family without a lead, or with one
 * shorter than a commit's allowance plus the time the fault's header runs
 * past its build, refuses, naming `STACK_LATE_SHIFT_LEAD_MS`.
 */
export const stackLateShiftFaultCommit = (
  input: Readonly<{
    category: JourneyCategory;
    leadMs: number | undefined;
    readAppointment(): Promise<OperatorShiftAppointment>;
    shiftDurationMs: number;
    /** How long a commit takes to land. */
    commitAllowanceMs: number;
    /** Ledger time in milliseconds. */
    now(): number;
    awaitLedgerTime(timeMs: number): Promise<void>;
    record(commit: StackLateShiftCommit): Promise<void>;
    onStage(name: string): void;
  }>,
): ((operator: string) => Promise<void>) => {
  if (stackDaExpectation(input.category) === "refusal")
    return () => Promise.resolve();
  const { leadMs, commitAllowanceMs } = input;
  if (leadMs === undefined)
    throw new Error(
      `Set ${STACK_LATE_SHIFT_LEAD_ENV} in the stack node environment: the ${input.category} drill commits its fault that long before operator-2's shift ends, so that our node's shift starts while the fault is the tail. Measure it on a live run (fault-tail.json); it has no default`,
    );
  const minimumLeadMs = lateShiftMinimumLeadMs(commitAllowanceMs);
  if (leadMs < minimumLeadMs)
    throw new Error(
      `${STACK_LATE_SHIFT_LEAD_ENV} must be at least ${minimumLeadMs.toString()} ms: a commit's allowance (${commitAllowanceMs.toString()} ms) plus the time the fault's header runs past its build (${(minimumLeadMs - commitAllowanceMs).toString()} ms), or the fault cannot end in operator-2's shift; it is ${leadMs.toString()} ms`,
    );
  return async (operator) => {
    const appointment = await input.readAppointment();
    if (appointment?.operator !== operator)
      throw new Error(
        `The scheduler does not appoint operator-2 (${operator}) for the fault's commit: ${appointment === undefined ? "no appointment" : `${appointment.operator}'s shift`}`,
      );
    const startMs = Number(appointment.startTime);
    const shift = { startMs, endMs: startMs + input.shiftDurationMs };
    const window = planLateShiftFaultCommit({
      shift,
      leadMs,
      commitAllowanceMs,
    });
    if (input.now() < window.notBeforeMs) {
      input.onStage(
        `awaiting the last ${leadMs.toString()} ms of operator-2's shift for the fault's commit`,
      );
      await input.awaitLedgerTime(window.notBeforeMs);
    }
    const commitStartMs = input.now();
    if (commitStartMs > window.notAfterMs)
      throw new Error(
        `The fault's commit missed its late-shift window: ledger time ${commitStartMs.toString()} is past ${window.notAfterMs.toString()}, the last start whose commit and fault header both end before operator-2's shift ends at ${shift.endMs.toString()}`,
      );
    await input.record({
      leadMs,
      commitAllowanceMs,
      shift,
      ...window,
      commitStartMs,
    });
  };
};

/** The appointment of the attached stack's scheduler. */
const readStackAppointment = async (context: StackJourneyContext) => {
  const { contracts } = context.deployment;
  const schedulers = await context.provider.getUtxosWithUnit(
    contracts.scheduler.spendingScriptAddress,
    toUnit(contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
  );
  if (schedulers.length !== 1)
    throw new Error("Expected one published scheduler");
  return operatorShiftAppointment(schedulers[0]!);
};

/**
 * `stackLateShiftFaultCommit` on an attached stack: operator-2's shift from
 * the stack's scheduler, the ledger clock of its chain, and each commit's
 * window recorded in the journey's `late-shift.json`. Refuses as it does,
 * so call it before any L1 work.
 */
export const stackLateShiftGate = (
  context: StackJourneyContext,
  input: Readonly<{
    category: JourneyCategory;
    /** How long a commit takes to land. */
    commitAllowanceMs: number;
    directory: string;
    onStage(name: string): void;
  }>,
) => {
  const { chain } = context.deployment;
  return stackLateShiftFaultCommit({
    category: input.category,
    leadMs: context.lateShiftLeadMs,
    readAppointment: () => readStackAppointment(context),
    shiftDurationMs: Number(SDK.SHIFT_DURATION_MS),
    commitAllowanceMs: input.commitAllowanceMs,
    now: () => chain.now(),
    awaitLedgerTime: async (timeMs) => {
      await chain.awaitLedgerTime(timeMs);
    },
    record: (commit) =>
      writeJourneyArtifact(join(input.directory, "late-shift.json"), commit),
    onStage: input.onStage,
  });
};

/**
 * `stackUserEventGate` on an attached stack: operator-2's shift from the
 * stack's scheduler, the ledger clock of its chain, the deployment's user
 * event timing, and the stack's undelivered user events read as a strike
 * reads them (`fetchNeglectedUserEventProgram`).
 */
export const stackUserEventPublicationGate = (
  context: StackJourneyContext,
  input: Readonly<{
    /** How long a commit takes to land. */
    commitAllowanceMs: number;
    onStage(name: string): void;
  }>,
) => {
  const { contracts, chain, operatorLucid } = context.deployment;
  return stackUserEventGate({
    readAppointment: () => readStackAppointment(context),
    timing: {
      eventTtlMs: SDK.USER_EVENT_TX_TTL_MS,
      eventWaitMs: SDK.EVENT_WAIT_DURATION_MS,
      negligenceTimeoutMs: Number(SDK.USER_EVENTS_NEGLIGENCE_TIMEOUT_MS),
      graceMs: Number(SDK.NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS),
      shiftDurationMs: Number(SDK.SHIFT_DURATION_MS),
    },
    commitAllowanceMs: input.commitAllowanceMs,
    readUndeliveredEvent: (tailEndTimeMs) =>
      Effect.runPromise(
        SDK.fetchNeglectedUserEventProgram(
          operatorLucid,
          contracts,
          tailEndTimeMs,
        ),
      ),
    now: () => chain.now(),
    wallClock: () => Date.now(),
    awaitLedgerTime: async (timeMs) => {
      await chain.awaitLedgerTime(timeMs);
    },
    onStage: input.onStage,
  });
};
