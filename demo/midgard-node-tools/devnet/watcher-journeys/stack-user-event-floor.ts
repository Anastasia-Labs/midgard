import * as SDK from "@al-ft/midgard-sdk";
import type { OperatorShiftAppointment } from "midgard-watcher/tests/support/published-block-actor.operator-shift";

import { lateShiftMinimumLeadMs } from "./honest-successor.observables.js";

/** The deployment timing a user event's strike threshold follows. */
export type StackUserEventTiming = Readonly<{
  /** How long past its build a user event's validity may end at the least. */
  eventTtlMs: number;
  /** How long after its validity ends a user event is due. */
  eventWaitMs: number;
  /** How long a due user event may stay undelivered before a strike. */
  negligenceTimeoutMs: number;
  /** How long into a new shift its holder cannot be struck. */
  graceMs: number;
  shiftDurationMs: number;
}>;

/**
 * How far into operator-2's shift the drill may build its first user event.
 * A user event's validity ends at the first slot boundary strictly past its
 * build plus its time to live (`resolveUserEventValidTo`), so at the soonest
 * a millisecond past it; it is due an event wait less a millisecond after
 * that (`resolveEventInclusionTime`), and its holder may be struck for it a
 * negligence timeout later. So an event built at this offset has its strike
 * threshold at the shift's end at the soonest, where the scheduler refuses a
 * strike, and one built a millisecond earlier can have it a millisecond
 * before the end: the floor has no margin. Built at or after it, no event
 * can get operator-2's node, stopped through the drill, struck. The builders
 * date validity from the wall clock, so the gate holds both the ledger and
 * the wall clock to it. Zero when the whole shift is that safe.
 */
export const stackUserEventFloorOffsetMs = (
  timing: Omit<StackUserEventTiming, "graceMs">,
) =>
  Math.max(
    0,
    timing.shiftDurationMs -
      (timing.eventTtlMs + timing.eventWaitMs + timing.negligenceTimeoutMs),
  );

/** One slot: a user event's validity ends at the slot after its time to live. */
const VALIDITY_SLOT_MS = 1_000;

/** How often a funding deposit held past a shift's end rereads the scheduler. */
const SCHEDULER_RECHECK_MS = 30_000;

/**
 * Before each user event the drill publishes on an attached stack.
 * `beforeEvent`, for a family's own events in operator-2's shift: refuses
 * when the scheduler does not appoint operator-2, waits until both ledger
 * time and the wall clock reach the floor (`stackUserEventFloorOffsetMs`),
 * then reads the earliest user event no
 * committed block covers. If another operator could strike operator-2 for
 * it before the fault's latest commit lands (the latest start of the
 * late-shift window plus a commit's allowance), it refuses before the drill
 * publishes anything more. `beforeFunding`, for the ledger owner's funding
 * deposit, which our node absorbs once it is due: in another operator's
 * shift, publishes at once while the deposit falls due inside that shift;
 * otherwise it waits for the shift's end and reads the scheduler again, as
 * long as it takes. In operator-2's shift, from its actual start, it waits
 * for the floor. Once the floor has passed neither waits.
 */
export const stackUserEventGate = (
  input: Readonly<{
    readAppointment(): Promise<OperatorShiftAppointment>;
    timing: StackUserEventTiming;
    /** How long a commit takes to land. */
    commitAllowanceMs: number;
    /** The earliest user event due after the queue tail's end, or null. */
    readUndeliveredEvent(
      tailEndTimeMs: bigint,
    ): Promise<SDK.NeglectedUserEventClaim | null>;
    /** Ledger time in milliseconds. */
    now(): number;
    /** The wall clock the SDK dates a user event's validity from. */
    wallClock(): number;
    awaitLedgerTime(timeMs: number): Promise<void>;
    onStage(name: string): void;
  }>,
) => {
  const { timing } = input;
  // The floor has no margin and the builders date validity from the wall
  // clock, so both the ledger and the wall clock must reach it. A wall clock
  // behind the ledger is waited for on the ledger, the gap further on.
  const awaitFloor = async (startMs: number) => {
    const floorMs = startMs + stackUserEventFloorOffsetMs(timing);
    if (input.now() < floorMs || input.wallClock() < floorMs)
      input.onStage(
        `awaiting ${floorMs.toString()} in operator-2's shift before publishing a user event, so that it cannot get operator-2 struck`,
      );
    if (input.now() < floorMs) await input.awaitLedgerTime(floorMs);
    for (let wallMs = input.wallClock(); wallMs < floorMs; ) {
      await input.awaitLedgerTime(input.now() + (floorMs - wallMs));
      wallMs = input.wallClock();
    }
  };
  return Object.freeze({
    beforeEvent: async (operator: string, tailEndTimeMs: bigint) => {
      const appointment = await input.readAppointment();
      if (appointment?.operator !== operator)
        throw new Error(
          `The scheduler does not appoint operator-2 (${operator}) for the drill's user events: ${appointment === undefined ? "no appointment" : `${appointment.operator}'s shift`}`,
        );
      const startMs = Number(appointment.startTime);
      await awaitFloor(startMs);
      const event = await input.readUndeliveredEvent(tailEndTimeMs);
      if (event === null) return;
      const threshold = SDK.computeInactivityThreshold({
        shiftStartMs: appointment.startTime,
        stateQueueTailEndTimeMs: tailEndTimeMs,
        neglectedEvent: event,
        params: {
          ...SDK.DEFAULT_INACTIVITY_TIMING_PARAMETERS,
          shiftDurationMs: BigInt(timing.shiftDurationMs),
          newShiftInactivityGracePeriodMs: BigInt(timing.graceMs),
          userEventsNegligenceTimeoutMs: BigInt(timing.negligenceTimeoutMs),
        },
      });
      const commitLandsMs =
        startMs +
        timing.shiftDurationMs -
        lateShiftMinimumLeadMs(input.commitAllowanceMs) +
        1 +
        input.commitAllowanceMs;
      if (
        threshold.kind === "threshold" &&
        threshold.thresholdMs <= BigInt(commitLandsMs)
      )
        throw new Error(
          `An undelivered ${event.kind} ${event.utxo.txHash}#${event.utxo.outputIndex.toString()}, due at ${event.inclusionTimeMs.toString()}, lets another operator strike operator-2 after ${threshold.thresholdMs.toString()}, before the fault's commit could land (by ${commitLandsMs.toString()}); refusing before the drill publishes its user events. Stage once no other user events are pending on the stack`,
        );
    },
    beforeFunding: async (operator: string) => {
      // The latest a deposit built now can fall due. Its builder dates its
      // validity from the wall clock (`resolveUserEventValidTo`), to the next
      // slot past its time to live, and it is due an event wait later. A
      // live stack's ledger trails the wall clock, so the wall clock bounds
      // it; the later of the two holds whichever clock leads.
      const latestDueMs = () =>
        Math.max(input.wallClock(), input.now()) +
        timing.eventTtlMs +
        VALIDITY_SLOT_MS +
        timing.eventWaitMs;
      let awaited: string | undefined;
      for (;;) {
        const appointment = await input.readAppointment();
        if (appointment === undefined) return;
        const startMs = Number(appointment.startTime);
        if (appointment.operator === operator) return awaitFloor(startMs);
        const endMs = startMs + timing.shiftDurationMs;
        if (latestDueMs() < endMs) return;
        const shift = `${appointment.operator}'s shift from ${startMs.toString()}`;
        if (awaited !== shift)
          input.onStage(
            `awaiting the end of ${shift} before funding the ledger owner, whose deposit could fall due in the next shift`,
          );
        awaited = shift;
        // Past its end the scheduler may not have moved yet.
        const nowMs = input.now();
        await input.awaitLedgerTime(
          nowMs < endMs ? endMs : nowMs + SCHEDULER_RECHECK_MS,
        );
      }
    },
  });
};
