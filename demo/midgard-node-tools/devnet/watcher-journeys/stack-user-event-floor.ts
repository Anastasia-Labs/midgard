import * as SDK from "@al-ft/midgard-sdk";
import type {
  OperatorShiftAppointment,
  OperatorShiftPorts,
} from "midgard-watcher/tests/support/published-block-actor.operator-shift";

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

const sameShift = (
  left: OperatorShiftAppointment,
  right: { operator: string; startTime: bigint },
) => left?.operator === right.operator && left.startTime === right.startTime;

const describeShift = (appointment: { operator: string; startTime: bigint }) =>
  `${appointment.operator}'s shift from ${appointment.startTime.toString()}`;

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
 * shift, publishes at once while the deposit falls due early enough in that
 * shift for its operator to commit it; otherwise it waits for the shift's
 * end, up to `notAfterMs`. In operator-2's shift, from its actual start, it
 * waits for the floor. Once the floor has passed neither waits.
 * `admitsFunding` checks the built deposit's actual due time the same way
 * before it is submitted.
 *
 * None of them decides on a shift that has ended. Who holds the next shift,
 * and from when, is not fixed until a transaction advances the scheduler: an
 * ended shift passes to its one successor, starting at that transaction's
 * validity start (`scheduler.ak` `validate_end_of_shift_and_get_operators`).
 * A node advances it only to commit, so on an idle stack an ended datum
 * stays put. `beforeFunding` therefore advances it itself, through
 * `refresh` (the node's own alignment, as each family's onboarding does), and
 * decides on the shift that appoints; `beforeEvent` and `admitsFunding`
 * refuse an ended one. The refresh signs as operator-2, so, as before each
 * family's staging, the gate first holds operator-2's node stopped
 * (`holdOperator2`); a gate that advances nothing leaves it running.
 */
export const stackUserEventGate = (
  input: Readonly<{
    readAppointment(): Promise<OperatorShiftAppointment>;
    /**
     * Advance an ended shift towards operator-2's as its node would before a
     * commit covering `targetTime`, or say how long until it can be.
     */
    refresh: OperatorShiftPorts["refresh"];
    /** Hold operator-2's node stopped, as each signature of its wallet requires. */
    holdOperator2(): Promise<unknown>;
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
  // A deposit due in a live operator's shift is committed in it only when
  // it falls due no later than the late-shift window's last commit start.
  const commitMarginMs = lateShiftMinimumLeadMs(input.commitAllowanceMs);
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
      if (input.now() >= startMs + timing.shiftDurationMs)
        throw new Error(
          `Operator-2's ${describeShift(appointment)} has ended; the drill publishes its user events only in a shift operator-2 holds`,
        );
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
    beforeFunding: async (operator: string, notAfterMs = Infinity) => {
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
        const endMs = startMs + timing.shiftDurationMs;
        const shift = describeShift(appointment);
        if (input.now() >= endMs) {
          // Nothing else need move an ended shift; the drill advances it.
          if (input.now() > notAfterMs)
            throw new Error(
              `The ledger owner's funding could not advance the scheduler past ${shift} by ${notAfterMs.toString()}, its deadline`,
            );
          input.onStage(
            `advancing the scheduler past ${shift}, which ended at ${endMs.toString()}, before funding the ledger owner`,
          );
          await input.holdOperator2();
          let refreshed: Awaited<ReturnType<typeof input.refresh>> | undefined;
          let failure: unknown;
          try {
            refreshed = await input.refresh(
              BigInt(input.now() + stackUserEventFloorOffsetMs(timing)),
            );
          } catch (error) {
            failure = error;
          }
          // Whoever advanced it, decide on the shift it now holds; only an
          // ended shift that did not move waits or refuses.
          if (!sameShift(await input.readAppointment(), appointment)) continue;
          if (refreshed === undefined) throw failure;
          if (!("notDueForMs" in refreshed))
            throw new Error(
              `The scheduler refresh reported it aligned, but the scheduler still holds ${shift}, which ended at ${endMs.toString()}`,
            );
          // At least a slot, so that a refresh due now is retried on a later
          // one; none past the deadline, where the funding refuses anyway.
          const retryMs =
            input.now() + Math.max(refreshed.notDueForMs, VALIDITY_SLOT_MS);
          if (retryMs > notAfterMs)
            throw new Error(
              `The ledger owner's funding could not advance the scheduler past ${shift} by ${notAfterMs.toString()}, its deadline: the node holds it not due until ${retryMs.toString()}`,
            );
          await input.awaitLedgerTime(retryMs);
          continue;
        }
        if (appointment.operator === operator) {
          const floorMs = startMs + stackUserEventFloorOffsetMs(timing);
          if (floorMs > notAfterMs)
            throw new Error(
              `The ledger owner's funding would wait for operator-2's floor ${floorMs.toString()} in ${shift} past ${notAfterMs.toString()}, its deadline`,
            );
          return awaitFloor(startMs);
        }
        if (latestDueMs() + commitMarginMs <= endMs) return;
        if (awaited !== shift)
          input.onStage(
            `awaiting the end of ${shift} before funding the ledger owner, whose deposit could fall due in the next shift`,
          );
        awaited = shift;
        if (endMs > notAfterMs)
          throw new Error(
            `The ledger owner's funding would wait for the end of ${shift} past ${notAfterMs.toString()}, its deadline`,
          );
        await input.awaitLedgerTime(endMs);
      }
    },
    // Whether a built funding deposit, due at `inclusionTimeMs`, may be
    // submitted, judged on the deposit itself rather than foreseen, in a
    // shift that has not ended. In
    // operator-2's shift its strike threshold must fall at or after the
    // shift's end; in another operator's shift it must fall due early
    // enough for that operator to commit a block covering it.
    admitsFunding: async (operator: string, inclusionTimeMs: number) => {
      const appointment = await input.readAppointment();
      if (appointment === undefined) return true;
      const endMs = Number(appointment.startTime) + timing.shiftDurationMs;
      // Built against an ended shift, its due time falls under a shift no
      // transaction has fixed yet.
      if (input.now() >= endMs) return false;
      return appointment.operator === operator
        ? inclusionTimeMs + timing.negligenceTimeoutMs >= endMs
        : inclusionTimeMs + commitMarginMs <= endMs;
    },
  });
};
