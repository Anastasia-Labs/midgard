import type { OperatorShiftAppointment } from "midgard-watcher/tests/support/published-block-actor.operator-shift";
import { describe, expect, it } from "vitest";

import { lateShiftMinimumLeadMs } from "./honest-successor.observables.js";
import {
  stackUserEventFloorOffsetMs,
  type StackUserEventTiming,
} from "./stack-user-event-floor.js";
import {
  eventTiming,
  gateOn,
  OPERATOR_2,
  operator2Shift,
  OUR_NODE,
  SHIFT_START_MS,
} from "./stack-user-event-floor.test-support.js";

// Our node absorbs the ledger owner's funding deposit only once it is due.
// One built late in our node's shift may fall due in operator-2's, where
// operator-2's stopped node cannot absorb it and another operator may strike
// operator-2 for it. Its validity ends at the next slot past its time to
// live, dated from the wall clock, and it is due an event wait after that.
describe("the gate before the ledger owner's funding deposit in our node's shift", () => {
  it("holds the ledger owner's funding deposit back to the floor in operator-2's shift", async () => {
    const { gate, ledger } = gateOn({
      timing: eventTiming("preprod-testing"),
      appointment: operator2Shift,
      nowMs: SHIFT_START_MS,
    });
    await gate.beforeFunding(OPERATOR_2);
    expect(ledger.nowMs).toBe(SHIFT_START_MS + 240_000);
    expect(ledger.tailsRead).toEqual([]);
  });

  const profiles = ["preprod-testing", "preprod-rehearsal"] as const;
  const latestDueAfterMs = (timing: StackUserEventTiming) =>
    timing.eventTtlMs + 1_000 + timing.eventWaitMs;
  // Due no later than this before the shift's end, its operator can still
  // commit it: the late-shift lead of `gateOn`'s commit allowance.
  const marginMs = lateShiftMinimumLeadMs(190_000);
  /** Our node's shift, then operator-2's from `operator2StartMs`. */
  const shifts =
    (operator2StartMs: number) =>
    (nowMs: number): OperatorShiftAppointment =>
      nowMs < operator2StartMs
        ? { operator: OUR_NODE, startTime: BigInt(SHIFT_START_MS) }
        : { operator: OPERATOR_2, startTime: BigInt(operator2StartMs) };

  it.each(profiles)(
    "publishes at once while a deposit falls due early enough in the shift to be committed, and otherwise waits for operator-2's floor, in one of %s's shifts",
    async (name) => {
      const timing = eventTiming(name);
      const endMs = SHIFT_START_MS + timing.shiftDurationMs;
      const boundMs = endMs - latestDueAfterMs(timing) - marginMs;
      const early = gateOn({
        timing,
        appointment: shifts(endMs),
        nowMs: boundMs,
      });
      await early.gate.beforeFunding(OPERATOR_2);
      expect(early.ledger.nowMs).toBe(boundMs);
      expect(early.ledger.stages).toEqual([]);
      const floorMs = endMs + stackUserEventFloorOffsetMs(timing);
      for (const nowMs of [boundMs + 1, endMs - 1]) {
        const late = gateOn({
          timing,
          appointment: shifts(endMs),
          nowMs,
        });
        await late.gate.beforeFunding(OPERATOR_2);
        expect(late.ledger.nowMs).toBe(floorMs);
        expect(late.ledger.stages[0]).toMatch(
          /^awaiting the end of .* before funding the ledger owner/u,
        );
        // Held to the floor, it still falls due in operator-2's shift, so
        // our node absorbs it in its own next shift.
        expect(late.ledger.nowMs + latestDueAfterMs(timing)).toBeLessThan(
          endMs + timing.shiftDurationMs,
        );
      }
    },
  );

  it.each(profiles)(
    "applies operator-2's floor from its actual start when the scheduler advances late, in one of %s's shifts",
    async (name) => {
      const timing = eventTiming(name);
      const endMs = SHIFT_START_MS + timing.shiftDurationMs;
      const operator2StartMs = endMs + 50_000;
      const { gate, ledger } = gateOn({
        timing,
        appointment: shifts(operator2StartMs),
        nowMs: endMs - 1,
      });
      await gate.beforeFunding(OPERATOR_2);
      expect(ledger.nowMs).toBe(
        operator2StartMs + stackUserEventFloorOffsetMs(timing),
      );
    },
  );

  // The ledger trails the wall clock the deposit is dated from; the bound
  // follows whichever of the two is later.
  it.each(profiles)(
    "bounds the deposit from the wall clock when the ledger trails it, in one of %s's shifts",
    async (name) => {
      const timing = eventTiming(name);
      const endMs = SHIFT_START_MS + timing.shiftDurationMs;
      const { gate, ledger } = gateOn({
        timing,
        appointment: shifts(endMs),
        // One past the bound by the wall clock, 4.999 s inside it by the ledger.
        nowMs: endMs - latestDueAfterMs(timing) - marginMs - 5_000 + 1,
        wallAheadMs: 5_000,
      });
      await gate.beforeFunding(OPERATOR_2);
      expect(ledger.nowMs).toBe(endMs + stackUserEventFloorOffsetMs(timing));
    },
  );
});
