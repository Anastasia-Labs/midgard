import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { lateShiftMinimumLeadMs } from "./honest-successor.observables.js";
import { stackUserEventFloorOffsetMs } from "./stack-user-event-floor.js";
import {
  eventTiming,
  gateOn,
  OPERATOR_2,
  OUR_NODE,
  SHIFT_START_MS,
  soonestDueOnGrid,
  strikeThreshold,
} from "./stack-user-event-floor.test-support.js";

/** A real 1 s slot grid, the Lucid emulator's, as twin4's chain has. */
let lucid: LucidEvolution;
beforeAll(async () => {
  lucid = await Lucid(
    new Emulator([generateEmulatorAccount({ lovelace: 1_000_000_000n })]),
    "Custom",
  );
});

type Shift = { operator: string; startTime: bigint };

/**
 * An idle stack's scheduler: no node commits, so nothing but the drill's own
 * refresh advances it. A refresh follows the on-chain rule: an ended shift
 * passes to its one successor, starting at the advancing transaction's
 * validity start (`scheduler.ak`, `validate_end_of_shift_and_get_operators`),
 * here the ledger time it is asked at. Like the node's alignment for
 * operator-2, it reports aligned once operator-2 holds the new shift, and
 * not due until a shift it pushed to another operator ends.
 */
const idleScheduler = (holder: string, shiftDurationMs: number) => {
  const state = {
    shift: { operator: holder, startTime: BigInt(SHIFT_START_MS) } as Shift,
    advancedAt: [] as number[],
  };
  const successor = (operator: string) =>
    operator === OUR_NODE ? OPERATOR_2 : OUR_NODE;
  return {
    state,
    appointment: () => state.shift,
    refresh: async (_: bigint, nowMs: number) => {
      const endMs = Number(state.shift.startTime) + shiftDurationMs;
      if (nowMs < endMs) return { notDueForMs: endMs - nowMs };
      state.shift = {
        operator: successor(state.shift.operator),
        startTime: BigInt(nowMs),
      };
      state.advancedAt.push(nowMs);
      return state.shift.operator === OPERATOR_2
        ? ({ kind: "aligned" } as const)
        : { notDueForMs: shiftDurationMs };
    },
  };
};

const profiles = ["preprod-testing", "preprod-rehearsal"] as const;

// The datum of a stack idle since long before the drill: its shift ended
// many shifts ago, and no node will move it.
const STALE_SHIFTS = 64;

describe("the ledger owner's funding on an idle stack's stale scheduler", () => {
  it.each(profiles)(
    "advances a stale shift of our node's into operator-2's and holds the deposit to its floor, where no strike can land, in %s",
    async (name) => {
      const timing = eventTiming(name);
      const shiftMs = timing.shiftDurationMs;
      const nowMs = SHIFT_START_MS + STALE_SHIFTS * shiftMs + 12_345;
      const scheduler = idleScheduler(OUR_NODE, shiftMs);
      const { gate, ledger } = gateOn({
        timing,
        appointment: scheduler.appointment,
        nowMs,
        refresh: scheduler.refresh,
      });
      await gate.beforeFunding(OPERATOR_2, nowMs + 2 * shiftMs + 600_000);
      // The drill's advance made the shift the deposit falls due in.
      expect(scheduler.state.advancedAt).toEqual([nowMs]);
      expect(scheduler.state.shift).toEqual({
        operator: OPERATOR_2,
        startTime: BigInt(nowMs),
      });
      expect(ledger.operator2Held).toBe(true);
      expect(ledger.nowMs).toBe(nowMs + stackUserEventFloorOffsetMs(timing));
      // Built now, the deposit falls due in operator-2's shift too late for
      // any operator to strike operator-2 for it there.
      const dueMs = soonestDueOnGrid(lucid, timing, ledger.nowMs);
      expect(dueMs).toBeLessThan(nowMs + shiftMs);
      expect(strikeThreshold(timing, nowMs, dueMs).kind).toBe("unsatisfiable");
      expect(await gate.admitsFunding(OPERATOR_2, dueMs)).toBe(true);
    },
  );

  it.each(profiles)(
    "advances a stale shift of operator-2's into our node's and funds at once, in %s",
    async (name) => {
      const timing = eventTiming(name);
      const shiftMs = timing.shiftDurationMs;
      const nowMs = SHIFT_START_MS + STALE_SHIFTS * shiftMs + 12_345;
      const scheduler = idleScheduler(OPERATOR_2, shiftMs);
      const { gate, ledger } = gateOn({
        timing,
        appointment: scheduler.appointment,
        nowMs,
        refresh: scheduler.refresh,
      });
      await gate.beforeFunding(OPERATOR_2, nowMs + 2 * shiftMs + 600_000);
      expect(scheduler.state.advancedAt).toEqual([nowMs]);
      expect(scheduler.state.shift).toEqual({
        operator: OUR_NODE,
        startTime: BigInt(nowMs),
      });
      expect(ledger.operator2Held).toBe(true);
      expect(ledger.nowMs).toBe(nowMs);
      // Due early enough in our node's own new shift for it to commit, the
      // deposit never falls due in a shift operator-2 holds.
      const latestDueMs =
        nowMs + timing.eventTtlMs + 1_000 + timing.eventWaitMs;
      expect(latestDueMs + lateShiftMinimumLeadMs(190_000)).toBeLessThanOrEqual(
        nowMs + shiftMs,
      );
      expect(await gate.admitsFunding(OPERATOR_2, latestDueMs)).toBe(true);
    },
  );

  const timing = eventTiming("preprod-testing");
  const shiftMs = timing.shiftDurationMs;
  const nowMs = SHIFT_START_MS + STALE_SHIFTS * shiftMs;
  const deadlineMs = nowMs + 2 * shiftMs + 600_000;
  const stale = (operator: string) => ({
    operator,
    startTime: BigInt(SHIFT_START_MS),
  });

  it("holds operator-2's node stopped only to advance an ended shift", async () => {
    const { gate, ledger } = gateOn({
      timing,
      appointment: { operator: OUR_NODE, startTime: BigInt(nowMs) },
      nowMs,
      refresh: () => Promise.reject(new Error("A live shift needs no advance")),
    });
    await gate.beforeFunding(OPERATOR_2, deadlineMs);
    expect(ledger.operator2Held).toBe(false);
    expect(ledger.nowMs).toBe(nowMs);
  });

  it("decides on another operator's advance that overtakes the drill's", async () => {
    const shift = { current: stale(OUR_NODE) };
    const { gate, ledger } = gateOn({
      timing,
      appointment: () => shift.current,
      nowMs,
      refresh: async (_, atMs) => {
        shift.current = {
          operator: OPERATOR_2,
          startTime: BigInt(atMs - 5_000),
        };
        throw new Error("The scheduler input was spent by another transaction");
      },
    });
    await gate.beforeFunding(OPERATOR_2, deadlineMs);
    expect(ledger.nowMs).toBe(
      nowMs - 5_000 + stackUserEventFloorOffsetMs(timing),
    );
  });

  it("refuses when its refresh fails and the scheduler stays put", async () => {
    const { gate } = gateOn({
      timing,
      appointment: stale(OUR_NODE),
      nowMs,
      refresh: () => Promise.reject(new Error("refresh rejected by the node")),
    });
    await expect(gate.beforeFunding(OPERATOR_2, deadlineMs)).rejects.toThrow(
      "refresh rejected by the node",
    );
  });

  it("refuses, without waiting past its deadline, when the stale shift never becomes advanceable", async () => {
    const { gate, ledger } = gateOn({
      timing,
      appointment: stale(OUR_NODE),
      nowMs,
      refresh: async () => ({ notDueForMs: 60_000 }),
    });
    await expect(gate.beforeFunding(OPERATOR_2, deadlineMs)).rejects.toThrow(
      new RegExp(
        `could not advance the scheduler past ${OUR_NODE}'s shift from ${SHIFT_START_MS.toString()} by ${deadlineMs.toString()}`,
        "u",
      ),
    );
    expect(ledger.nowMs).toBeGreaterThan(deadlineMs - 60_000);
    expect(ledger.nowMs).toBeLessThanOrEqual(deadlineMs);
  });

  it("refuses, without waiting past its deadline, when operator-2's floor falls after it", async () => {
    const floorMs = nowMs + stackUserEventFloorOffsetMs(timing);
    const { gate, ledger } = gateOn({
      timing,
      appointment: { operator: OPERATOR_2, startTime: BigInt(nowMs) },
      nowMs,
    });
    await expect(gate.beforeFunding(OPERATOR_2, floorMs - 1)).rejects.toThrow(
      /would wait for operator-2's floor .* past .*, its deadline/u,
    );
    expect(ledger.nowMs).toBe(nowMs);
  });
});

describe("the drill's user-event gate on a shift that has ended", () => {
  const timing = eventTiming("preprod-testing");
  const endMs = SHIFT_START_MS + timing.shiftDurationMs;

  it.each([OPERATOR_2, OUR_NODE])(
    "admits no funding deposit built in %s's ended shift",
    async (operator) => {
      const { gate } = gateOn({
        timing,
        appointment: { operator, startTime: BigInt(SHIFT_START_MS) },
        nowMs: endMs,
      });
      expect(
        await gate.admitsFunding(OPERATOR_2, endMs + timing.shiftDurationMs),
      ).toBe(false);
    },
  );

  it("publishes no family user event in operator-2's ended shift", async () => {
    const { gate, ledger } = gateOn({
      timing,
      appointment: { operator: OPERATOR_2, startTime: BigInt(SHIFT_START_MS) },
      nowMs: endMs,
    });
    await expect(gate.beforeEvent(OPERATOR_2, 0n)).rejects.toThrow(
      /shift from .* has ended/u,
    );
    expect(ledger.tailsRead).toEqual([]);
  });
});
