import { describe, expect, it } from "vitest";

import { JOURNEY_FIXTURE_OWNERS } from "./catalogue.js";
import type { JourneyCategory } from "./fixture.js";
import { STACK_DA_REFUSED_CATEGORIES } from "./stack-da-publication.js";
import {
  readStackLateShiftLeadMs,
  STACK_LATE_SHIFT_LEAD_ENV,
  stackLateShiftFaultCommit,
} from "./stack-late-shift.js";

const categories = Object.keys(JOURNEY_FIXTURE_OWNERS) as JourneyCategory[];
const convicted = categories.find(
  (category) => !STACK_DA_REFUSED_CATEGORIES.has(category),
)!;
const refused = [...STACK_DA_REFUSED_CATEGORIES][0]!;
const operator2 = "a2".repeat(28);
const SHIFT_MS = 10_000;

/** A ledger clock and scheduler, scripted; waits move the clock. */
const scripted = (
  input: Readonly<{
    nowMs: number;
    appointment: { operator: string; startTime: bigint } | undefined;
  }>,
) => {
  let now = input.nowMs;
  const stages: string[] = [];
  const records: unknown[] = [];
  const waits: number[] = [];
  const ports = {
    readAppointment: () => Promise.resolve(input.appointment),
    shiftDurationMs: SHIFT_MS,
    commitAllowanceMs: 1_000,
    now: () => now,
    awaitLedgerTime: (timeMs: number) => {
      waits.push(timeMs);
      now = Math.max(now, timeMs);
      return Promise.resolve();
    },
    record: (plan: unknown) => {
      records.push(plan);
      return Promise.resolve();
    },
    onStage: (name: string) => {
      stages.push(name);
    },
  };
  return { ports, stages, records, waits, now: () => now };
};

describe("the late-shift lead of an attached stack drill", () => {
  it("is read from the stack node environment, absent when unset", () => {
    expect(readStackLateShiftLeadMs({})).toBeUndefined();
    expect(readStackLateShiftLeadMs({ [STACK_LATE_SHIFT_LEAD_ENV]: "" })).toBe(
      undefined,
    );
    expect(
      readStackLateShiftLeadMs({ [STACK_LATE_SHIFT_LEAD_ENV]: "420000" }),
    ).toBe(420_000);
  });

  it("refuses a malformed lead by its variable's name", () => {
    for (const value of ["0", "-5", "1.5", "7 minutes", "1e400"])
      expect(() =>
        readStackLateShiftLeadMs({ [STACK_LATE_SHIFT_LEAD_ENV]: value }),
      ).toThrow(STACK_LATE_SHIFT_LEAD_ENV);
  });

  it("is required before staging a family the stack's committee attests", () => {
    const { ports } = scripted({ nowMs: 0, appointment: undefined });
    expect(() =>
      stackLateShiftFaultCommit({
        category: convicted,
        leadMs: undefined,
        ...ports,
      }),
    ).toThrow(STACK_LATE_SHIFT_LEAD_ENV);
  });

  it("is not needed for a family the committee refuses, which commits as soon as it may", async () => {
    const drill = scripted({
      nowMs: 1_000,
      appointment: { operator: operator2, startTime: 0n },
    });
    await stackLateShiftFaultCommit({
      category: refused,
      leadMs: undefined,
      ...drill.ports,
    })(operator2);
    expect(drill.waits).toEqual([]);
    expect(drill.records).toEqual([]);
  });
});

describe("the fault's commit late in operator-2's shift", () => {
  it("waits for the lead before the shift's end, then records its window", async () => {
    const drill = scripted({
      nowMs: 21_000,
      appointment: { operator: operator2, startTime: 20_000n },
    });
    await stackLateShiftFaultCommit({
      category: convicted,
      leadMs: 4_000,
      ...drill.ports,
    })(operator2);
    expect(drill.now()).toBe(26_000);
    expect(drill.records).toEqual([
      {
        leadMs: 4_000,
        commitAllowanceMs: 1_000,
        shift: { startMs: 20_000, endMs: 30_000 },
        notBeforeMs: 26_000,
        notAfterMs: 29_000,
        commitStartMs: 26_000,
      },
    ]);
  });

  it("commits at once inside the window", async () => {
    const drill = scripted({
      nowMs: 27_500,
      appointment: { operator: operator2, startTime: 20_000n },
    });
    await stackLateShiftFaultCommit({
      category: convicted,
      leadMs: 4_000,
      ...drill.ports,
    })(operator2);
    expect(drill.waits).toEqual([]);
    expect(drill.records).toMatchObject([{ commitStartMs: 27_500 }]);
  });

  it("refuses past the window, where the commit could not land in operator-2's shift", async () => {
    const drill = scripted({
      nowMs: 29_500,
      appointment: { operator: operator2, startTime: 20_000n },
    });
    await expect(
      stackLateShiftFaultCommit({
        category: convicted,
        leadMs: 4_000,
        ...drill.ports,
      })(operator2),
    ).rejects.toThrow(/missed its late-shift window/u);
    expect(drill.records).toEqual([]);
  });

  it("refuses when the scheduler does not appoint operator-2", async () => {
    for (const appointment of [
      undefined,
      { operator: "b1".repeat(28), startTime: 20_000n },
    ]) {
      const drill = scripted({ nowMs: 21_000, appointment });
      await expect(
        stackLateShiftFaultCommit({
          category: convicted,
          leadMs: 4_000,
          ...drill.ports,
        })(operator2),
      ).rejects.toThrow(/does not appoint operator-2/u);
    }
  });

  it("refuses a lead no longer than a commit takes", async () => {
    const drill = scripted({
      nowMs: 21_000,
      appointment: { operator: operator2, startTime: 20_000n },
    });
    await expect(
      stackLateShiftFaultCommit({
        category: convicted,
        leadMs: 1_000,
        ...drill.ports,
      })(operator2),
    ).rejects.toThrow(/lead must exceed the commit allowance/u);
  });
});
