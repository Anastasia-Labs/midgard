import { describe, expect, it } from "vitest";

import { JOURNEY_FIXTURE_OWNERS } from "./catalogue.js";
import {
  JOURNEY_HEADER_END_AHEAD_MS,
  type JourneyCategory,
} from "./fixture.js";
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
const SHIFT_MS = 100_000;
const COMMIT_ALLOWANCE_MS = 1_000;
/**
 * The shortest lead: a commit's allowance plus the time the fault's header
 * runs past its build, so that both end inside operator-2's shift.
 */
const MINIMUM_LEAD_MS = COMMIT_ALLOWANCE_MS + JOURNEY_HEADER_END_AHEAD_MS + 1;
/** The drill's lead: four seconds of window past the shortest. */
const LEAD_MS = MINIMUM_LEAD_MS + 4_000;

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
    commitAllowanceMs: COMMIT_ALLOWANCE_MS,
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
      leadMs: LEAD_MS,
      ...drill.ports,
    })(operator2);
    const endMs = 20_000 + SHIFT_MS;
    expect(drill.now()).toBe(endMs - LEAD_MS);
    expect(drill.records).toEqual([
      {
        leadMs: LEAD_MS,
        commitAllowanceMs: COMMIT_ALLOWANCE_MS,
        shift: { startMs: 20_000, endMs },
        notBeforeMs: endMs - LEAD_MS,
        notAfterMs: endMs - MINIMUM_LEAD_MS + 1,
        commitStartMs: endMs - LEAD_MS,
      },
    ]);
  });

  it("commits at once inside the window", async () => {
    const drill = scripted({
      nowMs: 20_000 + SHIFT_MS - LEAD_MS + 500,
      appointment: { operator: operator2, startTime: 20_000n },
    });
    await stackLateShiftFaultCommit({
      category: convicted,
      leadMs: LEAD_MS,
      ...drill.ports,
    })(operator2);
    expect(drill.waits).toEqual([]);
    expect(drill.records).toMatchObject([
      { commitStartMs: 20_000 + SHIFT_MS - LEAD_MS + 500 },
    ]);
  });

  it("refuses a commit at a commit allowance before the shift's end, where the fault's header would end past it", async () => {
    const drill = scripted({
      nowMs: 20_000 + SHIFT_MS - COMMIT_ALLOWANCE_MS,
      appointment: { operator: operator2, startTime: 20_000n },
    });
    await expect(
      stackLateShiftFaultCommit({
        category: convicted,
        leadMs: LEAD_MS,
        ...drill.ports,
      })(operator2),
    ).rejects.toThrow(/missed its late-shift window/u);
    expect(drill.records).toEqual([]);
  });

  it("refuses past the window, where the commit could not land in operator-2's shift", async () => {
    const drill = scripted({
      nowMs: 20_000 + SHIFT_MS - COMMIT_ALLOWANCE_MS + 500,
      appointment: { operator: operator2, startTime: 20_000n },
    });
    await expect(
      stackLateShiftFaultCommit({
        category: convicted,
        leadMs: LEAD_MS,
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
          leadMs: LEAD_MS,
          ...drill.ports,
        })(operator2),
      ).rejects.toThrow(/does not appoint operator-2/u);
    }
  });

  it("refuses up front a lead shorter than a commit's allowance plus its fault header's end", () => {
    const { ports } = scripted({ nowMs: 21_000, appointment: undefined });
    for (const leadMs of [COMMIT_ALLOWANCE_MS, MINIMUM_LEAD_MS - 1])
      expect(() =>
        stackLateShiftFaultCommit({ category: convicted, leadMs, ...ports }),
      ).toThrow(
        `${STACK_LATE_SHIFT_LEAD_ENV} must be at least ${MINIMUM_LEAD_MS.toString()} ms`,
      );
  });
});
