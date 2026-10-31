import { describe, expect, it } from "vitest";

import { planCommitEventWindow } from "../src/workers/utils/commit-block-planner.event-window.js";
export const registerCatchUpEventWindowCases = () => {
  describe("whole-instant catch-up event selection", () => {
    const limits = {
      deposits: 10,
      forcedTransactions: 10,
      withdrawals: 10,
      totalEvents: 30,
    };
    const group = (
      inclusionTimeMs: number,
      deposits = 0,
      forcedTransactions = 0,
      withdrawals = 0,
    ) => ({ inclusionTimeMs, deposits, forcedTransactions, withdrawals });
    const plan = (
      groups: readonly ReturnType<typeof group>[],
      horizonMs = 100,
      blockTimeMs = 100,
    ) =>
      planCommitEventWindow({
        groups,
        startTimeMs: 0,
        horizonMs,
        blockTimeMs,
        limits,
      });

    it("caps a six-hour backlog at the last complete instant and leaves the next instant pending", () => {
      expect(
        plan([group(10, 6, 2), group(20, 4, 3), group(30, 1, 1)]),
      ).toMatchObject({
        mode: "catch_up",
        endTimeMs: 20,
        counts: { deposits: 10, forcedTransactions: 5, withdrawals: 0 },
        includedInstantCount: 2,
      });
    });
    it("admits an exact kind cap but never cuts the overflowing instant", () => {
      expect(plan([group(10, 10), group(20, 0, 11)])).toMatchObject({
        mode: "catch_up",
        endTimeMs: 10,
        includedInstantCount: 1,
      });
      expect(plan([group(10, 11)])).toMatchObject({
        mode: "instant_exceeds_cap",
        inclusionTimeMs: 10,
      });
    });
    it("uses the total cap across all kinds", () => {
      expect(
        planCommitEventWindow({
          groups: [group(10, 6, 6, 6), group(20, 2, 2, 2)],
          startTimeMs: 0,
          horizonMs: 100,
          blockTimeMs: 100,
          limits: { ...limits, totalEvents: 20 },
        }),
      ).toMatchObject({ mode: "catch_up", endTimeMs: 10 });
    });
    it("skips a quiet stretch without claiming events beyond the authenticated horizon", () => {
      expect(plan([group(10, 1), group(90, 1)], 80)).toMatchObject({
        mode: "catch_up",
        endTimeMs: 10,
      });
      expect(plan([], 80)).toMatchObject({ mode: "empty" });
    });
    it("keeps normal mode only when complete coverage reaches the commit clock", () => {
      expect(plan([group(10, 1)])).toMatchObject({
        mode: "normal",
        endTimeMs: 100,
      });
      expect(plan([group(10, 1)], 90)).toMatchObject({
        mode: "catch_up",
        endTimeMs: 10,
      });
      expect(plan([])).toMatchObject({ mode: "normal", endTimeMs: 100 });
    });
    it("refuses stale unassigned events instead of silently skipping them", () => {
      expect(() =>
        planCommitEventWindow({
          groups: [group(10, 1)],
          startTimeMs: 10,
          horizonMs: 100,
          blockTimeMs: 100,
          limits,
        }),
      ).toThrow("unassigned event does not follow the previous coverage end");
    });
  });
};
