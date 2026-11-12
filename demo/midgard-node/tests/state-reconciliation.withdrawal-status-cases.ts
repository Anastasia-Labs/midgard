import { describe, expect, it } from "vitest";

import { evaluateStateReconciliation } from "../src/commands/state-reconciliation.js";
import {
  CONFIRMED,
  expectOnlyFailure,
  input,
  statuses,
  TIP,
} from "./state-reconciliation.consistent-sql.js";

/**
 * A withdrawal's status against its header's place: projected once its
 * block is applied, finalized only by the fold of the merge that takes the
 * block.
 */
export const registerWithdrawalStatusReconciliationCases = () => {
  describe("withdrawal status", () => {
    it("accepts a projected withdrawal of an applied block that is not merged yet", () => {
      const world = input();
      expect(
        world.sql.withdrawals.find((w) => w.projectedHeaderHash === TIP)
          ?.status,
      ).toBe("projected");
      expect(statuses(evaluateStateReconciliation(world)).withdrawals).toBe(
        "PASS",
      );
    });

    it("withdrawals fails alone when a merged header's withdrawal, not paid out yet, is not finalized", () => {
      const report = evaluateStateReconciliation(
        input(({ l1, sql }) => ({
          l1: { ...l1, payouts: [] },
          sql: {
            ...sql,
            withdrawals: sql.withdrawals.map((w) =>
              w.projectedHeaderHash === CONFIRMED
                ? { ...w, status: "projected" }
                : w,
            ),
          },
        })),
      );
      expectOnlyFailure(report, "withdrawals", "expected finalized");
    });
  });
};
