import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  evaluateStateReconciliation,
  type StateReconciliationInput,
} from "../src/commands/state-reconciliation.js";
import {
  DA_TIMEOUT,
  expectOnlyFailure,
  h32,
  input,
  NOW,
  TIP,
} from "./state-reconciliation.consistent-sql.js";
export const registerDaAttestationReconciliationCases = () => {
  describe("da-attestation", () => {
    // The tip header ends at 3_000, so its deadline is 3_000 + DA_TIMEOUT.
    const DEADLINE = 3_000 + DA_TIMEOUT;
    const tipWith = (
      daStatus: SDK.DaAvailabilityStateQueueStatusKind,
      nowMs: number,
      allowInFlight = false,
    ): StateReconciliationInput => ({
      ...input(
        ({ l1 }) => ({
          l1: {
            ...l1,
            unmerged: l1.unmerged.map((h) => ({ ...h, daStatus })),
          },
        }),
        allowInFlight,
      ),
      nowMs,
    });
    const daCheck = (report: ReturnType<typeof evaluateStateReconciliation>) =>
      report.checks.find((c) => c.id === "da-attestation")!;

    it("keeps an honest catch-up header before its commit-based DA deadline", () => {
      const value = tipWith("Unattested", DEADLINE + 5_000);
      if (value.l1.kind !== "observed")
        throw new Error("fixture must carry an L1 view");
      const catchUp = {
        ...value,
        l1: {
          kind: "observed" as const,
          view: {
            ...value.l1.view,
            unmerged: value.l1.view.unmerged.map((header) => ({
              ...header,
              blockTimeMs: 100_000,
            })),
          },
        },
      };
      expect(daCheck(evaluateStateReconciliation(catchUp)).status).toBe("PASS");
    });

    it("fails alone when an unmerged header is Unattested past its deadline, naming it, the deadline and the lateness", () => {
      const report = evaluateStateReconciliation(
        tipWith("Unattested", DEADLINE + 5_000),
      );
      expectOnlyFailure(
        report,
        "da-attestation",
        `header ${TIP} (${h32("0a")}#0) is Unattested past its DA-attestation deadline ${new Date(DEADLINE).toISOString()} (${DEADLINE.toString()} ms) by 5000 ms`,
      );
      // A missed deadline is an inconsistency, not an in-flight state.
      expect(
        daCheck(
          evaluateStateReconciliation(
            tipWith("Unattested", DEADLINE + 5_000, true),
          ),
        ).status,
      ).toBe("FAIL");
    });

    it.each([
      ["well before", NOW, `in ${(DEADLINE - NOW).toString()} ms`],
      ["exactly at", DEADLINE, "in 0 ms"],
    ])(
      "passes with a note when an Unattested header is %s its deadline",
      (_label, nowMs, remaining) => {
        const report = evaluateStateReconciliation(
          tipWith("Unattested", nowMs),
        );
        expect(report.ok).toBe(true);
        const check = daCheck(report);
        expect(check.status).toBe("PASS");
        expect(check.inFlight).toEqual([]);
        expect(check.notes).toEqual([
          `header ${TIP} (${h32("0a")}#0) awaits DA attestation; deadline ${new Date(DEADLINE).toISOString()} (${DEADLINE.toString()} ms) ${remaining}`,
        ]);
      },
    );

    it.each(["Attested", "Challenged", "Published"] as const)(
      "passes silently for a %s header past the deadline",
      (daStatus) => {
        const report = evaluateStateReconciliation(
          tipWith(daStatus, DEADLINE + 5_000),
        );
        expect(report.ok).toBe(true);
        const check = daCheck(report);
        expect(check.status).toBe("PASS");
        expect(check.notes).toEqual([]);
      },
    );

    it("passes with a note, and leaves the failure to state-queue-journal, when a header datum is undecodable", () => {
      const report = evaluateStateReconciliation({
        ...input(({ l1 }) => ({
          l1: {
            ...l1,
            unmerged: l1.unmerged.map((h) => ({
              ...h,
              daStatus: null,
              blockTimeMs: null,
              endTimeMs: null,
              decodeError: "header datum is not a state-queue node",
            })),
          },
        })),
        nowMs: DEADLINE + 5_000,
      });
      const check = daCheck(report);
      expect(check.status).toBe("PASS");
      expect(check.notes).toEqual([
        `header ${TIP} (${h32("0a")}#0): DA status unknown, datum undecodable (state-queue-journal reports it)`,
      ]);
    });
  });
};
