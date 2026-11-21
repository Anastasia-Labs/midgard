import { expect, it } from "vitest";

import {
  createE2ERunSummary,
  updateE2ERunSummary,
} from "../src/e2e/summary.js";
import { step } from "./e2e-summary.stress-summary.js";

export const registerRequiredGateFailureScenarios = () => {
  it("fails the run when a required evidence gate fails", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-failed-gate" }),
      {
        steps: [step({ id: "readyz", status: "success" })],
        db: [
          {
            label: "stack_fresh_deployment",
            status: "failed",
            source: "e2e-stack",
            details: { missing: "initialize-submit" },
          },
        ],
      },
    );

    expect(summary.verdict).toBe("failed");
    expect(summary.cleanRunVerdict).toBe("success");
    expect(summary.functionalVerdict).toBe("failed");
    expect(summary.nextSafeAction).toBe("investigate_unknown");
  });

  it("reports a failed gate as failed when another gate is blocked", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-failed-and-blocked" }),
      {
        steps: [
          step({ id: "readyz", status: "success" }),
          step({ id: "deposit", status: "timeout" }),
          step({ id: "withdraw", status: "failed" }),
        ],
        db: [
          {
            label: "stack_fresh_deployment",
            status: "failed",
            source: "e2e-stack",
            details: { missing: "initialize-submit" },
          },
          {
            label: "deposit_projection",
            status: "blocked",
            source: "e2e-stack",
            details: {},
          },
        ],
        cleanRunGates: [
          {
            label: "stack_services",
            status: "blocked",
            source: "e2e-stack",
            details: {},
          },
          {
            label: "stack_deployment",
            status: "failed",
            source: "e2e-stack",
            details: {},
          },
        ],
      },
    );

    expect(summary.cleanRunVerdict).toBe("failed");
    expect(summary.functionalVerdict).toBe("failed");
    expect(summary.verdict).toBe("failed");
  });
};
