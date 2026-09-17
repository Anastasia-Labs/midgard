import { describe, expect, it } from "vitest";

import {
  nextResolvedOutputAction,
  RESOLVED_OUTPUT_STAGES,
  type ResolvedOutputAction,
  type ResolvedOutputStage,
} from "../src/resolved-output-non-canonical/workflow.js";

/**
 * The submitter action owed at each durable stage, written out from the
 * family's documented step order (init, steps 01-03, the reconstruction loop,
 * step 05, removal, then nothing) rather than read back out of the production
 * switch. A dropped stage, a renamed action, or a wrong edge for the three
 * stages the old five-sample test never exercised fails here.
 */
const EXPECTED_NEXT_ACTION: Readonly<
  Record<ResolvedOutputStage, ResolvedOutputAction>
> = {
  none: "submitInit",
  step01: "submitStep01",
  step02: "submitStep02",
  step03: "submitStep03",
  reconstructing: "submitReconstruction",
  step05: "submitStep05",
  proven: "removeDescendants",
  removed: "done",
  cancelled: "done",
};

describe("resolvedOutputNonCanonical durable workflow", () => {
  it("declares exactly the nine durable stages the driver reconciles", () => {
    expect([...RESOLVED_OUTPUT_STAGES]).toEqual([
      "none",
      "step01",
      "step02",
      "step03",
      "reconstructing",
      "step05",
      "proven",
      "removed",
      "cancelled",
    ]);
  });

  it.each(RESOLVED_OUTPUT_STAGES)("owes %s its documented action", (stage) => {
    expect(nextResolvedOutputAction(stage)).toBe(EXPECTED_NEXT_ACTION[stage]);
  });

  it("terminates only after removal or cancellation", () => {
    const terminal = RESOLVED_OUTPUT_STAGES.filter(
      (stage) => nextResolvedOutputAction(stage) === "done",
    );
    expect(terminal).toEqual(["removed", "cancelled"]);
  });

  it("owes a distinct submission at every non-terminal stage", () => {
    const submitting = RESOLVED_OUTPUT_STAGES.map(
      nextResolvedOutputAction,
    ).filter((action) => action !== "done");
    expect(new Set(submitting).size).toBe(submitting.length);
  });
});
