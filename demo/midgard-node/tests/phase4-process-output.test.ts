import { describe, expect, it } from "vitest";

import {
  assertSatisfiedReconciliation,
  parseTrailingJsonObject,
} from "@/commands/phase4-process-output.js";

describe("Phase 4 process command output", () => {
  it("parses the final reconciliation object after structured CLI logs", () => {
    const output = [
      '[04:00:00.000] INFO: L1 provider route: {"primary":"kupmios"}',
      "[04:00:00.100] INFO: reconciliation complete",
      JSON.stringify(
        { status: "satisfied", evidence: [{ count: 27 }] },
        null,
        2,
      ),
    ].join("\n");
    expect(parseTrailingJsonObject("PHAS registration", output)).toEqual({
      status: "satisfied",
      evidence: [{ count: 27 }],
    });
    expect(() =>
      assertSatisfiedReconciliation("PHAS registration", output),
    ).not.toThrow();
  });

  it("fails closed on missing, trailing, or unsatisfied reconciliation evidence", () => {
    expect(() =>
      parseTrailingJsonObject("PHAS registration", "only logs"),
    ).toThrow("did not emit a trailing JSON object");
    expect(() =>
      parseTrailingJsonObject(
        "PHAS registration",
        '{"status":"satisfied"}\ntrailing output',
      ),
    ).toThrow("did not emit a trailing JSON object");
    expect(() =>
      assertSatisfiedReconciliation(
        "PHAS registration",
        'log\n{"status":"unsatisfied"}',
      ),
    ).toThrow("is not satisfied");
  });
});
