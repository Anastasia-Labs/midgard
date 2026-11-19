import { describe, expect, it } from "vitest";

import { RESOLVED_OUTPUT_STAGES } from "../src/resolved-output-non-canonical/workflow.js";

describe("resolvedOutputNonCanonical durable workflow", () => {
  it("declares exactly the ten durable stages", () => {
    expect([...RESOLVED_OUTPUT_STAGES]).toEqual([
      "none",
      "step01",
      "step02",
      "step03",
      "reconstructing",
      "step04Datum",
      "step05",
      "proven",
      "removed",
      "cancelled",
    ]);
  });
});
