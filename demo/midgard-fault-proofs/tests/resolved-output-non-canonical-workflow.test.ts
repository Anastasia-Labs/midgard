import { describe, expect, it } from "vitest";

import { nextResolvedOutputAction } from "../src/resolved-output-non-canonical/workflow.js";

describe("resolvedOutputNonCanonical durable workflow", () => {
  it("orders all five steps, the reconstruction loop, and removal", () => {
    expect(nextResolvedOutputAction("none")).toBe("submitInit");
    expect(nextResolvedOutputAction("step03")).toBe("submitStep03");
    expect(nextResolvedOutputAction("reconstructing")).toBe(
      "submitReconstruction",
    );
    expect(nextResolvedOutputAction("step05")).toBe("submitStep05");
    expect(nextResolvedOutputAction("proven")).toBe("removeDescendants");
  });
});
