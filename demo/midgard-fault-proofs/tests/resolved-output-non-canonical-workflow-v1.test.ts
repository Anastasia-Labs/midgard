import { describe, expect, it } from "vitest";

import { nextResolvedOutputActionV1 } from "../src/resolved-output-non-canonical/workflow-v1.js";

describe("resolvedOutputNonCanonical durable workflow", () => {
  it("orders all five steps, the reconstruction loop, and removal", () => {
    expect(nextResolvedOutputActionV1("none")).toBe("submitInit");
    expect(nextResolvedOutputActionV1("step03")).toBe("submitStep03");
    expect(nextResolvedOutputActionV1("reconstructing")).toBe(
      "submitReconstruction",
    );
    expect(nextResolvedOutputActionV1("step05")).toBe("submitStep05");
    expect(nextResolvedOutputActionV1("proven")).toBe("removeDescendants");
  });
});
