import { describe, expect, it } from "vitest";

import { MIDGARD_CONSENSUS_LIMITS, SCRIPT_BUDGET_BASIS } from "../src/index.js";

describe("script budgets", () => {
  it("hold every cost to 80% of the supported L1 per-transaction limits", () => {
    // The basis is a literal so the Node-run judge can import the module
    // without the rest of the package; this keeps it tied to the limits.
    expect(SCRIPT_BUDGET_BASIS).toStrictEqual({
      mem: (MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxMemoryUnits * 4) / 5,
      cpu: (MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxCpuUnits * 4) / 5,
    });
  });
});
