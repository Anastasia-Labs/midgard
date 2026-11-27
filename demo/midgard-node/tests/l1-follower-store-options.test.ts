import { describe, expect, it } from "vitest";

import { nodeFollowerStoreOptions } from "../src/services/l1-follower.index-spec.js";

describe("nodeFollowerStoreOptions", () => {
  it("retains k + d blocks: the security parameter plus the commit event depth", () => {
    const options = nodeFollowerStoreOptions(
      {
        trackedSet: {
          addresses: new Set(),
          paymentCredentials: new Set(["aa".repeat(28)]),
          policies: new Set(["bb".repeat(28)]),
        },
        projections: {
          projections: [],
          bindActivity: () => undefined,
          bindRuntime: () => undefined,
        },
        securityParameter: 2160,
        wallets: [],
      },
      7,
    );
    expect(options.securityParameter).toBe(2160);
    expect(options.retainedDepth).toBe(2167);
  });
});
