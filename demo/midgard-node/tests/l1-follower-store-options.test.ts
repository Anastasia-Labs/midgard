import { describe, expect, it } from "vitest";

import { nodeFollowerStoreOptions } from "../src/services/l1-follower.store-options.js";

describe("nodeFollowerStoreOptions", () => {
  it("retains k + d blocks: the security parameter plus the commit event depth", () => {
    const options = nodeFollowerStoreOptions({
      projections: [],
      securityParameter: 2160,
      commitEventDepth: 7,
      protocolPaymentCredentials: ["aa".repeat(28)],
      hubOraclePolicyId: "bb".repeat(28),
    });
    expect(options.securityParameter).toBe(2160);
    expect(options.retainedDepth).toBe(2167);
    expect(options.trackedSet.paymentCredentials).toEqual(
      new Set(["aa".repeat(28)]),
    );
    expect(options.trackedSet.policies).toEqual(new Set(["bb".repeat(28)]));
  });
});
