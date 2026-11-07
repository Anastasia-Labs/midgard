import { describe, expect, it } from "vitest";

import { isRetryableProviderError } from "../src/provider-retry.js";

// The startup retry loop in `commands/listen.ts` classifies through this
// shared predicate, so transient HTTP statuses retry at startup too.
describe("isRetryableProviderError", () => {
  it.each(["429", "500", "502", "503", "504"])(
    "retries a transient HTTP %s provider response",
    (status) => {
      expect(
        isRetryableProviderError(
          new Error(`Couldn't perform query. Received status code ${status}`),
        ),
      ).toBe(true);
    },
  );

  it("does not retry a deterministic provider failure", () => {
    expect(
      isRetryableProviderError(
        new Error("Expected at most one hub-oracle witness UTxO"),
      ),
    ).toBe(false);
  });
});
