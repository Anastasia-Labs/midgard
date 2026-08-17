import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    environment: "node",
    include: ["./tests/**/*.test.ts"],
    restoreMocks: true,
    // The focused-test evidence gate runs this suite single-worker on shared
    // CI runners, where the heaviest W13 restart/rewind tests (~3s locally)
    // overrun Vitest's 5s default. Headroom for slow runners, not a license
    // for slow tests.
    testTimeout: 60_000,
  },
});
