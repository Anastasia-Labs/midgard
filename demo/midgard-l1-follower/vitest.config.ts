import { midgardSourceEnvironments } from "@al-ft/midgard-test-support/vitest";
import { configDefaults, defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    environment: "node",
    include: ["tests/**/*.test.ts"],
    exclude: [...configDefaults.exclude, "**/dist/**"],
    // The property tests replay a random chain after every step; their
    // budget is the 10^4-operation run on the shared test Postgres.
    testTimeout: 900_000,
    hookTimeout: 120_000,
  },
  environments: midgardSourceEnvironments(),
});
