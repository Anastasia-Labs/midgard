import {
  isolatedForksPool,
  midgardSourceSsr,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

/** Read-only protocol checks need no database provisioning or operator startup. */
export default defineConfig({
  test: {
    ...isolatedForksPool({ maxForks: 1 }),
    include: ["tests/acceptance-native*.test.ts"],
    testTimeout: 10000,
    environment: "node",
  },
  ssr: midgardSourceSsr(),
});
