import {
  blueprintStampGlobalSetup,
  midgardSourceEnvironments,
} from "@al-ft/midgard-test-support/vitest";
import { configDefaults, defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    // The emulator suites read onchain/aiken/plutus.json; refuse a stale one.
    globalSetup: [blueprintStampGlobalSetup],
    environment: "node",
    include: ["tests/**/*.test.ts"],
    exclude: [...configDefaults.exclude, "**/dist/**"],
  },
  environments: midgardSourceEnvironments(),
});
