import {
  blueprintStampGlobalSetup,
  midgardSourceSsr,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    // Refuses the run when onchain/aiken/plutus.json is stale.
    globalSetup: [blueprintStampGlobalSetup],
    reporters: "verbose",
    include: ["./tests/**/*.test.{ts,tsx}"],
  },
  ssr: midgardSourceSsr(),
});
