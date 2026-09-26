import { midgardSourceSsr } from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    environment: "node",
    globalSetup: ["./tests/global-setup.ts"],
    restoreMocks: true,
  },
  ssr: midgardSourceSsr(),
});
