import path from "node:path";

import { midgardSourceSsr } from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    reporters: "verbose",
    include: ["./tests/**/*.test.{ts,tsx}"],
  },
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
  ssr: midgardSourceSsr(),
});
