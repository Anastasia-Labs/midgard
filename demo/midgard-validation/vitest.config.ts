import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    reporters: "verbose",
    include: ["./tests/**/*.test.{ts,tsx}"],
  },
});
