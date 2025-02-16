import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    pool: "forks",
    reporters: "verbose",
    include: ["./test/**/*.test.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
    testTimeout: 420_000,
    bail: 3,
  },
});
