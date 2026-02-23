import { defineConfig } from "vitest/config";
import path from "path";

export default defineConfig({
  test: {
    pool: "forks",
    reporters: [["default", { summary: false }]],
    include: ["./tests/**/*.test.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
    testTimeout: 420_000,
    bail: 3,
    environment: "node",
  },
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
  esbuild: {
    target: "es2020",
  },
});
