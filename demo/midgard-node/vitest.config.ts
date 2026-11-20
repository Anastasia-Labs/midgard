import fs from "node:fs";

import path from "path";
import { configDefaults, defineConfig } from "vitest/config";

export default defineConfig({
  plugins: [
    {
      name: "raw-sql-loader",
      load(id) {
        if (!id.endsWith(".sql")) {
          return null;
        }
        return `export default ${JSON.stringify(fs.readFileSync(id, "utf8"))};`;
      },
    },
  ],
  test: {
    pool: "forks",
    fileParallelism: false,
    reporters: [["default", { summary: false }]],
    include: ["./tests/**/*.test.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
    exclude: [
      ...configDefaults.exclude,
      "./tests/phase4-pipelined-process-summary-verifier.test.mjs",
    ],
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
