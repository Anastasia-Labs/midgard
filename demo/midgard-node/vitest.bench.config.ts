import fs from "node:fs";

import { defineConfig } from "vitest/config";

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
    maxWorkers: 1,
    minWorkers: 1,
    reporters: [["default", { summary: false }]],
    include: ["./tests/**/*.bench.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
    testTimeout: 900_000,
    bail: 1,
    environment: "node",
  },
  ssr: {
    resolve: {
      // Same source-first workspace resolution as vitest.config.ts.
      conditions: ["midgard-source", "node", "development|production"],
    },
  },
  esbuild: {
    target: "es2020",
  },
});
