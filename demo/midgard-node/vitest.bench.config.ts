import { defineConfig } from "vitest/config";
import fs from "node:fs";
import path from "path";

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
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
  esbuild: {
    target: "es2020",
  },
});
