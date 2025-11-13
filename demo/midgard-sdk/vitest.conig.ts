import { defineConfig } from "vitest/config";
import path from "path";

export default defineConfig({
  test: {
    reporters: "verbose",
    include: ["./tests/**/*.test.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"],
  },
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src/tx-builder"),
    },
  },
});
