import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    reporters: "verbose",
    include: ["./tests/**/*.test.{ts,tsx}"],
    // Fails a file that is approaching the wasm32 ceiling with a message that
    // names the cause, so a leaked-heap trap is never re-diagnosed as an
    // on-chain rejection. See tests/support/uplc-heap-guard.ts.
    setupFiles: ["./tests/support/uplc-heap-guard.ts"],
  },
});
