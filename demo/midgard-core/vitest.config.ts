import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    reporters: "verbose",
    include: ["./tests/**/*.test.{ts,tsx}"],
  },
  ssr: {
    resolve: {
      // Resolve workspace packages from source via the `midgard-source` exports
      // condition so a stale or missing dist can never shape a test result.
      // Vitest resolves test modules through Vite's SSR pipeline and sets
      // `ssr.resolve.conditions` itself, so the root `resolve.conditions` is
      // not consulted; the other entries restate Vitest's server defaults.
      conditions: ["midgard-source", "node", "development|production"],
    },
  },
});
