import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    environment: "node",
    include: ["./tests/**/*.test.ts"],
    restoreMocks: true,
    // Several files perform CPU-heavy replay and emulator evaluation. An
    // unbounded thread pool can starve Vitest's worker RPC long enough for
    // successful tests to be reported as `Timeout calling onTaskUpdate`.
    // Isolated forks keep evaluator state file-local; the two-fork ceiling
    // bounds contention and memory on shared CI runners.
    pool: "forks",
    poolOptions: {
      forks: {
        isolate: true,
        minForks: 1,
        maxForks: 2,
      },
    },
    // The heaviest restart/rewind tests can exceed Vitest's 5s default on
    // shared runners. Headroom for slow runners, not a license for slow tests.
    testTimeout: 60_000,
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
