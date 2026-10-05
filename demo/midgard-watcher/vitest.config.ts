import { availableParallelism } from "node:os";
import { fileURLToPath } from "node:url";

import {
  blueprintStampGlobalSetup,
  interactiveEmulatorBlueprint,
  interactiveEmulatorPlugin,
  interactiveEmulatorSetup,
  midgardSourceSsr,
  rawSqlLoaderPlugin,
  workspaceBundleProjects,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

/**
 * How many test files may run at once. Overridable with
 * `MIDGARD_WATCHER_FORKS`; anything that is not a positive integer falls back
 * to the default, which is half the host's CPUs clamped to [2, 8]. A two-core
 * shared runner therefore keeps the historical two-fork ceiling, while a
 * development host runs the suite in roughly a fifth of that wall time; the
 * suite's total CPU work does not change with the cap.
 */
const DEFAULT_MIN_FORKS = 2;
const DEFAULT_MAX_FORKS = 8;

const parseMaxForks = (raw: string | undefined): number => {
  if (raw !== undefined) {
    const parsed = Number(raw.trim());
    if (Number.isInteger(parsed) && parsed >= 1) {
      return parsed;
    }
  }
  return Math.min(
    DEFAULT_MAX_FORKS,
    Math.max(DEFAULT_MIN_FORKS, Math.floor(availableParallelism() / 2)),
  );
};

const maxForks = parseMaxForks(process.env.MIDGARD_WATCHER_FORKS);

export default defineConfig({
  plugins: [rawSqlLoaderPlugin()],
  test: {
    // Refuses the run when onchain/aiken/plutus.json is stale.
    workspace: [
      // Workspace packages load from a per-run source bundle; files that need
      // them module-by-module run in `testing-profile:source`.
      ...workspaceBundleProjects(
        {
          extends: true,
          test: {
            name: "testing-profile",
            include: ["./tests/**/*.test.ts"],
            exclude: ["./tests/fault-proofs/watcher-installed-journey.test.ts"],
            globalSetup: [blueprintStampGlobalSetup],
          },
        },
        { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
      ),
      // Not bundled: its plugin rewrites a midgard-core module.
      {
        extends: true,
        plugins: [interactiveEmulatorPlugin()],
        test: {
          name: "interactive-emulator",
          include: ["./tests/fault-proofs/watcher-installed-journey.test.ts"],
          globalSetup: [interactiveEmulatorSetup],
          env: { MIDGARD_REAL_BLUEPRINT_PATH: interactiveEmulatorBlueprint },
        },
      },
    ],
    environment: "node",
    restoreMocks: true,
    // Several files perform CPU-heavy replay and emulator evaluation. An
    // unbounded thread pool can starve Vitest's worker RPC long enough for
    // successful tests to be reported as `Timeout calling onTaskUpdate`.
    // Isolated forks keep evaluator state file-local (running the suite
    // without isolation fails ~75 tests on shared module state); the fork
    // ceiling bounds contention and memory on shared CI runners.
    pool: "forks",
    poolOptions: {
      forks: {
        isolate: true,
        minForks: 1,
        maxForks,
      },
    },
    // The heaviest restart/rewind tests can exceed Vitest's 5s default on
    // shared runners. Headroom for slow runners, not a license for slow tests.
    testTimeout: 60_000,
  },
  ssr: midgardSourceSsr(),
});
