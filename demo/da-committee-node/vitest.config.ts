import { fileURLToPath } from "node:url";

import {
  blueprintStampGlobalSetup,
  midgardSourceEnvironments,
  rawSqlLoaderPlugin,
  runSharedFixturesGlobalSetup,
  workspaceBundleProjects,
} from "@al-ft/midgard-test-support/vitest";
import { configDefaults, defineConfig } from "vitest/config";

export default defineConfig({
  // The deployment input is derived through midgard-node's sources, which
  // import their migrations as raw SQL.
  plugins: [rawSqlLoaderPlugin()],
  test: {
    environment: "node",
    // Vitest 4 narrowed `restoreMocks` to `vi.spyOn` spies; `mockReset` keeps
    // the Vitest 3 reset of every `vi.fn` (implementation, once-queue, calls)
    // before each test, so a module-scope mock cannot carry state forward.
    restoreMocks: true,
    mockReset: true,
    // Suites drop their test databases in `afterAll`; each `DROP DATABASE …
    // WITH (FORCE)` can outlast the 10 s hook default on a loaded CI runner.
    hookTimeout: 120_000,
    // Workspace packages load from a per-run source bundle; files that need
    // them module-by-module run in `da-committee-node:source`. Each project
    // gets its own `tempRoot` from the global setup. The deployment input is
    // derived from onchain/aiken/plutus.json once per run.
    projects: workspaceBundleProjects(
      {
        extends: true,
        test: {
          name: "da-committee-node",
          include: configDefaults.include,
          // `tsc` emits compiled copies of `tests/` under `dist/`; Vitest 4
          // dropped `dist` from its default excludes. Benchmarks run only
          // through `pnpm bench` (`vitest.bench.config.ts`): their timings
          // mean nothing under the default timeout on a shared runner.
          exclude: [...configDefaults.exclude, "**/dist/**", "bench/**"],
          globalSetup: [
            blueprintStampGlobalSetup,
            runSharedFixturesGlobalSetup,
            "./tests/global-setup.ts",
          ],
        },
      },
      { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
    ),
  },
  environments: midgardSourceEnvironments(),
});
