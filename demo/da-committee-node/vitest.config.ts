import { fileURLToPath } from "node:url";

import {
  midgardSourceEnvironments,
  workspaceBundleProjects,
} from "@al-ft/midgard-test-support/vitest";
import { configDefaults, defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    environment: "node",
    restoreMocks: true,
    // Workspace packages load from a per-run source bundle; files that need
    // them module-by-module run in `da-committee-node:source`. Each project
    // gets its own `tempRoot` from the global setup.
    projects: workspaceBundleProjects(
      {
        extends: true,
        test: {
          name: "da-committee-node",
          include: configDefaults.include,
          // `tsc` emits compiled copies of `tests/` under `dist/`; Vitest 4
          // dropped `dist` from its default excludes.
          exclude: [...configDefaults.exclude, "**/dist/**"],
          globalSetup: ["./tests/global-setup.ts"],
        },
      },
      { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
    ),
  },
  environments: midgardSourceEnvironments(),
});
