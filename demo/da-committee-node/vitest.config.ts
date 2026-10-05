import { fileURLToPath } from "node:url";

import {
  midgardSourceSsr,
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
    workspace: workspaceBundleProjects(
      {
        extends: true,
        test: {
          name: "da-committee-node",
          include: configDefaults.include,
          globalSetup: ["./tests/global-setup.ts"],
        },
      },
      { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
    ),
  },
  ssr: midgardSourceSsr(),
});
