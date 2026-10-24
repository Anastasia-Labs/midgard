import { fileURLToPath } from "node:url";

import {
  midgardSourceEnvironments,
  workspaceBundleProjects,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    reporters: "verbose",
    // Workspace packages load from a per-run source bundle; files that need
    // them module-by-module run in `lucid-midgard:source`.
    projects: workspaceBundleProjects(
      {
        extends: true,
        test: {
          name: "lucid-midgard",
          include: ["./tests/**/*.test.{ts,tsx}"],
        },
      },
      { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
    ),
  },
  environments: midgardSourceEnvironments(),
});
