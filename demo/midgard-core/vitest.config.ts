import { fileURLToPath } from "node:url";

import {
  midgardSourceEnvironments,
  workspaceBundleProjects,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

const WORKER_THREAD_TESTS = "./tests/**/*.worker-thread.test.ts";

export default defineConfig({
  test: {
    reporters: "verbose",
    projects: [
      // Workspace code loads from a per-run source bundle; files that need it
      // module-by-module run in `core:source`.
      ...workspaceBundleProjects(
        {
          extends: true,
          test: {
            name: "core",
            include: ["./tests/**/*.test.{ts,tsx}"],
            exclude: [WORKER_THREAD_TESTS],
          },
        },
        { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
      ),
      {
        // Depth cases that must also hold on a worker thread's stack, where
        // the node's validation worker runs the same code.
        extends: true,
        test: {
          name: "core-worker-thread",
          include: [WORKER_THREAD_TESTS],
          pool: "threads",
        },
      },
    ],
  },
  environments: midgardSourceEnvironments(),
});
