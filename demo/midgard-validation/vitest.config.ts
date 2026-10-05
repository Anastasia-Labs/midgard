import { fileURLToPath } from "node:url";

import {
  blueprintStampGlobalSetup,
  midgardSourceSsr,
  workspaceBundleProjects,
} from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

const WORKER_THREAD_TESTS = "./tests/**/*.worker-thread.test.ts";

export default defineConfig({
  test: {
    // Refuses the run when onchain/aiken/plutus.json is stale.
    globalSetup: [blueprintStampGlobalSetup],
    reporters: "verbose",
    workspace: [
      // Workspace packages load from a per-run source bundle; files that need
      // them module-by-module run in `validation:source`.
      ...workspaceBundleProjects(
        {
          extends: true,
          test: {
            name: "validation",
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
          name: "validation-worker-thread",
          include: [WORKER_THREAD_TESTS],
          pool: "threads",
        },
      },
    ],
  },
  ssr: midgardSourceSsr(),
});
