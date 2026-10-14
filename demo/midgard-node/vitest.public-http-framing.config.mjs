import { fileURLToPath } from "node:url";

import { workspaceBundleProjects } from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

import ordinary from "./vitest.config.ts";

/** Explicit manual framing gate: retained genuine constructor inputs are required.
 * Ordinary node CI does not collect it, and an absent input refuses this selector.
 */
export default defineConfig({
  ...ordinary,
  test: {
    ...ordinary.test,
    projects: workspaceBundleProjects(
      {
        extends: true,
        test: {
          name: "midgard-node-public-http-framing",
          include: ["./tests/public-http-release-framing.acceptance.test.ts"],
        },
      },
      { packageDirectory: fileURLToPath(new URL(".", import.meta.url)) },
    ),
  },
});
