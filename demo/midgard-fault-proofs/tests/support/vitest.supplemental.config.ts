import { rawSqlLoaderPlugin } from "@al-ft/midgard-test-support/vitest";
import { defineConfig } from "vitest/config";

import baseConfig from "../../vitest.config.js";

/** Explicit supplemental release measurements; no maximum-shape cases. */
export default defineConfig({
  ...baseConfig,
  plugins: [rawSqlLoaderPlugin()],
  test: {
    ...baseConfig.test,
    include: [
      "./tests/validation-trace-dispute-installed-lifecycle.test.ts",
      "./tests/submit-init-emulator-transition-trace.test.ts",
      "../midgard-node/tests/cek-material-publication-emulator.test.ts",
    ],
    setupFiles: ["./tests/support/supplemental-measurement-setup.ts"],
    testNamePattern:
      /^(?:validation trace dispute installed production workflow awards an inconsistent committed terminal counter and removes the block directly after source verification|validation trace dispute installed production workflow plays the full honest game to award and removal, refusing forged and caller-authored material at the exact checks|validation trace dispute installed production workflow claims the timeout and removes the block when the operator stalls past its deadline|fault-proof emulator integration submits and removes a tail transition-trace fraud proof end to end|immutable CEK material publication publishes an ordinary blob chunk and ingests its exact typed material)$/u,
    poolOptions: { forks: { minForks: 1, maxForks: 1 } },
  },
});
