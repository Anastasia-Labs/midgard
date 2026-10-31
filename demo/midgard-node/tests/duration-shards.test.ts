import { fileURLToPath } from "node:url";

import { describeDurationShards } from "@al-ft/midgard-test-support/duration-shards-contract";

// Node CI runs this package as `--shard=i/3` (`.github/workflows/midgard-node-ci.yml`).
describeDurationShards({
  packageRoot: fileURLToPath(new URL("..", import.meta.url)),
  tablePath: fileURLToPath(
    new URL("./support/ci-file-durations.json", import.meta.url),
  ),
  ciShardCount: 3,
  // The suite's include globs: `.test.ts` and the other script extensions.
  testFile: /\.test\.(?:[cm]?[jt]s|[jt]sx)$/u,
});
