import { fileURLToPath } from "node:url";

import { durationShardSequencer } from "@al-ft/midgard-test-support/duration-shards";

/**
 * Packs `--shard=i/n` by the CI seconds each file took (see
 * `@al-ft/midgard-test-support/duration-shards`) and starts the longest files
 * first. A few emulator files run for many minutes while most take seconds;
 * Vitest's default hash sharding put 75 minutes of them in one CI shard and 41
 * in another, and a whale that starts late sets a fork's wall time by itself.
 *
 * Regenerate the table after a CI run from the three shard logs:
 *   node ../midgard-test-support/scripts/ci-file-durations.mjs --package . \
 *     --out tests/support/ci-file-durations.json <job logs>...
 */
export const EmulatorSequencer = durationShardSequencer({
  tablePath: fileURLToPath(
    new URL("./ci-file-durations.json", import.meta.url),
  ),
});
