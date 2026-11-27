import { mkdtemp } from "node:fs/promises";
import { join } from "node:path";

// A durable root the journals and stores accept (they refuse /tmp), outside
// the package so a test run leaves the checkout clean. Each test takes its own
// mkdtemp directory under it.
export const testStorageRoot =
  process.env.MIDGARD_TEST_STORAGE_ROOT ?? "/var/tmp";

export const createFundingRecoveryDirectory = () =>
  mkdtemp(join(testStorageRoot, ".watcher-funding-recovery-"));
