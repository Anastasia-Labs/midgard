import { mkdtemp } from "node:fs/promises";
import { join } from "node:path";
export const testStorageRoot =
  process.env.MIDGARD_TEST_STORAGE_ROOT ?? process.cwd();

export const createFundingRecoveryDirectory = () =>
  mkdtemp(join(testStorageRoot, ".watcher-funding-recovery-"));
