import { join } from "node:path";

import { consolidateStressWalletsUnlocked } from "./consolidation.consolidate-stress-wallets-unlocked.js";
import { DEFAULT_STRESS_WALLET_DIR } from "./constants.js";
import {
  withExclusiveConsolidationStateLock,
  withExclusiveStressWalletFundsLock,
} from "./files.js";
import {
  type ConsolidateStressWalletsOptions,
  type StressWalletConsolidateResult,
  type StressWalletConsolidateRuntime,
} from "./types.js";

export const consolidateStressWallets = async (
  options: ConsolidateStressWalletsOptions,
  runtime: StressWalletConsolidateRuntime,
): Promise<StressWalletConsolidateResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const statePath = join(outDir, "consolidation-state.json");
  return withExclusiveStressWalletFundsLock(outDir, () =>
    withExclusiveConsolidationStateLock(statePath, () =>
      consolidateStressWalletsUnlocked(options, runtime),
    ),
  );
};
