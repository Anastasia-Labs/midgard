import { DEFAULT_STRESS_WALLET_DIR } from "./constants.js";
import { withExclusiveStressWalletFundsLock } from "./files.js";
import { terminalDrainStressWalletsUnlocked } from "./terminal-drain.terminal-drain-stress-wallets-unlocked.js";
import {
  type StressWalletTerminalDrainResult,
  type StressWalletTerminalDrainRuntime,
  type TerminalDrainStressWalletsOptions,
} from "./types.js";

export const terminalDrainStressWallets = async (
  options: TerminalDrainStressWalletsOptions,
  runtime: StressWalletTerminalDrainRuntime,
): Promise<StressWalletTerminalDrainResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  return withExclusiveStressWalletFundsLock(outDir, () =>
    terminalDrainStressWalletsUnlocked(options, runtime),
  );
};
