import type { Network } from "@lucid-evolution/lucid";

/**
 * Returns whether the node should run the genesis bootstrap program at startup.
 *
 * Mainnet is intentionally excluded even when the flag is set so genesis
 * execution remains a non-production convenience.
 */
export const shouldRunGenesisOnStartup = (input: {
  readonly network: Network;
  readonly runGenesisOnStartup: boolean;
}): boolean => input.network !== "Mainnet" && input.runGenesisOnStartup;
