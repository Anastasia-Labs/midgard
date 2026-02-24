import type { Network } from "@lucid-evolution/lucid";

export const shouldRunGenesisOnStartup = (input: {
  readonly network: Network;
  readonly runGenesisOnStartup: boolean;
}): boolean => input.network !== "Mainnet" && input.runGenesisOnStartup;
