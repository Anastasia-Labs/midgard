import { type Network, walletFromSeed } from "@lucid-evolution/lucid";

const SUPPORTED_NETWORKS = ["Mainnet", "Preprod", "Preview"] as const;

/**
 * Normalizes a seed phrase for deterministic CLI handling.
 */
export const parseSeedPhraseArgument = (seedPhrase: string): string => {
  const normalized = seedPhrase.trim().replace(/\s+/g, " ");
  if (normalized.length === 0) {
    throw new Error("Seed phrase must not be empty.");
  }
  return normalized;
};

/**
 * Parses an explicit Cardano network for local-provider CLI helpers.
 */
export const parseNetworkArgument = (network: string): Network => {
  const normalized = network.trim();
  const matched = SUPPORTED_NETWORKS.find((candidate) => candidate === normalized);
  if (matched === undefined) {
    throw new Error(
      `Unsupported network "${network}". Expected one of ${SUPPORTED_NETWORKS.join(", ")}.`,
    );
  }
  return matched;
};

/**
 * Resolves the network from an explicit argument or the runtime environment.
 */
export const resolveNetwork = (input?: {
  readonly network?: string;
  readonly env?: NodeJS.ProcessEnv;
}): Network => {
  const rawNetwork =
    input?.network?.trim() ?? input?.env?.NETWORK?.trim() ?? process.env.NETWORK?.trim() ?? "";
  if (rawNetwork.length === 0) {
    throw new Error(
      "Network is required. Pass --network or set NETWORK.",
    );
  }
  return parseNetworkArgument(rawNetwork);
};

/**
 * Derives the payment address for a seed phrase on the requested network.
 */
export const deriveAddressFromSeedPhrase = (
  seedPhrase: string,
  network: Network,
): string =>
  walletFromSeed(parseSeedPhraseArgument(seedPhrase), {
    network,
  }).address;
