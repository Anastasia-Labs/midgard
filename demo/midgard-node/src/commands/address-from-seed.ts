import { type Network, walletFromSeed } from "@lucid-evolution/lucid";

/**
 * Normalizes a seed phrase for deterministic CLI handling.
 */
export const parseSeedPhraseArgument = (seedPhrase: string): string => {
  const normalized = seedPhrase
    .trim()
    .split(/\s+/)
    .filter((word) => word.length > 0)
    .join(" ");
  if (normalized.length === 0) {
    throw new Error("Seed phrase must not be empty.");
  }
  return normalized;
};

/**
 * Resolves the active Blockfrost API URL for CLI commands that need to align
 * their network with the current L1 provider target.
 */
export const resolveBlockfrostApiUrl = (input?: {
  readonly blockfrostApiUrl?: string;
  readonly env?: NodeJS.ProcessEnv;
}): string => {
  const rawApiUrl =
    input?.blockfrostApiUrl?.trim() ??
    input?.env?.L1_BLOCKFROST_API_URL?.trim() ??
    process.env.L1_BLOCKFROST_API_URL?.trim() ??
    "";
  if (rawApiUrl.length === 0) {
    throw new Error(
      "Blockfrost API URL is required. Pass --blockfrost-api-url or set L1_BLOCKFROST_API_URL.",
    );
  }

  try {
    return new URL(rawApiUrl).toString().replace(/\/+$/, "");
  } catch (cause) {
    throw new Error(
      `Invalid Blockfrost API URL "${rawApiUrl}": ${String(cause)}`,
    );
  }
};

/**
 * Infers the target Cardano network from the configured Blockfrost API URL.
 */
export const inferNetworkFromBlockfrostApiUrl = (
  blockfrostApiUrl: string,
): Network => {
  const normalized = blockfrostApiUrl.toLowerCase();
  const matchedNetworks = [
    normalized.includes("mainnet") ? "Mainnet" : null,
    normalized.includes("preprod") ? "Preprod" : null,
    normalized.includes("preview") ? "Preview" : null,
  ].filter((network): network is Network => network !== null);

  if (matchedNetworks.length !== 1) {
    throw new Error(
      `Could not infer Blockfrost network from URL "${blockfrostApiUrl}". Expected one of mainnet, preprod, or preview in the URL.`,
    );
  }

  return matchedNetworks[0];
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
