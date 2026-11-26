/**
 * `midgard-user address`: derives and prints the user's Cardano base address
 * from a wallet seed phrase and network (or deployment manifest) so a user
 * knows where to fund Preprod tADA or receive L2 transfers.
 */
import { type Network } from "@lucid-evolution/lucid";

import { type UserCliDeps } from "./deps.js";
import { loadUserDeployment } from "./manifest.js";
import { parseNetwork, refuse } from "./options.js";
import {
  resolveSeedPhrase,
  type SeedCliOptions,
  seedSourceOf,
  walletOf,
} from "./seed.js";

export type AddressOptions = SeedCliOptions & {
  readonly network?: string;
  readonly manifest?: string;
  readonly json?: boolean;
};

export type AddressResult = {
  readonly network: Network;
  readonly address: string;
  readonly paymentKeyHash: string;
};

const resolveNetwork = async (
  options: Pick<AddressOptions, "network" | "manifest">,
  deps: UserCliDeps,
): Promise<Network> => {
  const explicitNetwork =
    options.network === undefined
      ? undefined
      : parseNetwork(options.network, "--network");
  if (explicitNetwork !== undefined) {
    if (options.manifest !== undefined) {
      const deployment = await loadUserDeployment(options.manifest);
      if (deployment.network !== explicitNetwork)
        refuse(
          `--network ${explicitNetwork} does not match manifest network ${deployment.network}.`,
        );
    }
    return explicitNetwork;
  }
  const manifestPath = options.manifest ?? deps.env.MIDGARD_DEPLOYMENT_MANIFEST;
  if (manifestPath !== undefined)
    return (await loadUserDeployment(manifestPath)).network;
  return refuse(
    "Give --network <Mainnet|Preprod|Preview|Custom> or --manifest <path> (or MIDGARD_DEPLOYMENT_MANIFEST).",
  );
};

export const runAddress = async (
  options: AddressOptions,
  deps: UserCliDeps,
): Promise<AddressResult> => {
  const seedPhrase = resolveSeedPhrase(seedSourceOf(options), deps.env);
  const network = await resolveNetwork(options, deps);
  const wallet = walletOf(seedPhrase, network);
  return {
    network,
    address: wallet.address,
    paymentKeyHash: wallet.paymentKeyHash,
  };
};

export const formatAddress = (result: AddressResult): string =>
  `${result.address}\n`;
