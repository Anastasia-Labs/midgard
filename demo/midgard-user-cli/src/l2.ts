/**
 * The wallet on L2: lucid-midgard over a node's public API, refused unless the
 * node serves the manifest's deployment.
 */
import { LucidMidgard } from "@al-ft/lucid-midgard";

import { type UserCliDeps } from "./deps.js";
import { type ShapeWallet } from "./l2-shapes.js";
import { type UserDeployment } from "./manifest.js";
import { refuse } from "./options.js";
import { resolveSeedPhrase, type UserWallet, walletOf } from "./seed.js";
import { manifestPathOf } from "./submission.js";

/** Options every L2 command takes. */
export type L2CommandOptions = {
  readonly nodeEndpoint: string;
  readonly manifest?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletSeedPhraseFile?: string;
  readonly json?: boolean;
};

export type L2Wallet = {
  readonly midgard: LucidMidgard;
  readonly shapeWallet: ShapeWallet;
};

type L2Deployment = Pick<UserDeployment, "manifestId" | "network">;

/** The seed, the deployment and the wallet, before any network contact. */
export const prepareL2Command = async <Deployment extends L2Deployment>(
  options: L2CommandOptions,
  deps: UserCliDeps,
  load: (manifestPath: string) => Promise<Deployment>,
) => {
  const seedPhrase = resolveSeedPhrase(
    {
      env: options.walletSeedPhraseEnv,
      file: options.walletSeedPhraseFile,
    },
    deps.env,
  );
  const deployment = await load(manifestPathOf(options, deps));
  return {
    deployment,
    seedPhrase,
    wallet: walletOf(seedPhrase, deployment.network),
  };
};

export const openL2Wallet = async (
  deps: UserCliDeps,
  endpoint: string,
  deployment: L2Deployment,
  wallet: UserWallet,
): Promise<L2Wallet> => {
  const provider = await deps.connectL2(endpoint);
  const served = (await provider.getProtocolInfo()).deploymentMarker
    ?.manifestId;
  if (served !== deployment.manifestId)
    refuse(
      `The node serves deployment ${served ?? "(none reported)"}, not this manifest's ${deployment.manifestId}.`,
    );
  const midgard = await LucidMidgard.new(provider, {
    networkId: deployment.network === "Mainnet" ? 1 : 0,
  });
  midgard.selectWallet.fromPrivateKey(wallet.paymentKey, wallet.address);
  return {
    midgard,
    shapeWallet: {
      address: wallet.address,
      paymentKeyHash: wallet.paymentKeyHash,
      utxos: await midgard.utxosAt(wallet.address),
      currentSlot: await midgard.currentSlot(),
    },
  };
};
