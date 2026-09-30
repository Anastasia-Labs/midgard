import {
  compareOutRefs,
  type OutRefLike,
  parseOutRefLabel,
} from "@al-ft/midgard-core/out-ref";
import {
  Blockfrost,
  CML,
  credentialToAddress,
  getAddressDetails,
  Kupmios,
  Lucid,
  type LucidEvolution,
  type Network,
  type PrivateKey,
  type SlotConfig,
  walletFromSeed,
} from "@lucid-evolution/lucid";

import { type ContractDeploymentInfo } from "./inspect-contracts.js";

const DEFAULT_WALLET_SEED_ENV = "USER_WALLET";

export const DEFAULT_CONFIRMATION_POLL_MS = 5_000;

export type ProviderKind = "Blockfrost" | "Kupmios";

export type SubmitProviderConfig = {
  readonly slotConfig?: SlotConfig;
  readonly network: Network;
  readonly provider?: ProviderKind;
  readonly blockfrostApiUrl?: string;
  readonly blockfrostKey?: string;
  readonly kupoUrl?: string;
  readonly ogmiosUrl?: string;
};

export type ProverSignerConfig = {
  readonly network: Network;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
};

export type ResolvedProverSigner = {
  readonly source: "direct-seed-phrase" | string;
  readonly address: string;
  readonly paymentKeyHash: string;
  readonly selectWallet: (lucid: LucidEvolution) => void;
};

export type ParsedOutRef = OutRefLike;

const normalizeNonEmpty = (value: string | undefined): string | undefined => {
  const trimmed = value?.trim() ?? "";
  return trimmed.length === 0 ? undefined : trimmed;
};

const parseProviderKind = (
  value: string | undefined,
  env: NodeJS.ProcessEnv = process.env,
): ProviderKind => {
  const resolved =
    normalizeNonEmpty(value) ?? normalizeNonEmpty(env.L1_PROVIDER);
  if (resolved === "Blockfrost" || resolved === "Kupmios") {
    return resolved;
  }
  if (resolved === undefined) {
    return "Blockfrost";
  }
  throw new Error('--provider must be either "Blockfrost" or "Kupmios".');
};

const requireConfigValue = (
  direct: string | undefined,
  envName: string,
  label: string,
  env: NodeJS.ProcessEnv,
): string => {
  const resolved = normalizeNonEmpty(direct) ?? normalizeNonEmpty(env[envName]);
  if (resolved === undefined) {
    throw new Error(
      `${label} is required; pass it directly or set ${envName}.`,
    );
  }
  return resolved;
};

export const makeLucidForSubmit = async (
  config: SubmitProviderConfig,
  env: NodeJS.ProcessEnv = process.env,
): Promise<LucidEvolution> => {
  const provider = parseProviderKind(config.provider, env);
  if (provider === "Blockfrost") {
    return await Lucid(
      new Blockfrost(
        requireConfigValue(
          config.blockfrostApiUrl,
          "L1_BLOCKFROST_API_URL",
          "--blockfrost-api-url",
          env,
        ),
        requireConfigValue(
          config.blockfrostKey,
          "L1_BLOCKFROST_KEY",
          "--blockfrost-key",
          env,
        ),
      ),
      config.network,
      { slotConfig: config.slotConfig },
    );
  }

  return await Lucid(
    new Kupmios(
      requireConfigValue(config.kupoUrl, "L1_KUPO_KEY", "--kupo-url", env),
      requireConfigValue(
        config.ogmiosUrl,
        "L1_OGMIOS_KEY",
        "--ogmios-url",
        env,
      ),
    ),
    config.network,
    { slotConfig: config.slotConfig },
  );
};

const paymentKeyHashFromAddress = (address: string): string => {
  const paymentCredential = getAddressDetails(address).paymentCredential;
  if (paymentCredential === undefined || paymentCredential.type !== "Key") {
    throw new Error("Prover wallet address must contain a payment key hash.");
  }
  return paymentCredential.hash;
};

const resolveSeedSigner = (
  seedPhrase: string,
  source: string,
  network: Network,
): ResolvedProverSigner => {
  const wallet = walletFromSeed(seedPhrase, {
    addressType: "Enterprise",
    network,
  });
  // `selectWallet.fromSeed` re-derives the whole BIP32 tree (PBKDF2 and four
  // hardened derivations, tens of milliseconds) every time it is called, and
  // every submit helper calls it. A wallet selected from this seed on a given
  // Lucid instance is a pure function of (seed, provider), so once one exists
  // and is still the instance's current wallet, reuse it. The only mutable
  // state a fresh wallet would reset is the UTxO override pin, which is
  // cleared explicitly so the observable behaviour is that of a new wallet.
  const selected = new WeakMap<
    LucidEvolution,
    { readonly wallet: unknown; readonly provider: unknown }
  >();
  return {
    source,
    address: wallet.address,
    paymentKeyHash: paymentKeyHashFromAddress(wallet.address),
    selectWallet: (lucid) => {
      const previous = selected.get(lucid);
      const currentWallet = lucid.wallet();
      if (
        previous !== undefined &&
        currentWallet !== undefined &&
        previous.wallet === currentWallet &&
        previous.provider === lucid.config().provider
      ) {
        lucid.clearUTxOOverride();
        return;
      }
      lucid.selectWallet.fromSeed(seedPhrase, { addressType: "Enterprise" });
      selected.set(lucid, {
        wallet: lucid.wallet(),
        provider: lucid.config().provider,
      });
    },
  };
};

const resolvePrivateKeySigner = (
  privateKey: string,
  source: string,
  network: Network,
): ResolvedProverSigner => {
  const parsedPrivateKey = CML.PrivateKey.from_bech32(privateKey);
  const paymentKeyHash = parsedPrivateKey.to_public().hash().to_hex();
  return {
    source,
    address: credentialToAddress(network, {
      type: "Key",
      hash: paymentKeyHash,
    }),
    paymentKeyHash,
    selectWallet: (lucid) =>
      lucid.selectWallet.fromPrivateKey(privateKey as PrivateKey),
  };
};

export const resolveProverSigner = (
  config: ProverSignerConfig,
  env: NodeJS.ProcessEnv = process.env,
): ResolvedProverSigner => {
  const directSeed = normalizeNonEmpty(config.walletSeedPhrase);
  const directPrivateKey = normalizeNonEmpty(config.walletPrivateKey);
  const privateKeyEnvName = normalizeNonEmpty(config.walletPrivateKeyEnv);
  const privateKeyFromEnv =
    privateKeyEnvName === undefined
      ? undefined
      : normalizeNonEmpty(env[privateKeyEnvName]);

  if (
    directSeed !== undefined &&
    (directPrivateKey !== undefined || privateKeyFromEnv !== undefined)
  ) {
    throw new Error(
      "Provide either a wallet seed phrase or a wallet private key, not both.",
    );
  }
  if (directPrivateKey !== undefined && privateKeyFromEnv !== undefined) {
    throw new Error(
      "Provide the wallet private key either directly or through an env var, not both.",
    );
  }
  if (directSeed !== undefined) {
    return resolveSeedSigner(directSeed, "direct-seed-phrase", config.network);
  }
  if (directPrivateKey !== undefined) {
    return resolvePrivateKeySigner(
      directPrivateKey,
      "direct-private-key",
      config.network,
    );
  }
  if (privateKeyFromEnv !== undefined && privateKeyEnvName !== undefined) {
    return resolvePrivateKeySigner(
      privateKeyFromEnv,
      privateKeyEnvName,
      config.network,
    );
  }

  const normalizedSeedEnvName = normalizeNonEmpty(
    config.walletSeedPhraseEnv ?? DEFAULT_WALLET_SEED_ENV,
  );
  if (normalizedSeedEnvName === undefined) {
    throw new Error("Wallet seed phrase env var name must not be empty.");
  }
  const seedFromEnv = normalizeNonEmpty(env[normalizedSeedEnvName]);
  if (seedFromEnv !== undefined) {
    return resolveSeedSigner(
      seedFromEnv,
      normalizedSeedEnvName,
      config.network,
    );
  }

  throw new Error(
    `No prover signer configured; pass --wallet-seed-phrase, set ${normalizedSeedEnvName}, or pass --wallet-private-key/--wallet-private-key-env.`,
  );
};

export const parseOutRef = (value: string, label: string): ParsedOutRef => {
  try {
    return parseOutRefLabel(value);
  } catch {
    throw new Error(`${label} must use the format <txHash>#<outputIndex>.`);
  }
};

export const compareUtxoOutRefs = compareOutRefs;

export const requireDeploymentScriptHash = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): string => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  return entry.scriptHash;
};

export const requireDeploymentReferenceScriptOutRef = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): OutRefLike => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${name}" is missing refScriptUTxO; publish the canonical reference script and regenerate deployment info before using this fraud-proof category.`,
    );
  }
  return entry.refScriptUTxO;
};

export const requireMatchingScriptHash = ({
  label,
  deployed,
  derived,
}: {
  readonly label: string;
  readonly deployed: string;
  readonly derived: string;
}): void => {
  if (deployed !== derived) {
    throw new Error(
      `${label} mismatch: deployment=${deployed}, derived=${derived}.`,
    );
  }
};
