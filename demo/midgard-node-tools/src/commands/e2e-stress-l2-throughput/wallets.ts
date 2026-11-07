import { type Network, walletFromSeed } from "@lucid-evolution/lucid";
import {
  type NodeUtxo,
  type ResolvedWalletSeedPhrase,
  resolveWalletSeedPhrase,
} from "midgard-node/commands/command-utils";

import { errorMessage } from "./runtime.js";
import { type E2EL2StressConfig, type StressWallet } from "./types.js";

const deriveWalletAddress = (
  resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase,
  network: Network,
): string =>
  walletFromSeed(resolvedWalletSeedPhrase.seedPhrase, { network }).address;

export const resolveStressWallet = ({
  walletSeedPhrase,
  walletSeedPhraseEnv,
  env,
  network,
}: {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv: string;
  readonly env: NodeJS.ProcessEnv;
  readonly network: Network;
}): StressWallet => {
  const resolvedWalletSeedPhrase = resolveWalletSeedPhrase({
    walletSeedPhrase,
    walletSeedPhraseEnv,
    env,
  });
  return {
    resolvedWalletSeedPhrase,
    address: deriveWalletAddress(resolvedWalletSeedPhrase, network),
  };
};

type StressWalletResolutionResult =
  | {
      readonly ok: true;
      readonly wallet: StressWallet;
    }
  | {
      readonly ok: false;
      readonly envName: string;
      readonly error: string;
    };

export const resolveStressWallets = ({
  envNames,
  env,
  network,
}: {
  readonly envNames: readonly string[];
  readonly env: NodeJS.ProcessEnv;
  readonly network: Network;
}): readonly StressWallet[] => {
  const results: readonly StressWalletResolutionResult[] = envNames.map(
    (envName) => {
      try {
        return {
          ok: true,
          wallet: resolveStressWallet({
            walletSeedPhraseEnv: envName,
            env,
            network,
          }),
        };
      } catch (error) {
        return { ok: false, envName, error: errorMessage(error) };
      }
    },
  );
  const failures = results.filter(
    (result): result is Extract<StressWalletResolutionResult, { ok: false }> =>
      !result.ok,
  );
  if (failures.length > 0) {
    throw new Error(
      `${failures.length.toString()}/${envNames.length.toString()} stress wallet env vars are unresolvable:\n` +
        failures
          .map((failure) => `  - ${failure.envName}: ${failure.error}`)
          .join("\n"),
    );
  }
  return results.flatMap((result) => (result.ok ? [result.wallet] : []));
};

export const validateDistinctStressWallets = (
  wallets: readonly StressWallet[],
): void => {
  const seedSources = new Set<string>();
  const addresses = new Set<string>();
  for (const wallet of wallets) {
    if (seedSources.has(wallet.resolvedWalletSeedPhrase.resolvedFrom)) {
      throw new Error(
        `Duplicate stress wallet seed source ${wallet.resolvedWalletSeedPhrase.resolvedFrom}.`,
      );
    }
    if (addresses.has(wallet.address)) {
      throw new Error(
        `Stress wallet seeds must derive distinct addresses; duplicate address ${wallet.address}.`,
      );
    }
    seedSources.add(wallet.resolvedWalletSeedPhrase.resolvedFrom);
    addresses.add(wallet.address);
  }
};

export const walletForWorker = (
  config: E2EL2StressConfig,
  workerIndex: number,
): StressWallet =>
  config.mode === "parallel-fanout"
    ? config.stressWallets[workerIndex]!
    : requirePrimaryWallet(config);

export const requirePrimaryWallet = (
  config: E2EL2StressConfig,
): StressWallet => {
  if (config.primaryWallet === undefined) {
    throw new Error("serial-chain stress requires a primary wallet.");
  }
  return config.primaryWallet;
};

export const spendableUtxosForLovelace = (
  utxos: readonly NodeUtxo[],
  lovelace: bigint,
): readonly NodeUtxo[] =>
  utxos.filter((utxo) => (utxo.assets.lovelace ?? 0n) >= lovelace);
