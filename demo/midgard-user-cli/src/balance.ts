/**
 * `midgard-user balance`: queries a Midgard node's public HTTP API
 * (`GET /utxos?address=...`) for an address (explicit or derived from the
 * user's seed), printing total lovelace, native assets, and every L2 UTxO's
 * `txHash#outputIndex` outRef for use with `withdraw` or inspection.
 */
import { type MidgardUtxo } from "@al-ft/lucid-midgard";
import { type Network } from "@lucid-evolution/lucid";

import { type UserCliDeps } from "./deps.js";
import { loadUserDeployment } from "./manifest.js";
import { parseAddress, parseNetwork, refuse } from "./options.js";
import {
  hasSeedCliOptions,
  resolveSeedPhrase,
  type SeedCliOptions,
  seedSourceOf,
  walletOf,
} from "./seed.js";

export type BalanceOptions = SeedCliOptions & {
  readonly nodeEndpoint: string;
  readonly address?: string;
  readonly network?: string;
  readonly manifest?: string;
  readonly json?: boolean;
};

export type BalanceUtxoSummary = {
  readonly outRef: string;
  readonly txHash: string;
  readonly outputIndex: number;
  readonly lovelace: string;
  readonly assets: Readonly<Record<string, string>>;
};

export type BalanceResult = {
  readonly address: string;
  readonly totalLovelace: string;
  readonly assets: Readonly<Record<string, string>>;
  readonly utxoCount: number;
  readonly utxos: readonly BalanceUtxoSummary[];
};

const formatAda = (lovelace: bigint): string => {
  const whole = lovelace / 1_000_000n;
  const frac = (lovelace % 1_000_000n).toString().padStart(6, "0");
  return `${whole.toString()}.${frac}`;
};

const summarizeUtxos = (
  address: string,
  utxos: readonly MidgardUtxo[],
): BalanceResult => {
  let totalLovelace = 0n;
  const totalAssets = new Map<string, bigint>();
  const summaries: BalanceUtxoSummary[] = utxos.map((utxo) => {
    const lovelace = utxo.output.assets.lovelace ?? 0n;
    totalLovelace += lovelace;
    const extraEntries: [string, string][] = [];
    for (const [unit, amount] of Object.entries(utxo.output.assets).sort(
      ([left], [right]) => (left < right ? -1 : left > right ? 1 : 0),
    )) {
      if (unit === "lovelace") continue;
      extraEntries.push([unit, amount.toString()]);
      totalAssets.set(unit, (totalAssets.get(unit) ?? 0n) + amount);
    }
    return {
      outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
      lovelace: lovelace.toString(),
      assets: Object.fromEntries(extraEntries),
    };
  });
  const sortedAssets = Object.fromEntries(
    [...totalAssets.entries()]
      .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
      .map(([unit, amount]) => [unit, amount.toString()]),
  );
  return {
    address,
    totalLovelace: totalLovelace.toString(),
    assets: sortedAssets,
    utxoCount: summaries.length,
    utxos: summaries,
  };
};

export const runBalance = async (
  options: BalanceOptions,
  deps: UserCliDeps,
): Promise<BalanceResult> => {
  const hasAddress = options.address !== undefined;
  const hasSeed = hasSeedCliOptions(options);
  if (hasAddress === hasSeed)
    refuse(
      "Give either --address <bech32> or a wallet seed source (--wallet-seed-phrase-env / --seed-env / --wallet-seed-phrase-file / --seed-file), not both.",
    );
  const seedPhrase = hasSeed
    ? resolveSeedPhrase(seedSourceOf(options), deps.env)
    : undefined;
  const explicitNetwork =
    options.network === undefined
      ? undefined
      : parseNetwork(options.network, "--network");
  const deployment =
    options.manifest !== undefined
      ? await loadUserDeployment(options.manifest)
      : undefined;
  if (
    explicitNetwork !== undefined &&
    deployment !== undefined &&
    deployment.network !== explicitNetwork
  )
    refuse(
      `--network ${explicitNetwork} does not match manifest network ${deployment.network}.`,
    );

  const provider = await deps.connectL2(options.nodeEndpoint);
  const protocolInfo = await provider.getProtocolInfo();
  if (
    deployment !== undefined &&
    protocolInfo.hubOraclePolicyId !== deployment.hubOraclePolicyId
  )
    refuse(
      `The node serves hubOraclePolicyId ${protocolInfo.hubOraclePolicyId ?? "(none reported)"}, not this manifest's ${deployment.hubOraclePolicyId}.`,
    );

  const network: Network =
    explicitNetwork ??
    deployment?.network ??
    parseNetwork(protocolInfo.network, "node protocol-info network");
  const address =
    options.address !== undefined
      ? parseAddress(options.address, "--address", network)
      : walletOf(seedPhrase!, network).address;

  const utxos = await provider.getUtxos(address);
  return summarizeUtxos(address, utxos);
};

export const formatBalance = (result: BalanceResult): string => {
  const lines: string[] = [
    `address: ${result.address}`,
    `lovelace: ${result.totalLovelace} (${formatAda(BigInt(result.totalLovelace))} ADA)`,
  ];
  const assetEntries = Object.entries(result.assets);
  if (assetEntries.length > 0) {
    lines.push("assets:");
    for (const [unit, amount] of assetEntries)
      lines.push(`  ${unit}: ${amount}`);
  }
  lines.push(`utxos (${result.utxoCount.toString()}):`);
  if (result.utxos.length === 0) {
    lines.push("  (none)");
  } else {
    for (const utxo of result.utxos) {
      const extras = Object.entries(utxo.assets)
        .map(([unit, amount]) => `${amount} ${unit}`)
        .join(", ");
      lines.push(
        `  ${utxo.outRef}  ${utxo.lovelace} lovelace${extras.length > 0 ? ` (+ ${extras})` : ""}`,
      );
    }
  }
  return `${lines.join("\n")}\n`;
};
