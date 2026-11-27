/**
 * `midgard-user send`: builds, signs, and submits an ordinary L2 transfer
 * via `@al-ft/lucid-midgard` against a Midgard node's public HTTP API,
 * optionally polling `/tx-status` until accepted or committed (`--wait`).
 */
import { LucidMidgard } from "@al-ft/lucid-midgard";
import { type Network } from "@lucid-evolution/lucid";

import { type UserCliDeps } from "./deps.js";
import { loadUserDeployment } from "./manifest.js";
import {
  parseAddress,
  parseAssets,
  parseLovelace,
  parseNetwork,
  parsePositiveMs,
  refuse,
} from "./options.js";
import {
  resolveSeedPhrase,
  type SeedCliOptions,
  seedSourceOf,
  walletOf,
} from "./seed.js";

export type SendOptions = SeedCliOptions & {
  readonly nodeEndpoint: string;
  readonly to: string;
  readonly lovelace: string;
  readonly asset?: readonly string[];
  readonly manifest?: string;
  readonly network?: string;
  readonly wait?: boolean;
  readonly timeoutMs?: string;
  readonly json?: boolean;
};

export type SendResult = {
  readonly txHash: string;
  readonly status: string;
  readonly from: string;
  readonly to: string;
  readonly lovelace: string;
  readonly assets: Readonly<Record<string, string>>;
};

export const runSend = async (
  options: SendOptions,
  deps: UserCliDeps,
): Promise<SendResult> => {
  const lovelace = parseLovelace(
    options.lovelace,
    "--lovelace must be greater than zero.",
  );
  const additionalAssets = parseAssets(options.asset ?? []);
  const timeoutMs =
    options.timeoutMs === undefined
      ? undefined
      : parsePositiveMs(options.timeoutMs, "--timeout-ms");
  const seedPhrase = resolveSeedPhrase(seedSourceOf(options), deps.env);
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
  const to = parseAddress(options.to, "--to", network);
  const wallet = walletOf(seedPhrase, network);

  const midgard = await LucidMidgard.new(provider, {
    networkId: network === "Mainnet" ? 1 : 0,
  });
  midgard.selectWallet.fromPrivateKey(wallet.paymentKey, wallet.address);
  const utxos = await midgard.utxosAt(wallet.address);
  if (utxos.length === 0)
    refuse(`The node holds no L2 output at ${wallet.address}.`);

  const built = await midgard
    .newTx()
    .pay.ToAddress(to, { lovelace, ...additionalAssets })
    .complete();
  const signed = await built.sign.withWallet().complete();
  const submitted = await signed.submit();

  let status: string = submitted.admission.status;
  if (options.wait === true) {
    const awaited = await submitted.awaitStatus(
      timeoutMs === undefined ? undefined : { timeoutMs },
    );
    if (awaited.kind === "rejected")
      refuse(
        `The node rejected L2 transaction ${awaited.txId}: ${awaited.code}${awaited.detail === null ? "" : ` (${awaited.detail})`}.`,
      );
    status = awaited.kind;
  }

  const extraAssets = Object.fromEntries(
    Object.entries(additionalAssets)
      .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
      .map(([unit, amount]) => [unit, amount.toString()]),
  );

  return {
    txHash: signed.txIdHex,
    status,
    from: wallet.address,
    to,
    lovelace: lovelace.toString(),
    assets: extraAssets,
  };
};

export const formatSend = (result: SendResult): string =>
  `L2 transfer ${result.status}: tx ${result.txHash}, ${result.lovelace} lovelace -> ${result.to}\n`;
