import { normalizeTxHash } from "@al-ft/midgard-core/out-ref";
import type { Assets, Network, UTxO } from "@lucid-evolution/lucid";
import * as LE from "@lucid-evolution/lucid";

import { compareOutRefs } from "../tx-context.js";
import { resolveNetwork } from "./address-from-seed.js";
import { parseAddressArgument } from "./command-utils.js";

export type L1Utxo = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly assets: Readonly<Assets>;
  readonly block: string | null;
  readonly txIndex: number | null;
  readonly dataHash: string | null;
  readonly inlineDatum: string | null;
  readonly referenceScriptHash: string | null;
};

export type L1UtxosResult = {
  readonly address: string;
  readonly utxoCount: number;
  readonly totals: Readonly<Assets>;
  readonly utxos: readonly L1Utxo[];
};

export type KupmiosFetchConfig = {
  readonly address: string;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly network: Network;
  readonly lucidFactory?: LucidFactory;
};

export type KupmiosConfig = {
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly network: Network;
};

type LucidUtxoReader = Pick<LE.LucidEvolution, "utxosAt">;
type LucidFactory = (
  provider: LE.Provider,
  network: Network,
) => Promise<LucidUtxoReader>;

const orderAssetsByUnit = (assets: Readonly<Assets>): Readonly<Assets> =>
  Object.fromEntries(
    Object.entries(assets).sort(([unitA], [unitB]) =>
      unitA.localeCompare(unitB),
    ),
  ) as Assets;

const sumL1UtxoAssets = (utxos: readonly L1Utxo[]): Readonly<Assets> => {
  const totals: Assets = { lovelace: 0n };
  for (const utxo of utxos) {
    for (const [unit, quantity] of Object.entries(utxo.assets)) {
      totals[unit] = (totals[unit] ?? 0n) + quantity;
    }
  }
  return orderAssetsByUnit(totals);
};

/**
 * Validates and normalizes local Kupmios connection settings for the command.
 */
export const resolveKupmiosConfig = (input?: {
  readonly kupoUrl?: string;
  readonly ogmiosUrl?: string;
  readonly network?: string;
  readonly env?: NodeJS.ProcessEnv;
}): KupmiosConfig => {
  const env = input?.env ?? process.env;
  const kupoUrl = input?.kupoUrl?.trim() ?? env.L1_KUPO_KEY?.trim() ?? "";
  const ogmiosUrl = input?.ogmiosUrl?.trim() ?? env.L1_OGMIOS_KEY?.trim() ?? "";
  const network = resolveNetwork({ network: input?.network, env });

  if (kupoUrl.length === 0) {
    throw new Error(
      "Kupo URL is required. Pass --kupo-url or set L1_KUPO_KEY.",
    );
  }
  if (ogmiosUrl.length === 0) {
    throw new Error(
      "Ogmios URL is required. Pass --ogmios-url or set L1_OGMIOS_KEY.",
    );
  }
  return {
    kupoUrl: new URL(kupoUrl).toString().replace(/\/+$/, ""),
    ogmiosUrl: new URL(ogmiosUrl).toString().replace(/\/+$/, ""),
    network,
  };
};

export const lucidUtxoToL1Utxo = (utxo: UTxO): L1Utxo => {
  return {
    txHash: normalizeTxHash(utxo.txHash, "utxo.txHash"),
    outputIndex: utxo.outputIndex,
    assets: orderAssetsByUnit(utxo.assets),
    block: null,
    txIndex: null,
    dataHash:
      typeof utxo.datumHash === "string" && utxo.datumHash.length > 0
        ? utxo.datumHash
        : null,
    inlineDatum: typeof utxo.datum === "string" ? utxo.datum : null,
    referenceScriptHash: utxo.scriptRef === undefined ? null : "present",
  };
};

/**
 * Fetches local Kupmios-visible UTxOs for a payment address.
 */
export const fetchKupmiosAddressUtxos = async ({
  address,
  kupoUrl,
  ogmiosUrl,
  network,
  lucidFactory = LE.Lucid,
}: KupmiosFetchConfig): Promise<L1UtxosResult> => {
  const normalizedAddress = parseAddressArgument(address);
  const lucid = await lucidFactory(new LE.Kupmios(kupoUrl, ogmiosUrl), network);
  const utxos = (await lucid.utxosAt(normalizedAddress)).map(lucidUtxoToL1Utxo);

  utxos.sort(compareOutRefs);

  return {
    address: normalizedAddress,
    utxoCount: utxos.length,
    totals: sumL1UtxoAssets(utxos),
    utxos,
  };
};
