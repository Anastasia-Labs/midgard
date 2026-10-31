import { normalizeTxHash } from "@al-ft/midgard-core/out-ref";
import type { Assets, Network, UTxO } from "@lucid-evolution/lucid";
import * as LE from "@lucid-evolution/lucid";

import { compareOutRefs } from "../tx-context.js";
import { resolveNetwork } from "./address-from-seed.js";
import { parseAddressArgument } from "./command-utils.js";
import { commandLucid, withCommandL1Access } from "./l1-command-access.js";

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

type LucidUtxoReader = Pick<LE.LucidEvolution, "utxosAt">;

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
 * The command's network, through the CLI network parser, which refuses
 * `Custom`.
 */
export const resolveL1UtxosNetwork = (input?: {
  readonly network?: string;
  readonly env?: NodeJS.ProcessEnv;
}): Network =>
  resolveNetwork({ network: input?.network, env: input?.env ?? process.env });

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

/** The UTxOs a Lucid reader answers for a payment address, in ledger order. */
export const readAddressUtxos = async ({
  address,
  reader,
}: {
  readonly address: string;
  readonly reader: LucidUtxoReader;
}): Promise<L1UtxosResult> => {
  const normalizedAddress = parseAddressArgument(address);
  const utxos = (await reader.utxosAt(normalizedAddress)).map(
    lucidUtxoToL1Utxo,
  );

  utxos.sort(compareOutRefs);

  return {
    address: normalizedAddress,
    utxoCount: utxos.length,
    totals: sumL1UtxoAssets(utxos),
    utxos,
  };
};

/**
 * The UTxOs at a payment address through the tool L1 access `--l1` selects
 * (`l1-command-access.ts`): by default the local node's ledger at its tip.
 */
export const fetchAddressUtxos = async ({
  address,
  network,
  env,
}: {
  readonly address: string;
  readonly network: Network;
  readonly env?: NodeJS.ProcessEnv;
}): Promise<L1UtxosResult> =>
  withCommandL1Access({ network, env }, async (access) =>
    readAddressUtxos({
      address,
      reader: await commandLucid(access, network),
    }),
  );
