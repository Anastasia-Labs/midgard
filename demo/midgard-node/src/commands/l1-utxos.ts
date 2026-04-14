import { parseAddressArgument } from "@/commands/utxos.js";
import type { Assets } from "@lucid-evolution/lucid";

export type BlockfrostAmount = {
  readonly unit: string;
  readonly quantity: string;
};

export type BlockfrostAddressUtxo = {
  readonly tx_hash: string;
  readonly output_index: number;
  readonly amount: readonly BlockfrostAmount[];
  readonly block?: string;
  readonly tx_index?: number;
  readonly data_hash?: string | null;
  readonly inline_datum?: string | null;
  readonly reference_script_hash?: string | null;
};

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

export type BlockfrostFetchConfig = {
  readonly address: string;
  readonly apiUrl: string;
  readonly apiKey: string;
  readonly fetchImpl?: typeof fetch;
};

const BLOCKFROST_PAGE_SIZE = 100;

const stringifyAssets = (assets: Readonly<Assets>): Record<string, string> =>
  Object.fromEntries(
    Object.entries(assets)
      .sort(([unitA], [unitB]) => unitA.localeCompare(unitB))
      .map(([unit, quantity]) => [unit, quantity.toString()]),
  );

const sumL1UtxoAssets = (utxos: readonly L1Utxo[]): Readonly<Assets> => {
  const totals: Assets = { lovelace: 0n };
  for (const utxo of utxos) {
    for (const [unit, quantity] of Object.entries(utxo.assets)) {
      totals[unit] = (totals[unit] ?? 0n) + quantity;
    }
  }
  return totals;
};

/**
 * Validates and normalizes the Blockfrost connection settings for the command.
 */
export const resolveBlockfrostConfig = (input?: {
  readonly apiUrl?: string;
  readonly apiKey?: string;
  readonly env?: NodeJS.ProcessEnv;
}): {
  readonly apiUrl: string;
  readonly apiKey: string;
} => {
  const env = input?.env ?? process.env;
  const rawApiUrl =
    input?.apiUrl?.trim() ?? env.L1_BLOCKFROST_API_URL?.trim() ?? "";
  const rawApiKey =
    input?.apiKey?.trim() ?? env.L1_BLOCKFROST_KEY?.trim() ?? "";

  if (rawApiUrl.length === 0) {
    throw new Error(
      "Blockfrost API URL is required. Pass --blockfrost-api-url or set L1_BLOCKFROST_API_URL.",
    );
  }
  if (rawApiKey.length === 0) {
    throw new Error(
      "Blockfrost API key is required. Pass --blockfrost-key or set L1_BLOCKFROST_KEY.",
    );
  }

  let parsed: URL;
  try {
    parsed = new URL(rawApiUrl);
  } catch (cause) {
    throw new Error(
      `Invalid Blockfrost API URL "${rawApiUrl}": ${String(cause)}`,
    );
  }

  return {
    apiUrl: parsed.toString().replace(/\/+$/, ""),
    apiKey: rawApiKey,
  };
};

const parseBlockfrostAmount = (amount: unknown, index: number): BlockfrostAmount => {
  if (
    typeof amount !== "object" ||
    amount === null ||
    typeof (amount as { unit?: unknown }).unit !== "string" ||
    typeof (amount as { quantity?: unknown }).quantity !== "string"
  ) {
    throw new Error(
      `Blockfrost amount[${index.toString()}] must contain string unit and quantity fields.`,
    );
  }

  const { unit, quantity } = amount as { unit: string; quantity: string };
  if (!/^\d+$/.test(quantity)) {
    throw new Error(
      `Blockfrost amount[${index.toString()}].quantity must be a non-negative integer string.`,
    );
  }

  return {
    unit,
    quantity,
  };
};

const parseBlockfrostAssets = (
  amounts: readonly BlockfrostAmount[],
): Readonly<Assets> => {
  const assets: Assets = {};
  for (const { unit, quantity } of amounts) {
    assets[unit] = BigInt(quantity);
  }
  if (assets.lovelace === undefined) {
    assets.lovelace = 0n;
  }
  return assets;
};

/**
 * Parses one raw Blockfrost address UTxO record into the command's stable shape.
 */
export const parseBlockfrostAddressUtxo = (
  entry: unknown,
  index: number,
): L1Utxo => {
  if (typeof entry !== "object" || entry === null) {
    throw new Error(
      `Blockfrost UTxO entry[${index.toString()}] must be an object.`,
    );
  }

  const candidate = entry as Record<string, unknown>;
  if (typeof candidate.tx_hash !== "string" || !/^[0-9a-f]{64}$/i.test(candidate.tx_hash)) {
    throw new Error(
      `Blockfrost UTxO entry[${index.toString()}].tx_hash must be a 32-byte hex string.`,
    );
  }
  if (
    typeof candidate.output_index !== "number" ||
    !Number.isInteger(candidate.output_index) ||
    candidate.output_index < 0
  ) {
    throw new Error(
      `Blockfrost UTxO entry[${index.toString()}].output_index must be a non-negative integer.`,
    );
  }
  if (!Array.isArray(candidate.amount)) {
    throw new Error(
      `Blockfrost UTxO entry[${index.toString()}].amount must be an array.`,
    );
  }

  const amount = candidate.amount.map((item, amountIndex) =>
    parseBlockfrostAmount(item, amountIndex),
  );
  const txIndex =
    typeof candidate.tx_index === "number" &&
    Number.isInteger(candidate.tx_index) &&
    candidate.tx_index >= 0
      ? candidate.tx_index
      : null;

  return {
    txHash: candidate.tx_hash.toLowerCase(),
    outputIndex: candidate.output_index,
    assets: parseBlockfrostAssets(amount),
    block: typeof candidate.block === "string" ? candidate.block : null,
    txIndex,
    dataHash: typeof candidate.data_hash === "string" ? candidate.data_hash : null,
    inlineDatum:
      typeof candidate.inline_datum === "string"
        ? candidate.inline_datum
        : null,
    referenceScriptHash:
      typeof candidate.reference_script_hash === "string"
        ? candidate.reference_script_hash
        : null,
  };
};

/**
 * Parses the raw Blockfrost address UTxO page payload.
 */
export const parseBlockfrostAddressUtxoPage = (
  payload: unknown,
): readonly L1Utxo[] => {
  if (!Array.isArray(payload)) {
    throw new Error("Blockfrost address UTxO response must be a JSON array.");
  }

  return payload.map((entry, index) => parseBlockfrostAddressUtxo(entry, index));
};

const canonicalL1UtxoOrder = (a: L1Utxo, b: L1Utxo): number => {
  const txHashOrder = a.txHash.localeCompare(b.txHash);
  if (txHashOrder !== 0) {
    return txHashOrder;
  }
  return a.outputIndex - b.outputIndex;
};

/**
 * Fetches every visible Blockfrost UTxO page for a payment address.
 */
export const fetchAllBlockfrostAddressUtxos = async ({
  address,
  apiUrl,
  apiKey,
  fetchImpl = fetch,
}: BlockfrostFetchConfig): Promise<L1UtxosResult> => {
  const normalizedAddress = parseAddressArgument(address);
  const utxos: L1Utxo[] = [];

  for (let page = 1; ; page += 1) {
    const response = await fetchImpl(
      `${apiUrl}/addresses/${normalizedAddress}/utxos?page=${page.toString()}&count=${BLOCKFROST_PAGE_SIZE.toString()}&order=asc`,
      {
        headers: {
          project_id: apiKey,
        },
      },
    );

    if (!response.ok) {
      const body = await response.text();
      throw new Error(
        `Blockfrost address UTxO lookup failed with status ${response.status}: ${body}`,
      );
    }

    const pageItems = parseBlockfrostAddressUtxoPage(await response.json());
    utxos.push(...pageItems);

    if (pageItems.length < BLOCKFROST_PAGE_SIZE) {
      break;
    }
  }

  utxos.sort(canonicalL1UtxoOrder);

  return {
    address: normalizedAddress,
    utxoCount: utxos.length,
    totals: sumL1UtxoAssets(utxos),
    utxos,
  };
};

/**
 * Renders the fetched Blockfrost UTxOs as stable CLI JSON with decimal strings.
 */
export const formatL1UtxosResult = (result: L1UtxosResult): string =>
  JSON.stringify(
    {
      address: result.address,
      utxoCount: result.utxoCount,
      totals: stringifyAssets(result.totals),
      utxos: result.utxos.map((utxo) => ({
        txHash: utxo.txHash,
        outputIndex: utxo.outputIndex,
        assets: stringifyAssets(utxo.assets),
        block: utxo.block,
        txIndex: utxo.txIndex,
        dataHash: utxo.dataHash,
        inlineDatum: utxo.inlineDatum,
        referenceScriptHash: utxo.referenceScriptHash,
      })),
    },
    null,
    2,
  );
