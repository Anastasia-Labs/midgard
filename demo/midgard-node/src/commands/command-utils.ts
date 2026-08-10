import {
  decodeMidgardSpendInputItemV1,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  encodeMidgardSpendInputItemV1,
  midgardAddressFromText,
  midgardValueToCmlValue,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  Data as LucidData,
  getAddressDetails,
  type Network,
  type UTxO,
  valueToAssets,
  walletFromSeed,
} from "@lucid-evolution/lucid";

import type { OutRefLike } from "@/tx-context.js";

const DEFAULT_MIDGARD_NODE_PORT = "3000";

export const DEFAULT_WALLET_SEED_ENV = "USER_WALLET";

export type ResolvedWalletSeedPhrase = {
  readonly seedPhrase: string;
  readonly resolvedFrom: "direct-argument" | string;
};

export type DerivedWalletInfo = {
  readonly seedPhrase: string;
  readonly seedSource: string;
  readonly address: string;
  readonly paymentKeyHash: string;
  readonly privateKey: ReturnType<typeof CML.PrivateKey.from_bech32>;
};

export type RawNodeUtxo = {
  readonly outref: string;
  readonly outputCbor: string;
};

export type NodeUtxo = OutRefLike & {
  readonly outrefCbor: Buffer;
  readonly outputCbor: Buffer;
  readonly address: string;
  readonly assets: Readonly<Assets>;
  readonly datum?: string;
};

export type ParsedTxOutRefLabel = OutRefLike & {
  readonly cbor: Buffer;
};

export const jsonReplacer = (_key: string, value: unknown): unknown => {
  if (typeof value === "bigint") {
    return value.toString(10);
  }
  if (value instanceof Map) {
    return Object.fromEntries([...value.entries()]);
  }
  if (Buffer.isBuffer(value)) {
    return value.toString("hex");
  }
  return value;
};

export const formatJson = (value: unknown): string =>
  JSON.stringify(value, jsonReplacer, 2);

export const parseNodeEndpoint = (value: string): string => {
  const normalized = value.trim();
  if (normalized.length === 0) {
    throw new Error("Midgard node endpoint must not be empty.");
  }
  let url: URL;
  try {
    url = new URL(normalized);
  } catch (cause) {
    throw new Error(
      `Invalid Midgard node endpoint "${value}": ${String(cause)}`,
    );
  }
  url.pathname = url.pathname.replace(/\/+$/, "");
  url.search = "";
  url.hash = "";
  return url.toString().replace(/\/+$/, "");
};

export const parseAddressArgument = (address: string): string => {
  try {
    return encodeMidgardAddressText(midgardAddressFromText(address));
  } catch (cause) {
    throw new Error(`Invalid address "${address.trim()}": ${String(cause)}`);
  }
};

export const defaultMidgardNodeEndpoint = (
  env: NodeJS.ProcessEnv = process.env,
): string =>
  parseNodeEndpoint(
    env.MIDGARD_NODE_URL ??
      env.ACTIVITY_SUBMIT_ENDPOINT ??
      env.STRESS_SUBMIT_ENDPOINT ??
      `http://127.0.0.1:${env.PORT?.trim() || DEFAULT_MIDGARD_NODE_PORT}`,
  );

export const fetchNodeTxStatus = async (
  nodeEndpoint: string,
  txHash: string,
  timeoutMs?: number,
): Promise<string> => {
  const response = await fetch(
    parseNodeEndpoint(nodeEndpoint) +
      "/tx-status?tx_hash=" +
      encodeURIComponent(txHash),
    timeoutMs === undefined
      ? undefined
      : { signal: AbortSignal.timeout(timeoutMs) },
  );
  let body: unknown;
  try {
    body = await response.json();
  } catch (cause) {
    throw new Error(
      "Failed to read /tx-status for " +
        txHash +
        ": HTTP " +
        response.status.toString() +
        " returned malformed JSON.",
      { cause },
    );
  }
  const record =
    typeof body === "object" && body !== null
      ? (body as { readonly txId?: unknown; readonly status?: unknown })
      : undefined;
  if (
    response.status === 404 &&
    record?.txId === txHash &&
    record.status === "not_found"
  ) {
    return "not_found";
  }
  if (
    !response.ok ||
    record?.txId !== txHash ||
    typeof record.status !== "string" ||
    record.status === "not_found"
  ) {
    throw new Error(
      "Failed to read /tx-status for " +
        txHash +
        ": HTTP " +
        response.status.toString() +
        " returned an invalid or mismatched status body.",
    );
  }
  return record.status;
};

export const networkIdFromName = (network: Network): bigint =>
  network === "Mainnet" ? 1n : 0n;

export const walletNetworkFromId = (
  networkId: bigint,
): "Mainnet" | "Preprod" => (networkId === 1n ? "Mainnet" : "Preprod");

export const parseHexBytes = (
  value: unknown,
  fieldName: string,
  expectedLength?: number,
): Buffer => {
  if (typeof value !== "string") {
    throw new Error(`${fieldName} must be a hex string.`);
  }
  return hexToBytes(value, { fieldName, byteLength: expectedLength });
};

export const parseTxOutRefLabel = (
  value: unknown,
  fieldName = "txOutRef",
): ParsedTxOutRefLabel => {
  if (typeof value !== "string") {
    throw new Error(`${fieldName} must be a string.`);
  }
  let parsed: OutRefLike;
  let cbor: Buffer;
  try {
    parsed = parseOutRefLabel(value);
    // The §5.3 field-0/1 item encoding — `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`,
    // a fixed 38 bytes with a deliberately non-minimal output index. These are
    // the ledger MPF trie key / `outref` column bytes that on-chain
    // `ledger_outref_key` derives through `encode_midgard_tx_input`, not CML's
    // minimal-index `TransactionInput` CBOR.
    cbor = encodeMidgardSpendInputItemV1({
      txId: hexToBytes(parsed.txHash, { fieldName: `${fieldName}.txHash` }),
      outputIndex: parsed.outputIndex,
    });
  } catch (cause) {
    const detail = cause instanceof Error ? cause.message : String(cause);
    throw new Error(`${fieldName}: ${detail}`);
  }
  return {
    ...parsed,
    cbor,
  };
};

export const parseTxOutRefCborHex = (
  value: unknown,
  fieldName = "txOutRef",
): Buffer => {
  const bytes = parseHexBytes(value, fieldName);
  try {
    // §5.3 field-0/1 item form, not CML's minimal-index `TransactionInput`
    // CBOR: the decoder asserts the fixed 38-byte width and the `0x19` index
    // head, so a minimal-index spelling is rejected rather than re-canonicalized.
    // Re-encoding proves the round trip lands on the one admissible spelling.
    return encodeMidgardSpendInputItemV1(decodeMidgardSpendInputItemV1(bytes));
  } catch (cause) {
    throw new Error(
      `Invalid ${fieldName}: failed to decode TxOutRef CBOR (${String(cause)}).`,
    );
  }
};

export const parseEventId = (value: unknown, fieldName = "eventId"): Buffer => {
  const bytes = parseHexBytes(value, fieldName);
  try {
    const decoded = LucidData.from(bytes.toString("hex"), SDK.OutputReference);
    return Buffer.from(LucidData.to(decoded, SDK.OutputReference), "hex");
  } catch (cause) {
    throw new Error(
      `Invalid ${fieldName}: failed to decode OutputReference CBOR (${String(cause)}).`,
    );
  }
};

const ENV_TRAILING_INDEX = /^(.+?)_(\d+)$/;

const candidateWalletSeedPhraseEnvNames = (name: string): readonly string[] => {
  const match = ENV_TRAILING_INDEX.exec(name);
  if (match === null) {
    return [name];
  }
  const [, prefix, digits] = match;
  const padded = `${prefix}_${digits.padStart(4, "0")}`;
  return padded === name ? [name] : [name, padded];
};

export const resolveWalletSeedPhrase = ({
  walletSeedPhrase,
  walletSeedPhraseEnv,
  env = process.env,
}: {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv: string;
  readonly env?: NodeJS.ProcessEnv;
}): ResolvedWalletSeedPhrase => {
  const direct = walletSeedPhrase?.trim() ?? "";
  if (direct.length > 0) {
    return { seedPhrase: direct, resolvedFrom: "direct-argument" };
  }
  const normalizedEnv = walletSeedPhraseEnv.trim();
  if (normalizedEnv.length === 0) {
    throw new Error("Wallet seed phrase env var name must not be empty.");
  }
  const candidates = candidateWalletSeedPhraseEnvNames(normalizedEnv);
  for (const candidate of candidates) {
    const seedPhrase = env[candidate]?.trim() ?? "";
    if (seedPhrase.length > 0) {
      return { seedPhrase, resolvedFrom: candidate };
    }
  }
  const triedCandidates =
    candidates.length === 1
      ? `"${normalizedEnv}"`
      : `"${normalizedEnv}" (also tried ${candidates
          .slice(1)
          .map((candidate) => `"${candidate}"`)
          .join(", ")})`;
  throw new Error(
    `Environment variable ${triedCandidates} does not contain a wallet seed phrase.`,
  );
};

export const deriveWalletInfo = (
  resolved: ResolvedWalletSeedPhrase,
  network: Network,
): DerivedWalletInfo => {
  const wallet = walletFromSeed(resolved.seedPhrase, { network });
  const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const paymentCredential = getAddressDetails(wallet.address).paymentCredential;
  if (paymentCredential === undefined || paymentCredential.type !== "Key") {
    throw new Error("Derived wallet address must contain a payment key hash.");
  }
  return {
    seedPhrase: resolved.seedPhrase,
    seedSource: resolved.resolvedFrom,
    address: wallet.address,
    paymentKeyHash: paymentCredential.hash,
    privateKey,
  };
};

export const parseNodeUtxoResponse = (
  payload: unknown,
): readonly RawNodeUtxo[] => {
  if (typeof payload !== "object" || payload === null) {
    throw new Error("Midgard node returned a non-object UTxO payload.");
  }
  const utxos = (payload as { readonly utxos?: unknown }).utxos;
  if (!Array.isArray(utxos)) {
    throw new Error("Midgard node UTxO response is missing an `utxos` array.");
  }
  return utxos.map((entry, index) => {
    if (typeof entry !== "object" || entry === null) {
      throw new Error(`UTxO entry ${index.toString()} is not an object.`);
    }
    const { outref, outputCbor } = entry as {
      readonly outref?: unknown;
      readonly outputCbor?: unknown;
    };
    if (typeof outref !== "string" || typeof outputCbor !== "string") {
      throw new Error(
        `UTxO entry ${index.toString()} must contain string outref/outputCbor fields.`,
      );
    }
    return { outref, outputCbor };
  });
};

export const decodeNodeUtxo = (raw: RawNodeUtxo): NodeUtxo => {
  const outrefCbor = parseHexBytes(raw.outref, "outref");
  const outputCbor = parseHexBytes(raw.outputCbor, "outputCbor");
  // §5.3 field-0/1 item form (fixed 38 bytes, `19 XXXX` output index) — the
  // ledger out-ref encoding matching on-chain `ledger_outref_key`, not CML's
  // minimal-index `TransactionInput` CBOR.
  const input = decodeMidgardSpendInputItemV1(outrefCbor);
  const output = decodeMidgardTxOutput(outputCbor);
  return {
    txHash: Buffer.from(input.txId).toString("hex"),
    outputIndex: input.outputIndex,
    outrefCbor,
    outputCbor,
    address: encodeMidgardAddressText(output.address),
    assets: valueToAssets(midgardValueToCmlValue(output.value)),
    ...(output.datum === undefined
      ? {}
      : { datum: output.datum.cbor.toString("hex") }),
  };
};

const parseUtxoResponseText = (responseText: string): readonly NodeUtxo[] =>
  parseNodeUtxoResponse(JSON.parse(responseText) as unknown).map(
    decodeNodeUtxo,
  );

export const fetchNodeUtxosByAddress = async (
  nodeEndpoint: string,
  address: string,
  options?: { readonly timeoutMs?: number },
): Promise<readonly NodeUtxo[]> => {
  const response = await fetch(
    `${nodeEndpoint}/utxos?address=${encodeURIComponent(address)}`,
    options?.timeoutMs === undefined
      ? undefined
      : { signal: AbortSignal.timeout(options.timeoutMs) },
  );
  const responseText = await response.text();
  if (!response.ok) {
    throw new Error(
      `Midgard node UTxO query failed (${response.status.toString()}): ${responseText}`,
    );
  }
  return parseUtxoResponseText(responseText);
};

export const fetchNodeUtxosByOutRefs = async (
  nodeEndpoint: string,
  outRefs: readonly string[],
): Promise<readonly NodeUtxo[]> => {
  const response = await fetch(`${nodeEndpoint}/utxos?by-outrefs`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(outRefs),
  });
  const responseText = await response.text();
  if (!response.ok) {
    throw new Error(
      `Midgard node UTxO query failed (${response.status.toString()}): ${responseText}`,
    );
  }
  return parseUtxoResponseText(responseText);
};

export const lucidUtxoFromNodeUtxo = (utxo: NodeUtxo): UTxO => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
  address: utxo.address,
  assets: { ...utxo.assets },
  ...(utxo.datum === undefined ? {} : { datum: utxo.datum }),
});
