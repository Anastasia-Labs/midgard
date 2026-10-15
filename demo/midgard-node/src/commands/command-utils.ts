import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  encodeMidgardSpendInputItem,
  midgardAddressFromText,
  midgardValueToCmlValue,
} from "@al-ft/midgard-core/codec";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
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

import type { OutRefLike } from "../tx-context.js";
import {
  paceUtxoPage,
  readUtxoBatchPages,
  readUtxoPage,
} from "./command-utils.utxo-http-request.js";
import {
  isPublicDeploymentProfile,
  readMountedHttpFile,
} from "./listen-public-http.js";

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
    cbor = encodeMidgardSpendInputItem({
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
    return encodeMidgardSpendInputItem(decodeMidgardSpendInputItem(bytes));
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
  walletSeedPhraseFile,
  walletSeedPhraseEnv,
  env = process.env,
}: {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseFile?: string;
  readonly walletSeedPhraseEnv: string;
  readonly env?: NodeJS.ProcessEnv;
}): ResolvedWalletSeedPhrase => {
  if (walletSeedPhraseFile !== undefined) {
    if (walletSeedPhrase !== undefined)
      throw new Error("Use only one wallet secret source");
    const seedPhrase = readMountedHttpFile(walletSeedPhraseFile, true)
      .toString("utf8")
      .trim();
    if (seedPhrase.length === 0)
      throw new Error("Mounted wallet seed is empty");
    return { seedPhrase, resolvedFrom: "mounted-file" };
  }
  if (isPublicDeploymentProfile(SELECTED_DEPLOYMENT_PROFILE.name))
    throw new Error(
      "Public wallet commands require a mounted wallet seed file; literal and environment seeds are unavailable",
    );
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
  const input = decodeMidgardSpendInputItem(outrefCbor);
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

export const fetchNodeUtxosByAddress = async (
  nodeEndpoint: string,
  address: string,
  options?: { readonly timeoutMs?: number },
): Promise<readonly NodeUtxo[]> => {
  const all: NodeUtxo[] = [];
  const seen = new Set<string>();
  const deadline = Date.now() + 60_000;
  let cursor: string | null = null;
  for (let page = 0; page < 100; page += 1) {
    if (page > 0) await paceUtxoPage();
    const parsed = await readUtxoPage(
      `${nodeEndpoint}/utxos?address=${encodeURIComponent(address)}&limit=100${cursor === null ? "" : `&cursor=${cursor}`}`,
      {},
      deadline,
      options?.timeoutMs ?? 10_000,
    );
    const entries = parseNodeUtxoResponse(parsed).map(decodeNodeUtxo);
    if (entries.length > 100)
      throw new Error("Midgard node UTxO page exceeds the row limit");
    all.push(...entries);
    if (
      typeof parsed !== "object" ||
      parsed === null ||
      !("nextCursor" in parsed)
    )
      throw new Error("Midgard node UTxO page is missing its cursor");
    const next = parsed.nextCursor;
    if (next === null) return all;
    if (
      typeof next !== "string" ||
      !/^(?:[0-9a-f]{2}){38}$/u.test(next) ||
      seen.has(next) ||
      (cursor !== null && next <= cursor)
    )
      throw new Error(
        "Midgard node UTxO page has an invalid or repeated cursor",
      );
    seen.add(next);
    cursor = next;
  }
  throw new Error("Midgard node UTxO query exceeds the page limit");
};

export const fetchNodeUtxosByOutRefs = async (
  nodeEndpoint: string,
  outRefs: readonly string[],
): Promise<readonly NodeUtxo[]> => {
  if (outRefs.length > 10_000 || new Set(outRefs).size !== outRefs.length)
    throw new Error(
      "Midgard node UTxO references exceed the limit or contain duplicates",
    );
  const all: NodeUtxo[] = [];
  const deadline = Date.now() + 60_000;
  for await (const { page, requestedCount } of readUtxoBatchPages(
    `${nodeEndpoint}/utxos?by-outrefs`,
    outRefs,
    deadline,
  )) {
    const entries = parseNodeUtxoResponse(page).map(decodeNodeUtxo);
    if (entries.length > requestedCount)
      throw new Error("Midgard node UTxO page exceeds the row limit");
    all.push(...entries);
  }
  return all;
};

export const lucidUtxoFromNodeUtxo = (utxo: NodeUtxo): UTxO => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
  address: utxo.address,
  assets: { ...utxo.assets },
  ...(utxo.datum === undefined ? {} : { datum: utxo.datum }),
});
