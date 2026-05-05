import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data as LucidData,
  credentialToAddress,
  getAddressDetails,
  type Assets,
  type Credential,
  type Network,
  type UTxO,
  valueToAssets,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import {
  decodeMidgardTxOutput,
  midgardOutputAddressText,
  midgardValueToCmlValue,
} from "@/validation/midgard-output.js";
import { isHexString } from "@/utils.js";

const DEFAULT_MIDGARD_NODE_PORT = "3000";

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

export type NodeUtxo = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly outrefCbor: Buffer;
  readonly outputCbor: Buffer;
  readonly address: string;
  readonly assets: Readonly<Assets>;
  readonly datum?: string;
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

export const defaultMidgardNodeEndpoint = (
  env: NodeJS.ProcessEnv = process.env,
): string =>
  parseNodeEndpoint(
    env.MIDGARD_NODE_URL?.trim() ??
      env.ACTIVITY_SUBMIT_ENDPOINT?.trim() ??
      env.STRESS_SUBMIT_ENDPOINT?.trim() ??
      `http://127.0.0.1:${env.PORT?.trim() || DEFAULT_MIDGARD_NODE_PORT}`,
  );

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
  const normalized = value.trim().toLowerCase();
  if (normalized.length === 0) {
    throw new Error(`${fieldName} must not be empty.`);
  }
  if (normalized.length % 2 !== 0 || !isHexString(normalized)) {
    throw new Error(`${fieldName} must be an even-length hex string.`);
  }
  const bytes = Buffer.from(normalized, "hex");
  if (expectedLength !== undefined && bytes.length !== expectedLength) {
    throw new Error(
      `${fieldName} must be ${expectedLength.toString()} bytes, got ${bytes.length.toString()}.`,
    );
  }
  return bytes;
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

export const parseTxOutRefLabel = (
  value: unknown,
  fieldName = "txOutRef",
): {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly cbor: Buffer;
  readonly outputReference: SDK.OutputReference;
} => {
  if (typeof value !== "string") {
    throw new Error(`${fieldName} must be a string.`);
  }
  const normalized = value.trim().toLowerCase();
  const parts = normalized.split("#");
  if (parts.length !== 2) {
    throw new Error(`${fieldName} must use the format <txHash>#<outputIndex>.`);
  }
  const [txHash, outputIndexRaw] = parts;
  if (txHash === undefined || txHash.length !== 64 || !isHexString(txHash)) {
    throw new Error(`${fieldName}.txHash must be a 32-byte hex string.`);
  }
  if (outputIndexRaw === undefined || !/^\d+$/.test(outputIndexRaw)) {
    throw new Error(`${fieldName}.outputIndex must be a non-negative integer.`);
  }
  const outputIndex = Number(outputIndexRaw);
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0) {
    throw new Error(`${fieldName}.outputIndex exceeds the safe integer range.`);
  }
  const input = CML.TransactionInput.new(
    CML.TransactionHash.from_hex(txHash),
    BigInt(outputIndex),
  );
  return {
    txHash,
    outputIndex,
    cbor: Buffer.from(input.to_cbor_bytes()),
    outputReference: {
      transactionId: txHash,
      outputIndex: BigInt(outputIndex),
    },
  };
};

export const txOutRefLabelFromCbor = (cbor: Buffer): string => {
  const input = CML.TransactionInput.from_cbor_bytes(cbor);
  return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
};

export const parseCardanoDatum = (
  value: string | undefined,
  fieldName = "datum",
): SDK.CardanoDatum => {
  if (value === undefined || value.trim().length === 0) {
    return "NoDatum";
  }
  const normalized = value.trim().toLowerCase();
  if (normalized.length % 2 !== 0 || !isHexString(normalized)) {
    throw new Error(`${fieldName} must be an even-length CBOR hex string.`);
  }
  try {
    return {
      InlineDatum: {
        data: LucidData.from(normalized),
      },
    } as SDK.CardanoDatum;
  } catch (cause) {
    throw new Error(
      `${fieldName} must decode as Plutus data CBOR: ${String(cause)}`,
    );
  }
};

export const cardanoDatumToJson = (datum: SDK.CardanoDatum): unknown =>
  datum === "NoDatum" ? "NoDatum" : LucidData.to(datum, SDK.CardanoDatum);

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
  const seedPhrase = env[normalizedEnv]?.trim() ?? "";
  if (seedPhrase.length === 0) {
    throw new Error(
      `Environment variable "${normalizedEnv}" does not contain a wallet seed phrase.`,
    );
  }
  return { seedPhrase, resolvedFrom: normalizedEnv };
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

export const addressDataToBech32 = (
  network: Network,
  address: SDK.AddressData,
): string => {
  const paymentCredential = credentialFromAddressData(
    address.paymentCredential,
  );
  if (address.stakeCredential === null) {
    return credentialToAddress(network, paymentCredential);
  }
  if ("Inline" in address.stakeCredential) {
    return credentialToAddress(
      network,
      paymentCredential,
      credentialFromAddressData(address.stakeCredential.Inline[0]),
    );
  }
  throw new Error("Pointer stake credentials are not supported by this CLI.");
};

const credentialFromAddressData = (credential: SDK.CredentialD): Credential => {
  if ("PublicKeyCredential" in credential) {
    return {
      type: "Key",
      hash: credential.PublicKeyCredential[0],
    };
  }
  return {
    type: "Script",
    hash: credential.ScriptCredential[0],
  };
};

const parseNodeUtxoResponse = (payload: unknown): readonly RawNodeUtxo[] => {
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
  const input = CML.TransactionInput.from_cbor_bytes(outrefCbor);
  const output = decodeMidgardTxOutput(outputCbor);
  return {
    txHash: input.transaction_id().to_hex(),
    outputIndex: Number(input.index()),
    outrefCbor,
    outputCbor,
    address: midgardOutputAddressText(output),
    assets: valueToAssets(midgardValueToCmlValue(output.value)),
    ...(output.datum === undefined
      ? {}
      : { datum: output.datum.cbor.toString("hex") }),
  };
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
  return parseNodeUtxoResponse(JSON.parse(responseText)).map(decodeNodeUtxo);
};

export const lucidUtxoFromNodeUtxo = (utxo: NodeUtxo): UTxO => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
  address: utxo.address,
  assets: { ...utxo.assets },
  ...(utxo.datum === undefined ? {} : { datum: utxo.datum }),
});
