import {
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  type ScriptLanguageName,
  type ScriptLanguageTag,
  scriptLanguageTagToName,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";

import { ProviderPayloadError } from "../core/errors.js";
import {
  decodeMidgardUtxo,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
} from "../core/index.js";
import { normalizeTxHash } from "../core/out-ref.js";
import type { ProtocolScriptLanguage } from "./types.js";

export const isObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

export const requireObject = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): Record<string, unknown> => {
  if (!isObject(value)) {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be an object`);
  }
  return value;
};

export const assertExactObjectKeys = (
  value: Record<string, unknown>,
  expectedKeys: readonly string[],
  fieldName: string,
  endpoint: string,
): void => {
  const expected = new Set(expectedKeys);
  const unknownKeys = Object.keys(value).filter((key) => !expected.has(key));
  if (unknownKeys.length > 0) {
    throw new ProviderPayloadError(
      endpoint,
      `${fieldName} contains unknown field${unknownKeys.length === 1 ? "" : "s"}`,
      unknownKeys.sort().join(","),
    );
  }
};

export const requireString = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): string => {
  if (typeof value !== "string") {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be a string`);
  }
  return value;
};

export const requireNumber = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value <= 0) {
    throw new ProviderPayloadError(
      endpoint,
      `${fieldName} must be a positive safe integer`,
    );
  }
  return value;
};

const requireNonNegativeSafeInteger = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new ProviderPayloadError(
      endpoint,
      `${fieldName} must be a non-negative safe integer`,
    );
  }
  return value;
};

export const parseNonNegativeBigInt = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): bigint => {
  const raw = requireString(value, fieldName, endpoint);
  if (!/^(0|[1-9][0-9]*)$/.test(raw)) {
    throw new ProviderPayloadError(
      endpoint,
      `${fieldName} must be a non-negative integer string`,
    );
  }
  return BigInt(raw);
};

export const cloneSupportedScriptLanguages = (
  languages: readonly ProtocolScriptLanguage[],
): readonly ProtocolScriptLanguage[] =>
  languages.map((language) => ({
    name: language.name,
    tag: language.tag,
  }));

const expectedScriptLanguageLabel = (
  language: ProtocolScriptLanguage,
): string => `${language.name}:${language.tag.toString(10)}`;

export const validateSupportedScriptLanguages = (
  languages: unknown,
  endpoint: string,
  fieldName = "supportedScriptLanguages",
  expectedLanguages: readonly ProtocolScriptLanguage[] = MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
): readonly ProtocolScriptLanguage[] => {
  if (!Array.isArray(languages)) {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be an array`);
  }
  const normalized = languages.map((raw, index) => {
    const language = requireObject(
      raw,
      `${fieldName}[${index.toString()}]`,
      endpoint,
    );
    assertExactObjectKeys(
      language,
      ["name", "tag"],
      `${fieldName}[${index.toString()}]`,
      endpoint,
    );
    const name = requireString(
      language.name,
      `${fieldName}[${index.toString()}].name`,
      endpoint,
    );
    const tag = requireNonNegativeSafeInteger(
      language.tag,
      `${fieldName}[${index.toString()}].tag`,
      endpoint,
    );
    let canonicalName: ScriptLanguageName;
    try {
      canonicalName = scriptLanguageTagToName(tag as ScriptLanguageTag);
    } catch (cause) {
      throw new ProviderPayloadError(
        endpoint,
        `unsupported script language tag ${tag.toString(10)}`,
        cause instanceof Error ? cause.message : String(cause),
      );
    }
    if (name !== canonicalName) {
      throw new ProviderPayloadError(
        endpoint,
        `script language tag/name mismatch for ${fieldName}[${index.toString()}]`,
        `${name}:${tag.toString(10)}`,
      );
    }
    return {
      name: canonicalName,
      tag: tag as ScriptLanguageTag,
    };
  });
  const expected = expectedLanguages.map(expectedScriptLanguageLabel).sort();
  const actual = normalized.map(expectedScriptLanguageLabel).sort();
  if (
    expected.length !== actual.length ||
    expected.some((label, index) => actual[index] !== label)
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "supported script languages must exactly match the Midgard protocol profile",
      `expected=${expected.join(",")} actual=${actual.join(",")}`,
    );
  }
  return cloneSupportedScriptLanguages(expectedLanguages);
};

const fromHex = (hex: string, fieldName: string, endpoint: string): Buffer => {
  try {
    return hexToBytes(hex, { fieldName });
  } catch {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be hex`);
  }
};

export const parseSubmitTxCanonicalCbor = (
  txCanonicalCborHex: string,
  endpoint: string,
  maxSubmitTxCborBytes?: number,
): Buffer => {
  const bytes = fromHex(txCanonicalCborHex, "tx_canonical_cbor", endpoint);
  if (
    maxSubmitTxCborBytes !== undefined &&
    bytes.length > maxSubmitTxCborBytes
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "tx_canonical_cbor exceeds protocol submit size limit",
      `size=${bytes.length.toString()} max=${maxSubmitTxCborBytes.toString()}`,
    );
  }
  return bytes;
};

export const normalizeTxIdHex = (txId: string, endpoint: string): string => {
  try {
    return normalizeTxHash(txId);
  } catch {
    throw new ProviderPayloadError(
      endpoint,
      "transaction id must be a 32-byte hex string",
    );
  }
};

export const txOutRefCborHex = (outRef: OutRef): string =>
  outRefToCbor(outRef).toString("hex");

export const decodeEncodedUtxo = (
  raw: unknown,
  endpoint: string,
): MidgardUtxo => {
  const utxo = requireObject(raw, "UTxO entry", endpoint);
  const outRefCbor = fromHex(
    requireString(utxo.outref, "utxo.outref", endpoint),
    "utxo.outref",
    endpoint,
  );
  const outputCbor = fromHex(
    requireString(utxo.outputCbor, "utxo.outputCbor", endpoint),
    "utxo.outputCbor",
    endpoint,
  );
  try {
    return decodeMidgardUtxo({
      outRefCbor,
      outputCbor,
    });
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "UTxO entry contains invalid Midgard CBOR",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

export const parseUtxosResponse = (
  payload: unknown,
  endpoint: string,
  message: string,
): readonly MidgardUtxo[] => {
  if (!isObject(payload) || !Array.isArray(payload.utxos)) {
    throw new ProviderPayloadError(endpoint, message);
  }
  return payload.utxos.map((entry) => decodeEncodedUtxo(entry, endpoint));
};

export const parseUtxoResponse = (
  payload: unknown,
  endpoint: string,
): MidgardUtxo => {
  if (!isObject(payload) || payload.utxo === undefined) {
    throw new ProviderPayloadError(
      endpoint,
      "GET /utxo response must contain utxo",
    );
  }
  return decodeEncodedUtxo(payload.utxo, endpoint);
};
