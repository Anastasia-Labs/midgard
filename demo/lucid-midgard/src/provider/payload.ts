import { CML } from "@lucid-evolution/lucid";
import {
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  encodeMidgardAddressText,
  midgardAddressFromText,
  scriptLanguageTagToName,
  type ScriptLanguageName,
  type ScriptLanguageTag,
} from "@al-ft/midgard-core/codec";
import {
  decodeMidgardUtxo,
  isSubmitAdmissionStatus,
  outRefToCbor,
  type Address,
  type MidgardUtxo,
  type OutRef,
  type SubmitTxResult,
  type TxStatus,
} from "../core/index.js";
import { ProviderPayloadError } from "../core/errors.js";
import { normalizeTxHash } from "../core/out-ref.js";
import type { MidgardProtocolInfo, ProtocolScriptLanguage } from "./types.js";

export const isObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const requireString = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): string => {
  if (typeof value !== "string") {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be a string`);
  }
  return value;
};

const requireNumber = (
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

const parseNonNegativeBigInt = (
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

const validateSupportedScriptLanguages = (
  languages: unknown,
  endpoint: string,
  fieldName = "supportedScriptLanguages",
): readonly ProtocolScriptLanguage[] => {
  if (!Array.isArray(languages)) {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be an array`);
  }
  const normalized = languages.map((raw, index) => {
    if (!isObject(raw)) {
      throw new ProviderPayloadError(
        endpoint,
        `${fieldName}[${index.toString()}] must be an object`,
      );
    }
    const name = requireString(
      raw.name,
      `${fieldName}[${index.toString()}].name`,
      endpoint,
    );
    const tag = requireNonNegativeSafeInteger(
      raw.tag,
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
  const expected = MIDGARD_SUPPORTED_SCRIPT_LANGUAGES.map(
    expectedScriptLanguageLabel,
  ).sort();
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
  return cloneSupportedScriptLanguages(MIDGARD_SUPPORTED_SCRIPT_LANGUAGES);
};

const fromHex = (hex: string, fieldName: string, endpoint: string): Buffer => {
  const normalized = hex.trim().toLowerCase();
  if (normalized.length === 0 || normalized.length % 2 !== 0) {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be hex`);
  }
  if (!/^[0-9a-f]+$/.test(normalized)) {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be hex`);
  }
  return Buffer.from(normalized, "hex");
};

export const normalizeSubmitTxEnvelopeCborHex = (
  txEnvelopeCborHex: string,
  endpoint: string,
  maxSubmitTxCborBytes?: number,
): string => {
  const bytes = fromHex(txEnvelopeCborHex, "tx_envelope_cbor", endpoint);
  if (
    maxSubmitTxCborBytes !== undefined &&
    bytes.length > maxSubmitTxCborBytes
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "tx_envelope_cbor exceeds protocol submit size limit",
      `size=${bytes.length.toString()} max=${maxSubmitTxCborBytes.toString()}`,
    );
  }
  return bytes.toString("hex");
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

export const normalizeAddressForUtxoQuery = (
  address: Address,
  endpoint: string,
): string => {
  try {
    return encodeMidgardAddressText(midgardAddressFromText(address));
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "address must be a valid Midgard bech32 address",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

export const txOutRefCborHex = (outRef: OutRef): string =>
  outRefToCbor(outRef).toString("hex");

export const decodeEncodedUtxo = (
  raw: unknown,
  endpoint: string,
): MidgardUtxo => {
  if (!isObject(raw)) {
    throw new ProviderPayloadError(endpoint, "UTxO entry must be an object");
  }
  const outRefCbor = fromHex(
    requireString(raw.outref, "utxo.outref", endpoint),
    "utxo.outref",
    endpoint,
  );
  const outputCbor = fromHex(
    requireString(raw.outputCbor, "utxo.outputCbor", endpoint),
    "utxo.outputCbor",
    endpoint,
  );
  let input: InstanceType<typeof CML.TransactionInput>;
  try {
    input = CML.TransactionInput.from_cbor_bytes(outRefCbor);
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "utxo.outref is not canonical TxOutRef CBOR",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const outputIndex = Number(input.index());
  if (!Number.isSafeInteger(outputIndex)) {
    throw new ProviderPayloadError(
      endpoint,
      "utxo.outref output index exceeds safe integer range",
    );
  }
  try {
    return decodeMidgardUtxo({
      outRef: {
        txHash: input.transaction_id().to_hex(),
        outputIndex,
      },
      outRefCbor,
      outputCbor,
    });
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "utxo.outputCbor is not valid Midgard output CBOR",
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

export const parseProtocolInfo = (
  payload: unknown,
  endpoint: string,
): MidgardProtocolInfo => {
  if (!isObject(payload)) {
    throw new ProviderPayloadError(endpoint, "protocol-info must be an object");
  }
  const protocolFeeParameters = payload.protocolFeeParameters;
  const submissionLimits = payload.submissionLimits;
  const validation = payload.validation;
  if (!isObject(protocolFeeParameters)) {
    throw new ProviderPayloadError(
      endpoint,
      "protocolFeeParameters must be an object",
    );
  }
  if (!isObject(submissionLimits)) {
    throw new ProviderPayloadError(
      endpoint,
      "submissionLimits must be an object",
    );
  }
  if (!isObject(validation)) {
    throw new ProviderPayloadError(endpoint, "validation must be an object");
  }
  if (validation.localValidationIsAuthoritative !== false) {
    throw new ProviderPayloadError(
      endpoint,
      "validation.localValidationIsAuthoritative must be false",
    );
  }
  return {
    apiVersion: requireNumber(payload.apiVersion, "apiVersion", endpoint),
    network: requireString(payload.network, "network", endpoint),
    midgardNativeTxVersion: requireNumber(
      payload.midgardNativeTxVersion,
      "midgardNativeTxVersion",
      endpoint,
    ),
    currentSlot: parseNonNegativeBigInt(
      payload.currentSlot,
      "currentSlot",
      endpoint,
    ),
    supportedScriptLanguages: validateSupportedScriptLanguages(
      payload.supportedScriptLanguages,
      endpoint,
    ),
    protocolFeeParameters: {
      minFeeA: parseNonNegativeBigInt(
        protocolFeeParameters.minFeeA,
        "protocolFeeParameters.minFeeA",
        endpoint,
      ),
      minFeeB: parseNonNegativeBigInt(
        protocolFeeParameters.minFeeB,
        "protocolFeeParameters.minFeeB",
        endpoint,
      ),
    },
    submissionLimits: {
      maxSubmitTxCborBytes: requireNumber(
        submissionLimits.maxSubmitTxCborBytes,
        "submissionLimits.maxSubmitTxCborBytes",
        endpoint,
      ),
    },
    validation: {
      strictnessProfile: requireString(
        validation.strictnessProfile,
        "validation.strictnessProfile",
        endpoint,
      ),
      localValidationIsAuthoritative: false,
    },
  };
};

export const validateFallbackProtocolInfo = (
  protocolInfo: MidgardProtocolInfo,
  endpoint: string,
): MidgardProtocolInfo => {
  if (!isObject(protocolInfo)) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback protocolInfo must be an object",
    );
  }
  if (
    typeof protocolInfo.apiVersion !== "number" ||
    !Number.isSafeInteger(protocolInfo.apiVersion) ||
    protocolInfo.apiVersion <= 0
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback apiVersion must be a positive safe integer",
    );
  }
  if (typeof protocolInfo.network !== "string") {
    throw new ProviderPayloadError(endpoint, "fallback network must be string");
  }
  if (
    typeof protocolInfo.midgardNativeTxVersion !== "number" ||
    !Number.isSafeInteger(protocolInfo.midgardNativeTxVersion) ||
    protocolInfo.midgardNativeTxVersion <= 0
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback midgardNativeTxVersion must be a positive safe integer",
    );
  }
  if (
    typeof protocolInfo.currentSlot !== "bigint" ||
    protocolInfo.currentSlot < 0n
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback currentSlot must be a non-negative bigint",
    );
  }
  const supportedScriptLanguages = validateSupportedScriptLanguages(
    protocolInfo.supportedScriptLanguages,
    endpoint,
    "fallback supportedScriptLanguages",
  );
  if (!isObject(protocolInfo.protocolFeeParameters)) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback protocolFeeParameters must be an object",
    );
  }
  if (
    typeof protocolInfo.protocolFeeParameters.minFeeA !== "bigint" ||
    protocolInfo.protocolFeeParameters.minFeeA < 0n ||
    typeof protocolInfo.protocolFeeParameters.minFeeB !== "bigint" ||
    protocolInfo.protocolFeeParameters.minFeeB < 0n
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback fee parameters must be non-negative bigints",
    );
  }
  if (!isObject(protocolInfo.submissionLimits)) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback submissionLimits must be an object",
    );
  }
  if (
    typeof protocolInfo.submissionLimits.maxSubmitTxCborBytes !== "number" ||
    !Number.isSafeInteger(protocolInfo.submissionLimits.maxSubmitTxCborBytes) ||
    protocolInfo.submissionLimits.maxSubmitTxCborBytes <= 0
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback maxSubmitTxCborBytes must be a positive safe integer",
    );
  }
  if (!isObject(protocolInfo.validation)) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback validation must be an object",
    );
  }
  if (
    typeof protocolInfo.validation.strictnessProfile !== "string" ||
    protocolInfo.validation.localValidationIsAuthoritative !== false
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback validation facts are invalid",
    );
  }
  return {
    ...protocolInfo,
    supportedScriptLanguages,
  };
};

export const parseSubmitTxResult = (
  payload: unknown,
  httpStatus: 200 | 202,
  endpoint: string,
): SubmitTxResult => {
  if (!isObject(payload)) {
    throw new ProviderPayloadError(endpoint, "submit response must be object");
  }
  const duplicate =
    typeof payload.duplicate === "boolean" ? payload.duplicate : undefined;
  if (duplicate === undefined) {
    throw new ProviderPayloadError(
      endpoint,
      "submit response must contain duplicate boolean",
    );
  }
  if (httpStatus === 202 && duplicate) {
    throw new ProviderPayloadError(
      endpoint,
      "new submit admission cannot be marked duplicate",
    );
  }
  if (httpStatus === 200 && !duplicate) {
    throw new ProviderPayloadError(
      endpoint,
      "duplicate submit admission must be marked duplicate",
    );
  }
  const status = requireString(payload.status, "status", endpoint);
  if (!isSubmitAdmissionStatus(status)) {
    throw new ProviderPayloadError(
      endpoint,
      "submit response status is not a supported durable admission status",
      status,
    );
  }
  if (httpStatus === 202 && status !== "queued") {
    throw new ProviderPayloadError(
      endpoint,
      "new submit admission must start queued",
    );
  }
  if (
    payload.firstSeenAt !== undefined &&
    typeof payload.firstSeenAt !== "string"
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "submit response firstSeenAt must be string when present",
    );
  }
  if (
    payload.lastSeenAt !== undefined &&
    typeof payload.lastSeenAt !== "string"
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "submit response lastSeenAt must be string when present",
    );
  }
  return {
    txId: normalizeTxIdHex(
      requireString(payload.txId, "txId", endpoint),
      endpoint,
    ),
    status,
    httpStatus,
    firstSeenAt: payload.firstSeenAt,
    lastSeenAt: payload.lastSeenAt,
    duplicate,
  };
};

export const parseTxStatus = (payload: unknown, endpoint: string): TxStatus => {
  if (!isObject(payload)) {
    throw new ProviderPayloadError(endpoint, "tx-status must be an object");
  }
  const txId = requireString(payload.txId, "txId", endpoint);
  const status = requireString(payload.status, "status", endpoint);
  if (status === "rejected") {
    const timestamps = payload.timestamps;
    const createdAt = isObject(timestamps)
      ? typeof timestamps.createdAt === "string"
        ? timestamps.createdAt
        : undefined
      : undefined;
    return {
      kind: "rejected",
      txId,
      code: requireString(payload.reasonCode, "reasonCode", endpoint),
      detail:
        typeof payload.reasonDetail === "string" ? payload.reasonDetail : null,
      createdAt,
    };
  }
  if (
    status === "committed" ||
    status === "accepted" ||
    status === "pending_commit" ||
    status === "awaiting_local_recovery" ||
    status === "validating" ||
    status === "queued" ||
    status === "not_found"
  ) {
    return { kind: status, txId };
  }
  throw new ProviderPayloadError(endpoint, `unsupported tx status ${status}`);
};
