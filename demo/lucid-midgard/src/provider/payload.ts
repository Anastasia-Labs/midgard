import {
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  type ScriptLanguageName,
  type ScriptLanguageTag,
  scriptLanguageTagToName,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_CONSENSUS_PROFILE_V1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { parseDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { hexToBytes } from "@al-ft/midgard-core/hex";

import { ProviderPayloadError } from "../core/errors.js";
import {
  decodeMidgardUtxo,
  isSubmitAdmissionStatus,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
  type SubmitTxResult,
  type TxStatus,
} from "../core/index.js";
import { normalizeTxHash } from "../core/out-ref.js";
import type { MidgardProtocolInfo, ProtocolScriptLanguage } from "./types.js";

export const isObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const requireObject = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): Record<string, unknown> => {
  if (!isObject(value)) {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be an object`);
  }
  return value;
};

const assertExactObjectKeys = (
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

export const parseProtocolInfo = (
  payload: unknown,
  endpoint: string,
): MidgardProtocolInfo => {
  const info = requireObject(payload, "protocol-info", endpoint);
  assertExactObjectKeys(
    info,
    [
      "apiVersion",
      "network",
      "midgardNativeTxVersion",
      "currentSlot",
      "consensusProfile",
      "deploymentMarker",
      "supportedScriptLanguages",
      "codecSupportedScriptLanguages",
      "protocolFeeParameters",
      "submissionLimits",
      "validation",
    ],
    "protocol-info",
    endpoint,
  );
  const protocolFeeParameters = requireObject(
    info.protocolFeeParameters,
    "protocolFeeParameters",
    endpoint,
  );
  assertExactObjectKeys(
    protocolFeeParameters,
    ["minFeeA", "minFeeB"],
    "protocolFeeParameters",
    endpoint,
  );
  const submissionLimits = requireObject(
    info.submissionLimits,
    "submissionLimits",
    endpoint,
  );
  assertExactObjectKeys(
    submissionLimits,
    ["maxSubmitTxCborBytes"],
    "submissionLimits",
    endpoint,
  );
  const validation = requireObject(info.validation, "validation", endpoint);
  assertExactObjectKeys(
    validation,
    ["strictnessProfile", "localValidationIsAuthoritative"],
    "validation",
    endpoint,
  );
  if (validation.localValidationIsAuthoritative !== false) {
    throw new ProviderPayloadError(
      endpoint,
      "validation.localValidationIsAuthoritative must be false",
    );
  }
  const apiVersion = requireNumber(info.apiVersion, "apiVersion", endpoint);
  if (apiVersion !== 1) {
    throw new ProviderPayloadError(
      endpoint,
      `apiVersion must equal 1; got ${apiVersion.toString()}`,
    );
  }
  if (!isMidgardConsensusProfileV1(info.consensusProfile)) {
    throw new ProviderPayloadError(
      endpoint,
      "consensusProfile does not exactly match the compiled V1 profile",
    );
  }
  const deploymentMarker = (() => {
    try {
      return parseDeploymentMarkerV1(info.deploymentMarker);
    } catch (cause) {
      throw new ProviderPayloadError(
        endpoint,
        "deploymentMarker must be the exact final DeploymentMarkerV1",
        cause instanceof Error ? cause.message : String(cause),
      );
    }
  })();
  const expectedNativeTxVersion = 1;
  const midgardNativeTxVersion = requireNumber(
    info.midgardNativeTxVersion,
    "midgardNativeTxVersion",
    endpoint,
  );
  if (midgardNativeTxVersion !== expectedNativeTxVersion) {
    throw new ProviderPayloadError(
      endpoint,
      `midgardNativeTxVersion must equal ${expectedNativeTxVersion.toString()} for API ${apiVersion.toString()}`,
    );
  }
  const supportedScriptLanguages = validateSupportedScriptLanguages(
    info.supportedScriptLanguages,
    endpoint,
    "supportedScriptLanguages",
    MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  );
  const profileTxLimit =
    MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes;
  const maxSubmitTxCborBytes = requireNumber(
    submissionLimits.maxSubmitTxCborBytes,
    "submissionLimits.maxSubmitTxCborBytes",
    endpoint,
  );
  if (
    !Number.isSafeInteger(maxSubmitTxCborBytes) ||
    maxSubmitTxCborBytes !== profileTxLimit
  ) {
    throw new ProviderPayloadError(
      endpoint,
      `submissionLimits.maxSubmitTxCborBytes must equal ${profileTxLimit.toString()}`,
    );
  }
  const common = {
    network: requireString(info.network, "network", endpoint),
    midgardNativeTxVersion,
    currentSlot: parseNonNegativeBigInt(
      info.currentSlot,
      "currentSlot",
      endpoint,
    ),
    supportedScriptLanguages,
    codecSupportedScriptLanguages: validateSupportedScriptLanguages(
      info.codecSupportedScriptLanguages,
      endpoint,
      "codecSupportedScriptLanguages",
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
      maxSubmitTxCborBytes,
    },
    validation: {
      strictnessProfile: requireString(
        validation.strictnessProfile,
        "validation.strictnessProfile",
        endpoint,
      ),
      localValidationIsAuthoritative: false as const,
    },
  };
  return {
    ...common,
    apiVersion: 1,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    deploymentMarker,
  };
};

export const parseSubmitTxResult = (
  payload: unknown,
  httpStatus: 200 | 202,
  endpoint: string,
): SubmitTxResult => {
  const response = requireObject(payload, "submit response", endpoint);
  if (typeof response.duplicate !== "boolean") {
    throw new ProviderPayloadError(
      endpoint,
      "submit response must contain duplicate boolean",
    );
  }
  const duplicate = response.duplicate;
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
  const status = requireString(response.status, "status", endpoint);
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
    response.firstSeenAt !== undefined &&
    typeof response.firstSeenAt !== "string"
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "submit response firstSeenAt must be string when present",
    );
  }
  if (
    response.lastSeenAt !== undefined &&
    typeof response.lastSeenAt !== "string"
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "submit response lastSeenAt must be string when present",
    );
  }
  return {
    txId: requireString(response.txId, "txId", endpoint),
    status,
    httpStatus,
    firstSeenAt: response.firstSeenAt,
    lastSeenAt: response.lastSeenAt,
    duplicate,
  };
};

export const parseTxStatus = (payload: unknown, endpoint: string): TxStatus => {
  const response = requireObject(payload, "tx-status", endpoint);
  const txId = requireString(response.txId, "txId", endpoint);
  const status = requireString(response.status, "status", endpoint);
  if (status === "rejected") {
    const timestamps = response.timestamps;
    const createdAt = isObject(timestamps)
      ? typeof timestamps.createdAt === "string"
        ? timestamps.createdAt
        : undefined
      : undefined;
    return {
      kind: "rejected",
      txId,
      code: requireString(response.reasonCode, "reasonCode", endpoint),
      detail:
        typeof response.reasonDetail === "string"
          ? response.reasonDetail
          : null,
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
