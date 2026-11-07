import { MIDGARD_SUPPORTED_SCRIPT_LANGUAGES } from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core/consensus-profile";
import { parseDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import { ProviderPayloadError } from "../core/errors.js";
import {
  isSubmitAdmissionStatus,
  type SubmitTxResult,
  type TxStatus,
} from "../core/index.js";
import {
  assertExactObjectKeys,
  isObject,
  parseNonNegativeBigInt,
  requireNumber,
  requireObject,
  requireString,
  validateSupportedScriptLanguages,
} from "./payload.validate-supported-script-languages.js";
import type { MidgardProtocolInfo } from "./types.js";

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
  if (!isMidgardConsensusProfile(info.consensusProfile)) {
    throw new ProviderPayloadError(
      endpoint,
      "consensusProfile does not exactly match the compiled V1 profile",
    );
  }
  const deploymentMarker = (() => {
    try {
      return parseDeploymentMarker(info.deploymentMarker);
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
  const profileTxLimit = MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes;
  const maxSubmitTxCborBytes = requireNumber(
    submissionLimits.maxSubmitTxCborBytes,
    "submissionLimits.maxSubmitTxCborBytes",
    endpoint,
  );
  if (maxSubmitTxCborBytes > profileTxLimit) {
    throw new ProviderPayloadError(
      endpoint,
      `submissionLimits.maxSubmitTxCborBytes must be between 1 and ${profileTxLimit.toString()}`,
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
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
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
