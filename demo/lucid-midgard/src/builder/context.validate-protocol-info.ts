import {
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_LIMITS,
} from "@al-ft/midgard-core/consensus-profile";

import {
  ProviderCapabilityError,
  ProviderPayloadError,
} from "../core/errors.js";
import type { MidgardProtocolInfo, ProviderDiagnostics } from "../provider.js";
import {
  cloneProtocolInfo,
  cloneProviderDiagnostics,
  cloneSupportedScriptLanguages,
  freezeDeep,
  type LucidMidgardConfig,
  type LucidMidgardConfigSnapshot,
  resolveNetworkId,
} from "./context.clone-protocol-info.js";

export const validateProtocolInfo = (
  info: MidgardProtocolInfo,
): MidgardProtocolInfo => {
  if (typeof info !== "object" || info === null) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "Protocol info must be an object",
    );
  }
  if (info.apiVersion !== 1) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "apiVersion must equal the compiled V1 protocol API version",
    );
  }
  if (typeof info.network !== "string" || info.network.trim().length === 0) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "network must be a non-empty string",
    );
  }
  if (
    !Number.isSafeInteger(info.midgardNativeTxVersion) ||
    info.midgardNativeTxVersion <= 0
  ) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "midgardNativeTxVersion must be a positive safe integer",
    );
  }
  if (typeof info.currentSlot !== "bigint" || info.currentSlot < 0n) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "currentSlot must be a non-negative bigint",
    );
  }
  if (!isMidgardConsensusProfile(info.consensusProfile)) {
    throw new ProviderCapabilityError(
      "/protocol-info",
      "Provider consensus profile does not match the compiled V1 profile",
    );
  }
  const languageLabels = (
    value: unknown,
    field: "supportedScriptLanguages" | "codecSupportedScriptLanguages",
  ): string[] => {
    if (!Array.isArray(value)) {
      throw new ProviderPayloadError(
        "/protocol-info",
        `${field} must be an array`,
      );
    }
    return value
      .map((language) => {
        if (
          typeof language !== "object" ||
          language === null ||
          typeof language.name !== "string" ||
          typeof language.tag !== "number" ||
          !Number.isSafeInteger(language.tag)
        ) {
          throw new ProviderPayloadError(
            "/protocol-info",
            `${field} entries must contain canonical name and tag values`,
          );
        }
        return `${language.name}:${language.tag.toString(10)}`;
      })
      .sort();
  };
  const compiledLanguages = languageLabels(
    MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    "codecSupportedScriptLanguages",
  );
  for (const [field, value] of [
    ["supportedScriptLanguages", info.supportedScriptLanguages],
    ["codecSupportedScriptLanguages", info.codecSupportedScriptLanguages],
  ] as const) {
    const labels = languageLabels(value, field);
    if (
      labels.length !== compiledLanguages.length ||
      compiledLanguages.some((label, index) => labels[index] !== label)
    ) {
      throw new ProviderPayloadError(
        "/protocol-info",
        `${field} must exactly match the compiled canonical script-language set`,
      );
    }
  }
  if (
    typeof info.protocolFeeParameters !== "object" ||
    info.protocolFeeParameters === null ||
    typeof info.protocolFeeParameters.minFeeA !== "bigint" ||
    typeof info.protocolFeeParameters.minFeeB !== "bigint" ||
    info.protocolFeeParameters.minFeeA < 0n ||
    info.protocolFeeParameters.minFeeB < 0n
  ) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "protocolFeeParameters must contain non-negative bigint fees",
    );
  }
  if (
    typeof info.submissionLimits !== "object" ||
    info.submissionLimits === null ||
    !Number.isSafeInteger(info.submissionLimits.maxSubmitTxCborBytes) ||
    info.submissionLimits.maxSubmitTxCborBytes <= 0 ||
    info.submissionLimits.maxSubmitTxCborBytes >
      MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes
  ) {
    throw new ProviderPayloadError(
      "/protocol-info",
      `submissionLimits.maxSubmitTxCborBytes must be between 1 and ${MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes.toString()}`,
    );
  }
  if (
    typeof info.validation !== "object" ||
    info.validation === null ||
    typeof info.validation.strictnessProfile !== "string" ||
    info.validation.strictnessProfile.trim().length === 0 ||
    info.validation.localValidationIsAuthoritative !== false
  ) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "validation profile must be explicit and non-authoritative locally",
    );
  }
  if (info.midgardNativeTxVersion !== Number(MIDGARD_NATIVE_TX_VERSION)) {
    throw new ProviderCapabilityError(
      "/protocol-info",
      "Provider Midgard native transaction version mismatch",
    );
  }
  return cloneProtocolInfo(info);
};

export const assertProviderNetwork = ({
  actual,
  expected,
  label,
}: {
  readonly actual: string;
  readonly expected: string | undefined;
  readonly label: string;
}): void => {
  if (expected !== undefined && actual !== expected) {
    throw new ProviderCapabilityError(
      "/protocol-info",
      `${label} network mismatch: expected ${expected}, actual ${actual}`,
    );
  }
};

export const buildConfigSnapshot = ({
  input,
  protocolInfo,
  diagnostics,
  providerGeneration,
  utxoOverrideGeneration,
  hasUtxoOverrides,
}: {
  readonly input: LucidMidgardConfig;
  readonly protocolInfo: MidgardProtocolInfo;
  readonly diagnostics: ProviderDiagnostics;
  readonly providerGeneration: number;
  readonly utxoOverrideGeneration: number;
  readonly hasUtxoOverrides: boolean;
}): LucidMidgardConfigSnapshot => {
  const network = input.network ?? protocolInfo.network;
  const networkId = resolveNetworkId(input.networkId, network);
  return freezeDeep({
    network,
    networkId,
    providerGeneration,
    apiVersion: protocolInfo.apiVersion,
    midgardNativeTxVersion: protocolInfo.midgardNativeTxVersion,
    currentSlot: protocolInfo.currentSlot,
    consensusProfile: protocolInfo.consensusProfile,
    ...(protocolInfo.deploymentMarker === undefined
      ? {}
      : { deploymentMarker: { ...protocolInfo.deploymentMarker } }),
    supportedScriptLanguages: cloneSupportedScriptLanguages(
      protocolInfo.supportedScriptLanguages,
    ),
    codecSupportedScriptLanguages: cloneSupportedScriptLanguages(
      protocolInfo.codecSupportedScriptLanguages,
    ),
    protocolFeeParameters: {
      minFeeA: protocolInfo.protocolFeeParameters.minFeeA,
      minFeeB: protocolInfo.protocolFeeParameters.minFeeB,
    },
    submissionLimits: {
      maxSubmitTxCborBytes: protocolInfo.submissionLimits.maxSubmitTxCborBytes,
    },
    validation: {
      strictnessProfile: protocolInfo.validation.strictnessProfile,
      localValidationIsAuthoritative:
        protocolInfo.validation.localValidationIsAuthoritative,
    },
    protocolInfoSource: diagnostics.protocolInfoSource,
    providerDiagnostics: cloneProviderDiagnostics(diagnostics),
    utxoOverrideGeneration,
    hasUtxoOverrides,
  });
};
