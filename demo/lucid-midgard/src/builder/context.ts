import type { Network } from "@lucid-evolution/lucid";
import {
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import {
  BuilderInvariantError,
  ProviderCapabilityError,
  ProviderPayloadError,
} from "../core/errors.js";
import type { AuthoredOutput } from "../core/output.js";
import type { BuilderScriptState } from "../core/scripts.js";
import type { MidgardUtxo } from "../core/types.js";
import type {
  MidgardProtocolInfo,
  MidgardProvider,
  ProtocolScriptLanguage,
  ProviderDiagnostics,
} from "../provider.js";
import type { MidgardWallet } from "../wallet.js";

export type BuilderState = {
  readonly spendInputs: readonly MidgardUtxo[];
  readonly referenceInputs: readonly MidgardUtxo[];
  readonly outputs: readonly AuthoredOutput[];
  readonly requiredSigners: readonly string[];
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly minimumFee?: bigint;
  readonly networkId?: bigint;
  readonly scripts: BuilderScriptState;
  readonly composition?: {
    readonly fragmentCount: number;
  };
};

export type ProviderSnapshot = {
  readonly provider: MidgardProvider;
  readonly generation: number;
  readonly protocolInfo: MidgardProtocolInfo;
  readonly diagnostics: ProviderDiagnostics;
};

export type UtxoOverrideSnapshot = {
  readonly generation: number;
  readonly utxos: readonly MidgardUtxo[];
};

export type BuilderContextSnapshot = {
  readonly provider: ProviderSnapshot;
  readonly wallet?: MidgardWallet;
  readonly config: LucidMidgardConfigSnapshot;
  readonly utxoOverrides?: UtxoOverrideSnapshot;
};

export type LucidMidgardConfig = {
  readonly network?: Network;
  readonly networkId?: number;
};

export type LucidMidgardConfigSnapshot = {
  readonly network?: string;
  readonly networkId?: number;
  readonly providerGeneration: number;
  readonly apiVersion: number;
  readonly midgardNativeTxVersion: number;
  readonly currentSlot: bigint;
  readonly supportedScriptLanguages: readonly ProtocolScriptLanguage[];
  readonly protocolFeeParameters: {
    readonly minFeeA: bigint;
    readonly minFeeB: bigint;
  };
  readonly submissionLimits: {
    readonly maxSubmitTxCborBytes: number;
  };
  readonly validation: {
    readonly strictnessProfile: string;
    readonly localValidationIsAuthoritative: false;
  };
  readonly protocolInfoSource: ProviderDiagnostics["protocolInfoSource"];
  readonly protocolInfoFallbackReason?: string;
  readonly providerDiagnostics: ProviderDiagnostics;
  readonly utxoOverrideGeneration: number;
  readonly hasUtxoOverrides: boolean;
};

export type SwitchProviderOptions = {
  readonly expectedNetwork?: string;
  readonly expectedNetworkId?: number;
  readonly expectedApiVersion?: number;
  readonly expectedMidgardNativeTxVersion?: number;
};

const knownNetworkId = (network: string | undefined): number | undefined => {
  switch (network) {
    case "Mainnet":
      return 1;
    case "Preprod":
    case "Preview":
      return 0;
    default:
      return undefined;
  }
};

const assertNetworkId = (
  networkId: number | undefined,
  fieldName: string,
): number | undefined => {
  if (networkId === undefined) {
    return undefined;
  }
  if (!Number.isInteger(networkId) || networkId < 0 || networkId > 255) {
    throw new BuilderInvariantError(
      `${fieldName} must be an integer between 0 and 255`,
      `${fieldName}=${String(networkId)}`,
    );
  }
  return networkId;
};

const resolveNetworkId = (
  configuredNetworkId: number | undefined,
  network: string | undefined,
): number => {
  const networkId = assertNetworkId(
    configuredNetworkId ?? knownNetworkId(network),
    "networkId",
  );
  if (networkId === undefined) {
    throw new ProviderCapabilityError(
      "/protocol-info",
      "Protocol network is unknown and no explicit networkId was configured",
    );
  }
  return networkId;
};

export const configNetworkId = (config: LucidMidgardConfigSnapshot): bigint => {
  if (config.networkId === undefined) {
    throw new BuilderInvariantError("Midgard network id is not configured");
  }
  return BigInt(config.networkId);
};

export const stateNetworkId = (state: BuilderState): bigint => {
  if (state.networkId === undefined) {
    throw new BuilderInvariantError("Builder network id is not configured");
  }
  return state.networkId;
};

const cloneSupportedScriptLanguages = (
  languages: readonly ProtocolScriptLanguage[],
): readonly ProtocolScriptLanguage[] =>
  languages.map((language) => ({
    name: language.name,
    tag: language.tag,
  }));

export const cloneProtocolInfo = (
  info: MidgardProtocolInfo,
): MidgardProtocolInfo => ({
  apiVersion: info.apiVersion,
  network: info.network,
  midgardNativeTxVersion: info.midgardNativeTxVersion,
  currentSlot: info.currentSlot,
  supportedScriptLanguages: cloneSupportedScriptLanguages(
    info.supportedScriptLanguages,
  ),
  protocolFeeParameters: {
    minFeeA: info.protocolFeeParameters.minFeeA,
    minFeeB: info.protocolFeeParameters.minFeeB,
  },
  submissionLimits: {
    maxSubmitTxCborBytes: info.submissionLimits.maxSubmitTxCborBytes,
  },
  validation: {
    strictnessProfile: info.validation.strictnessProfile,
    localValidationIsAuthoritative:
      info.validation.localValidationIsAuthoritative,
  },
});

export const cloneProviderDiagnostics = (
  diagnostics: ProviderDiagnostics,
): ProviderDiagnostics => {
  if (typeof diagnostics !== "object" || diagnostics === null) {
    throw new ProviderPayloadError(
      "/provider/diagnostics",
      "Provider diagnostics must be an object",
    );
  }
  if (
    typeof diagnostics.endpoint !== "string" ||
    diagnostics.endpoint.trim().length === 0
  ) {
    throw new ProviderPayloadError(
      "/provider/diagnostics",
      "Provider diagnostics endpoint must be a non-empty string",
    );
  }
  if (
    diagnostics.protocolInfoSource !== "node" &&
    diagnostics.protocolInfoSource !== "fallback" &&
    diagnostics.protocolInfoSource !== "unknown"
  ) {
    throw new ProviderPayloadError(
      "/provider/diagnostics",
      "Provider diagnostics protocolInfoSource must be node, fallback, or unknown",
    );
  }
  if (
    diagnostics.protocolInfoSource === "fallback" &&
    (typeof diagnostics.protocolInfoFallbackReason !== "string" ||
      diagnostics.protocolInfoFallbackReason.trim().length === 0)
  ) {
    throw new ProviderPayloadError(
      "/provider/diagnostics",
      "Fallback protocol-info diagnostics require a non-empty reason",
    );
  }
  return {
    endpoint: diagnostics.endpoint,
    protocolInfoSource: diagnostics.protocolInfoSource,
    protocolInfoFallbackReason: diagnostics.protocolInfoFallbackReason,
  };
};

const freezeDeep = <T>(value: T): T => {
  if (typeof value !== "object" || value === null) {
    return value;
  }
  for (const property of Object.getOwnPropertyNames(value)) {
    const child = (value as Record<string, unknown>)[property];
    if (typeof child === "object" && child !== null) {
      freezeDeep(child);
    }
  }
  return Object.freeze(value);
};

const requireProviderMethod = (
  value: Record<string, unknown>,
  method: keyof MidgardProvider,
): void => {
  if (typeof value[method] !== "function") {
    throw new ProviderCapabilityError(
      "/provider",
      `Provider is not a MidgardProvider: missing ${String(method)}()`,
    );
  }
};

const assertMidgardProvider = (provider: unknown): MidgardProvider => {
  if (typeof provider !== "object" || provider === null) {
    throw new ProviderCapabilityError(
      "/provider",
      "Provider is not a MidgardProvider object",
    );
  }
  const candidate = provider as Record<string, unknown>;
  for (const method of [
    "getUtxos",
    "getUtxoByOutRef",
    "getProtocolInfo",
    "getProtocolParameters",
    "getCurrentSlot",
    "submitTx",
    "getTxStatus",
    "diagnostics",
  ] as const) {
    requireProviderMethod(candidate, method);
  }
  return provider as MidgardProvider;
};

export const validateProtocolInfo = (
  info: MidgardProtocolInfo,
): MidgardProtocolInfo => {
  if (typeof info !== "object" || info === null) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "Protocol info must be an object",
    );
  }
  if (!Number.isSafeInteger(info.apiVersion) || info.apiVersion <= 0) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "apiVersion must be a positive safe integer",
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
  const expectedLanguages = MIDGARD_SUPPORTED_SCRIPT_LANGUAGES.map(
    (language) => `${language.name}:${language.tag.toString(10)}`,
  ).sort();
  const actualLanguages = Array.isArray(info.supportedScriptLanguages)
    ? info.supportedScriptLanguages
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
              "supportedScriptLanguages entries must contain name and tag",
            );
          }
          return `${language.name}:${language.tag.toString(10)}`;
        })
        .sort()
    : undefined;
  if (
    actualLanguages === undefined ||
    expectedLanguages.length !== actualLanguages.length ||
    expectedLanguages.some((label, index) => actualLanguages[index] !== label)
  ) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "supportedScriptLanguages must exactly match PlutusV3 and MidgardV1 protocol tags",
    );
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
    info.submissionLimits.maxSubmitTxCborBytes <= 0
  ) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "submissionLimits.maxSubmitTxCborBytes must be a positive safe integer",
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

const assertProviderNetwork = ({
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
    supportedScriptLanguages: cloneSupportedScriptLanguages(
      protocolInfo.supportedScriptLanguages,
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
    protocolInfoFallbackReason: diagnostics.protocolInfoFallbackReason,
    providerDiagnostics: cloneProviderDiagnostics(diagnostics),
    utxoOverrideGeneration,
    hasUtxoOverrides,
  });
};

export const readProviderSnapshot = async ({
  provider,
  generation,
  config,
  currentConfig,
  options,
}: {
  readonly provider: unknown;
  readonly generation: number;
  readonly config: LucidMidgardConfig;
  readonly currentConfig?: LucidMidgardConfigSnapshot;
  readonly options?: SwitchProviderOptions;
}): Promise<{
  readonly snapshot: ProviderSnapshot;
  readonly config: LucidMidgardConfigSnapshot;
}> => {
  const midgardProvider = assertMidgardProvider(provider);
  const protocolInfo = validateProtocolInfo(
    await midgardProvider.getProtocolInfo(),
  );
  assertProviderNetwork({
    actual: protocolInfo.network,
    expected: config.network,
    label: "Configured",
  });
  assertProviderNetwork({
    actual: protocolInfo.network,
    expected: currentConfig?.network,
    label: "Current provider",
  });
  assertProviderNetwork({
    actual: protocolInfo.network,
    expected: options?.expectedNetwork,
    label: "Expected",
  });

  const derivedNetworkId = resolveNetworkId(
    config.networkId,
    config.network ?? protocolInfo.network,
  );
  const expectedNetworkId = assertNetworkId(
    options?.expectedNetworkId,
    "expectedNetworkId",
  );
  if (
    currentConfig?.networkId !== undefined &&
    currentConfig.networkId !== derivedNetworkId
  ) {
    throw new ProviderCapabilityError(
      "/protocol-info",
      `Provider network id mismatch: expected ${currentConfig.networkId.toString()}, actual ${derivedNetworkId.toString()}`,
    );
  }
  if (
    expectedNetworkId !== undefined &&
    expectedNetworkId !== derivedNetworkId
  ) {
    throw new ProviderCapabilityError(
      "/protocol-info",
      `Expected network id mismatch: expected ${expectedNetworkId.toString()}, actual ${derivedNetworkId.toString()}`,
    );
  }
  if (
    options?.expectedApiVersion !== undefined &&
    options.expectedApiVersion !== protocolInfo.apiVersion
  ) {
    throw new ProviderCapabilityError(
      "/protocol-info",
      `API version mismatch: expected ${options.expectedApiVersion.toString()}, actual ${protocolInfo.apiVersion.toString()}`,
    );
  }
  const expectedNativeVersion =
    options?.expectedMidgardNativeTxVersion ??
    Number(MIDGARD_NATIVE_TX_VERSION);
  if (protocolInfo.midgardNativeTxVersion !== expectedNativeVersion) {
    throw new ProviderCapabilityError(
      "/protocol-info",
      `Midgard native transaction version mismatch: expected ${expectedNativeVersion.toString()}, actual ${protocolInfo.midgardNativeTxVersion.toString()}`,
    );
  }

  const diagnostics = cloneProviderDiagnostics(midgardProvider.diagnostics());
  const configSnapshot = buildConfigSnapshot({
    input: config,
    protocolInfo,
    diagnostics,
    providerGeneration: generation,
    utxoOverrideGeneration: currentConfig?.utxoOverrideGeneration ?? 0,
    hasUtxoOverrides: currentConfig?.hasUtxoOverrides ?? false,
  });
  return {
    snapshot: {
      provider: midgardProvider,
      generation,
      protocolInfo,
      diagnostics,
    },
    config: configSnapshot,
  };
};

export const assertBuilderContextsComposable = (
  left: BuilderContextSnapshot,
  right: BuilderContextSnapshot,
): void => {
  if (left.provider.provider !== right.provider.provider) {
    throw new BuilderInvariantError(
      "Cannot compose builders with different providers",
    );
  }
  if (left.provider.generation !== right.provider.generation) {
    throw new BuilderInvariantError(
      "Cannot compose builders with different provider generations",
      `left=${left.provider.generation.toString()} right=${right.provider.generation.toString()}`,
    );
  }
  if (left.wallet !== right.wallet) {
    throw new BuilderInvariantError(
      "Cannot compose builders with different wallets",
    );
  }
  if (left.config.networkId !== right.config.networkId) {
    throw new BuilderInvariantError(
      "Cannot compose builders with different network ids",
      `left=${String(left.config.networkId)} right=${String(right.config.networkId)}`,
    );
  }
  if (
    left.config.midgardNativeTxVersion !== right.config.midgardNativeTxVersion
  ) {
    throw new BuilderInvariantError(
      "Cannot compose builders with different native transaction versions",
      `left=${left.config.midgardNativeTxVersion.toString()} right=${right.config.midgardNativeTxVersion.toString()}`,
    );
  }
  if (
    left.provider.diagnostics.endpoint !==
      right.provider.diagnostics.endpoint ||
    left.provider.diagnostics.protocolInfoSource !==
      right.provider.diagnostics.protocolInfoSource ||
    left.provider.diagnostics.protocolInfoFallbackReason !==
      right.provider.diagnostics.protocolInfoFallbackReason
  ) {
    throw new BuilderInvariantError(
      "Cannot compose builders with different provider diagnostics",
    );
  }
  const leftOverride = left.utxoOverrides?.generation;
  const rightOverride = right.utxoOverrides?.generation;
  if (leftOverride !== rightOverride) {
    throw new BuilderInvariantError(
      "Cannot compose builders with different UTxO override generations",
      `left=${String(leftOverride)} right=${String(rightOverride)}`,
    );
  }
};
