import {
  BuilderInvariantError,
  ProviderCapabilityError,
} from "../core/errors.js";
import {
  assertMidgardProvider,
  assertNetworkId,
  type BuilderContextSnapshot,
  cloneProviderDiagnostics,
  type LucidMidgardConfig,
  type LucidMidgardConfigSnapshot,
  type ProviderSnapshot,
  resolveNetworkId,
  type SwitchProviderOptions,
} from "./context.clone-protocol-info.js";
import {
  assertProviderNetwork,
  buildConfigSnapshot,
  validateProtocolInfo,
} from "./context.validate-protocol-info.js";

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
    protocolInfo.consensusProfile.nativeTransactionVersion;
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
    left.config.apiVersion !== right.config.apiVersion ||
    left.config.consensusProfile.profileId !==
      right.config.consensusProfile.profileId
  ) {
    throw new BuilderInvariantError(
      "Cannot compose builders with different consensus profiles",
      `left=${left.config.consensusProfile.profileId} right=${right.config.consensusProfile.profileId}`,
    );
  }
  if (
    left.provider.diagnostics.endpoint !==
      right.provider.diagnostics.endpoint ||
    left.provider.diagnostics.protocolInfoSource !==
      right.provider.diagnostics.protocolInfoSource
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
