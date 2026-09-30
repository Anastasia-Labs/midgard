import {
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import type { DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { Network } from "@lucid-evolution/lucid";

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
  readonly consensusProfile: MidgardConsensusProfile;
  readonly deploymentMarker?: DeploymentMarker;
  readonly supportedScriptLanguages: readonly ProtocolScriptLanguage[];
  readonly codecSupportedScriptLanguages: readonly ProtocolScriptLanguage[];
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
    case undefined:
    default:
      return undefined;
  }
};

export const assertNetworkId = (
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

export const resolveNetworkId = (
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

export const cloneSupportedScriptLanguages = (
  languages: readonly ProtocolScriptLanguage[],
): readonly ProtocolScriptLanguage[] =>
  languages.map(({ name, tag }) => ({ name, tag }));

export const cloneProtocolInfo = (
  info: MidgardProtocolInfo,
): MidgardProtocolInfo => {
  const common = {
    network: info.network,
    midgardNativeTxVersion: info.midgardNativeTxVersion,
    currentSlot: info.currentSlot,
    supportedScriptLanguages: cloneSupportedScriptLanguages(
      info.supportedScriptLanguages,
    ),
    codecSupportedScriptLanguages: cloneSupportedScriptLanguages(
      info.codecSupportedScriptLanguages,
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
  };
  return {
    ...common,
    apiVersion: 1,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    ...(info.deploymentMarker === undefined
      ? {}
      : { deploymentMarker: { ...info.deploymentMarker } }),
  };
};

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
    diagnostics.protocolInfoSource !== "offline" &&
    diagnostics.protocolInfoSource !== "unknown"
  ) {
    throw new ProviderPayloadError(
      "/provider/diagnostics",
      "Provider diagnostics protocolInfoSource must be node, offline, or unknown",
    );
  }
  return {
    endpoint: diagnostics.endpoint,
    protocolInfoSource: diagnostics.protocolInfoSource,
  };
};

export const freezeDeep = <T>(value: T): T => {
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

export const assertMidgardProvider = (provider: unknown): MidgardProvider => {
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
