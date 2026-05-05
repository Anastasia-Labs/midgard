import { CML, type Network } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";
import {
  LedgerColumns,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  type PhaseAConfig,
  type PhaseAResult,
  type PhaseBConfig,
  type PhaseBResultWithPatch,
  type QueuedTx,
  type RejectedTx,
} from "@al-ft/midgard-validation";
import {
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  computeScriptIntegrityHashForLanguages,
  encodeMidgardVersionedScript,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  asArray,
  asBytes,
  decodeSingleCbor,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
  deriveMidgardNativeTxCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxFull,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardAddressFromText,
  midgardAddressToText,
  verifyMidgardNativeTxFullConsistency,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxFull,
  type MidgardVersionedScript,
  type ScriptLanguageName,
} from "./codec/index.js";
import {
  authoredOutput,
  decodeMidgardTxOutput,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  normalizeScriptRef,
  normalizePlutusData,
  outputAddressPaymentKeyHash,
  outputAddressPaymentScriptHash,
  outputAddressProtected,
  utxoAddress,
  utxoAssets as utxoOutputAssets,
  utxoOutRefCbor,
  utxoOutputCbor,
  utxoProtectedAddress,
  type AuthoredOutput,
  type OutputOptions,
  type PlutusDataLike,
  type ScriptRefLike,
} from "./core/output.js";
import {
  addAssets,
  assetQuantity,
  isZeroAssets,
  normalizeAssets,
  subtractAssets,
  type Assets,
  type AssetUnit,
  type ValueLike,
} from "./core/assets.js";
import {
  emptyBuilderScriptState,
  isSubmitAdmissionStatus,
  type Address,
  type BuilderSnapshot,
  type CompleteOptions,
  type LocalValidationPreStateSource,
  type LocalValidationPreState,
  type LocalValidationReport,
  type MidgardScript,
  type MidgardUtxo,
  type MidgardProtocolParameters,
  type MidgardResult,
  type SubmitTxResult,
  type TxStatus,
  type WalletInputSource,
} from "./core/types.js";
import {
  BuilderInvariantError,
  InsufficientFundsError,
  LucidMidgardError,
  ProviderCapabilityError,
  ProviderError,
  ProviderPayloadError,
  SigningError,
} from "./core/errors.js";
import {
  compareOutRefs,
  normalizeOutRef,
  outRefLabel,
  type OutRef,
} from "./core/out-ref.js";
import type {
  BuilderScriptState,
  DatumWitness,
  MintIntent,
  ObserverIntent,
  ObserverValidator,
  ReceiveRedeemerIntent,
  Redeemer,
  ScriptSource,
  ScriptLanguage,
  TrustedReferenceScriptMetadata,
  MintAssets,
  MintingPolicy,
  SpendInputIntent,
  SpendingValidator,
} from "./core/scripts.js";
import type {
  MidgardProtocolInfo,
  MidgardProvider,
  ProtocolScriptLanguage,
  ProviderDiagnostics,
} from "./provider.js";
import {
  assertAddressNetwork,
  assertVKeyWitness,
  makeVKeyWitness,
  paymentKeyHashFromAddress,
  walletFromExternalSigner,
  walletFromPrivateKey,
  walletFromSeedPhrase,
  type ExternalBodyHashSigner,
  type MidgardWallet,
  type PrivateKey,
  type VKeyWitness,
} from "./wallet.js";

type BuilderState = {
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

type ProviderSnapshot = {
  readonly provider: MidgardProvider;
  readonly generation: number;
  readonly protocolInfo: MidgardProtocolInfo;
  readonly diagnostics: ProviderDiagnostics;
};

type UtxoOverrideSnapshot = {
  readonly generation: number;
  readonly utxos: readonly MidgardUtxo[];
};

type BuilderContextSnapshot = {
  readonly provider: ProviderSnapshot;
  readonly wallet?: MidgardWallet;
  readonly config: LucidMidgardConfigSnapshot;
  readonly utxoOverrides?: UtxoOverrideSnapshot;
};

type ResolvedWalletInputs = {
  readonly source: WalletInputSource;
  readonly inputs: readonly MidgardUtxo[];
  readonly overrideGeneration?: number;
};

type BalancedCompletionInputs = {
  readonly changeAddress: Address;
  readonly feePolicy: FeePolicy;
  readonly walletInputs: ResolvedWalletInputs;
};

export type CompleteTxMetadata = {
  readonly fee: bigint;
  readonly inputCount: number;
  readonly referenceInputCount: number;
  readonly outputCount: number;
  readonly requiredSignerCount: number;
  readonly txByteLength: number;
  readonly feeIterations: number;
  readonly balanced: boolean;
  readonly changeAddress?: Address;
  readonly changeAssets?: Assets;
  readonly changeOutputIndex?: number;
  readonly expectedAddrWitnessCount?: number;
  readonly expectedAddrWitnessKeyHashes?: readonly string[];
  readonly expectedAddrWitnessesComplete?: boolean;
  readonly estimatedSignedTxByteLength?: number;
  readonly addrWitnessCount?: number;
  readonly signedBy?: readonly string[];
  readonly localValidation?: LocalValidationReport;
  readonly providerGeneration?: number;
  readonly providerDiagnostics?: ProviderDiagnostics;
  readonly walletInputSource?: WalletInputSource;
  readonly walletInputCount?: number;
  readonly utxoOverrideGeneration?: number;
};

type CompleteTxContext = {
  readonly provider?: MidgardProvider;
  readonly wallet?: () => MidgardWallet | undefined;
  readonly networkId?: bigint;
  readonly maxSubmitTxCborBytes?: number;
};

export type TxStatusKind = TxStatus["kind"];

export type SubmitOptions = {
  readonly provider?: MidgardProvider;
};

export type AwaitTxOptions = {
  readonly provider?: MidgardProvider;
  readonly until?: TxStatusKind | readonly TxStatusKind[];
  readonly pollIntervalMs?: number;
  readonly timeoutMs?: number;
  readonly signal?: AbortSignal;
};

export type LocalPreflightPhase = "phase-a" | "phase-b";

export type LocalPreflightOptions = {
  readonly provider?: MidgardProvider;
  readonly localPreState?: LocalValidationPreState;
  readonly localPreStateSource?: LocalValidationPreStateSource;
  readonly nowCardanoSlotNo?: bigint | number;
  readonly validationConcurrency?: number;
  readonly enforceScriptBudget?: boolean;
};

export type FromTxInput =
  | CompleteTx
  | MidgardNativeTxFull
  | Uint8Array
  | string
  | { readonly txCbor: Uint8Array | string }
  | { readonly txHex: string };

export type FromTxOptions = {
  readonly resolvedSpendInputs?: readonly MidgardUtxo[];
  readonly allowUnexpectedResolvedInputs?: boolean;
  readonly allowUnknownExpectedWitnesses?: boolean;
  readonly partial?: boolean;
};

export type VKeyWitnessInput = VKeyWitness | Uint8Array | string;

export type MidgardPartialWitnessBundle = {
  readonly kind: "MidgardPartialWitnessBundle";
  readonly version: 1;
  readonly midgardNativeTxVersion: number;
  readonly txId: string;
  readonly bodyHash: string;
  readonly witnesses: readonly string[];
  readonly signerKeyHashes: readonly string[];
};

export type PartialWitnessBundleInput =
  | MidgardPartialWitnessBundle
  | Uint8Array
  | string
  | { readonly cbor: Uint8Array | string }
  | { readonly cborHex: string };

export type AssemblePartialWitnessOptions = {
  readonly allowPartial?: boolean;
};

export type PartialSignApi = {
  readonly withWallet: (wallet?: MidgardWallet) => TxPartialSignBuilder;
  readonly withWalletSafe: (
    wallet?: MidgardWallet,
  ) => Promise<MidgardResult<MidgardPartialWitnessBundle, LucidMidgardError>>;
  readonly withWalletProgram: (
    wallet?: MidgardWallet,
  ) => MidgardEffect<MidgardPartialWitnessBundle>;
  readonly withWalletEffect: (
    wallet?: MidgardWallet,
  ) => MidgardEffect<MidgardPartialWitnessBundle>;
  readonly withPrivateKey: (
    privateKey: PrivateKey | string,
  ) => TxPartialSignBuilder;
  readonly withPrivateKeySafe: (
    privateKey: PrivateKey | string,
  ) => Promise<MidgardResult<MidgardPartialWitnessBundle, LucidMidgardError>>;
  readonly withPrivateKeyProgram: (
    privateKey: PrivateKey | string,
  ) => MidgardEffect<MidgardPartialWitnessBundle>;
  readonly withPrivateKeyEffect: (
    privateKey: PrivateKey | string,
  ) => MidgardEffect<MidgardPartialWitnessBundle>;
  readonly withExternalSigner: (
    signer: ExternalBodyHashSigner,
  ) => TxPartialSignBuilder;
  readonly withExternalSignerSafe: (
    signer: ExternalBodyHashSigner,
  ) => Promise<MidgardResult<MidgardPartialWitnessBundle, LucidMidgardError>>;
  readonly withExternalSignerProgram: (
    signer: ExternalBodyHashSigner,
  ) => MidgardEffect<MidgardPartialWitnessBundle>;
  readonly withExternalSignerEffect: (
    signer: ExternalBodyHashSigner,
  ) => MidgardEffect<MidgardPartialWitnessBundle>;
  readonly withWitness: (witness: VKeyWitnessInput) => TxPartialSignBuilder;
  readonly withWitnesses: (
    witnesses: readonly VKeyWitnessInput[],
  ) => TxPartialSignBuilder;
};

export type ChainOptions = CompleteOptions;

export type ChainResult = readonly [
  newWalletUtxos: readonly MidgardUtxo[],
  derivedOutputs: readonly MidgardUtxo[],
  tx: CompleteTx,
];

export type MidgardEffect<T> = Effect.Effect<T, LucidMidgardError>;

const midgardSafe = async <T>(
  operation: () => Promise<T>,
): Promise<MidgardResult<T, LucidMidgardError>> => {
  try {
    return { ok: true, value: await operation() };
  } catch (error) {
    if (error instanceof LucidMidgardError) {
      return { ok: false, error };
    }
    throw error;
  }
};

const midgardProgram = <T>(operation: () => Promise<T>): MidgardEffect<T> =>
  Effect.tryPromise({
    try: () => operation(),
    catch: (error) => {
      if (error instanceof LucidMidgardError) {
        return error;
      }
      throw error;
    },
  });

export type UtxosByOutRefOptions = {
  readonly missing?: "error" | "omit";
};

export type DatumOfOptions =
  | {
      readonly expectedHash?: string;
    }
  | string;

const DEFAULT_AWAIT_TX_STATUSES: readonly TxStatusKind[] = [
  "accepted",
  "committed",
  "rejected",
];
const TX_STATUS_KINDS: ReadonlySet<string> = new Set([
  "queued",
  "validating",
  "accepted",
  "committed",
  "pending_commit",
  "awaiting_local_recovery",
  "not_found",
  "rejected",
]);
const DEFAULT_POLL_INTERVAL_MS = 1_000;
const DEFAULT_AWAIT_TIMEOUT_MS = 60_000;
const txBuilderConstructorToken = Symbol("TxBuilder.constructor");
const submittedTxConstructorToken = Symbol("SubmittedTx.constructor");
const partiallySignedTxConstructorToken = Symbol(
  "PartiallySignedTx.constructor",
);
const PARTIAL_WITNESS_BUNDLE_KIND = "MidgardPartialWitnessBundle";
const PARTIAL_WITNESS_BUNDLE_VERSION = 1;

const compareCanonicalStrings = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

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

const configNetworkId = (config: LucidMidgardConfigSnapshot): bigint => {
  if (config.networkId === undefined) {
    throw new BuilderInvariantError("Midgard network id is not configured");
  }
  return BigInt(config.networkId);
};

const stateNetworkId = (state: BuilderState): bigint => {
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

const cloneProtocolInfo = (info: MidgardProtocolInfo): MidgardProtocolInfo => ({
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

const validateProviderDiagnostics = (
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

const cloneProviderDiagnostics = (
  diagnostics: ProviderDiagnostics,
): ProviderDiagnostics => validateProviderDiagnostics(diagnostics);

const cloneLocalValidationReport = (
  report: LocalValidationReport | undefined,
): LocalValidationReport | undefined =>
  report === undefined
    ? undefined
    : {
        ...report,
        acceptedTxIds: [...report.acceptedTxIds],
        rejected: report.rejected.map((entry) => ({ ...entry })),
        statePatch:
          report.statePatch === undefined
            ? undefined
            : {
                deletedOutRefs: [...report.statePatch.deletedOutRefs],
                upsertedOutRefs: report.statePatch.upsertedOutRefs.map(
                  ([outRef, output]) => [outRef, output] as const,
                ),
              },
      };

const cloneCompleteTxMetadata = (
  metadata: CompleteTxMetadata,
): CompleteTxMetadata => ({
  ...metadata,
  changeAssets:
    metadata.changeAssets === undefined
      ? undefined
      : { ...metadata.changeAssets },
  expectedAddrWitnessKeyHashes:
    metadata.expectedAddrWitnessKeyHashes === undefined
      ? undefined
      : [...metadata.expectedAddrWitnessKeyHashes],
  signedBy:
    metadata.signedBy === undefined ? undefined : [...metadata.signedBy],
  localValidation: cloneLocalValidationReport(metadata.localValidation),
  providerDiagnostics:
    metadata.providerDiagnostics === undefined
      ? undefined
      : cloneProviderDiagnostics(metadata.providerDiagnostics),
});

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

const validateProtocolInfo = (
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

const buildConfigSnapshot = ({
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

const readProviderSnapshot = async ({
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

  const diagnostics = validateProviderDiagnostics(
    midgardProvider.diagnostics(),
  );
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

const normalizeTxIdHex = (txId: string): string => {
  const normalized = txId.trim().toLowerCase();
  if (!/^[0-9a-f]{64}$/.test(normalized)) {
    throw new BuilderInvariantError("Invalid transaction id", txId);
  }
  return normalized;
};

const normalizeProviderTxIdHex = (txId: string, endpoint: string): string => {
  try {
    return normalizeTxIdHex(txId);
  } catch {
    throw new ProviderPayloadError(
      endpoint,
      "Provider returned invalid tx id",
      txId,
    );
  }
};

const assertSubmitSizeWithinLimit = (
  txBytes: Uint8Array,
  maxSubmitTxCborBytes: number | undefined,
): void => {
  if (maxSubmitTxCborBytes === undefined) {
    return;
  }
  if (
    !Number.isSafeInteger(maxSubmitTxCborBytes) ||
    maxSubmitTxCborBytes <= 0
  ) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "maxSubmitTxCborBytes must be a positive safe integer",
    );
  }
  if (txBytes.length > maxSubmitTxCborBytes) {
    throw new ProviderPayloadError(
      "/submit",
      "Midgard native transaction exceeds provider submit size limit",
      `size=${txBytes.length.toString()} max=${maxSubmitTxCborBytes.toString()}`,
    );
  }
};

const resolveSubmitSizeLimit = async ({
  provider,
  providerParams,
  context,
}: {
  readonly provider: MidgardProvider;
  readonly providerParams: MidgardProtocolParameters;
  readonly context: CompleteTxContext | undefined;
}): Promise<number | undefined> => {
  if (providerParams.maxSubmitTxCborBytes !== undefined) {
    return providerParams.maxSubmitTxCborBytes;
  }
  if (
    context?.provider === provider &&
    context.maxSubmitTxCborBytes !== undefined
  ) {
    return context.maxSubmitTxCborBytes;
  }
  return validateProtocolInfo(await provider.getProtocolInfo()).submissionLimits
    .maxSubmitTxCborBytes;
};

const outRefToCbor = (outRef: OutRef): Buffer => {
  const normalized = normalizeOutRef(outRef);
  return Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(normalized.txHash),
      BigInt(normalized.outputIndex),
    ).to_cbor_bytes(),
  );
};

const outRefFromCbor = (inputCbor: Uint8Array, fieldName: string): OutRef => {
  try {
    const input = CML.TransactionInput.from_cbor_bytes(inputCbor);
    return normalizeOutRef({
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    });
  } catch (cause) {
    throw new BuilderInvariantError(
      `Invalid ${fieldName} input CBOR`,
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

const assertTxStatusMatches = (
  requestedTxId: string,
  status: TxStatus,
): TxStatus => {
  if (typeof status !== "object" || status === null) {
    throw new ProviderPayloadError(
      "/tx-status",
      "Provider returned malformed transaction status",
      status,
    );
  }
  const rawStatus = status as { readonly kind?: unknown };
  if (
    typeof rawStatus.kind !== "string" ||
    !TX_STATUS_KINDS.has(rawStatus.kind)
  ) {
    throw new ProviderPayloadError(
      "/tx-status",
      "Provider returned unsupported transaction status",
      String(rawStatus.kind),
    );
  }
  if (
    rawStatus.kind === "rejected" &&
    (typeof (status as { readonly code?: unknown }).code !== "string" ||
      ((status as { readonly detail?: unknown }).detail !== null &&
        typeof (status as { readonly detail?: unknown }).detail !== "string"))
  ) {
    throw new ProviderPayloadError(
      "/tx-status",
      "Provider returned malformed rejection status",
    );
  }
  const normalizedRequested = normalizeTxIdHex(requestedTxId);
  const normalizedActual = normalizeProviderTxIdHex(status.txId, "/tx-status");
  if (normalizedActual !== normalizedRequested) {
    throw new ProviderPayloadError(
      "/tx-status",
      "Provider returned status for a different tx id",
      `expected=${normalizedRequested} actual=${status.txId}`,
    );
  }
  return status.txId === normalizedActual
    ? status
    : ({ ...status, txId: normalizedActual } as TxStatus);
};

const assertSubmitAdmissionMatches = (
  requestedTxId: string,
  admission: SubmitTxResult,
): SubmitTxResult => {
  if (typeof admission !== "object" || admission === null) {
    throw new ProviderPayloadError(
      "/submit",
      "Provider returned malformed submit admission",
      admission,
    );
  }
  const raw = admission as {
    readonly txId?: unknown;
    readonly status?: unknown;
    readonly httpStatus?: unknown;
    readonly firstSeenAt?: unknown;
    readonly lastSeenAt?: unknown;
    readonly duplicate?: unknown;
  };
  if (typeof raw.txId !== "string") {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response tx id must be a string",
    );
  }
  const normalizedRequested = normalizeTxIdHex(requestedTxId);
  const normalizedActual = normalizeProviderTxIdHex(raw.txId, "/submit");
  if (normalizedActual !== normalizedRequested) {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response returned a different tx id than the submitted tx",
      `expected=${normalizedRequested} actual=${raw.txId}`,
    );
  }
  if (typeof raw.status !== "string" || !isSubmitAdmissionStatus(raw.status)) {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response status is not a supported durable admission status",
      String(raw.status),
    );
  }
  if (raw.httpStatus !== 200 && raw.httpStatus !== 202) {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response httpStatus must be 200 or 202",
      String(raw.httpStatus),
    );
  }
  if (raw.firstSeenAt !== undefined && typeof raw.firstSeenAt !== "string") {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response firstSeenAt must be a string",
    );
  }
  if (raw.lastSeenAt !== undefined && typeof raw.lastSeenAt !== "string") {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response lastSeenAt must be a string",
    );
  }
  if (typeof raw.duplicate !== "boolean") {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response duplicate flag must be boolean",
    );
  }
  if (raw.httpStatus === 202 && raw.duplicate) {
    throw new ProviderPayloadError(
      "/submit",
      "HTTP 202 submit admission must not be marked duplicate",
    );
  }
  if (raw.httpStatus === 200 && !raw.duplicate) {
    throw new ProviderPayloadError(
      "/submit",
      "HTTP 200 submit admission must be marked duplicate",
    );
  }
  if (raw.httpStatus === 202 && raw.status !== "queued") {
    throw new ProviderPayloadError(
      "/submit",
      "HTTP 202 submit admission must start queued",
    );
  }
  return {
    ...admission,
    txId: normalizedActual,
    status: raw.status,
    httpStatus: raw.httpStatus,
    firstSeenAt: raw.firstSeenAt,
    lastSeenAt: raw.lastSeenAt,
    duplicate: raw.duplicate,
  };
};

const normalizePositiveSafeInteger = (
  value: number | undefined,
  fieldName: string,
  defaultValue: number,
): number => {
  if (value === undefined) {
    return defaultValue;
  }
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new BuilderInvariantError(
      `${fieldName} must be a positive safe integer`,
      `${fieldName}=${String(value)}`,
    );
  }
  return value;
};

const normalizeAwaitTargets = (
  until: AwaitTxOptions["until"],
): ReadonlySet<TxStatusKind> => {
  const targets = Array.isArray(until)
    ? until
    : until === undefined
      ? DEFAULT_AWAIT_TX_STATUSES
      : [until];
  if (targets.length === 0) {
    throw new BuilderInvariantError(
      "await status target set must not be empty",
    );
  }
  return new Set(targets);
};

const resolveProvider = (
  provider: MidgardProvider | undefined,
  context: CompleteTxContext | undefined,
): MidgardProvider => {
  const resolved = provider ?? context?.provider;
  if (resolved === undefined) {
    throw new BuilderInvariantError("No Midgard provider available");
  }
  return resolved;
};

const throwIfPollingAborted = (signal: AbortSignal | undefined): void => {
  if (signal?.aborted) {
    throw new ProviderError({
      endpoint: "/tx-status",
      message: "Transaction status polling aborted",
      detail:
        signal.reason === undefined
          ? "aborted"
          : signal.reason instanceof Error
            ? signal.reason.message
            : String(signal.reason),
      retryable: false,
    });
  }
};

const waitForPollInterval = (
  milliseconds: number,
  signal: AbortSignal | undefined,
): Promise<void> =>
  new Promise((resolve, reject) => {
    throwIfPollingAborted(signal);
    let timeout: ReturnType<typeof setTimeout> | undefined;
    const onAbort = () => {
      if (timeout !== undefined) {
        clearTimeout(timeout);
      }
      reject(
        new ProviderError({
          endpoint: "/tx-status",
          message: "Transaction status polling aborted",
          detail:
            signal?.reason === undefined
              ? "aborted"
              : signal.reason instanceof Error
                ? signal.reason.message
                : String(signal.reason),
          retryable: false,
        }),
      );
    };
    timeout = setTimeout(() => {
      signal?.removeEventListener("abort", onAbort);
      resolve();
    }, milliseconds);
    signal?.addEventListener("abort", onAbort, { once: true });
  });

const pollTxStatus = async (
  provider: MidgardProvider,
  txId: string,
  options: AwaitTxOptions = {},
): Promise<TxStatus> => {
  const normalizedTxId = normalizeTxIdHex(txId);
  const targets = normalizeAwaitTargets(options.until);
  const pollIntervalMs = normalizePositiveSafeInteger(
    options.pollIntervalMs,
    "pollIntervalMs",
    DEFAULT_POLL_INTERVAL_MS,
  );
  const timeoutMs = normalizePositiveSafeInteger(
    options.timeoutMs,
    "timeoutMs",
    DEFAULT_AWAIT_TIMEOUT_MS,
  );
  const deadline = Date.now() + timeoutMs;
  let lastStatus: TxStatus | undefined;

  for (;;) {
    throwIfPollingAborted(options.signal);
    lastStatus = assertTxStatusMatches(
      normalizedTxId,
      await provider.getTxStatus(normalizedTxId),
    );
    if (targets.has(lastStatus.kind)) {
      return lastStatus;
    }
    if (Date.now() >= deadline) {
      throw new ProviderError({
        endpoint: "/tx-status",
        message: "Timed out waiting for transaction status",
        detail: `tx_id=${normalizedTxId} last_status=${lastStatus.kind}`,
        retryable: true,
      });
    }
    await waitForPollInterval(
      Math.min(pollIntervalMs, Math.max(1, deadline - Date.now())),
      options.signal,
    );
  }
};

const decodeAddrWitnesses = (
  preimageCbor: Uint8Array,
): readonly VKeyWitness[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, "native.addr_tx_wits").map(
    (witnessBytes, index) =>
      decodeCanonicalVKeyWitness(
        witnessBytes,
        `native.addr_tx_wits[${index.toString()}]`,
      ),
  );

const decodeCanonicalVKeyWitness = (
  witnessBytes: Uint8Array,
  fieldName: string,
): VKeyWitness => {
  try {
    const bytes = Buffer.from(witnessBytes);
    const witness = CML.Vkeywitness.from_cbor_bytes(bytes);
    const canonical = Buffer.from(witness.to_cbor_bytes());
    if (!canonical.equals(bytes)) {
      throw new SigningError(
        `${fieldName} must be canonical vkey witness CBOR`,
      );
    }
    return witness;
  } catch (cause) {
    if (cause instanceof SigningError) {
      throw cause;
    }
    throw new SigningError(
      `Invalid ${fieldName}`,
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

const addrWitnessMetadata = (
  witnesses: readonly VKeyWitness[],
): Pick<CompleteTxMetadata, "addrWitnessCount" | "signedBy"> => ({
  addrWitnessCount: witnesses.length,
  signedBy: witnesses.map((witness) => witness.vkey().hash().to_hex()),
});

const addrWitnessKeyHashes = (
  witnesses: readonly VKeyWitness[],
): readonly string[] =>
  witnesses.map((witness) => witness.vkey().hash().to_hex());

const nonEmptyBytesFromHex = (hex: string, fieldName: string): Buffer => {
  const normalized = hex.trim().toLowerCase();
  if (
    normalized.length === 0 ||
    normalized.length % 2 !== 0 ||
    !/^[0-9a-f]+$/.test(normalized)
  ) {
    throw new BuilderInvariantError(`${fieldName} must be hex`, hex);
  }
  return Buffer.from(normalized, "hex");
};

const strictBytesFromHex = (
  hex: string,
  fieldName: string,
  expectedBytes?: 28 | 32,
): Buffer => {
  const bytes = nonEmptyBytesFromHex(hex, fieldName);
  if (expectedBytes !== undefined && bytes.length !== expectedBytes) {
    throw new BuilderInvariantError(
      `${fieldName} must be a ${expectedBytes.toString()}-byte hex string`,
      hex,
    );
  }
  return bytes;
};

const vkeyWitnessInputBytes = (
  witness: VKeyWitnessInput,
  fieldName: string,
): Buffer => {
  if (typeof witness === "string") {
    return nonEmptyBytesFromHex(witness, fieldName);
  }
  if (witness instanceof Uint8Array) {
    return Buffer.from(witness);
  }
  return Buffer.from(witness.to_cbor_bytes());
};

const normalizeVKeyWitnessInput = (
  witness: VKeyWitnessInput,
  bodyHash: Uint8Array,
  fieldName: string,
): VKeyWitness => {
  const decoded = decodeCanonicalVKeyWitness(
    vkeyWitnessInputBytes(witness, fieldName),
    fieldName,
  );
  return assertVKeyWitness(bodyHash, decoded);
};

const canonicalizeAddrWitnesses = (
  bodyHash: Uint8Array,
  witnesses: readonly VKeyWitness[],
): readonly VKeyWitness[] => {
  const byKeyHash = new Map<string, VKeyWitness>();
  for (const witness of witnesses) {
    const verified = assertVKeyWitness(bodyHash, witness);
    const keyHash = verified.vkey().hash().to_hex();
    const existing = byKeyHash.get(keyHash);
    if (existing !== undefined) {
      const existingBytes = Buffer.from(existing.to_cbor_bytes());
      const nextBytes = Buffer.from(verified.to_cbor_bytes());
      if (!existingBytes.equals(nextBytes)) {
        throw new SigningError(
          "Conflicting vkey witnesses for the same key hash",
          keyHash,
        );
      }
      continue;
    }
    byKeyHash.set(keyHash, verified);
  }
  return [...byKeyHash.entries()]
    .sort(([left], [right]) => compareCanonicalStrings(left, right))
    .map(([, witness]) => witness);
};

const applyAddrWitnessesToTx = (
  tx: MidgardNativeTxFull,
  witnesses: readonly VKeyWitness[],
): {
  readonly tx: MidgardNativeTxFull;
  readonly witnesses: readonly VKeyWitness[];
} => {
  const bodyHash = tx.compact.transactionBodyHash;
  const merged = canonicalizeAddrWitnesses(bodyHash, [
    ...decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor),
    ...witnesses,
  ]);
  const nextAddrTxWitsPreimageCbor = encodeAddrWitnesses(merged);
  const witnessSet = {
    ...tx.witnessSet,
    addrTxWitsRoot: computeHash32(nextAddrTxWitsPreimageCbor),
    addrTxWitsPreimageCbor: nextAddrTxWitsPreimageCbor,
  };
  const signedTx: MidgardNativeTxFull = {
    ...tx,
    witnessSet,
    compact: deriveMidgardNativeTxCompact(
      tx.body,
      witnessSet,
      tx.compact.validity,
      tx.version,
    ),
  };
  verifyMidgardNativeTxFullConsistency(signedTx);
  return { tx: signedTx, witnesses: merged };
};

const decodeImportAddrWitnesses = (
  tx: MidgardNativeTxFull,
): readonly VKeyWitness[] => {
  let witnesses: readonly VKeyWitness[];
  try {
    witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor);
  } catch (cause) {
    throw new SigningError(
      "Invalid address witness preimage",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const byKeyHash = new Map<string, Buffer>();
  for (const witness of witnesses) {
    assertVKeyWitness(tx.compact.transactionBodyHash, witness);
    const keyHash = witness.vkey().hash().to_hex();
    const bytes = Buffer.from(witness.to_cbor_bytes());
    const existing = byKeyHash.get(keyHash);
    if (existing !== undefined) {
      throw new SigningError(
        existing.equals(bytes)
          ? "Duplicate address witness"
          : "Conflicting address witness",
        keyHash,
      );
    }
    byKeyHash.set(keyHash, bytes);
  }
  return witnesses;
};

const nativeInputOutRefs = (tx: MidgardNativeTxFull): readonly OutRef[] =>
  decodeMidgardNativeByteListPreimage(
    tx.body.spendInputsPreimageCbor,
    "native.spend_inputs",
  ).map((inputCbor, index) =>
    outRefFromCbor(inputCbor, `native.spend_inputs[${index.toString()}]`),
  );

const nativeOutputBytes = (tx: MidgardNativeTxFull): readonly Buffer[] =>
  decodeMidgardNativeByteListPreimage(
    tx.body.outputsPreimageCbor,
    "native.outputs",
  );

const requiredSignerKeyHashesFromTx = (
  tx: MidgardNativeTxFull,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    tx.body.requiredSignersPreimageCbor,
    "native.required_signers",
  ).map((bytes, index) =>
    normalizeHashHex(
      bytes.toString("hex"),
      `native.required_signers[${index.toString()}]`,
      28,
    ),
  );

const protectedOutputKeyHashesFromTx = (
  tx: MidgardNativeTxFull,
): readonly string[] =>
  nativeOutputBytes(tx).flatMap((outputCbor, index) => {
    let decoded: ReturnType<typeof decodeMidgardTxOutput>;
    try {
      decoded = decodeMidgardTxOutput(outputCbor);
    } catch (cause) {
      throw new BuilderInvariantError(
        `Invalid native output at index ${index.toString()}`,
        cause instanceof Error ? cause.message : String(cause),
      );
    }
    if (!outputAddressProtected(decoded.address)) {
      return [];
    }
    const keyHash = outputAddressPaymentKeyHash(decoded.address);
    return keyHash === undefined ? [] : [keyHash];
  });

const normalizeResolvedSpendInputs = (
  tx: MidgardNativeTxFull,
  options: FromTxOptions,
): {
  readonly inputs: readonly MidgardUtxo[];
  readonly complete: boolean;
} => {
  const spendRefs = nativeInputOutRefs(tx);
  if (spendRefs.length === 0) {
    return { inputs: [], complete: true };
  }
  if (options.resolvedSpendInputs === undefined) {
    return { inputs: [], complete: false };
  }

  const required = new Set(spendRefs.map(outRefLabel));
  const byLabel = new Map<string, MidgardUtxo>();
  for (const input of options.resolvedSpendInputs) {
    const normalized = normalizeUtxo(input);
    const label = outRefLabel(normalized);
    if (byLabel.has(label)) {
      throw new BuilderInvariantError("Duplicate resolved spend input", label);
    }
    if (!required.has(label) && !options.allowUnexpectedResolvedInputs) {
      throw new BuilderInvariantError("Unexpected resolved spend input", label);
    }
    byLabel.set(label, normalized);
  }

  const ordered: MidgardUtxo[] = [];
  for (const ref of spendRefs) {
    const label = outRefLabel(ref);
    const input = byLabel.get(label);
    if (input === undefined) {
      throw new BuilderInvariantError("Missing resolved spend input", label);
    }
    ordered.push(input);
  }
  return { inputs: ordered, complete: true };
};

const importedExpectedWitnesses = (
  tx: MidgardNativeTxFull,
  options: FromTxOptions,
): {
  readonly keyHashes: readonly string[];
  readonly complete: boolean;
  readonly resolvedSpendInputs: readonly MidgardUtxo[];
} => {
  const keyHashes = new Set<string>(requiredSignerKeyHashesFromTx(tx));
  for (const keyHash of protectedOutputKeyHashesFromTx(tx)) {
    keyHashes.add(keyHash);
  }
  const resolved = normalizeResolvedSpendInputs(tx, options);
  for (const input of resolved.inputs) {
    const keyHash = paymentPubKeyHashFromUtxo(input);
    if (keyHash !== undefined) {
      keyHashes.add(keyHash);
    }
  }
  return {
    keyHashes: [...keyHashes].sort(),
    complete: resolved.complete,
    resolvedSpendInputs: resolved.inputs,
  };
};

const assertExpectedAddrWitnesses = ({
  actual,
  expected,
  expectedComplete = true,
  requireComplete,
}: {
  readonly actual: readonly string[];
  readonly expected: readonly string[] | undefined;
  readonly expectedComplete?: boolean;
  readonly requireComplete: boolean;
}): void => {
  if (expected === undefined || !expectedComplete) {
    if (requireComplete) {
      throw new SigningError(
        "Cannot prove expected address witness set",
        "supply resolved spend input pre-state before submitting",
      );
    }
    return;
  }
  const expectedSet = new Set(expected);
  const unexpected = actual.filter((keyHash) => !expectedSet.has(keyHash));
  if (unexpected.length > 0) {
    throw new SigningError(
      "Unexpected address witness",
      unexpected.sort().join(","),
    );
  }
  if (!requireComplete) {
    return;
  }
  const actualSet = new Set(actual);
  const missing = expected.filter((keyHash) => !actualSet.has(keyHash));
  if (missing.length > 0) {
    throw new SigningError(
      "Missing expected address witnesses",
      missing.join(","),
    );
  }
};

const encodeAddrWitnesses = (witnesses: readonly VKeyWitness[]): Buffer => {
  const byKeyHash = new Map<string, VKeyWitness>();
  for (const witness of witnesses) {
    const keyHash = witness.vkey().hash().to_hex();
    const existing = byKeyHash.get(keyHash);
    if (existing !== undefined) {
      const existingBytes = Buffer.from(existing.to_cbor_bytes());
      const nextBytes = Buffer.from(witness.to_cbor_bytes());
      if (!existingBytes.equals(nextBytes)) {
        throw new SigningError(
          "Conflicting vkey witnesses for the same key hash",
          keyHash,
        );
      }
      continue;
    }
    byKeyHash.set(keyHash, witness);
  }
  return encodeCbor(
    [...byKeyHash.entries()]
      .sort(([left], [right]) => compareCanonicalStrings(left, right))
      .map(([, witness]) => Buffer.from(witness.to_cbor_bytes())),
  );
};

const signMidgardNativeTx = async (
  tx: MidgardNativeTxFull,
  wallet: MidgardWallet,
): Promise<MidgardNativeTxFull> => {
  const bodyHash = tx.compact.transactionBodyHash;
  const witness = assertVKeyWitness(
    bodyHash,
    await wallet.signBodyHash(bodyHash),
  );
  return applyAddrWitnessesToTx(tx, [witness]).tx;
};

const normalizePartialWitnessBundle = (
  bundle: MidgardPartialWitnessBundle,
): MidgardPartialWitnessBundle => {
  if (typeof bundle !== "object" || bundle === null) {
    throw new SigningError("Partial witness bundle must be an object");
  }
  if (bundle.kind !== PARTIAL_WITNESS_BUNDLE_KIND) {
    throw new SigningError("Unsupported partial witness bundle kind");
  }
  if (bundle.version !== PARTIAL_WITNESS_BUNDLE_VERSION) {
    throw new SigningError("Unsupported partial witness bundle version");
  }
  if (
    !Number.isSafeInteger(bundle.midgardNativeTxVersion) ||
    bundle.midgardNativeTxVersion <= 0
  ) {
    throw new SigningError("Invalid partial witness bundle tx version");
  }
  const txId = partialBundleHexString(bundle.txId, "partial bundle txId", 32);
  const bodyHash = partialBundleHexString(
    bundle.bodyHash,
    "partial bundle bodyHash",
    32,
  );
  if (bodyHash !== txId) {
    throw new SigningError("Partial witness bundle tx id/body hash mismatch");
  }
  if (!Array.isArray(bundle.witnesses)) {
    throw new SigningError("Partial witness bundle witnesses must be an array");
  }
  if (!Array.isArray(bundle.signerKeyHashes)) {
    throw new SigningError(
      "Partial witness bundle signerKeyHashes must be an array",
    );
  }
  if (bundle.witnesses.length === 0) {
    throw new SigningError("Partial witness bundle must contain witnesses");
  }
  const witnesses = canonicalizeAddrWitnesses(
    strictBytesFromHex(bodyHash, "partial bundle bodyHash", 32),
    bundle.witnesses.map((witnessHex, index) =>
      decodeCanonicalVKeyWitness(
        partialBundleHexBytes(
          witnessHex,
          `partial bundle witnesses[${index.toString()}]`,
        ),
        `partial bundle witnesses[${index.toString()}]`,
      ),
    ),
  );
  const signerKeyHashes = witnesses.map((witness) =>
    witness.vkey().hash().to_hex(),
  );
  const declaredSignerKeyHashes = bundle.signerKeyHashes.map((keyHash, index) =>
    partialBundleHexString(
      keyHash,
      `partial bundle signerKeyHashes[${index.toString()}]`,
      28,
    ),
  );
  if (
    declaredSignerKeyHashes.length !== signerKeyHashes.length ||
    declaredSignerKeyHashes.some(
      (keyHash, index) => keyHash !== signerKeyHashes[index],
    )
  ) {
    throw new SigningError(
      "Partial witness bundle signer metadata does not match witnesses",
    );
  }
  return {
    kind: PARTIAL_WITNESS_BUNDLE_KIND,
    version: PARTIAL_WITNESS_BUNDLE_VERSION,
    midgardNativeTxVersion: bundle.midgardNativeTxVersion,
    txId,
    bodyHash,
    witnesses: witnesses.map((witness) =>
      Buffer.from(witness.to_cbor_bytes()).toString("hex"),
    ),
    signerKeyHashes,
  };
};

const partialWitnessBundleFromWitnesses = (
  tx: MidgardNativeTxFull,
  witnesses: readonly VKeyWitness[],
): MidgardPartialWitnessBundle => {
  const canonical = canonicalizeAddrWitnesses(
    tx.compact.transactionBodyHash,
    witnesses,
  );
  if (canonical.length === 0) {
    throw new SigningError("Partial witness bundle must contain witnesses");
  }
  const txId = computeMidgardNativeTxIdFromFull(tx).toString("hex");
  return normalizePartialWitnessBundle({
    kind: PARTIAL_WITNESS_BUNDLE_KIND,
    version: PARTIAL_WITNESS_BUNDLE_VERSION,
    midgardNativeTxVersion: Number(tx.version),
    txId,
    bodyHash: Buffer.from(tx.compact.transactionBodyHash).toString("hex"),
    witnesses: canonical.map((witness) =>
      Buffer.from(witness.to_cbor_bytes()).toString("hex"),
    ),
    signerKeyHashes: canonical.map((witness) => witness.vkey().hash().to_hex()),
  });
};

export const encodePartialWitnessBundle = (
  bundle: MidgardPartialWitnessBundle,
): Buffer => {
  const normalized = normalizePartialWitnessBundle(bundle);
  return encodeCbor([
    normalized.kind,
    normalized.version,
    normalized.midgardNativeTxVersion,
    strictBytesFromHex(normalized.txId, "partial bundle txId", 32),
    strictBytesFromHex(normalized.bodyHash, "partial bundle bodyHash", 32),
    normalized.witnesses.map((witness) =>
      nonEmptyBytesFromHex(witness, "partial bundle witness"),
    ),
    normalized.signerKeyHashes.map((keyHash) =>
      strictBytesFromHex(keyHash, "partial bundle signerKeyHash", 28),
    ),
  ]);
};

const assertPartialBundleNumber = (
  value: unknown,
  fieldName: string,
): number => {
  if (!Number.isSafeInteger(value) || Number(value) <= 0) {
    throw new SigningError(`${fieldName} must be a positive safe integer`);
  }
  return Number(value);
};

const partialBundleHexBytes = (
  value: unknown,
  fieldName: string,
  expectedBytes?: 28 | 32,
): Buffer => {
  if (typeof value !== "string") {
    throw new SigningError(`${fieldName} must be hex`);
  }
  const normalized = value.trim().toLowerCase();
  if (
    normalized.length === 0 ||
    normalized.length % 2 !== 0 ||
    !/^[0-9a-f]+$/.test(normalized)
  ) {
    throw new SigningError(`${fieldName} must be hex`);
  }
  const bytes = Buffer.from(normalized, "hex");
  if (expectedBytes !== undefined && bytes.length !== expectedBytes) {
    throw new SigningError(
      `${fieldName} must be a ${expectedBytes.toString()}-byte hex string`,
    );
  }
  return bytes;
};

const partialBundleHexString = (
  value: unknown,
  fieldName: string,
  expectedBytes?: 28 | 32,
): string =>
  partialBundleHexBytes(value, fieldName, expectedBytes).toString("hex");

export const decodePartialWitnessBundle = (
  input: Uint8Array | string,
): MidgardPartialWitnessBundle => {
  const bytes =
    typeof input === "string"
      ? nonEmptyBytesFromHex(input, "partial witness bundle CBOR")
      : Buffer.from(input);
  const decoded = asArray(decodeSingleCbor(bytes), "partial_witness_bundle");
  if (decoded.length !== 7) {
    throw new SigningError("Partial witness bundle must be a 7-item tuple");
  }
  if (decoded[0] !== PARTIAL_WITNESS_BUNDLE_KIND) {
    throw new SigningError("Unsupported partial witness bundle kind");
  }
  const version = assertPartialBundleNumber(
    decoded[1],
    "partial witness bundle version",
  );
  const midgardNativeTxVersion = assertPartialBundleNumber(
    decoded[2],
    "partial witness bundle tx version",
  );
  const witnessBytes = asArray(
    decoded[5],
    "partial witness bundle witnesses",
  ).map((item, index) =>
    asBytes(
      item,
      `partial witness bundle witnesses[${index.toString()}]`,
    ).toString("hex"),
  );
  const signerKeyHashes = asArray(
    decoded[6],
    "partial witness bundle signer key hashes",
  ).map((item, index) =>
    asBytes(
      item,
      `partial witness bundle signerKeyHashes[${index.toString()}]`,
    ).toString("hex"),
  );
  const normalized = normalizePartialWitnessBundle({
    kind: PARTIAL_WITNESS_BUNDLE_KIND,
    version: version as 1,
    midgardNativeTxVersion,
    txId: asBytes(decoded[3], "partial witness bundle tx id").toString("hex"),
    bodyHash: asBytes(decoded[4], "partial witness bundle body hash").toString(
      "hex",
    ),
    witnesses: witnessBytes,
    signerKeyHashes,
  });
  if (!encodePartialWitnessBundle(normalized).equals(bytes)) {
    throw new SigningError("Partial witness bundle CBOR is not canonical");
  }
  return normalized;
};

export const parsePartialWitnessBundle = (
  input: PartialWitnessBundleInput,
): MidgardPartialWitnessBundle => {
  if (input instanceof Uint8Array || typeof input === "string") {
    return decodePartialWitnessBundle(input);
  }
  if (typeof input !== "object" || input === null) {
    throw new SigningError("Partial witness bundle input must be an object");
  }
  if ("cbor" in input) {
    return decodePartialWitnessBundle(input.cbor);
  }
  if ("cborHex" in input) {
    return decodePartialWitnessBundle(input.cborHex);
  }
  return normalizePartialWitnessBundle(input);
};

const assertPartialBundleMatchesTx = (
  tx: MidgardNativeTxFull,
  bundle: MidgardPartialWitnessBundle,
): void => {
  const txId = computeMidgardNativeTxIdFromFull(tx).toString("hex");
  const bodyHash = Buffer.from(tx.compact.transactionBodyHash).toString("hex");
  if (bundle.txId !== txId || bundle.bodyHash !== bodyHash) {
    throw new SigningError(
      "Partial witness bundle belongs to a different transaction",
      `expected=${txId} actual=${bundle.txId}`,
    );
  }
  if (bundle.midgardNativeTxVersion !== Number(tx.version)) {
    throw new SigningError(
      "Partial witness bundle native transaction version mismatch",
      `expected=${tx.version.toString()} actual=${bundle.midgardNativeTxVersion.toString()}`,
    );
  }
};

const dummyWitnessPrivateKey = (index: number): PrivateKey => {
  const seed = Buffer.alloc(32);
  let value = index + 1;
  for (let offset = seed.length - 1; offset >= 0 && value > 0; offset -= 1) {
    seed[offset] = value & 0xff;
    value = Math.floor(value / 0x100);
  }
  return CML.PrivateKey.from_normal_bytes(seed);
};

const withEstimatedAddrWitnesses = (
  tx: MidgardNativeTxFull,
  expectedWitnessCount: number,
): MidgardNativeTxFull => {
  if (expectedWitnessCount === 0) {
    return tx;
  }
  const witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor);
  if (witnesses.length >= expectedWitnessCount) {
    return tx;
  }
  const estimatedWitnesses = [...witnesses];
  for (let index = witnesses.length; index < expectedWitnessCount; index += 1) {
    estimatedWitnesses.push(
      makeVKeyWitness(
        tx.compact.transactionBodyHash,
        dummyWitnessPrivateKey(index),
      ),
    );
  }
  const addrTxWitsPreimageCbor = encodeAddrWitnesses(estimatedWitnesses);
  const witnessSet = {
    ...tx.witnessSet,
    addrTxWitsRoot: computeHash32(addrTxWitsPreimageCbor),
    addrTxWitsPreimageCbor,
  };
  const estimatedTx: MidgardNativeTxFull = {
    ...tx,
    witnessSet,
    compact: deriveMidgardNativeTxCompact(
      tx.body,
      witnessSet,
      tx.compact.validity,
      tx.version,
    ),
  };
  verifyMidgardNativeTxFullConsistency(estimatedTx);
  return estimatedTx;
};

const estimatedSignedTxByteLength = (
  tx: MidgardNativeTxFull,
  expectedWitnessCount: number,
): number =>
  encodeMidgardNativeTxFull(
    withEstimatedAddrWitnesses(tx, expectedWitnessCount),
  ).length;

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

const canonicalImportedTxFromBytes = (
  bytes: Uint8Array,
): MidgardNativeTxFull => {
  let tx: MidgardNativeTxFull;
  try {
    tx = decodeMidgardNativeTxFull(bytes);
  } catch (cause) {
    throw new BuilderInvariantError(
      "fromTx accepts only Midgard native full transaction bytes",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const canonical = encodeMidgardNativeTxFull(tx);
  if (!canonical.equals(Buffer.from(bytes))) {
    throw new BuilderInvariantError(
      "fromTx requires canonical Midgard native full transaction bytes",
    );
  }
  return tx;
};

const canonicalImportedTxFromObject = (
  tx: MidgardNativeTxFull,
): MidgardNativeTxFull => {
  try {
    verifyMidgardNativeTxFullConsistency(tx);
    return decodeMidgardNativeTxFull(encodeMidgardNativeTxFull(tx));
  } catch (cause) {
    throw new BuilderInvariantError(
      "fromTx received an invalid Midgard native full transaction object",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

const decodeFromTxInput = (
  input: Exclude<FromTxInput, CompleteTx>,
): MidgardNativeTxFull => {
  if (input instanceof Uint8Array) {
    return canonicalImportedTxFromBytes(input);
  }
  if (typeof input === "string") {
    return canonicalImportedTxFromBytes(nonEmptyBytesFromHex(input, "txHex"));
  }
  if (isRecord(input)) {
    if ("txHex" in input && typeof input.txHex === "string") {
      return canonicalImportedTxFromBytes(
        nonEmptyBytesFromHex(input.txHex, "txHex"),
      );
    }
    if ("txCbor" in input) {
      if (input.txCbor instanceof Uint8Array) {
        return canonicalImportedTxFromBytes(input.txCbor);
      }
      if (typeof input.txCbor === "string") {
        return canonicalImportedTxFromBytes(
          nonEmptyBytesFromHex(input.txCbor, "txCbor"),
        );
      }
      throw new BuilderInvariantError("txCbor must be bytes or hex");
    }
    return canonicalImportedTxFromObject(
      input as unknown as MidgardNativeTxFull,
    );
  }
  throw new BuilderInvariantError("Unsupported fromTx input");
};

const nativePreimageCount = (
  preimageCbor: Uint8Array,
  fieldName: string,
): number =>
  decodeMidgardNativeByteListPreimage(preimageCbor, fieldName).length;

const importedTxMetadata = (
  tx: MidgardNativeTxFull,
  options: FromTxOptions,
): CompleteTxMetadata => {
  const witnesses = decodeImportAddrWitnesses(tx);
  const expected = importedExpectedWitnesses(tx, options);
  if (
    witnesses.length > 0 &&
    !expected.complete &&
    !options.allowUnknownExpectedWitnesses &&
    options.partial !== true
  ) {
    throw new SigningError(
      "Cannot import signed transaction with unknown expected address witnesses",
      "supply resolved spend input pre-state",
    );
  }
  assertExpectedAddrWitnesses({
    actual: addrWitnessKeyHashes(witnesses),
    expected: expected.keyHashes,
    expectedComplete: expected.complete,
    requireComplete: witnesses.length > 0 && expected.complete,
  });
  const txBytes = encodeMidgardNativeTxFull(tx);
  return {
    fee: tx.body.fee,
    inputCount: nativeInputOutRefs(tx).length,
    referenceInputCount: nativePreimageCount(
      tx.body.referenceInputsPreimageCbor,
      "native.reference_inputs",
    ),
    outputCount: nativeOutputBytes(tx).length,
    requiredSignerCount: requiredSignerKeyHashesFromTx(tx).length,
    txByteLength: txBytes.length,
    feeIterations: 0,
    balanced: false,
    expectedAddrWitnessCount: expected.complete
      ? expected.keyHashes.length
      : undefined,
    expectedAddrWitnessKeyHashes: expected.keyHashes,
    expectedAddrWitnessesComplete: expected.complete,
    estimatedSignedTxByteLength: expected.complete
      ? estimatedSignedTxByteLength(tx, expected.keyHashes.length)
      : undefined,
    ...addrWitnessMetadata(witnesses),
  };
};

const assertTxNetworkMatchesExpected = (
  tx: MidgardNativeTxFull,
  expectedNetworkId: bigint | undefined,
  context: string,
): void => {
  if (expectedNetworkId === undefined) {
    return;
  }
  if (tx.body.networkId !== expectedNetworkId) {
    throw new BuilderInvariantError(
      `${context} network id mismatch`,
      `expected=${expectedNetworkId.toString()} actual=${tx.body.networkId.toString()}`,
    );
  }
};

const trustedCompleteTxs = new WeakSet<object>();

const localUtxosFromTx = (
  tx: MidgardNativeTxFull,
  _metadata: CompleteTxMetadata,
): readonly MidgardUtxo[] => {
  const txId = computeMidgardNativeTxIdFromFull(tx).toString("hex");
  return nativeOutputBytes(tx).map((outputCbor, index) => {
    const outRef = { txHash: txId, outputIndex: index };
    return decodeMidgardUtxo({
      outRef,
      outRefCbor: outRefToCbor(outRef),
      outputCbor,
    });
  });
};

const localUtxoAt = (
  tx: MidgardNativeTxFull,
  metadata: CompleteTxMetadata,
  outputIndex: number,
): MidgardUtxo => {
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0) {
    throw new BuilderInvariantError(
      "Invalid local output index",
      outputIndex.toString(),
    );
  }
  const outputs = localUtxosFromTx(tx, metadata);
  const output = outputs[outputIndex];
  if (output === undefined) {
    throw new BuilderInvariantError(
      "Local output index is out of range",
      outputIndex.toString(),
    );
  }
  return cloneUtxo(output);
};

const midgardJsonSafe = (value: unknown): unknown => {
  if (typeof value === "bigint") {
    return value.toString(10);
  }
  if (Buffer.isBuffer(value) || value instanceof Uint8Array) {
    return Buffer.from(value).toString("hex");
  }
  if (Array.isArray(value)) {
    return value.map(midgardJsonSafe);
  }
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value).map(([key, child]) => [
        key,
        midgardJsonSafe(child),
      ]),
    );
  }
  return value;
};

export type MidgardTxJson = {
  readonly txId: string;
  readonly txCbor: string;
  readonly metadata: unknown;
};

const completeTxMetadataWithAddrWitnesses = (
  tx: MidgardNativeTxFull,
  metadata: CompleteTxMetadata,
): CompleteTxMetadata => {
  const witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor);
  return {
    ...metadata,
    txByteLength: encodeMidgardNativeTxFull(tx).length,
    ...addrWitnessMetadata(witnesses),
  };
};

const privateKeyFromInput = (privateKey: PrivateKey | string): PrivateKey =>
  typeof privateKey === "string"
    ? CML.PrivateKey.from_bech32(privateKey)
    : privateKey;

export class TxPartialSignBuilder {
  readonly #tx: CompleteTx;
  readonly #witnesses: readonly VKeyWitnessInput[];
  readonly #wallet?: MidgardWallet;
  readonly #privateKey?: PrivateKey | string;
  readonly #externalSigner?: ExternalBodyHashSigner;
  readonly #expectedNetworkId?: number;

  constructor({
    tx,
    witnesses = [],
    wallet,
    privateKey,
    externalSigner,
    expectedNetworkId,
  }: {
    readonly tx: CompleteTx;
    readonly witnesses?: readonly VKeyWitnessInput[];
    readonly wallet?: MidgardWallet;
    readonly privateKey?: PrivateKey | string;
    readonly externalSigner?: ExternalBodyHashSigner;
    readonly expectedNetworkId?: number;
  }) {
    this.#tx = tx;
    this.#witnesses = witnesses;
    this.#wallet = wallet;
    this.#privateKey = privateKey;
    this.#externalSigner = externalSigner;
    this.#expectedNetworkId = expectedNetworkId;
  }

  async partial(): Promise<MidgardPartialWitnessBundle> {
    return partialWitnessBundleFromWitnesses(
      this.#tx.tx,
      await this.collectWitnesses(),
    );
  }

  async complete(): Promise<CompleteTx> {
    const assembled = this.#tx.assemble(await this.partial());
    if (assembled instanceof CompleteTx) {
      return assembled;
    }
    throw new SigningError("Incomplete partial witness assembly");
  }

  completeProgram(): MidgardEffect<CompleteTx> {
    return midgardProgram(() => this.complete());
  }

  completeSafe(): Promise<MidgardResult<CompleteTx, LucidMidgardError>> {
    return midgardSafe(() => this.complete());
  }

  partialProgram(): MidgardEffect<MidgardPartialWitnessBundle> {
    return midgardProgram(() => this.partial());
  }

  partialSafe(): Promise<
    MidgardResult<MidgardPartialWitnessBundle, LucidMidgardError>
  > {
    return midgardSafe(() => this.partial());
  }

  private async collectWitnesses(): Promise<readonly VKeyWitness[]> {
    const nativeTx = this.#tx.tx;
    const bodyHash = nativeTx.compact.transactionBodyHash;
    const witnesses: VKeyWitness[] = this.#witnesses.map((witness, index) =>
      normalizeVKeyWitnessInput(
        witness,
        bodyHash,
        `partial witness[${index.toString()}]`,
      ),
    );
    if (this.#wallet !== undefined) {
      witnesses.push(
        assertVKeyWitness(bodyHash, await this.#wallet.signBodyHash(bodyHash)),
      );
    }
    if (this.#privateKey !== undefined) {
      witnesses.push(
        makeVKeyWitness(bodyHash, privateKeyFromInput(this.#privateKey)),
      );
    }
    if (this.#externalSigner !== undefined) {
      const wallet = walletFromExternalSigner(this.#externalSigner, {
        expectedNetworkId: this.#expectedNetworkId,
      });
      witnesses.push(
        assertVKeyWitness(bodyHash, await wallet.signBodyHash(bodyHash)),
      );
    }
    if (witnesses.length === 0) {
      throw new SigningError("No partial witnesses supplied");
    }
    return witnesses;
  }
}

export type CompleteTxSignApi = ((
  wallet?: MidgardWallet,
) => Promise<CompleteTx>) &
  PartialSignApi;

export class CompleteTx {
  readonly #txCbor: Buffer;
  readonly #txId: Buffer;
  readonly #metadata: CompleteTxMetadata;
  readonly #context?: CompleteTxContext;
  readonly txHex: string;
  readonly txIdHex: string;
  readonly sign: CompleteTxSignApi;
  readonly partialSign: PartialSignApi;

  constructor(
    tx: MidgardNativeTxFull,
    metadata: CompleteTxMetadata,
    context?: CompleteTxContext,
  ) {
    verifyMidgardNativeTxFullConsistency(tx);
    this.#txCbor = encodeMidgardNativeTxFull(tx);
    this.txHex = this.#txCbor.toString("hex");
    this.#txId = computeMidgardNativeTxIdFromFull(tx);
    this.txIdHex = this.#txId.toString("hex");
    this.#metadata = cloneCompleteTxMetadata(metadata);
    this.#context = context;
    this.partialSign = this.makePartialSignApi();
    this.sign = Object.assign(
      (wallet?: MidgardWallet): Promise<CompleteTx> =>
        this.signWithWallet(wallet),
      this.partialSign,
    );
  }

  get tx(): MidgardNativeTxFull {
    return decodeMidgardNativeTxFull(this.#txCbor);
  }

  get txCbor(): Buffer {
    return Buffer.from(this.#txCbor);
  }

  get txId(): Buffer {
    return Buffer.from(this.#txId);
  }

  get metadata(): CompleteTxMetadata {
    return cloneCompleteTxMetadata(this.#metadata);
  }

  toCBOR(): string {
    return this.txHex;
  }

  toHash(): string {
    return this.txIdHex;
  }

  toJSON(): MidgardTxJson {
    return {
      txId: this.txIdHex,
      txCbor: this.txHex,
      metadata: midgardJsonSafe(this.metadata),
    };
  }

  producedOutputs(): readonly MidgardUtxo[] {
    assertTrustedCompleteTx(this, "derive produced outputs");
    return localUtxosFromTx(this.tx, this.#metadata).map(cloneUtxo);
  }

  producedOutput(outputIndex: number): MidgardUtxo {
    assertTrustedCompleteTx(this, "derive a produced output");
    return localUtxoAt(this.tx, this.#metadata, outputIndex);
  }

  assemble(
    bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
    options: AssemblePartialWitnessOptions = {},
  ): CompleteTx | PartiallySignedTx {
    assertTrustedCompleteTx(this, "assemble partial witnesses");
    return assemblePartialWitnessBundles(
      this.tx,
      this.#metadata,
      this.#context,
      bundles,
      options,
    );
  }

  toPartialWitnessBundle(): MidgardPartialWitnessBundle {
    assertTrustedCompleteTx(this, "export partial witnesses");
    return partialWitnessBundleFromWitnesses(
      this.tx,
      decodeAddrWitnesses(this.tx.witnessSet.addrTxWitsPreimageCbor),
    );
  }

  toPartialWitnessBundleCbor(): Buffer {
    return encodePartialWitnessBundle(this.toPartialWitnessBundle());
  }

  async validate(
    phase: LocalPreflightPhase,
    options: LocalPreflightOptions = {},
  ): Promise<LocalValidationReport> {
    assertTrustedCompleteTx(this, "run local preflight validation");
    assertTxNetworkMatchesExpected(
      this.tx,
      this.#context?.networkId,
      "Transaction",
    );
    return runSharedLocalPreflight({
      completed: this,
      phase: normalizeLocalPreflightPhase(phase),
      provider: resolveProvider(options.provider, this.#context),
      options,
      missingPreStateMessage: 'validate("phase-b") requires localPreState',
    });
  }

  validateProgram(
    phase: LocalPreflightPhase,
    options: LocalPreflightOptions = {},
  ): MidgardEffect<LocalValidationReport> {
    return midgardProgram(() => this.validate(phase, options));
  }

  validateSafe(
    phase: LocalPreflightPhase,
    options: LocalPreflightOptions = {},
  ): Promise<MidgardResult<LocalValidationReport, LucidMidgardError>> {
    return midgardSafe(() => this.validate(phase, options));
  }

  private expectedNetworkIdNumber(): number | undefined {
    if (this.#context?.networkId === undefined) {
      return undefined;
    }
    return Number(this.#context.networkId);
  }

  private makePartialSignApi(): PartialSignApi {
    const withWallet = (wallet?: MidgardWallet): TxPartialSignBuilder => {
      const signer = wallet ?? this.#context?.wallet?.();
      return new TxPartialSignBuilder({
        tx: this,
        wallet: signer,
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    };
    const withPrivateKey = (
      privateKey: PrivateKey | string,
    ): TxPartialSignBuilder =>
      new TxPartialSignBuilder({
        tx: this,
        privateKey,
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    const withExternalSigner = (
      signer: ExternalBodyHashSigner,
    ): TxPartialSignBuilder =>
      new TxPartialSignBuilder({
        tx: this,
        externalSigner: signer,
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    const withWitness = (witness: VKeyWitnessInput): TxPartialSignBuilder =>
      new TxPartialSignBuilder({
        tx: this,
        witnesses: [witness],
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    const withWitnesses = (
      witnesses: readonly VKeyWitnessInput[],
    ): TxPartialSignBuilder =>
      new TxPartialSignBuilder({
        tx: this,
        witnesses,
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    return {
      withWallet,
      withWalletSafe: (wallet?: MidgardWallet) =>
        midgardSafe(() => withWallet(wallet).partial()),
      withWalletProgram: (wallet?: MidgardWallet) =>
        midgardProgram(() => withWallet(wallet).partial()),
      withWalletEffect: (wallet?: MidgardWallet) =>
        midgardProgram(() => withWallet(wallet).partial()),
      withPrivateKey,
      withPrivateKeySafe: (privateKey: PrivateKey | string) =>
        midgardSafe(() => withPrivateKey(privateKey).partial()),
      withPrivateKeyProgram: (privateKey: PrivateKey | string) =>
        midgardProgram(() => withPrivateKey(privateKey).partial()),
      withPrivateKeyEffect: (privateKey: PrivateKey | string) =>
        midgardProgram(() => withPrivateKey(privateKey).partial()),
      withExternalSigner,
      withExternalSignerSafe: (signer: ExternalBodyHashSigner) =>
        midgardSafe(() => withExternalSigner(signer).partial()),
      withExternalSignerProgram: (signer: ExternalBodyHashSigner) =>
        midgardProgram(() => withExternalSigner(signer).partial()),
      withExternalSignerEffect: (signer: ExternalBodyHashSigner) =>
        midgardProgram(() => withExternalSigner(signer).partial()),
      withWitness,
      withWitnesses,
    };
  }

  private async signWithWallet(wallet?: MidgardWallet): Promise<CompleteTx> {
    assertTrustedCompleteTx(this, "sign");
    assertTxNetworkMatchesExpected(
      this.tx,
      this.#context?.networkId,
      "Transaction",
    );
    const signer = wallet ?? this.#context?.wallet?.();
    if (signer === undefined) {
      throw new SigningError("No Midgard wallet available for signing");
    }
    const signedTx = await signMidgardNativeTx(this.tx, signer);
    const signedWitnesses = decodeAddrWitnesses(
      signedTx.witnessSet.addrTxWitsPreimageCbor,
    );
    assertExpectedAddrWitnesses({
      actual: addrWitnessKeyHashes(signedWitnesses),
      expected: this.#metadata.expectedAddrWitnessKeyHashes,
      expectedComplete: this.#metadata.expectedAddrWitnessesComplete,
      requireComplete: false,
    });
    return makeCompleteTx(
      signedTx,
      {
        ...this.#metadata,
        txByteLength: encodeMidgardNativeTxFull(signedTx).length,
        ...addrWitnessMetadata(signedWitnesses),
      },
      this.#context,
    );
  }

  async submit(options: SubmitOptions = {}): Promise<SubmittedTx> {
    assertTrustedCompleteTx(this, "submit");
    const provider = resolveProvider(options.provider, this.#context);
    assertTxNetworkMatchesExpected(
      this.tx,
      this.#context?.networkId,
      "Transaction",
    );
    const providerParams = await provider.getProtocolParameters();
    assertTxNetworkMatchesExpected(
      this.tx,
      providerParams.networkId,
      "Provider",
    );
    const maxSubmitTxCborBytes = await resolveSubmitSizeLimit({
      provider,
      providerParams,
      context: this.#context,
    });
    assertSubmitSizeWithinLimit(this.txCbor, maxSubmitTxCborBytes);
    const verifiedWitnesses = decodeImportAddrWitnesses(this.tx);
    assertExpectedAddrWitnesses({
      actual: addrWitnessKeyHashes(verifiedWitnesses),
      expected: this.#metadata.expectedAddrWitnessKeyHashes,
      expectedComplete: this.#metadata.expectedAddrWitnessesComplete,
      requireComplete: true,
    });
    const admission = assertSubmitAdmissionMatches(
      this.txIdHex,
      await provider.submitTx(this.txHex),
    );
    return makeSubmittedTx(this, admission, provider);
  }

  submitProgram(options: SubmitOptions = {}): MidgardEffect<SubmittedTx> {
    return midgardProgram(() => this.submit(options));
  }

  submitSafe(
    options: SubmitOptions = {},
  ): Promise<MidgardResult<SubmittedTx, LucidMidgardError>> {
    return midgardSafe(() => this.submit(options));
  }

  async status(options: SubmitOptions = {}): Promise<TxStatus> {
    assertTrustedCompleteTx(this, "query status");
    const provider = resolveProvider(options.provider, this.#context);
    return assertTxStatusMatches(
      this.txIdHex,
      await provider.getTxStatus(this.txIdHex),
    );
  }

  statusProgram(options: SubmitOptions = {}): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.status(options));
  }

  statusSafe(
    options: SubmitOptions = {},
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.status(options));
  }

  async awaitStatus(options: AwaitTxOptions = {}): Promise<TxStatus> {
    assertTrustedCompleteTx(this, "poll status");
    const provider = resolveProvider(options.provider, this.#context);
    return pollTxStatus(provider, this.txIdHex, options);
  }

  awaitStatusProgram(options: AwaitTxOptions = {}): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.awaitStatus(options));
  }

  awaitStatusSafe(
    options: AwaitTxOptions = {},
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.awaitStatus(options));
  }
}

const assertTrustedCompleteTx = (tx: CompleteTx, action: string): void => {
  if (!trustedCompleteTxs.has(tx)) {
    throw new BuilderInvariantError(
      `Untrusted CompleteTx cannot ${action}`,
      "construct transactions through LucidMidgard.newTx(), LucidMidgard.fromTx(), or signing APIs",
    );
  }
};

const makeCompleteTx = (
  tx: MidgardNativeTxFull,
  metadata: CompleteTxMetadata,
  context?: CompleteTxContext,
): CompleteTx => {
  const completed = new CompleteTx(tx, metadata, context);
  trustedCompleteTxs.add(completed);
  return completed;
};

const partialBundleInputs = (
  bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
): readonly PartialWitnessBundleInput[] =>
  Array.isArray(bundles) ? bundles : [bundles];

const completeWitnessSet = (
  witnesses: readonly VKeyWitness[],
  metadata: CompleteTxMetadata,
): boolean => {
  assertExpectedAddrWitnesses({
    actual: addrWitnessKeyHashes(witnesses),
    expected: metadata.expectedAddrWitnessKeyHashes,
    expectedComplete: metadata.expectedAddrWitnessesComplete,
    requireComplete: false,
  });
  try {
    assertExpectedAddrWitnesses({
      actual: addrWitnessKeyHashes(witnesses),
      expected: metadata.expectedAddrWitnessKeyHashes,
      expectedComplete: metadata.expectedAddrWitnessesComplete,
      requireComplete: true,
    });
    return true;
  } catch (cause) {
    if (cause instanceof SigningError) {
      return false;
    }
    throw cause;
  }
};

const assemblePartialWitnessBundles = (
  tx: MidgardNativeTxFull,
  metadata: CompleteTxMetadata,
  context: CompleteTxContext | undefined,
  bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
  options: AssemblePartialWitnessOptions,
): CompleteTx | PartiallySignedTx => {
  const inputs = partialBundleInputs(bundles);
  if (inputs.length === 0) {
    throw new SigningError("At least one partial witness bundle is required");
  }
  const bodyHash = tx.compact.transactionBodyHash;
  const witnesses = inputs.flatMap((input) => {
    const bundle = parsePartialWitnessBundle(input);
    assertPartialBundleMatchesTx(tx, bundle);
    return bundle.witnesses.map((witnessHex, index) =>
      normalizeVKeyWitnessInput(
        witnessHex,
        bodyHash,
        `partial bundle witness[${index.toString()}]`,
      ),
    );
  });
  const assembled = applyAddrWitnessesToTx(tx, witnesses);
  const nextMetadata = completeTxMetadataWithAddrWitnesses(
    assembled.tx,
    metadata,
  );
  const isComplete = completeWitnessSet(assembled.witnesses, nextMetadata);
  if (isComplete) {
    return makeCompleteTx(assembled.tx, nextMetadata, context);
  }
  if (options.allowPartial === true) {
    return makePartiallySignedTx(assembled.tx, nextMetadata, context);
  }
  assertExpectedAddrWitnesses({
    actual: addrWitnessKeyHashes(assembled.witnesses),
    expected: nextMetadata.expectedAddrWitnessKeyHashes,
    expectedComplete: nextMetadata.expectedAddrWitnessesComplete,
    requireComplete: true,
  });
  throw new SigningError("Incomplete partial witness assembly");
};

export class PartiallySignedTx {
  readonly #txCbor: Buffer;
  readonly #txId: Buffer;
  readonly #metadata: CompleteTxMetadata;
  readonly #context?: CompleteTxContext;
  readonly txHex: string;
  readonly txIdHex: string;

  constructor(
    tx: MidgardNativeTxFull,
    metadata: CompleteTxMetadata,
    context?: CompleteTxContext,
    token?: symbol,
  ) {
    if (token !== partiallySignedTxConstructorToken) {
      throw new BuilderInvariantError(
        "PartiallySignedTx constructor is internal; use CompleteTx.assemble(..., { allowPartial: true })",
      );
    }
    verifyMidgardNativeTxFullConsistency(tx);
    this.#txCbor = encodeMidgardNativeTxFull(tx);
    this.txHex = this.#txCbor.toString("hex");
    this.#txId = computeMidgardNativeTxIdFromFull(tx);
    this.txIdHex = this.#txId.toString("hex");
    this.#metadata = cloneCompleteTxMetadata(metadata);
    this.#context = context;
  }

  get tx(): MidgardNativeTxFull {
    return decodeMidgardNativeTxFull(this.#txCbor);
  }

  get txCbor(): Buffer {
    return Buffer.from(this.#txCbor);
  }

  get txId(): Buffer {
    return Buffer.from(this.#txId);
  }

  get metadata(): CompleteTxMetadata {
    return cloneCompleteTxMetadata(this.#metadata);
  }

  toCBOR(): string {
    return this.txHex;
  }

  toHash(): string {
    return this.txIdHex;
  }

  toJSON(): MidgardTxJson {
    return {
      txId: this.txIdHex,
      txCbor: this.txHex,
      metadata: midgardJsonSafe(this.metadata),
    };
  }

  producedOutputs(): readonly MidgardUtxo[] {
    return localUtxosFromTx(this.tx, this.#metadata).map(cloneUtxo);
  }

  producedOutput(outputIndex: number): MidgardUtxo {
    return localUtxoAt(this.tx, this.#metadata, outputIndex);
  }

  assemble(
    bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
    options: AssemblePartialWitnessOptions = {},
  ): CompleteTx | PartiallySignedTx {
    return assemblePartialWitnessBundles(
      this.tx,
      this.#metadata,
      this.#context,
      bundles,
      options,
    );
  }

  toPartialWitnessBundle(): MidgardPartialWitnessBundle {
    return partialWitnessBundleFromWitnesses(
      this.tx,
      decodeAddrWitnesses(this.tx.witnessSet.addrTxWitsPreimageCbor),
    );
  }

  toPartialWitnessBundleCbor(): Buffer {
    return encodePartialWitnessBundle(this.toPartialWitnessBundle());
  }
}

const makePartiallySignedTx = (
  tx: MidgardNativeTxFull,
  metadata: CompleteTxMetadata,
  context?: CompleteTxContext,
): PartiallySignedTx =>
  new PartiallySignedTx(
    tx,
    metadata,
    context,
    partiallySignedTxConstructorToken,
  );

export class SubmittedTx {
  readonly #tx: CompleteTx;
  readonly #admission: SubmitTxResult;
  readonly #provider: MidgardProvider;
  readonly txIdHex: string;

  constructor(
    tx: CompleteTx,
    admission: SubmitTxResult,
    provider: MidgardProvider,
    token?: symbol,
  ) {
    if (token !== submittedTxConstructorToken) {
      throw new BuilderInvariantError(
        "SubmittedTx constructor is internal; use CompleteTx.submit()",
      );
    }
    assertTrustedCompleteTx(tx, "construct SubmittedTx");
    this.#tx = tx;
    this.#admission = assertSubmitAdmissionMatches(tx.txIdHex, admission);
    this.#provider = provider;
    this.txIdHex = tx.txIdHex;
  }

  get tx(): CompleteTx {
    return this.#tx;
  }

  get admission(): SubmitTxResult {
    return { ...this.#admission };
  }

  toCBOR(): string {
    return this.#tx.toCBOR();
  }

  toHash(): string {
    return this.txIdHex;
  }

  toJSON(): MidgardTxJson & { readonly admission: SubmitTxResult } {
    return {
      ...this.#tx.toJSON(),
      admission: this.admission,
    };
  }

  producedOutputs(): readonly MidgardUtxo[] {
    return this.#tx.producedOutputs();
  }

  producedOutput(outputIndex: number): MidgardUtxo {
    return this.#tx.producedOutput(outputIndex);
  }

  async status(): Promise<TxStatus> {
    return assertTxStatusMatches(
      this.txIdHex,
      await this.#provider.getTxStatus(this.txIdHex),
    );
  }

  statusProgram(): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.status());
  }

  statusSafe(): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.status());
  }

  async awaitStatus(options: AwaitTxOptions = {}): Promise<TxStatus> {
    return pollTxStatus(
      options.provider ?? this.#provider,
      this.txIdHex,
      options,
    );
  }

  awaitStatusProgram(options: AwaitTxOptions = {}): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.awaitStatus(options));
  }

  awaitStatusSafe(
    options: AwaitTxOptions = {},
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.awaitStatus(options));
  }
}

const makeSubmittedTx = (
  tx: CompleteTx,
  admission: SubmitTxResult,
  provider: MidgardProvider,
): SubmittedTx =>
  new SubmittedTx(tx, admission, provider, submittedTxConstructorToken);

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

export type PayApi = {
  readonly ToAddress: (
    address: Address,
    value: ValueLike,
    options?: Omit<OutputOptions, "kind">,
  ) => TxBuilder;
  readonly ToContract: (
    address: Address,
    datum: PlutusDataLike,
    value: ValueLike,
    options?: Omit<OutputOptions, "datum" | "kind">,
  ) => TxBuilder;
  readonly ToProtectedAddress: (
    address: Address,
    value: ValueLike,
    options?: Omit<OutputOptions, "kind">,
  ) => TxBuilder;
};

export type AttachApi = {
  readonly Script: (source: ScriptSource) => TxBuilder;
  readonly NativeScript: (
    script: InstanceType<typeof CML.NativeScript> | Uint8Array | string,
  ) => TxBuilder;
  readonly SpendingValidator: (validator: SpendingValidator) => TxBuilder;
  readonly MintingPolicy: (policy: MintingPolicy) => TxBuilder;
  readonly ObserverValidator: (validator: ObserverValidator) => TxBuilder;
  readonly ReferenceScriptMetadata: (
    metadata:
      | TrustedReferenceScriptMetadata
      | readonly TrustedReferenceScriptMetadata[],
  ) => TxBuilder;
  readonly Datum: (data: PlutusDataLike, hash?: string) => TxBuilder;
};

export type ReadFromOptions = {
  readonly trustedReferenceScripts?: readonly TrustedReferenceScriptMetadata[];
};

const cloneBytesLike = <T extends Uint8Array | string>(value: T): T =>
  (typeof value === "string" ? value : Buffer.from(value)) as T;

const clonePlutusDataLike = (data: PlutusDataLike): PlutusDataLike => {
  if (typeof data === "string" || data instanceof Uint8Array) {
    return cloneBytesLike(data);
  }
  return Buffer.from(data.to_cbor_bytes());
};

const cloneScriptRefLike = (scriptRef: ScriptRefLike): ScriptRefLike => {
  if (typeof scriptRef === "string" || scriptRef instanceof Uint8Array) {
    return cloneBytesLike(scriptRef);
  }
  if (!(scriptRef instanceof CML.Script)) {
    return { ...scriptRef };
  }
  return CML.Script.from_cbor_bytes(scriptRef.to_cbor_bytes());
};

const validatorScriptSource = (
  validator: SpendingValidator | MintingPolicy | ObserverValidator,
  fieldName: string,
): ScriptSource => {
  if (typeof validator !== "object" || validator === null) {
    throw new BuilderInvariantError(`${fieldName} must be an object`);
  }
  const candidate = validator as {
    readonly language?: unknown;
    readonly script?: unknown;
  };
  if (candidate.language === "PlutusV3") {
    const script = candidate.script;
    if (
      !(
        typeof script === "string" ||
        script instanceof Uint8Array ||
        (typeof script === "object" &&
          script !== null &&
          "to_cbor_bytes" in script)
      )
    ) {
      throw new BuilderInvariantError(
        `${fieldName}.script is not PlutusV3 script bytes`,
      );
    }
    if (script instanceof CML.Script && script.as_plutus_v3() === undefined) {
      throw new BuilderInvariantError(
        `${fieldName}.script is not a PlutusV3 script`,
      );
    }
    return {
      kind: "plutus-v3",
      language: "PlutusV3",
      script: cloneScriptRefLike(script as ScriptRefLike),
    };
  }
  if (candidate.language === "MidgardV1") {
    const script = candidate.script;
    if (!(typeof script === "string" || script instanceof Uint8Array)) {
      throw new BuilderInvariantError(
        `${fieldName}.script is not MidgardV1 bytes`,
      );
    }
    return {
      kind: "midgard-v1",
      language: "MidgardV1",
      script: cloneBytesLike(script),
    };
  }
  throw new BuilderInvariantError(
    `${fieldName} language must be PlutusV3 or MidgardV1`,
    String(candidate.language),
  );
};

const cloneRedeemer = (redeemer: Redeemer): Redeemer => ({
  data: clonePlutusDataLike(redeemer.data),
  exUnits:
    redeemer.exUnits === undefined
      ? undefined
      : {
          mem: redeemer.exUnits.mem,
          steps: redeemer.exUnits.steps,
        },
});

const cloneScriptSource = (script: ScriptSource): ScriptSource => {
  switch (script.kind) {
    case "native":
      return {
        ...script,
        script:
          typeof script.script === "string" ||
          script.script instanceof Uint8Array
            ? cloneBytesLike(script.script)
            : Buffer.from(script.script.to_cbor_bytes()),
      };
    case "plutus-v3":
      return {
        ...script,
        script: cloneScriptRefLike(script.script),
      };
    case "midgard-v1":
    case "dual-plutus-v3-midgard-v1":
      return {
        ...script,
        script: cloneBytesLike(script.script),
      };
  }
};

const cloneDatumWitness = (datum: DatumWitness): DatumWitness => ({
  data: clonePlutusDataLike(datum.data),
  hash: datum.hash,
});

const cloneSpendIntent = (intent: SpendInputIntent): SpendInputIntent => ({
  txHash: intent.txHash,
  outputIndex: intent.outputIndex,
  redeemer:
    intent.redeemer === undefined ? undefined : cloneRedeemer(intent.redeemer),
});

const cloneMintIntent = (intent: MintIntent): MintIntent => ({
  policyId: intent.policyId,
  assets: { ...intent.assets },
  redeemer:
    intent.redeemer === undefined ? undefined : cloneRedeemer(intent.redeemer),
});

const cloneObserverIntent = (intent: ObserverIntent): ObserverIntent => ({
  scriptHash: intent.scriptHash,
  redeemer:
    intent.redeemer === undefined ? undefined : cloneRedeemer(intent.redeemer),
});

const cloneReceiveRedeemerIntent = (
  intent: ReceiveRedeemerIntent,
): ReceiveRedeemerIntent => ({
  scriptHash: intent.scriptHash,
  redeemer: cloneRedeemer(intent.redeemer),
});

const cloneTrustedReferenceScriptMetadata = (
  metadata: TrustedReferenceScriptMetadata,
): TrustedReferenceScriptMetadata => ({
  txHash: metadata.txHash,
  outputIndex: metadata.outputIndex,
  language: metadata.language,
  scriptHash: metadata.scriptHash,
  scriptCborHash: metadata.scriptCborHash,
});

const cloneScripts = (scripts: BuilderScriptState): BuilderScriptState => ({
  spendRedeemers: scripts.spendRedeemers.map(cloneSpendIntent),
  referenceScriptMetadata: scripts.referenceScriptMetadata.map(
    cloneTrustedReferenceScriptMetadata,
  ),
  scripts: scripts.scripts.map(cloneScriptSource),
  datumWitnesses: scripts.datumWitnesses.map(cloneDatumWitness),
  mints: scripts.mints.map(cloneMintIntent),
  observers: scripts.observers.map(cloneObserverIntent),
  receiveRedeemers: scripts.receiveRedeemers.map(cloneReceiveRedeemerIntent),
});

const cloneOutputDatum = (
  datum: AuthoredOutput["datum"],
): AuthoredOutput["datum"] => {
  if (datum === undefined || datum.kind === "none") {
    return datum;
  }
  if (datum.kind === "hash") {
    return { ...datum };
  }
  return { kind: "inline", data: clonePlutusDataLike(datum.data) };
};

const cloneOutput = (output: AuthoredOutput): AuthoredOutput => ({
  ...output,
  assets: { ...output.assets },
  datum: cloneOutputDatum(output.datum),
  scriptRef:
    output.scriptRef === undefined
      ? undefined
      : cloneScriptRefLike(output.scriptRef),
});

const cloneUtxo = (utxo: MidgardUtxo): MidgardUtxo => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
  output: {
    address: utxo.output.address,
    assets: { ...utxo.output.assets },
    datum:
      utxo.output.datum === undefined || utxo.output.datum === null
        ? utxo.output.datum
        : { ...utxo.output.datum },
    scriptRef:
      utxo.output.scriptRef === undefined || utxo.output.scriptRef === null
        ? utxo.output.scriptRef
        : { ...utxo.output.scriptRef },
  },
  cbor:
    utxo.cbor === undefined
      ? undefined
      : {
          outRef:
            utxo.cbor.outRef === undefined
              ? undefined
              : Buffer.from(utxo.cbor.outRef),
          output:
            utxo.cbor.output === undefined
              ? undefined
              : Buffer.from(utxo.cbor.output),
        },
});

const emptyState = (networkId?: bigint): BuilderState => ({
  spendInputs: [],
  referenceInputs: [],
  outputs: [],
  requiredSigners: [],
  minimumFee: undefined,
  networkId,
  scripts: emptyBuilderScriptState(),
});

const cloneState = (state: BuilderState): BuilderState => ({
  spendInputs: state.spendInputs.map(cloneUtxo),
  referenceInputs: state.referenceInputs.map(cloneUtxo),
  outputs: state.outputs.map(cloneOutput),
  requiredSigners: [...state.requiredSigners],
  validityIntervalStart: state.validityIntervalStart,
  validityIntervalEnd: state.validityIntervalEnd,
  minimumFee: state.minimumFee,
  networkId: state.networkId,
  scripts: cloneScripts(state.scripts),
  composition:
    state.composition === undefined ? undefined : { ...state.composition },
});

const assertUniqueUtxos = (
  spendInputs: readonly MidgardUtxo[],
  referenceInputs: readonly MidgardUtxo[],
): void => {
  const spend = new Set<string>();
  for (const input of spendInputs) {
    const label = outRefLabel(input);
    if (spend.has(label)) {
      throw new BuilderInvariantError("Duplicate spend input", label);
    }
    spend.add(label);
  }

  const refs = new Set<string>();
  for (const input of referenceInputs) {
    const label = outRefLabel(input);
    if (refs.has(label)) {
      throw new BuilderInvariantError("Duplicate reference input", label);
    }
    if (spend.has(label)) {
      throw new BuilderInvariantError(
        "Input cannot be both spend and reference",
        label,
      );
    }
    refs.add(label);
  }
};

const assertBuilderContextsComposable = (
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

const maxOptionalBigInt = (
  left: bigint | undefined,
  right: bigint | undefined,
): bigint | undefined =>
  left === undefined
    ? right
    : right === undefined
      ? left
      : left > right
        ? left
        : right;

const minOptionalBigInt = (
  left: bigint | undefined,
  right: bigint | undefined,
): bigint | undefined =>
  left === undefined
    ? right
    : right === undefined
      ? left
      : left < right
        ? left
        : right;

const assertNoDuplicateStrings = (
  values: readonly string[],
  message: string,
): void => {
  const seen = new Set<string>();
  for (const value of values) {
    if (seen.has(value)) {
      throw new BuilderInvariantError(message, value);
    }
    seen.add(value);
  }
};

const assertComposableScriptState = (scripts: BuilderScriptState): void => {
  assertNoDuplicateStrings(
    scripts.datumWitnesses
      .map((datum) => datum.hash)
      .filter((hash): hash is string => hash !== undefined),
    "Duplicate datum witness",
  );
  assertNoDuplicateStrings(
    scripts.observers.map((observer) =>
      normalizeScriptHash(observer.scriptHash, "observer script hash"),
    ),
    "Duplicate observer intent",
  );
  assertNoDuplicateStrings(
    scripts.receiveRedeemers.map((entry) =>
      normalizeScriptHash(entry.scriptHash, "receive script hash"),
    ),
    "Duplicate receive redeemer",
  );
  assertNoDuplicateStrings(
    scripts.spendRedeemers.map((entry) =>
      outRefLabel({ txHash: entry.txHash, outputIndex: entry.outputIndex }),
    ),
    "Duplicate spend redeemer",
  );
  assertNoDuplicateStrings(
    scripts.referenceScriptMetadata.map((entry) =>
      outRefLabel({ txHash: entry.txHash, outputIndex: entry.outputIndex }),
    ),
    "Duplicate trusted reference script metadata",
  );
  assertNoDuplicateStrings(
    scripts.mints
      .filter((mint) => mint.redeemer !== undefined)
      .map((mint) => normalizePolicyId(mint.policyId)),
    "Duplicate mint redeemer for policy",
  );
};

const composeStates = (states: readonly BuilderState[]): BuilderState => {
  const [first, ...rest] = states;
  if (first === undefined) {
    throw new BuilderInvariantError("compose requires at least one builder");
  }
  let validityIntervalStart = first.validityIntervalStart;
  let validityIntervalEnd = first.validityIntervalEnd;
  let minimumFee = first.minimumFee;
  for (const state of rest) {
    if (state.networkId !== first.networkId) {
      throw new BuilderInvariantError(
        "Cannot compose builders with different state network ids",
        `left=${String(first.networkId)} right=${String(state.networkId)}`,
      );
    }
    validityIntervalStart = maxOptionalBigInt(
      validityIntervalStart,
      state.validityIntervalStart,
    );
    validityIntervalEnd = minOptionalBigInt(
      validityIntervalEnd,
      state.validityIntervalEnd,
    );
    minimumFee = maxOptionalBigInt(minimumFee, state.minimumFee);
  }
  const requiredSigners = states.flatMap((state) => state.requiredSigners);
  assertNoDuplicateStrings(requiredSigners, "Duplicate required signer");
  const scripts: BuilderScriptState = {
    spendRedeemers: states.flatMap((state) =>
      state.scripts.spendRedeemers.map(cloneSpendIntent),
    ),
    referenceScriptMetadata: states.flatMap((state) =>
      state.scripts.referenceScriptMetadata.map(
        cloneTrustedReferenceScriptMetadata,
      ),
    ),
    scripts: states.flatMap((state) =>
      state.scripts.scripts.map(cloneScriptSource),
    ),
    datumWitnesses: states.flatMap((state) =>
      state.scripts.datumWitnesses.map(cloneDatumWitness),
    ),
    mints: states.flatMap((state) => state.scripts.mints.map(cloneMintIntent)),
    observers: states.flatMap((state) =>
      state.scripts.observers.map(cloneObserverIntent),
    ),
    receiveRedeemers: states.flatMap((state) =>
      state.scripts.receiveRedeemers.map(cloneReceiveRedeemerIntent),
    ),
  };
  assertComposableScriptState(scripts);
  const composed: BuilderState = {
    spendInputs: states.flatMap((state) => state.spendInputs.map(cloneUtxo)),
    referenceInputs: states.flatMap((state) =>
      state.referenceInputs.map(cloneUtxo),
    ),
    outputs: states.flatMap((state) => state.outputs.map(cloneOutput)),
    requiredSigners,
    validityIntervalStart,
    validityIntervalEnd,
    minimumFee,
    networkId: first.networkId,
    scripts,
    composition: {
      fragmentCount: states.reduce(
        (count, state) => count + (state.composition?.fragmentCount ?? 1),
        0,
      ),
    },
  };
  assertUniqueUtxos(composed.spendInputs, composed.referenceInputs);
  assertValidityInterval(composed);
  return composed;
};

const assetsEqual = (
  left: Record<string, bigint>,
  right: Record<string, bigint>,
): boolean => {
  const leftEntries = Object.entries(normalizeAssets(left));
  const rightNormalized = normalizeAssets(right);
  return (
    leftEntries.length === Object.keys(rightNormalized).length &&
    leftEntries.every(([unit, amount]) => rightNormalized[unit] === amount)
  );
};

const hasDefinedProperty = <K extends PropertyKey>(
  value: object,
  property: K,
): value is object & Record<K, unknown> =>
  Object.prototype.hasOwnProperty.call(value, property) &&
  (value as Record<K, unknown>)[property] !== undefined;

const scriptHex = (
  scriptRef: NonNullable<MidgardUtxo["output"]["scriptRef"]>,
): string => {
  const normalized = scriptRef.script.trim().toLowerCase();
  if (normalized.length % 2 !== 0 || !/^[0-9a-f]*$/.test(normalized)) {
    throw new BuilderInvariantError("UTxO scriptRef script must be hex");
  }
  return normalized;
};

const scriptRefsCompatible = (
  supplied: MidgardUtxo["output"]["scriptRef"],
  decoded: MidgardUtxo["output"]["scriptRef"],
): boolean => {
  if (supplied === undefined) {
    return true;
  }
  if (supplied === null) {
    return decoded === undefined || decoded === null;
  }
  if (decoded === undefined || decoded === null) {
    return false;
  }
  if (scriptHex(supplied) !== scriptHex(decoded)) {
    return false;
  }
  return supplied.type === decoded.type;
};

const normalizeUtxo = (utxo: MidgardUtxo): MidgardUtxo => {
  const normalized = normalizeOutRef(utxo);
  let outRefCbor: Buffer;
  try {
    const input = CML.TransactionInput.from_cbor_bytes(utxoOutRefCbor(utxo));
    outRefCbor = Buffer.from(input.to_cbor_bytes());
    const cborTxHash = input.transaction_id().to_hex();
    const cborOutputIndex = Number(input.index());
    if (
      cborTxHash !== normalized.txHash ||
      cborOutputIndex !== normalized.outputIndex
    ) {
      throw new Error("outref CBOR does not match txHash/outputIndex");
    }
  } catch (cause) {
    throw new BuilderInvariantError(
      "Invalid UTxO outRefCbor",
      cause instanceof Error ? cause.message : String(cause),
    );
  }

  let decodedOutput: ReturnType<typeof decodeMidgardTxOutput>;
  try {
    decodedOutput = decodeMidgardTxOutput(utxoOutputCbor(utxo));
  } catch (cause) {
    throw new BuilderInvariantError(
      "Invalid UTxO outputCbor",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  if (utxo.output.address !== decodedOutput.address) {
    throw new BuilderInvariantError("UTxO address does not match output CBOR");
  }
  if (!assetsEqual(utxo.output.assets, decodedOutput.assets)) {
    throw new BuilderInvariantError("UTxO assets do not match output CBOR");
  }
  const datum = hasDefinedProperty(utxo.output, "datum")
    ? utxo.output.datum
    : undefined;
  const decodedDatum = decodedOutput.txOutput.datum;
  if (
    datum !== undefined &&
    JSON.stringify(datum) !== JSON.stringify(decodedDatum ?? null)
  ) {
    throw new BuilderInvariantError("UTxO datum does not match output CBOR");
  }
  const scriptRef = hasDefinedProperty(utxo.output, "scriptRef")
    ? utxo.output.scriptRef
    : undefined;
  if (!scriptRefsCompatible(scriptRef, decodedOutput.txOutput.scriptRef)) {
    throw new BuilderInvariantError(
      "UTxO scriptRef does not match output CBOR",
    );
  }

  return {
    txHash: normalized.txHash,
    outputIndex: normalized.outputIndex,
    output: {
      address: decodedOutput.txOutput.address,
      assets: { ...decodedOutput.txOutput.assets },
      datum: datum === undefined ? decodedOutput.txOutput.datum : datum,
      scriptRef:
        scriptRef === undefined ? decodedOutput.txOutput.scriptRef : scriptRef,
    },
    cbor: {
      outRef: outRefCbor,
      output: Buffer.from(decodedOutput.outputCbor),
    },
  };
};

const normalizeProviderUtxo = (
  utxo: MidgardUtxo,
  endpoint: string,
): MidgardUtxo => {
  try {
    return normalizeUtxo(utxo);
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "Provider returned invalid Midgard UTxO",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

const normalizeProviderUtxos = (
  utxos: readonly MidgardUtxo[],
  endpoint: string,
): readonly MidgardUtxo[] => {
  if (!Array.isArray(utxos)) {
    throw new ProviderPayloadError(
      endpoint,
      "Provider UTxO result must be an array",
    );
  }
  return utxos.map((utxo) => normalizeProviderUtxo(utxo, endpoint));
};

const normalizeBuilderInputUtxo = (
  utxo: MidgardUtxo,
  _context: BuilderContextSnapshot,
): MidgardUtxo => {
  return normalizeUtxo(utxo);
};

const normalizeWalletInputUtxos = (
  utxos: readonly MidgardUtxo[],
  source: WalletInputSource,
  expectedNetworkId: number | undefined,
): readonly MidgardUtxo[] => {
  const seen = new Set<string>();
  return utxos.map((utxo) => {
    const normalized = normalizeUtxo(utxo);
    assertAddressNetwork(utxoAddress(normalized), expectedNetworkId);
    const label = outRefLabel(normalized);
    if (seen.has(label)) {
      throw new BuilderInvariantError(`Duplicate ${source} UTxO outref`, label);
    }
    seen.add(label);
    return normalized;
  });
};

const cloneUtxoOverrideSnapshot = (
  snapshot: UtxoOverrideSnapshot | undefined,
): UtxoOverrideSnapshot | undefined =>
  snapshot === undefined
    ? undefined
    : {
        generation: snapshot.generation,
        utxos: snapshot.utxos.map(cloneUtxo),
      };

const normalizeSigner = (signer: string): string => {
  const trimmed = signer.trim();
  if (/^[0-9a-fA-F]{56}$/.test(trimmed)) {
    return trimmed.toLowerCase();
  }
  return paymentKeyHashFromAddress(trimmed);
};

const normalizeNonNegativeBigInt = (
  value: bigint | number,
  fieldName: string,
): bigint => {
  if (typeof value === "number" && !Number.isSafeInteger(value)) {
    throw new BuilderInvariantError(`${fieldName} must be a safe integer`);
  }
  const normalized = BigInt(value);
  if (normalized < 0n) {
    throw new BuilderInvariantError(`${fieldName} must be non-negative`);
  }
  return normalized;
};

const normalizeHashHex = (
  value: string,
  fieldName: string,
  bytes: 28 | 32,
): string => {
  const normalized = value.trim().toLowerCase();
  if (normalized.length !== bytes * 2 || !/^[0-9a-f]+$/.test(normalized)) {
    throw new BuilderInvariantError(
      `${fieldName} must be a ${bytes.toString()}-byte hex string`,
      value,
    );
  }
  return normalized;
};

const normalizeAssetUnit = (unit: AssetUnit): AssetUnit => {
  if (typeof unit !== "string") {
    throw new BuilderInvariantError("Asset unit must be a string");
  }
  const normalized = unit.trim().toLowerCase();
  if (normalized === "lovelace") {
    return "lovelace";
  }
  if (
    normalized.length < 56 ||
    normalized.length > 120 ||
    normalized.length % 2 !== 0 ||
    !/^[0-9a-f]+$/.test(normalized)
  ) {
    throw new BuilderInvariantError(
      "Asset unit must be lovelace or policy-id plus hex asset-name",
      unit,
    );
  }
  return normalized;
};

const normalizeAddressQuery = (
  address: Address,
  expectedNetworkId: number | undefined,
  endpoint: string,
): Address => {
  if (typeof address !== "string") {
    throw new ProviderPayloadError(
      endpoint,
      "Midgard address queries require a bech32 address string",
    );
  }
  let normalized: Address;
  try {
    normalized = midgardAddressToText(midgardAddressFromText(address));
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "address must be a valid Midgard bech32 address",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  assertAddressNetwork(normalized, expectedNetworkId);
  return normalized;
};

const normalizeMissingMode = (
  options: UtxosByOutRefOptions | undefined,
): "error" | "omit" => {
  const mode = options?.missing ?? "error";
  if (mode !== "error" && mode !== "omit") {
    throw new BuilderInvariantError(
      'utxosByOutRef missing option must be "error" or "omit"',
      String(mode),
    );
  }
  return mode;
};

const assertRequestedOutRefsUnique = (
  outRefs: readonly OutRef[],
  endpoint: string,
): readonly OutRef[] => {
  const normalized = outRefs.map(normalizeOutRef);
  const labels = new Set<string>();
  for (const outRef of normalized) {
    const label = outRefLabel(outRef);
    if (labels.has(label)) {
      throw new BuilderInvariantError("Duplicate requested outref", label);
    }
    labels.add(label);
  }
  if (normalized.length === 0) {
    throw new BuilderInvariantError(
      "OutRef query must include at least one outref",
      endpoint,
    );
  }
  return normalized;
};

const orderedUtxosByOutRef = async (
  provider: MidgardProvider,
  outRefs: readonly OutRef[],
  options?: UtxosByOutRefOptions,
): Promise<readonly MidgardUtxo[]> => {
  const endpoint = "/utxos?by-outrefs";
  const requested = assertRequestedOutRefsUnique(outRefs, endpoint);
  const missingMode = normalizeMissingMode(options);
  const raw =
    provider.getUtxosByOutRefs === undefined
      ? await Promise.all(
          requested.map(async (outRef) => provider.getUtxoByOutRef(outRef)),
        ).then((items) =>
          items.filter((item): item is MidgardUtxo => item !== undefined),
        )
      : await provider.getUtxosByOutRefs(requested);
  const byLabel = new Map<string, MidgardUtxo>();
  for (const utxo of normalizeProviderUtxos(raw, endpoint)) {
    const label = outRefLabel(utxo);
    if (!requested.some((outRef) => outRefLabel(outRef) === label)) {
      throw new ProviderPayloadError(
        endpoint,
        "Provider returned an unrequested outref",
        label,
      );
    }
    if (byLabel.has(label)) {
      throw new ProviderPayloadError(
        endpoint,
        "Provider returned a duplicate outref",
        label,
      );
    }
    byLabel.set(label, utxo);
  }
  const ordered: MidgardUtxo[] = [];
  for (const outRef of requested) {
    const label = outRefLabel(outRef);
    const found = byLabel.get(label);
    if (found === undefined) {
      if (missingMode === "error") {
        throw new ProviderPayloadError(
          endpoint,
          "Missing requested UTxO",
          label,
        );
      }
      continue;
    }
    ordered.push(cloneUtxo(found));
  }
  return ordered;
};

const datumExpectedHash = (
  options: DatumOfOptions | undefined,
): string | undefined => {
  const expected =
    typeof options === "string" ? options : options?.expectedHash;
  return expected === undefined
    ? undefined
    : normalizeHashHex(expected, "datum hash", 32);
};

const inlineDatumFromUtxo = (
  utxo: MidgardUtxo,
  options?: DatumOfOptions,
): Buffer => {
  const endpoint = "/datum";
  const normalized = normalizeProviderUtxo(utxo, endpoint);
  let datumCbor: Buffer;
  try {
    const output = decodeMidgardTxOutput(utxoOutputCbor(normalized)).txOutput;
    if (output.datum === undefined || output.datum === null) {
      throw new ProviderPayloadError(endpoint, "UTxO does not contain a datum");
    }
    datumCbor = Buffer.from(output.datum.cbor, "hex");
  } catch (cause) {
    if (cause instanceof ProviderPayloadError) {
      throw cause;
    }
    throw new ProviderPayloadError(
      endpoint,
      "UTxO output is not a valid Midgard output",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const datum = CML.PlutusData.from_cbor_bytes(datumCbor);
  const expectedHash = datumExpectedHash(options);
  const actualHash = CML.hash_plutus_data(datum).to_hex();
  if (expectedHash !== undefined && expectedHash !== actualHash) {
    throw new ProviderPayloadError(
      endpoint,
      "Datum hash mismatch",
      `expected=${expectedHash} actual=${actualHash}`,
    );
  }
  return datumCbor;
};

const bytesFromHex = (value: string, fieldName: string): Buffer => {
  const normalized = value.trim().toLowerCase();
  if (normalized.length % 2 !== 0 || !/^[0-9a-f]*$/.test(normalized)) {
    throw new BuilderInvariantError(`${fieldName} must be hex`, value);
  }
  return Buffer.from(normalized, "hex");
};

const bytesFromBytesLike = (
  value: Uint8Array | string,
  fieldName: string,
): Buffer =>
  typeof value === "string"
    ? bytesFromHex(value, fieldName)
    : Buffer.from(value);

const normalizeExUnits = (
  redeemer: Redeemer,
): { mem: bigint; steps: bigint } => {
  if (redeemer.exUnits === undefined) {
    throw new BuilderInvariantError("Redeemer exUnits are required");
  }
  return {
    mem: normalizeNonNegativeBigInt(
      redeemer.exUnits.mem,
      "redeemer.exUnits.mem",
    ),
    steps: normalizeNonNegativeBigInt(
      redeemer.exUnits.steps,
      "redeemer.exUnits.steps",
    ),
  };
};

const redeemerDataBytes = (redeemer: Redeemer): Buffer =>
  Buffer.from(normalizePlutusData(redeemer.data).to_cbor_bytes());

const hashMidgardV1Script = (scriptBytes: Uint8Array): string =>
  Buffer.from(
    blake2b(Buffer.concat([Buffer.from([0x80]), Buffer.from(scriptBytes)]), {
      dkLen: 28,
    }),
  ).toString("hex");

const hashPlutusV3Script = (scriptBytes: Uint8Array): string =>
  CML.Script.new_plutus_v3(CML.PlutusV3Script.from_raw_bytes(scriptBytes))
    .hash()
    .to_hex();

type KnownScriptSource = {
  readonly sourceId: string;
  readonly witnessScript?: MidgardVersionedScript;
  readonly hashes: ReadonlyMap<"NativeCardano" | ScriptLanguageName, string>;
  readonly inline: boolean;
  readonly nonNative: boolean;
};

const nativeScriptFromLike = (
  script: InstanceType<typeof CML.NativeScript> | Uint8Array | string,
): InstanceType<typeof CML.NativeScript> =>
  script instanceof CML.NativeScript
    ? script
    : CML.NativeScript.from_cbor_bytes(
        bytesFromBytesLike(script, "native script"),
      );

const knownNativeScriptSource = (
  script: InstanceType<typeof CML.NativeScript> | Uint8Array | string,
  sourceId: string,
  inline: boolean,
): KnownScriptSource => {
  const native = nativeScriptFromLike(script);
  const versioned = normalizeScriptRef(CML.Script.new_native(native));
  return {
    sourceId,
    inline,
    nonNative: false,
    witnessScript: versioned,
    hashes: new Map([["NativeCardano", hashMidgardVersionedScript(versioned)]]),
  };
};

const knownPlutusScriptFromCml = (
  script: InstanceType<typeof CML.Script>,
  sourceId: string,
  inline: boolean,
): KnownScriptSource => {
  const native = script.as_native();
  if (native !== undefined) {
    return knownNativeScriptSource(native, sourceId, inline);
  }
  const plutusV3 = script.as_plutus_v3();
  if (plutusV3 === undefined) {
    throw new BuilderInvariantError(
      "Only native and PlutusV3 scripts are supported",
    );
  }
  const versioned = normalizeScriptRef(script);
  return {
    sourceId,
    inline,
    nonNative: true,
    witnessScript: versioned,
    hashes: new Map([["PlutusV3", hashMidgardVersionedScript(versioned)]]),
  };
};

const knownScriptSource = (
  source: ScriptSource,
  sourceId: string,
  inline: boolean,
): KnownScriptSource => {
  switch (source.kind) {
    case "native":
      return knownNativeScriptSource(source.script, sourceId, inline);
    case "plutus-v3": {
      if (source.script instanceof CML.Script) {
        const known = knownPlutusScriptFromCml(source.script, sourceId, inline);
        const plutusHash = known.hashes.get("PlutusV3");
        if (plutusHash === undefined) {
          throw new BuilderInvariantError(
            "PlutusV3 script source did not hash as PlutusV3",
          );
        }
        return {
          ...known,
          hashes: new Map([["PlutusV3", plutusHash]]),
        };
      }
      if (
        !(
          typeof source.script === "string" ||
          source.script instanceof Uint8Array
        )
      ) {
        throw new BuilderInvariantError(
          "PlutusV3 script source must be script bytes",
        );
      }
      const raw = bytesFromBytesLike(source.script, "PlutusV3 script");
      const versioned = {
        language: "PlutusV3" as const,
        scriptBytes: raw,
      };
      return {
        sourceId,
        inline,
        nonNative: true,
        witnessScript: versioned,
        hashes: new Map([["PlutusV3", hashMidgardVersionedScript(versioned)]]),
      };
    }
    case "midgard-v1": {
      const raw = bytesFromBytesLike(source.script, "MidgardV1 script");
      const versioned = {
        language: "MidgardV1" as const,
        scriptBytes: raw,
      };
      return {
        sourceId,
        inline,
        nonNative: true,
        witnessScript: versioned,
        hashes: new Map([["MidgardV1", hashMidgardVersionedScript(versioned)]]),
      };
    }
    case "dual-plutus-v3-midgard-v1": {
      throw new BuilderInvariantError(
        "Dual PlutusV3/MidgardV1 script witnesses are not supported; attach explicit versioned scripts",
        sourceId,
      );
    }
  }
};

const knownReferenceScriptSource = (
  script: MidgardScript,
  sourceId: string,
  metadata?: TrustedReferenceScriptMetadata,
): KnownScriptSource => {
  const versioned = normalizeScriptRef(script);
  const localLanguage = versioned.language;
  const localHash = hashMidgardVersionedScript(versioned);
  if (metadata !== undefined && metadata.language !== localLanguage) {
    throw new BuilderInvariantError(
      "Reference script metadata language does not match local script reference",
      `${sourceId} ${metadata.language}`,
    );
  }
  if (metadata !== undefined && localHash !== metadata.scriptHash) {
    throw new BuilderInvariantError(
      "Reference script metadata hash does not match local script reference",
      `${sourceId} ${metadata.scriptHash}`,
    );
  }
  if (metadata?.scriptCborHash !== undefined) {
    const localScriptCborHash = computeHash32(
      encodeMidgardVersionedScript(versioned),
    ).toString("hex");
    if (localScriptCborHash !== metadata.scriptCborHash) {
      throw new BuilderInvariantError(
        "Reference script metadata scriptCborHash does not match local script reference",
        `${sourceId} ${metadata.scriptCborHash}`,
      );
    }
  }
  return {
    sourceId,
    inline: false,
    nonNative: localLanguage !== "NativeCardano",
    witnessScript: undefined,
    hashes: new Map([[localLanguage, localHash]]),
  };
};

const knownTrustedReferenceScriptMetadataSource = (
  metadata: TrustedReferenceScriptMetadata,
  sourceId: string,
): KnownScriptSource => ({
  sourceId,
  inline: false,
  nonNative: metadata.language !== "NativeCardano",
  witnessScript: undefined,
  hashes: new Map([[metadata.language, metadata.scriptHash]]),
});

const collectKnownScriptSources = (
  state: BuilderState,
): KnownScriptSource[] => {
  const inline = state.scripts.scripts.map((source, index) =>
    knownScriptSource(source, `inline:${index.toString()}`, true),
  );
  const metadataByOutRef = new Map(
    state.scripts.referenceScriptMetadata.map((metadata) => [
      outRefLabel(metadata),
      metadata,
    ]),
  );
  if (metadataByOutRef.size !== state.scripts.referenceScriptMetadata.length) {
    throw new BuilderInvariantError(
      "Duplicate trusted reference script metadata",
    );
  }
  const consumedMetadata = new Set<string>();
  const reference = state.referenceInputs.flatMap((input) => {
    const label = outRefLabel(input);
    const metadata = metadataByOutRef.get(label);
    const scriptRef = decodeMidgardTxOutput(utxoOutputCbor(input)).txOutput
      .scriptRef;
    if (metadata !== undefined) {
      consumedMetadata.add(label);
    }
    if (scriptRef === undefined || scriptRef === null) {
      return metadata === undefined
        ? []
        : [
            knownTrustedReferenceScriptMetadataSource(
              metadata,
              `reference:${label}`,
            ),
          ];
    }
    return [
      knownReferenceScriptSource(scriptRef, `reference:${label}`, metadata),
    ];
  });
  for (const label of metadataByOutRef.keys()) {
    if (!consumedMetadata.has(label)) {
      throw new BuilderInvariantError(
        "Trusted reference script metadata has no matching reference input",
        label,
      );
    }
  }
  return [...inline, ...reference];
};

const resolveKnownScript = (
  scriptHash: string,
  sources: readonly KnownScriptSource[],
):
  | {
      readonly language: "NativeCardano" | ScriptLanguageName;
      readonly source: KnownScriptSource;
    }
  | undefined => {
  let resolved:
    | {
        readonly language: "NativeCardano" | ScriptLanguageName;
        readonly source: KnownScriptSource;
      }
    | undefined;
  for (const source of sources) {
    for (const [language, hash] of source.hashes.entries()) {
      if (hash !== scriptHash) {
        continue;
      }
      if (resolved !== undefined) {
        throw new BuilderInvariantError(
          "Ambiguous script source resolution",
          scriptHash,
        );
      }
      resolved = { language, source };
    }
  }
  return resolved;
};

const assertValidityInterval = (state: BuilderState): void => {
  if (
    state.validityIntervalStart !== undefined &&
    state.validityIntervalEnd !== undefined &&
    state.validityIntervalStart > state.validityIntervalEnd
  ) {
    throw new BuilderInvariantError(
      "validityIntervalStart must be less than or equal to validityIntervalEnd",
      `${state.validityIntervalStart.toString()} > ${state.validityIntervalEnd.toString()}`,
    );
  }
};

const assertScriptStateEmptyForT07 = (state: BuilderState): void => {
  const scripts = state.scripts;
  if (
    scripts.spendRedeemers.length > 0 ||
    scripts.scripts.length > 0 ||
    scripts.datumWitnesses.length > 0 ||
    scripts.mints.length > 0 ||
    scripts.observers.length > 0 ||
    scripts.receiveRedeemers.length > 0
  ) {
    throw new BuilderInvariantError(
      "Script, datum, mint, observer, and redeemer finalization is implemented in T09",
    );
  }
};

const normalizeValidationConcurrency = (
  value: number | undefined,
  fieldName: string,
): number => {
  if (value === undefined) {
    return 1;
  }
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new BuilderInvariantError(
      `${fieldName} must be a non-negative safe integer`,
    );
  }
  return value;
};

const rejectRuntimeKindOption = (
  options: object | undefined,
  methodName: string,
): void => {
  if (options !== undefined && "kind" in options) {
    throw new BuilderInvariantError(
      `${methodName} does not accept an output kind option; use the explicit helper`,
    );
  }
};

const encodeByteListPreimage = (items: readonly Uint8Array[]): Buffer =>
  encodeCbor(items.map((item) => Buffer.from(item)));

const normalizeScriptHash = (hash: string, fieldName = "script hash"): string =>
  normalizeHashHex(hash, fieldName, 28);

const normalizePolicyId = (policyId: string): string =>
  normalizeHashHex(policyId, "policy id", 28);

const normalizeScriptLanguage = (
  language: unknown,
  fieldName: string,
): ScriptLanguage => {
  if (
    language === "NativeCardano" ||
    language === "PlutusV3" ||
    language === "MidgardV1"
  ) {
    return language;
  }
  throw new BuilderInvariantError(
    `${fieldName} must be NativeCardano, PlutusV3, or MidgardV1`,
    String(language),
  );
};

const normalizeTrustedReferenceScriptMetadata = (
  metadata: TrustedReferenceScriptMetadata,
): TrustedReferenceScriptMetadata => {
  if (typeof metadata !== "object" || metadata === null) {
    throw new BuilderInvariantError(
      "Reference script metadata must be an object",
    );
  }
  const candidate = metadata as {
    readonly txHash?: unknown;
    readonly outputIndex?: unknown;
    readonly language?: unknown;
    readonly scriptHash?: unknown;
    readonly scriptCborHash?: unknown;
  };
  if (typeof candidate.txHash !== "string") {
    throw new BuilderInvariantError(
      "Reference script metadata txHash must be a string",
    );
  }
  if (
    typeof candidate.outputIndex !== "number" ||
    !Number.isSafeInteger(candidate.outputIndex)
  ) {
    throw new BuilderInvariantError(
      "Reference script metadata outputIndex must be a safe integer",
    );
  }
  if (typeof candidate.scriptHash !== "string") {
    throw new BuilderInvariantError(
      "Reference script metadata scriptHash must be a string",
    );
  }
  if (
    candidate.scriptCborHash !== undefined &&
    typeof candidate.scriptCborHash !== "string"
  ) {
    throw new BuilderInvariantError(
      "Reference script metadata scriptCborHash must be a string when present",
    );
  }
  const outRef = normalizeOutRef({
    txHash: candidate.txHash,
    outputIndex: candidate.outputIndex,
  } as OutRef);
  return {
    ...outRef,
    language: normalizeScriptLanguage(
      candidate.language,
      "Reference script metadata language",
    ),
    scriptHash: normalizeScriptHash(
      candidate.scriptHash,
      "reference script metadata scriptHash",
    ),
    scriptCborHash:
      candidate.scriptCborHash === undefined
        ? undefined
        : normalizeHashHex(
            candidate.scriptCborHash,
            "reference script metadata scriptCborHash",
            32,
          ),
  };
};

const normalizeTrustedReferenceScriptMetadataList = (
  metadata:
    | TrustedReferenceScriptMetadata
    | readonly TrustedReferenceScriptMetadata[],
): readonly TrustedReferenceScriptMetadata[] => {
  const entries = Array.isArray(metadata) ? metadata : [metadata];
  const normalized = entries.map(normalizeTrustedReferenceScriptMetadata);
  assertNoDuplicateStrings(
    normalized.map((entry) => outRefLabel(entry)),
    "Duplicate trusted reference script metadata",
  );
  return normalized;
};

const normalizeMintAssetName = (policyId: string, unit: string): string => {
  const normalized = unit.trim().toLowerCase();
  if (normalized === "lovelace") {
    throw new BuilderInvariantError("Mint assets cannot include lovelace");
  }
  const assetName =
    normalized.length >= 56 && normalized.startsWith(policyId)
      ? normalized.slice(56)
      : normalized;
  if (assetName.length % 2 !== 0 || !/^[0-9a-f]*$/.test(assetName)) {
    throw new BuilderInvariantError("Mint asset names must be hex", unit);
  }
  return assetName;
};

const normalizeMintAssetsForPolicy = (
  policyId: string,
  assets: Assets,
): Assets => {
  const normalizedPolicy = normalizePolicyId(policyId);
  const normalized: Record<string, bigint> = {};
  for (const [unit, amount] of Object.entries(normalizeAssets(assets))) {
    const quantity = BigInt(amount);
    if (quantity === 0n) {
      continue;
    }
    normalized[normalizeMintAssetName(normalizedPolicy, unit)] = quantity;
  }
  if (Object.keys(normalized).length === 0) {
    throw new BuilderInvariantError("Mint assets must not be empty");
  }
  return normalized;
};

type EffectiveMint = {
  readonly policyId: string;
  readonly assets: Assets;
  readonly redeemer?: Redeemer;
};

const effectiveMints = (
  mints: readonly MintIntent[],
): readonly EffectiveMint[] => {
  const byPolicy = new Map<string, Map<string, bigint>>();
  const redeemers = new Map<string, Redeemer>();
  for (const mint of mints) {
    const policyId = normalizePolicyId(mint.policyId);
    if (mint.redeemer !== undefined) {
      if (redeemers.has(policyId)) {
        throw new BuilderInvariantError(
          "Duplicate mint redeemer for policy",
          policyId,
        );
      }
      redeemers.set(policyId, cloneRedeemer(mint.redeemer));
    }
    const assets = normalizeMintAssetsForPolicy(policyId, mint.assets);
    const policyAssets = byPolicy.get(policyId) ?? new Map<string, bigint>();
    for (const [assetName, quantity] of Object.entries(assets)) {
      const next = (policyAssets.get(assetName) ?? 0n) + quantity;
      if (next === 0n) {
        policyAssets.delete(assetName);
      } else {
        policyAssets.set(assetName, next);
      }
    }
    if (policyAssets.size === 0) {
      byPolicy.delete(policyId);
    } else {
      byPolicy.set(policyId, policyAssets);
    }
  }

  for (const policyId of redeemers.keys()) {
    if (!byPolicy.has(policyId)) {
      throw new BuilderInvariantError(
        "Mint redeemer has no effective mint policy",
        policyId,
      );
    }
  }

  return [...byPolicy.entries()]
    .sort(([a], [b]) => compareCanonicalStrings(a, b))
    .map(([policyId, assets]) => ({
      policyId,
      assets: Object.fromEntries(
        [...assets.entries()].sort(([a], [b]) => compareCanonicalStrings(a, b)),
      ),
      redeemer: redeemers.get(policyId),
    }));
};

const mintPreimageCbor = (mints: readonly EffectiveMint[]): Buffer => {
  if (mints.length === 0) {
    return EMPTY_CBOR_LIST;
  }
  const cborMap = new Map<Buffer, Map<Buffer, bigint>>();
  for (const { policyId, assets } of mints) {
    const assetMap = new Map<Buffer, bigint>();
    for (const [assetName, quantity] of Object.entries(assets)) {
      assetMap.set(Buffer.from(assetName, "hex"), quantity);
    }
    cborMap.set(Buffer.from(policyId, "hex"), assetMap);
  }
  return encodeCbor(cborMap);
};

const mintDeltaAssets = (mints: readonly EffectiveMint[]): Assets => {
  let total: Assets = {};
  for (const mint of mints) {
    const fullUnits = Object.fromEntries(
      Object.entries(mint.assets).map(([assetName, quantity]) => [
        `${mint.policyId}${assetName}`,
        quantity,
      ]),
    );
    total = addAssets(total, fullUnits);
  }
  return total;
};

const splitSignedAssets = (
  assets: Assets,
): { readonly positive: Assets; readonly negative: Assets } => {
  const positive: Record<string, bigint> = {};
  const negative: Record<string, bigint> = {};
  for (const [unit, amount] of Object.entries(normalizeAssets(assets))) {
    if (amount > 0n) {
      positive[unit] = amount;
    } else if (amount < 0n) {
      negative[unit] = -amount;
    }
  }
  return { positive, negative };
};

const subtractAssetsFloor = (left: Assets, right: Assets): Assets => {
  const result: Record<string, bigint> = {};
  for (const [unit, amount] of Object.entries(normalizeAssets(left))) {
    const remaining = amount - BigInt(right[unit] ?? 0n);
    if (remaining > 0n) {
      result[unit] = remaining;
    }
  }
  return result;
};

const addFeeToAssets = (assets: Assets, fee: bigint): Assets =>
  fee === 0n ? assets : addAssets(assets, { lovelace: fee });

const balanceSides = ({
  inputs,
  outputs,
  fee,
  mintDelta,
}: {
  readonly inputs: Assets;
  readonly outputs: Assets;
  readonly fee: bigint;
  readonly mintDelta: Assets;
}): { readonly available: Assets; readonly required: Assets } => {
  const mint = splitSignedAssets(mintDelta);
  return {
    available: addAssets(inputs, mint.positive),
    required: addAssets(addFeeToAssets(outputs, fee), mint.negative),
  };
};

const requiredObserversPreimageCbor = (
  observers: readonly ObserverIntent[],
): Buffer =>
  observers.length === 0
    ? EMPTY_CBOR_LIST
    : encodeByteListPreimage(
        [
          ...new Set(
            observers.map((observer) =>
              normalizeScriptHash(observer.scriptHash, "observer script hash"),
            ),
          ),
        ]
          .sort()
          .map((hash) => Buffer.from(hash, "hex")),
      );

const sumAssets = (items: readonly Assets[]): Assets =>
  items.reduce<Assets>((acc, assets) => addAssets(acc, assets), {});

const jsonAssets = (assets: Assets): Record<string, string> =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, amount]) => [unit, amount.toString(10)]),
  );

const assertBalancedWithoutChange = (
  inputTotal: Assets,
  outputTotal: Assets,
  fee: bigint,
  mintDelta: Assets,
): void => {
  const { available, required } = balanceSides({
    inputs: inputTotal,
    outputs: outputTotal,
    fee,
    mintDelta,
  });
  const remainder = subtractAssets(available, required);
  if (!isZeroAssets(remainder)) {
    throw new BuilderInvariantError(
      "Explicit completion requires balanced inputs, outputs, and fee",
      JSON.stringify(
        {
          remainder: jsonAssets(remainder),
        },
        null,
        2,
      ),
    );
  }
};

const sortedInputCbors = (inputs: readonly MidgardUtxo[]): Buffer[] =>
  [...inputs].sort(compareOutRefs).map((input) => utxoOutRefCbor(input));

const sortedRequiredSignerCbors = (signers: readonly string[]): Buffer[] =>
  [...signers]
    .sort((left, right) =>
      Buffer.from(left, "hex").compare(Buffer.from(right, "hex")),
    )
    .map((signer) => Buffer.from(signer, "hex"));

const outputCbors = (outputs: readonly AuthoredOutput[]): Buffer[] =>
  outputs.map((output) =>
    encodeMidgardTxOutput(output.address, output.assets, {
      kind: output.kind,
      datum: output.datum,
      scriptRef: output.scriptRef,
    }),
  );

const RedeemerTags = {
  Spend: Number(CML.RedeemerTag.Spend),
  Mint: Number(CML.RedeemerTag.Mint),
  Reward: Number(CML.RedeemerTag.Reward),
  Receive: 6,
} as const;

type RedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

type DerivedRedeemer = {
  readonly pointer: RedeemerPointer;
  readonly redeemer: Redeemer;
};

type ScriptMaterialization = {
  readonly requiredObserversPreimageCbor: Buffer;
  readonly mintPreimageCbor: Buffer;
  readonly scriptTxWitsPreimageCbor: Buffer;
  readonly redeemerTxWitsPreimageCbor: Buffer;
  readonly scriptIntegrityHash: Buffer;
  readonly mintDelta: Assets;
};

const pointerKey = (pointer: RedeemerPointer): string =>
  `${pointer.tag.toString()}:${pointer.index.toString(10)}`;

const outRefIntentKey = (
  intent: Pick<SpendInputIntent, "txHash" | "outputIndex">,
): string => outRefLabel(intent);

const redeemerIntentKey = (
  purpose: "spend" | "mint" | "observe" | "receive",
  id: string,
): string => `${purpose}:${id}`;

const recordConsumedRedeemer = (consumed: Set<string>, key: string): void => {
  if (consumed.has(key)) {
    throw new BuilderInvariantError("Duplicate consumed redeemer intent", key);
  }
  consumed.add(key);
};

const assertAllRedeemerIntentsConsumed = (
  state: BuilderState,
  effective: readonly EffectiveMint[],
  consumed: ReadonlySet<string>,
): void => {
  for (const intent of state.scripts.spendRedeemers) {
    if (intent.redeemer === undefined) {
      continue;
    }
    const key = redeemerIntentKey("spend", outRefIntentKey(intent));
    if (!consumed.has(key)) {
      throw new BuilderInvariantError("Unconsumed spend redeemer", key);
    }
  }
  for (const mint of effective) {
    if (mint.redeemer === undefined) {
      continue;
    }
    const key = redeemerIntentKey("mint", mint.policyId);
    if (!consumed.has(key)) {
      throw new BuilderInvariantError("Unconsumed mint redeemer", key);
    }
  }
  for (const observer of state.scripts.observers) {
    if (observer.redeemer === undefined) {
      continue;
    }
    const key = redeemerIntentKey(
      "observe",
      normalizeScriptHash(observer.scriptHash, "observer script hash"),
    );
    if (!consumed.has(key)) {
      throw new BuilderInvariantError("Unconsumed observer redeemer", key);
    }
  }
  for (const receive of state.scripts.receiveRedeemers) {
    const key = redeemerIntentKey(
      "receive",
      normalizeScriptHash(receive.scriptHash, "receive script hash"),
    );
    if (!consumed.has(key)) {
      throw new BuilderInvariantError("Unconsumed receive redeemer", key);
    }
  }
};

const findSpendRedeemer = (
  state: BuilderState,
  input: MidgardUtxo,
): Redeemer | undefined =>
  state.scripts.spendRedeemers.find(
    (intent) => outRefIntentKey(intent) === outRefLabel(input),
  )?.redeemer;

const findMintRedeemer = (
  mints: readonly EffectiveMint[],
  policyId: string,
): Redeemer | undefined =>
  mints.find((mint) => mint.policyId === policyId)?.redeemer;

const findObserverRedeemer = (
  state: BuilderState,
  scriptHash: string,
): Redeemer | undefined =>
  state.scripts.observers.find(
    (observer) =>
      normalizeScriptHash(observer.scriptHash, "observer script hash") ===
      scriptHash,
  )?.redeemer;

const findReceiveRedeemer = (
  state: BuilderState,
  scriptHash: string,
): Redeemer | undefined =>
  state.scripts.receiveRedeemers.find(
    (entry) =>
      normalizeScriptHash(entry.scriptHash, "receive script hash") ===
      scriptHash,
  )?.redeemer;

const paymentScriptHashFromAddress = (address: Address): string | undefined =>
  outputAddressPaymentScriptHash(address);

const paymentPubKeyHashFromAddress = (address: Address): string | undefined =>
  outputAddressPaymentKeyHash(address);

const paymentScriptHashFromUtxo = (utxo: MidgardUtxo): string | undefined =>
  outputAddressPaymentScriptHash(
    decodeMidgardTxOutput(utxoOutputCbor(utxo)).address,
  );

const paymentPubKeyHashFromUtxo = (utxo: MidgardUtxo): string | undefined =>
  outputAddressPaymentKeyHash(
    decodeMidgardTxOutput(utxoOutputCbor(utxo)).address,
  );

const assertWalletOwnsInputs = async (
  wallet: MidgardWallet | undefined,
  inputs: readonly MidgardUtxo[],
  source: WalletInputSource,
): Promise<void> => {
  if (inputs.length === 0) {
    return;
  }
  if (wallet === undefined) {
    throw new BuilderInvariantError(
      `${source} wallet inputs require a selected wallet`,
    );
  }
  const walletKeyHash = await wallet.keyHash();
  for (const input of inputs) {
    const inputKeyHash = paymentPubKeyHashFromUtxo(input);
    if (inputKeyHash === undefined) {
      throw new BuilderInvariantError(
        `${source} wallet input is not spendable by a public-key wallet`,
        outRefLabel(input),
      );
    }
    if (inputKeyHash !== walletKeyHash) {
      throw new BuilderInvariantError(
        `${source} wallet input does not belong to the selected wallet`,
        `outref=${outRefLabel(input)} wallet_key_hash=${walletKeyHash} input_key_hash=${inputKeyHash}`,
      );
    }
  }
};

const expectedAddrWitnessKeyHashes = (
  state: BuilderState,
): readonly string[] => {
  const keyHashes = new Set(state.requiredSigners);
  for (const input of state.spendInputs) {
    const keyHash = paymentPubKeyHashFromUtxo(input);
    if (keyHash !== undefined) {
      keyHashes.add(keyHash);
    }
  }
  for (const output of state.outputs) {
    if (!outputAddressProtected(output.address)) {
      continue;
    }
    const keyHash = paymentPubKeyHashFromAddress(output.address);
    if (keyHash !== undefined) {
      keyHashes.add(keyHash);
    }
  }
  return [...keyHashes].sort();
};

const addRequiredExecution = ({
  scriptHash,
  purpose,
  pointer,
  redeemer,
  sources,
  redeemers,
  usedSources,
  languages,
}: {
  readonly scriptHash: string;
  readonly purpose: string;
  readonly pointer: RedeemerPointer;
  readonly redeemer: Redeemer | undefined;
  readonly sources: readonly KnownScriptSource[];
  readonly redeemers: DerivedRedeemer[];
  readonly usedSources: Set<string>;
  readonly languages: Set<ScriptLanguageName>;
}): void => {
  const resolved = resolveKnownScript(scriptHash, sources);
  if (resolved === undefined) {
    throw new BuilderInvariantError(
      `Missing script source for ${purpose}`,
      scriptHash,
    );
  }
  usedSources.add(resolved.source.sourceId);
  if (resolved.language === "NativeCardano") {
    if (redeemer !== undefined) {
      throw new BuilderInvariantError(
        `Native script ${purpose} cannot have a redeemer`,
        scriptHash,
      );
    }
    return;
  }
  if (purpose === "receive" && resolved.language === "PlutusV3") {
    throw new BuilderInvariantError(
      "PlutusV3 receive scripts are not supported",
    );
  }
  if (redeemer === undefined) {
    throw new BuilderInvariantError(
      `Missing redeemer for ${purpose}`,
      scriptHash,
    );
  }
  languages.add(resolved.language);
  redeemers.push({ pointer, redeemer });
};

const encodeRedeemers = (redeemers: readonly DerivedRedeemer[]): Buffer => {
  if (redeemers.length === 0) {
    return EMPTY_CBOR_LIST;
  }
  const seen = new Set<string>();
  const entries = [...redeemers].sort((left, right) => {
    if (left.pointer.tag !== right.pointer.tag) {
      return left.pointer.tag - right.pointer.tag;
    }
    return left.pointer.index < right.pointer.index
      ? -1
      : left.pointer.index > right.pointer.index
        ? 1
        : 0;
  });
  const encoded: [bigint, bigint, Buffer, readonly [bigint, bigint]][] = [];
  for (const entry of entries) {
    const key = pointerKey(entry.pointer);
    if (seen.has(key)) {
      throw new BuilderInvariantError("Duplicate redeemer pointer", key);
    }
    seen.add(key);
    const exUnits = normalizeExUnits(entry.redeemer);
    encoded.push([
      BigInt(entry.pointer.tag),
      entry.pointer.index,
      redeemerDataBytes(entry.redeemer),
      [exUnits.mem, exUnits.steps],
    ]);
  }
  return encodeCbor(encoded);
};

const deriveScriptMaterialization = (
  state: BuilderState,
): ScriptMaterialization => {
  if (state.scripts.datumWitnesses.length > 0) {
    throw new BuilderInvariantError(
      "Datum witnesses are not supported by Midgard native transactions; use inline datums",
    );
  }
  const sources = collectKnownScriptSources(state);
  const usedSources = new Set<string>();
  const languages = new Set<ScriptLanguageName>();
  const redeemers: DerivedRedeemer[] = [];
  const consumedRedeemers = new Set<string>();
  const effective = effectiveMints(state.scripts.mints);

  const spent = [...state.spendInputs].sort(compareOutRefs);
  for (let index = 0; index < spent.length; index += 1) {
    const input = spent[index]!;
    const scriptHash = paymentScriptHashFromUtxo(input);
    if (scriptHash === undefined) {
      continue;
    }
    addRequiredExecution({
      scriptHash,
      purpose: "spend",
      pointer: { tag: RedeemerTags.Spend, index: BigInt(index) },
      redeemer: findSpendRedeemer(state, input),
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (findSpendRedeemer(state, input) !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("spend", outRefLabel(input)),
      );
    }
  }

  const policyIds = effective.map((mint) => mint.policyId);
  for (let index = 0; index < policyIds.length; index += 1) {
    const policyId = policyIds[index]!;
    const redeemer = findMintRedeemer(effective, policyId);
    addRequiredExecution({
      scriptHash: policyId,
      purpose: "mint",
      pointer: { tag: RedeemerTags.Mint, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("mint", policyId),
      );
    }
  }

  const observers = [
    ...new Set(
      state.scripts.observers.map((observer) =>
        normalizeScriptHash(observer.scriptHash, "observer script hash"),
      ),
    ),
  ].sort();
  for (let index = 0; index < observers.length; index += 1) {
    const observer = observers[index]!;
    const redeemer = findObserverRedeemer(state, observer);
    addRequiredExecution({
      scriptHash: observer,
      purpose: "observe",
      pointer: { tag: RedeemerTags.Reward, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("observe", observer),
      );
    }
  }

  const receivingHashes = [
    ...new Set(
      state.outputs.flatMap((output) => {
        if (!outputAddressProtected(output.address)) {
          return [];
        }
        const scriptHash = paymentScriptHashFromAddress(output.address);
        return scriptHash === undefined ? [] : [scriptHash];
      }),
    ),
  ].sort();
  for (let index = 0; index < receivingHashes.length; index += 1) {
    const scriptHash = receivingHashes[index]!;
    const redeemer = findReceiveRedeemer(state, scriptHash);
    addRequiredExecution({
      scriptHash,
      purpose: "receive",
      pointer: { tag: RedeemerTags.Receive, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("receive", scriptHash),
      );
    }
  }

  assertAllRedeemerIntentsConsumed(state, effective, consumedRedeemers);

  for (const source of sources) {
    if (source.inline && !usedSources.has(source.sourceId)) {
      throw new BuilderInvariantError(
        "Extraneous script witness",
        source.sourceId,
      );
    }
  }

  const redeemerTxWitsPreimageCbor = encodeRedeemers(redeemers);
  const redeemerTxWitsRoot = computeHash32(redeemerTxWitsPreimageCbor);
  const requiredLanguages = [...languages].sort();
  return {
    requiredObserversPreimageCbor: requiredObserversPreimageCbor(
      state.scripts.observers,
    ),
    mintPreimageCbor: mintPreimageCbor(effective),
    scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage(
      sources
        .filter((source) => source.inline)
        .map((known) => {
          if (known.witnessScript === undefined) {
            throw new BuilderInvariantError(
              "Inline script source missing witness bytes",
            );
          }
          return known.witnessScript;
        }),
    ),
    redeemerTxWitsPreimageCbor,
    scriptIntegrityHash:
      requiredLanguages.length === 0
        ? EMPTY_NULL_ROOT
        : computeScriptIntegrityHashForLanguages(
            redeemerTxWitsRoot,
            requiredLanguages,
          ),
    mintDelta: mintDeltaAssets(effective),
  };
};

type FeePolicy = {
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
};

const shouldBalance = (options: CompleteOptions): boolean =>
  options.changeAddress !== undefined ||
  options.feePolicy !== undefined ||
  options.maxFeeIterations !== undefined;

const shouldBalanceWithWalletDefault = (
  options: CompleteOptions,
  hasSelectedWallet: boolean,
): boolean => shouldBalance(options) || hasSelectedWallet;

const resolveMaxFeeIterations = (value: number | undefined): number => {
  if (value === undefined) {
    return 20;
  }
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new BuilderInvariantError(
      "maxFeeIterations must be a non-negative safe integer",
    );
  }
  return value;
};

const normalizeFeePolicy = (feePolicy: {
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
}): FeePolicy => ({
  minFeeA: normalizeNonNegativeBigInt(feePolicy.minFeeA, "minFeeA"),
  minFeeB: normalizeNonNegativeBigInt(feePolicy.minFeeB, "minFeeB"),
});

const maxBigInt = (left: bigint, right: bigint): bigint =>
  left > right ? left : right;

const resolveInitialFee = (
  state: BuilderState,
  fee: bigint | number | undefined,
): bigint =>
  maxBigInt(
    normalizeNonNegativeBigInt(fee ?? 0n, "fee"),
    state.minimumFee ?? 0n,
  );

const validationLevel = (
  options: CompleteOptions,
): "none" | LocalPreflightPhase => options.localValidation ?? "none";

const normalizeLocalPreflightPhase = (
  phase: LocalPreflightPhase,
): LocalPreflightPhase => {
  if (phase === "phase-a" || phase === "phase-b") {
    return phase;
  }
  throw new BuilderInvariantError(
    'local preflight phase must be "phase-a" or "phase-b"',
    String(phase),
  );
};

const queuedTxFromComplete = (tx: CompleteTx): QueuedTx => ({
  txId: tx.txId,
  txCbor: tx.txCbor,
  arrivalSeq: 0n,
  createdAt: new Date(0),
});

const localValidationRejected = (
  rejected: readonly RejectedTx[],
): LocalValidationReport["rejected"] =>
  rejected.map((item) => ({
    txId: item.txId.toString("hex"),
    code: item.code,
    detail: item.detail,
  }));

const localValidationReportFromPhaseA = (
  result: PhaseAResult,
  preStateSource?: LocalValidationPreStateSource,
): LocalValidationReport => ({
  phase: "phase-a",
  acceptedTxIds: result.accepted.map((item) => item.txId.toString("hex")),
  rejected: localValidationRejected(result.rejected),
  ...(preStateSource === undefined
    ? {}
    : { preStateSource, preStateAuthoritative: false as const }),
});

const localValidationReportFromPhaseB = (
  result: PhaseBResultWithPatch,
  preStateSource?: LocalValidationPreStateSource,
): LocalValidationReport => ({
  phase: "phase-b",
  acceptedTxIds: result.accepted.map((item) => item.txId.toString("hex")),
  rejected: localValidationRejected(result.rejected),
  ...(preStateSource === undefined
    ? {}
    : { preStateSource, preStateAuthoritative: false as const }),
  statePatch: {
    deletedOutRefs: result.statePatch.deletedOutRefs,
    upsertedOutRefs: result.statePatch.upsertedOutRefs.map(
      ([outRefHex, output]) => [outRefHex, Buffer.from(output).toString("hex")],
    ),
  },
});

const assertAcceptedLocalValidation = (
  report: LocalValidationReport,
  txIdHex: string,
): void => {
  if (
    report.rejected.length === 0 &&
    report.acceptedTxIds.length === 1 &&
    report.acceptedTxIds[0] === txIdHex
  ) {
    return;
  }
  throw new BuilderInvariantError(
    `Local ${report.phase} validation rejected transaction`,
    JSON.stringify(
      {
        expectedTxId: txIdHex,
        acceptedTxIds: report.acceptedTxIds,
        rejected: report.rejected,
      },
      null,
      2,
    ),
  );
};

const runSharedLocalPreflight = async ({
  completed,
  phase,
  provider,
  options,
  missingPreStateMessage,
}: {
  readonly completed: CompleteTx;
  readonly phase: LocalPreflightPhase;
  readonly provider: MidgardProvider;
  readonly options: LocalPreflightOptions;
  readonly missingPreStateMessage: string;
}): Promise<LocalValidationReport> => {
  if (phase === "phase-b" && options.localPreState === undefined) {
    throw new BuilderInvariantError(missingPreStateMessage);
  }

  const params = await provider.getProtocolParameters();
  assertTxNetworkMatchesExpected(completed.tx, params.networkId, "Provider");
  const concurrency = normalizeValidationConcurrency(
    options.validationConcurrency,
    "validationConcurrency",
  );
  const phaseAConfig: PhaseAConfig = {
    expectedNetworkId: params.networkId,
    minFeeA: params.minFeeA,
    minFeeB: params.minFeeB,
    concurrency,
    strictnessProfile: params.strictnessProfile ?? "production",
  };
  const phaseA = await Effect.runPromise(
    runPhaseAValidation([queuedTxFromComplete(completed)], phaseAConfig),
  );
  if (phase === "phase-a" || phaseA.rejected.length > 0) {
    return localValidationReportFromPhaseA(phaseA, options.localPreStateSource);
  }

  const preState = materializeLocalPreState(options.localPreState!);
  const nowCardanoSlotNo =
    options.nowCardanoSlotNo === undefined
      ? (params.currentSlot ?? (await provider.getCurrentSlot()))
      : normalizeNonNegativeBigInt(
          options.nowCardanoSlotNo,
          "nowCardanoSlotNo",
        );
  const phaseBConfig: PhaseBConfig = {
    nowCardanoSlotNo,
    bucketConcurrency: concurrency,
    enforceScriptBudget: options.enforceScriptBudget ?? true,
  };
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(phaseA.accepted, preState, phaseBConfig),
  );
  return localValidationReportFromPhaseB(
    phaseB,
    options.localPreStateSource ?? "explicit",
  );
};

const materializeLocalPreState = (
  preState: LocalValidationPreState,
): Map<string, Buffer> => {
  if (Array.isArray(preState)) {
    return new Map(
      preState.map((entry) => [
        entry[LedgerColumns.OUTREF].toString("hex"),
        Buffer.from(entry[LedgerColumns.OUTPUT]),
      ]),
    );
  }
  const mapPreState = preState as ReadonlyMap<string, Uint8Array>;
  return new Map(
    [...mapPreState.entries()].map(([outRefHex, output]) => [
      outRefHex,
      Buffer.from(output),
    ]),
  );
};

const requiredAssetsWithFee = (outputsTotal: Assets, fee: bigint): Assets =>
  fee === 0n ? outputsTotal : addAssets(outputsTotal, { lovelace: fee });

const utxoAssets = (inputs: readonly MidgardUtxo[]): Assets =>
  sumAssets(inputs.map(utxoOutputAssets));

const markFeeIncluded = (cause: InsufficientFundsError, fee: bigint): never => {
  throw new InsufficientFundsError({
    unit: cause.unit,
    required: cause.required,
    available: cause.available,
    feeIncluded: fee > 0n,
  });
};

const subtractAssetsWithFeeContext = (
  left: Assets,
  right: Assets,
  fee: bigint,
): Assets => {
  try {
    return subtractAssets(left, right);
  } catch (cause) {
    if (cause instanceof InsufficientFundsError) {
      markFeeIncluded(cause, fee);
    }
    throw cause;
  }
};

const trySubtractAssets = (left: Assets, right: Assets): Assets | undefined => {
  try {
    return subtractAssets(left, right);
  } catch (cause) {
    if (cause instanceof InsufficientFundsError) {
      return undefined;
    }
    throw cause;
  }
};

const assetCoverageScore = (
  assets: Assets,
  required: Assets,
): {
  readonly tokenKinds: number;
  readonly tokenQuantity: bigint;
  readonly lovelace: bigint;
} => {
  let tokenKinds = 0;
  let tokenQuantity = 0n;
  for (const [unit, requiredQuantity] of Object.entries(required)) {
    if (unit === "lovelace") {
      continue;
    }
    const available = BigInt(assets[unit] ?? 0n);
    if (available > 0n) {
      tokenKinds += 1;
      tokenQuantity +=
        available < requiredQuantity ? available : requiredQuantity;
    }
  }
  return {
    tokenKinds,
    tokenQuantity,
    lovelace: BigInt(assets.lovelace ?? 0n),
  };
};

const compareUtxosByCoverage = (
  left: MidgardUtxo,
  right: MidgardUtxo,
  required: Assets,
): number => {
  const leftScore = assetCoverageScore(utxoOutputAssets(left), required);
  const rightScore = assetCoverageScore(utxoOutputAssets(right), required);
  if (leftScore.tokenKinds !== rightScore.tokenKinds) {
    return rightScore.tokenKinds - leftScore.tokenKinds;
  }
  if (leftScore.tokenQuantity !== rightScore.tokenQuantity) {
    return leftScore.tokenQuantity > rightScore.tokenQuantity ? -1 : 1;
  }
  if (leftScore.lovelace !== rightScore.lovelace) {
    return leftScore.lovelace > rightScore.lovelace ? -1 : 1;
  }
  return compareOutRefs(left, right);
};

const selectDeterministicInputs = (
  explicitInputs: readonly MidgardUtxo[],
  candidateInputs: readonly MidgardUtxo[],
  required: Assets,
  fee: bigint,
): readonly MidgardUtxo[] => {
  const selected = [...explicitInputs].sort(compareOutRefs);
  const selectedLabels = new Set(selected.map(outRefLabel));
  const candidates = [...candidateInputs]
    .filter((candidate) => !selectedLabels.has(outRefLabel(candidate)))
    .filter((candidate) => !utxoProtectedAddress(candidate))
    .sort((left, right) => compareUtxosByCoverage(left, right, required));

  for (const candidate of candidates) {
    if (trySubtractAssets(utxoAssets(selected), required) !== undefined) {
      break;
    }
    selected.push(candidate);
    selectedLabels.add(outRefLabel(candidate));
  }

  if (trySubtractAssets(utxoAssets(selected), required) === undefined) {
    subtractAssetsWithFeeContext(utxoAssets(selected), required, fee);
  }

  return selected;
};

const assertNoPresetInputOverlap = (
  explicitInputs: readonly MidgardUtxo[],
  presetInputs: readonly MidgardUtxo[],
): void => {
  const explicit = new Set(explicitInputs.map(outRefLabel));
  for (const input of presetInputs) {
    const label = outRefLabel(input);
    if (explicit.has(label)) {
      throw new BuilderInvariantError(
        "Completion preset wallet input duplicates an explicit spend input",
        label,
      );
    }
  }
};

const addChangeOutput = (
  outputs: readonly AuthoredOutput[],
  changeAddress: Address,
  changeAssets: Assets,
): readonly AuthoredOutput[] =>
  isZeroAssets(changeAssets)
    ? outputs.map(cloneOutput)
    : [
        ...outputs.map(cloneOutput),
        authoredOutput({
          kind: "ordinary",
          address: changeAddress,
          value: changeAssets,
        }),
      ];

const buildCanonicalUnsignedTx = (
  state: BuilderState,
  fee: bigint,
  scriptMaterialization = deriveScriptMaterialization(state),
): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: encodeByteListPreimage(
      sortedInputCbors(state.spendInputs),
    ),
    referenceInputsPreimageCbor: encodeByteListPreimage(
      sortedInputCbors(state.referenceInputs),
    ),
    outputsPreimageCbor: encodeByteListPreimage(outputCbors(state.outputs)),
    fee,
    validityIntervalStart:
      state.validityIntervalStart ?? MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: state.validityIntervalEnd ?? MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor:
      scriptMaterialization.requiredObserversPreimageCbor,
    requiredSignersPreimageCbor: encodeByteListPreimage(
      sortedRequiredSignerCbors(state.requiredSigners),
    ),
    mintPreimageCbor: scriptMaterialization.mintPreimageCbor,
    scriptIntegrityHash: scriptMaterialization.scriptIntegrityHash,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: stateNetworkId(state),
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: scriptMaterialization.scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor:
      scriptMaterialization.redeemerTxWitsPreimageCbor,
  },
});

export class TxBuilder {
  readonly pay: PayApi;
  readonly attach: AttachApi;

  constructor(
    private readonly context: BuilderContextSnapshot,
    private readonly state: BuilderState,
    token?: symbol,
  ) {
    if (token !== txBuilderConstructorToken) {
      throw new BuilderInvariantError(
        "TxBuilder constructor is internal; use LucidMidgard.newTx()",
      );
    }
    this.attach = {
      Script: (source) =>
        this.next({
          scripts: {
            ...this.state.scripts,
            scripts: [...this.state.scripts.scripts, cloneScriptSource(source)],
          },
        }),
      NativeScript: (script) =>
        this.attach.Script({
          kind: "native",
          language: "NativeCardano",
          script,
        }),
      SpendingValidator: (validator) =>
        this.attach.Script(
          validatorScriptSource(validator, "SpendingValidator"),
        ),
      MintingPolicy: (policy) =>
        this.attach.Script(validatorScriptSource(policy, "MintingPolicy")),
      ObserverValidator: (validator) =>
        this.attach.Script(
          validatorScriptSource(validator, "ObserverValidator"),
        ),
      ReferenceScriptMetadata: (metadata) =>
        this.attachReferenceScriptMetadata(metadata),
      Datum: (data, hash) => this.attachDatum(data, hash),
    };
    this.pay = {
      ToAddress: (address, value, options) => {
        rejectRuntimeKindOption(options, "pay.ToAddress");
        return this.addOutput(
          authoredOutput({
            kind: "ordinary",
            address,
            value,
            datum: options?.datum,
            scriptRef: options?.scriptRef,
          }),
        );
      },
      ToContract: (address, datum, value, options) => {
        rejectRuntimeKindOption(options, "pay.ToContract");
        return this.addOutput(
          authoredOutput({
            kind: "ordinary",
            address,
            value,
            datum,
            scriptRef: options?.scriptRef,
          }),
        );
      },
      ToProtectedAddress: (address, value, options) => {
        rejectRuntimeKindOption(options, "pay.ToProtectedAddress");
        return this.addOutput(
          authoredOutput({
            kind: "protected",
            address,
            value,
            datum: options?.datum,
            scriptRef: options?.scriptRef,
          }),
        );
      },
    };
  }

  private attachDatum(data: PlutusDataLike, hash?: string): TxBuilder {
    const normalizedData = clonePlutusDataLike(data);
    const datumHash =
      hash ??
      CML.hash_plutus_data(normalizePlutusData(normalizedData)).to_hex();
    const normalizedHash = normalizeHashHex(datumHash, "datum hash", 32);
    if (
      this.state.scripts.datumWitnesses.some(
        (datum) => datum.hash === normalizedHash,
      )
    ) {
      throw new BuilderInvariantError(
        "Duplicate datum witness",
        normalizedHash,
      );
    }
    return this.next({
      scripts: {
        ...this.state.scripts,
        datumWitnesses: [
          ...this.state.scripts.datumWitnesses,
          { data: normalizedData, hash: normalizedHash },
        ],
      },
    });
  }

  private attachReferenceScriptMetadata(
    metadata:
      | TrustedReferenceScriptMetadata
      | readonly TrustedReferenceScriptMetadata[],
  ): TxBuilder {
    const nextMetadata = [
      ...this.state.scripts.referenceScriptMetadata,
      ...normalizeTrustedReferenceScriptMetadataList(metadata),
    ];
    assertNoDuplicateStrings(
      nextMetadata.map((entry) => outRefLabel(entry)),
      "Duplicate trusted reference script metadata",
    );
    return this.next({
      scripts: {
        ...this.state.scripts,
        referenceScriptMetadata: nextMetadata,
      },
    });
  }

  private next(patch: Partial<BuilderState>): TxBuilder {
    const nextState: BuilderState = {
      ...cloneState(this.state),
      ...patch,
      scripts:
        patch.scripts === undefined
          ? cloneScripts(this.state.scripts)
          : cloneScripts(patch.scripts),
    };
    assertUniqueUtxos(nextState.spendInputs, nextState.referenceInputs);
    assertValidityInterval(nextState);
    return makeTxBuilder(this.context, nextState);
  }

  private addOutput(output: AuthoredOutput): TxBuilder {
    assertAddressNetwork(output.address, this.context.config.networkId);
    return this.next({
      outputs: [...this.state.outputs.map(cloneOutput), cloneOutput(output)],
    });
  }

  collectFrom(utxos: readonly MidgardUtxo[], redeemer?: Redeemer): TxBuilder {
    const normalizedUtxos = utxos.map((utxo) =>
      normalizeBuilderInputUtxo(utxo, this.context),
    );
    const spendRedeemers =
      redeemer === undefined
        ? this.state.scripts.spendRedeemers
        : [
            ...this.state.scripts.spendRedeemers,
            ...normalizedUtxos.map((utxo) => ({
              txHash: utxo.txHash,
              outputIndex: utxo.outputIndex,
              redeemer: cloneRedeemer(redeemer),
            })),
          ];
    return this.next({
      spendInputs: [...this.state.spendInputs, ...normalizedUtxos],
      scripts: {
        ...this.state.scripts,
        spendRedeemers,
      },
    });
  }

  readFrom(
    utxos: readonly MidgardUtxo[],
    options: ReadFromOptions = {},
  ): TxBuilder {
    const normalizedUtxos = utxos.map((utxo) =>
      normalizeBuilderInputUtxo(utxo, this.context),
    );
    const trustedReferenceScripts =
      options.trustedReferenceScripts === undefined
        ? []
        : normalizeTrustedReferenceScriptMetadataList(
            options.trustedReferenceScripts,
          );
    if (trustedReferenceScripts.length > 0) {
      const referenceLabels = new Set(normalizedUtxos.map(outRefLabel));
      for (const metadata of trustedReferenceScripts) {
        if (!referenceLabels.has(outRefLabel(metadata))) {
          throw new BuilderInvariantError(
            "readFrom trusted reference script metadata must match a supplied reference input",
            outRefLabel(metadata),
          );
        }
      }
    }
    const nextReferenceScriptMetadata = [
      ...this.state.scripts.referenceScriptMetadata,
      ...trustedReferenceScripts,
    ];
    assertNoDuplicateStrings(
      nextReferenceScriptMetadata.map((entry) => outRefLabel(entry)),
      "Duplicate trusted reference script metadata",
    );
    return this.next({
      referenceInputs: [...this.state.referenceInputs, ...normalizedUtxos],
      scripts: {
        ...this.state.scripts,
        referenceScriptMetadata: nextReferenceScriptMetadata,
      },
    });
  }

  mintAssets(policyId: string, assets: Assets, redeemer?: Redeemer): TxBuilder {
    const normalizedPolicy = normalizePolicyId(policyId);
    return this.next({
      scripts: {
        ...this.state.scripts,
        mints: [
          ...this.state.scripts.mints,
          {
            policyId: normalizedPolicy,
            assets: normalizeMintAssetsForPolicy(normalizedPolicy, assets),
            redeemer:
              redeemer === undefined ? undefined : cloneRedeemer(redeemer),
          },
        ],
      },
    });
  }

  mint(mints: MintAssets, redeemer?: Redeemer): TxBuilder {
    return Object.entries(mints).reduce<TxBuilder>(
      (builder, [policyId, assets]) =>
        builder.mintAssets(policyId, assets, redeemer),
      this,
    );
  }

  attachObserverScript(validator: ObserverValidator): TxBuilder {
    return this.attach.ObserverValidator(validator);
  }

  observe(scriptHash: string, redeemer?: Redeemer): TxBuilder {
    const normalized = normalizeScriptHash(scriptHash, "observer script hash");
    if (
      this.state.scripts.observers.some(
        (observer) =>
          normalizeScriptHash(observer.scriptHash, "observer script hash") ===
          normalized,
      )
    ) {
      throw new BuilderInvariantError("Duplicate observer intent", normalized);
    }
    return this.next({
      scripts: {
        ...this.state.scripts,
        observers: [
          ...this.state.scripts.observers,
          {
            scriptHash: normalized,
            redeemer:
              redeemer === undefined ? undefined : cloneRedeemer(redeemer),
          },
        ],
      },
    });
  }

  receiveRedeemer(scriptHash: string, redeemer: Redeemer): TxBuilder {
    const normalized = normalizeScriptHash(scriptHash, "receive script hash");
    if (
      this.state.scripts.receiveRedeemers.some(
        (entry) =>
          normalizeScriptHash(entry.scriptHash, "receive script hash") ===
          normalized,
      )
    ) {
      throw new BuilderInvariantError("Duplicate receive redeemer", normalized);
    }
    return this.next({
      scripts: {
        ...this.state.scripts,
        receiveRedeemers: [
          ...this.state.scripts.receiveRedeemers,
          {
            scriptHash: normalized,
            redeemer: cloneRedeemer(redeemer),
          },
        ],
      },
    });
  }

  addSigner(keyHashOrAddress: string): TxBuilder {
    if (!/^[0-9a-fA-F]{56}$/.test(keyHashOrAddress.trim())) {
      assertAddressNetwork(keyHashOrAddress, this.context.config.networkId);
    }
    const signer = normalizeSigner(keyHashOrAddress);
    const requiredSigners = [...this.state.requiredSigners, signer];
    if (new Set(requiredSigners).size !== requiredSigners.length) {
      throw new BuilderInvariantError("Duplicate required signer", signer);
    }
    return this.next({ requiredSigners });
  }

  addSignerKey(keyHash: string): TxBuilder {
    const normalized = normalizeHashHex(
      keyHash,
      "required signer key hash",
      28,
    );
    return this.addSigner(normalized);
  }

  setMinFee(fee: bigint | number): TxBuilder {
    return this.next({
      minimumFee: normalizeNonNegativeBigInt(fee, "minimumFee"),
    });
  }

  validFrom(slotOrPosix: bigint | number): TxBuilder {
    return this.next({
      validityIntervalStart: normalizeNonNegativeBigInt(
        slotOrPosix,
        "validityIntervalStart",
      ),
    });
  }

  validTo(slotOrPosix: bigint | number): TxBuilder {
    return this.next({
      validityIntervalEnd: normalizeNonNegativeBigInt(
        slotOrPosix,
        "validityIntervalEnd",
      ),
    });
  }

  debugSnapshot(): BuilderSnapshot {
    return {
      ...cloneState(this.state),
      providerGeneration: this.context.provider.generation,
      utxoOverrideGeneration: this.context.utxoOverrides?.generation,
      hasUtxoOverrides: this.context.utxoOverrides !== undefined,
    };
  }

  snapshot(): BuilderSnapshot {
    return this.debugSnapshot();
  }

  config(): LucidMidgardConfigSnapshot {
    return this.context.config;
  }

  rawConfig(): MidgardProtocolInfo {
    return cloneProtocolInfo(this.context.provider.protocolInfo);
  }

  compose(other: TxBuilder, ...others: readonly TxBuilder[]): TxBuilder {
    const fragments = [other, ...others];
    for (const fragment of fragments) {
      assertBuilderContextsComposable(this.context, fragment.context);
    }
    return makeTxBuilder(
      this.context,
      composeStates([
        this.state,
        ...fragments.map((fragment) => fragment.state),
      ]),
    );
  }

  private async resolveChangeAddress(
    options: CompleteOptions,
  ): Promise<Address> {
    const changeAddress =
      options.changeAddress ?? (await this.context.wallet?.address());
    if (changeAddress === undefined) {
      throw new BuilderInvariantError(
        "Balancing requires a change address or a selected wallet",
      );
    }
    assertAddressNetwork(changeAddress, this.context.config.networkId);
    return changeAddress;
  }

  private async resolveFeePolicy(
    option: CompleteOptions["feePolicy"],
  ): Promise<FeePolicy> {
    if (option !== undefined && option !== "provider") {
      return normalizeFeePolicy(option);
    }
    const params = await this.context.provider.provider.getProtocolParameters();
    return normalizeFeePolicy({
      minFeeA: params.minFeeA,
      minFeeB: params.minFeeB,
    });
  }

  private async runLocalValidation(
    completed: CompleteTx,
    options: CompleteOptions,
  ): Promise<LocalValidationReport | undefined> {
    const level = validationLevel(options);
    if (level === "none") {
      return undefined;
    }
    const report = await runSharedLocalPreflight({
      completed,
      phase: level,
      provider: this.context.provider.provider,
      options,
      missingPreStateMessage:
        'complete({ localValidation: "phase-b" }) requires localPreState',
    });
    assertAcceptedLocalValidation(report, completed.txIdHex);
    return report;
  }

  private async finalizeCompleteTx(
    tx: MidgardNativeTxFull,
    metadata: Omit<CompleteTxMetadata, "localValidation">,
    options: CompleteOptions,
  ): Promise<CompleteTx> {
    const context: CompleteTxContext = {
      provider: this.context.provider.provider,
      wallet: () => this.context.wallet,
      networkId: configNetworkId(this.context.config),
      maxSubmitTxCborBytes:
        this.context.config.submissionLimits.maxSubmitTxCborBytes,
    };
    const enrichedMetadata: CompleteTxMetadata = {
      ...metadata,
      providerGeneration: this.context.provider.generation,
      providerDiagnostics: cloneProviderDiagnostics(
        this.context.provider.diagnostics,
      ),
    };
    const completed = makeCompleteTx(tx, enrichedMetadata, context);
    const localValidation = await this.runLocalValidation(completed, options);
    if (localValidation === undefined) {
      return completed;
    }
    return makeCompleteTx(
      tx,
      { ...enrichedMetadata, localValidation },
      context,
    );
  }

  private async resolveWalletInputs(
    state: BuilderState,
    changeAddress: Address,
    options: CompleteOptions,
  ): Promise<ResolvedWalletInputs> {
    const referenceLabels = new Set(state.referenceInputs.map(outRefLabel));
    const normalizeCandidates = (
      inputs: readonly MidgardUtxo[],
      source: WalletInputSource,
    ): readonly MidgardUtxo[] =>
      normalizeWalletInputUtxos(
        inputs,
        source,
        this.context.config.networkId,
      ).filter((utxo) => !referenceLabels.has(outRefLabel(utxo)));

    if (options.presetWalletInputs !== undefined) {
      const inputs = normalizeCandidates(
        options.presetWalletInputs,
        "completion-preset",
      );
      assertNoPresetInputOverlap(state.spendInputs, inputs);
      await assertWalletOwnsInputs(
        this.context.wallet,
        inputs,
        "completion-preset",
      );
      return { source: "completion-preset", inputs };
    }

    if (this.context.utxoOverrides !== undefined) {
      const inputs = normalizeCandidates(
        this.context.utxoOverrides.utxos,
        "instance-override",
      );
      await assertWalletOwnsInputs(
        this.context.wallet,
        inputs,
        "instance-override",
      );
      return {
        source: "instance-override",
        inputs,
        overrideGeneration: this.context.utxoOverrides.generation,
      };
    }

    const fetchedUtxos =
      await this.context.provider.provider.getUtxos(changeAddress);
    return {
      source: "provider",
      inputs: fetchedUtxos
        .map(normalizeUtxo)
        .filter((utxo) => !referenceLabels.has(outRefLabel(utxo))),
    };
  }

  private async completeBalanced(
    state: BuilderState,
    options: CompleteOptions,
    resolved?: BalancedCompletionInputs,
  ): Promise<CompleteTx> {
    const maxFeeIterations = resolveMaxFeeIterations(options.maxFeeIterations);
    const changeAddress =
      resolved?.changeAddress ?? (await this.resolveChangeAddress(options));
    const feePolicy =
      resolved?.feePolicy ?? (await this.resolveFeePolicy(options.feePolicy));
    const walletInputs =
      resolved?.walletInputs ??
      (await this.resolveWalletInputs(state, changeAddress, options));
    const candidateInputs = walletInputs.inputs;
    const outputsTotal = sumAssets(
      state.outputs.map((output) => output.assets),
    );
    const mintDelta = mintDeltaAssets(state.scripts.mints);
    const mint = splitSignedAssets(mintDelta);
    let selectedInputs = [...state.spendInputs].sort(compareOutRefs);
    let fee = resolveInitialFee(state, options.fee);

    for (let iteration = 1; iteration <= maxFeeIterations; iteration += 1) {
      const required = subtractAssetsFloor(
        addAssets(requiredAssetsWithFee(outputsTotal, fee), mint.negative),
        mint.positive,
      );
      selectedInputs = [
        ...selectDeterministicInputs(
          selectedInputs,
          candidateInputs,
          required,
          fee,
        ),
      ];
      const sides = balanceSides({
        inputs: utxoAssets(selectedInputs),
        outputs: outputsTotal,
        fee,
        mintDelta,
      });
      const changeAssets = subtractAssetsWithFeeContext(
        sides.available,
        sides.required,
        fee,
      );
      const outputs = addChangeOutput(
        state.outputs,
        changeAddress,
        changeAssets,
      );
      const candidateState: BuilderState = {
        ...state,
        spendInputs: selectedInputs.map(cloneUtxo),
        outputs,
      };
      const candidateCanonical = buildCanonicalUnsignedTx(candidateState, fee);
      const candidateTx =
        materializeMidgardNativeTxFromCanonical(candidateCanonical);
      const candidateTxBytes = encodeMidgardNativeTxFull(candidateTx);
      const expectedWitnessKeyHashes =
        expectedAddrWitnessKeyHashes(candidateState);
      const expectedAddrWitnessCount = expectedWitnessKeyHashes.length;
      const estimatedSignedLength = estimatedSignedTxByteLength(
        candidateTx,
        expectedAddrWitnessCount,
      );
      const nextFee = maxBigInt(
        feePolicy.minFeeA * BigInt(estimatedSignedLength) + feePolicy.minFeeB,
        state.minimumFee ?? 0n,
      );
      if (nextFee === fee) {
        const changeOutputIndex = isZeroAssets(changeAssets)
          ? undefined
          : state.outputs.length;
        return this.finalizeCompleteTx(
          candidateTx,
          {
            fee,
            inputCount: candidateState.spendInputs.length,
            referenceInputCount: candidateState.referenceInputs.length,
            outputCount: candidateState.outputs.length,
            requiredSignerCount: candidateState.requiredSigners.length,
            txByteLength: candidateTxBytes.length,
            feeIterations: iteration,
            balanced: true,
            changeAddress,
            changeAssets,
            changeOutputIndex,
            expectedAddrWitnessCount,
            expectedAddrWitnessKeyHashes: expectedWitnessKeyHashes,
            estimatedSignedTxByteLength: estimatedSignedLength,
            walletInputSource: walletInputs.source,
            walletInputCount: candidateInputs.length,
            utxoOverrideGeneration: walletInputs.overrideGeneration,
          },
          options,
        );
      }
      fee = nextFee;
    }

    throw new BuilderInvariantError(
      "Fee convergence failed",
      `maxFeeIterations=${maxFeeIterations.toString()}`,
    );
  }

  async complete(options: CompleteOptions = {}): Promise<CompleteTx> {
    const state = cloneState(this.state);
    assertUniqueUtxos(state.spendInputs, state.referenceInputs);
    assertValidityInterval(state);
    const balanceRequested = shouldBalanceWithWalletDefault(
      options,
      this.context.wallet !== undefined,
    );
    if (state.spendInputs.length === 0) {
      if (!balanceRequested) {
        throw new BuilderInvariantError(
          "Cannot complete a transaction with no spend inputs",
        );
      }
    }
    if (balanceRequested) {
      return this.completeBalanced(state, options);
    }

    const fee = resolveInitialFee(state, options.fee);
    const scriptMaterialization = deriveScriptMaterialization(state);
    const inputTotal = sumAssets(state.spendInputs.map(utxoOutputAssets));
    const outputTotal = sumAssets(state.outputs.map((output) => output.assets));
    assertBalancedWithoutChange(
      inputTotal,
      outputTotal,
      fee,
      scriptMaterialization.mintDelta,
    );

    const canonical = buildCanonicalUnsignedTx(
      state,
      fee,
      scriptMaterialization,
    );
    const tx = materializeMidgardNativeTxFromCanonical(canonical);
    verifyMidgardNativeTxFullConsistency(tx);
    const txCbor = encodeMidgardNativeTxFull(tx);
    const expectedWitnessKeyHashes = expectedAddrWitnessKeyHashes(state);
    const expectedAddrWitnessCount = expectedWitnessKeyHashes.length;
    if (
      !computeHash32(tx.body.spendInputsPreimageCbor).equals(
        tx.body.spendInputsRoot,
      )
    ) {
      throw new BuilderInvariantError(
        "spend input root/preimage consistency check failed",
      );
    }

    return this.finalizeCompleteTx(
      tx,
      {
        fee,
        inputCount: state.spendInputs.length,
        referenceInputCount: state.referenceInputs.length,
        outputCount: state.outputs.length,
        requiredSignerCount: state.requiredSigners.length,
        txByteLength: txCbor.length,
        feeIterations: 0,
        balanced: false,
        expectedAddrWitnessCount,
        expectedAddrWitnessKeyHashes: expectedWitnessKeyHashes,
        estimatedSignedTxByteLength: estimatedSignedTxByteLength(
          tx,
          expectedAddrWitnessCount,
        ),
      },
      options,
    );
  }

  completeProgram(options: CompleteOptions = {}): MidgardEffect<CompleteTx> {
    return midgardProgram(() => this.complete(options));
  }

  completeSafe(
    options: CompleteOptions = {},
  ): Promise<MidgardResult<CompleteTx, LucidMidgardError>> {
    return midgardSafe(() => this.complete(options));
  }

  private async resolveBalancedChainInputs(
    state: BuilderState,
    options: CompleteOptions,
  ): Promise<BalancedCompletionInputs> {
    const changeAddress = await this.resolveChangeAddress(options);
    const [feePolicy, walletInputs] = await Promise.all([
      this.resolveFeePolicy(options.feePolicy),
      this.resolveWalletInputs(state, changeAddress, options),
    ]);
    return { changeAddress, feePolicy, walletInputs };
  }

  private async explicitChainBaseWalletInputs(
    state: BuilderState,
    options: CompleteOptions,
  ): Promise<readonly MidgardUtxo[]> {
    if (this.context.wallet === undefined) {
      return [];
    }
    const referenceLabels = new Set(state.referenceInputs.map(outRefLabel));
    const normalizeBase = async (
      inputs: readonly MidgardUtxo[],
      source: WalletInputSource,
    ): Promise<readonly MidgardUtxo[]> => {
      const normalized = normalizeWalletInputUtxos(
        inputs,
        source,
        this.context.config.networkId,
      ).filter((utxo) => !referenceLabels.has(outRefLabel(utxo)));
      await assertWalletOwnsInputs(this.context.wallet, normalized, source);
      return normalized;
    };
    if (options.presetWalletInputs !== undefined) {
      return normalizeBase(options.presetWalletInputs, "completion-preset");
    }
    if (this.context.utxoOverrides !== undefined) {
      return normalizeBase(
        this.context.utxoOverrides.utxos,
        "instance-override",
      );
    }
    return [];
  }

  private async newWalletUtxosAfterChain(
    completed: CompleteTx,
    baseWalletInputs: readonly MidgardUtxo[],
  ): Promise<readonly MidgardUtxo[]> {
    if (this.context.wallet === undefined) {
      return [];
    }
    const walletKeyHash = await this.context.wallet.keyHash();
    const spent = new Set(nativeInputOutRefs(completed.tx).map(outRefLabel));
    const unspent = baseWalletInputs.filter(
      (utxo) => !spent.has(outRefLabel(utxo)),
    );
    const producedForWallet = completed
      .producedOutputs()
      .filter((utxo) => paymentPubKeyHashFromUtxo(utxo) === walletKeyHash);
    return [
      ...unspent.map(cloneUtxo),
      ...producedForWallet.map(cloneUtxo),
    ].sort(compareOutRefs);
  }

  async chain(options: ChainOptions = {}): Promise<ChainResult> {
    const state = cloneState(this.state);
    assertUniqueUtxos(state.spendInputs, state.referenceInputs);
    assertValidityInterval(state);
    const balanceRequested = shouldBalanceWithWalletDefault(
      options,
      this.context.wallet !== undefined,
    );
    if (state.spendInputs.length === 0 && !balanceRequested) {
      throw new BuilderInvariantError(
        "Cannot chain a transaction with no spend inputs",
      );
    }

    const balancedInputs = balanceRequested
      ? await this.resolveBalancedChainInputs(state, options)
      : undefined;
    const completed =
      balancedInputs === undefined
        ? await this.complete(options)
        : await this.completeBalanced(state, options, balancedInputs);
    const baseWalletInputs =
      balancedInputs?.walletInputs.inputs ??
      (await this.explicitChainBaseWalletInputs(state, options));
    const derivedOutputs = completed.producedOutputs();
    const newWalletUtxos = await this.newWalletUtxosAfterChain(
      completed,
      baseWalletInputs,
    );
    return [newWalletUtxos, derivedOutputs, completed] as const;
  }

  chainProgram(options: ChainOptions = {}): MidgardEffect<ChainResult> {
    return midgardProgram(() => this.chain(options));
  }

  chainSafe(
    options: ChainOptions = {},
  ): Promise<MidgardResult<ChainResult, LucidMidgardError>> {
    return midgardSafe(() => this.chain(options));
  }
}

const makeTxBuilder = (
  context: BuilderContextSnapshot,
  state: BuilderState,
): TxBuilder => new TxBuilder(context, state, txBuilderConstructorToken);

const readOnlyWalletFromAddress = (
  address: Address,
  expectedNetworkId: number | undefined,
): MidgardWallet => {
  assertAddressNetwork(address, expectedNetworkId);
  const keyHash = paymentKeyHashFromAddress(address);
  return {
    address: async () => address,
    keyHash: async () => keyHash,
    signBodyHash: async () => {
      throw new SigningError(
        "Read-only Midgard wallet cannot sign body hashes",
      );
    },
  };
};

const normalizeLucidMidgardConfig = (
  networkOrConfig?: Network | LucidMidgardConfig,
): LucidMidgardConfig =>
  typeof networkOrConfig === "string"
    ? { network: networkOrConfig }
    : { ...(networkOrConfig ?? {}) };

const networkForSeedWallet = (network: string | undefined): Network => {
  if (network === "Mainnet" || network === "Preprod" || network === "Preview") {
    return network;
  }
  throw new BuilderInvariantError(
    "Selecting a seed wallet requires a known Cardano network",
    network ?? "undefined",
  );
};

const validateSelectedWalletForConfig = async (
  wallet: MidgardWallet | undefined,
  config: LucidMidgardConfigSnapshot,
): Promise<void> => {
  if (wallet === undefined || config.networkId === undefined) {
    return;
  }
  try {
    assertAddressNetwork(await wallet.address(), config.networkId);
  } catch (cause) {
    if (
      cause instanceof SigningError &&
      cause.message.includes("does not expose an address")
    ) {
      return;
    }
    throw cause;
  }
};

export class LucidMidgard {
  selectedWallet: MidgardWallet | undefined;
  #providerSnapshot: ProviderSnapshot;
  #config: LucidMidgardConfigSnapshot;
  #baseConfig: LucidMidgardConfig;
  #utxoOverrideGeneration = 0;
  #utxoOverrides: UtxoOverrideSnapshot | undefined;

  readonly selectWallet = {
    fromSeed: (seedPhrase: string): LucidMidgard => {
      this.selectedWallet = walletFromSeedPhrase(seedPhrase, {
        network: networkForSeedWallet(this.#config.network),
        expectedNetworkId: this.#config.networkId,
      });
      return this;
    },
    fromPrivateKey: (
      privateKey: PrivateKey | string,
      address: Address,
    ): LucidMidgard => {
      this.selectedWallet = walletFromPrivateKey(privateKey, address, {
        expectedNetworkId: this.#config.networkId,
      });
      return this;
    },
    fromExternalSigner: (signer: ExternalBodyHashSigner): LucidMidgard => {
      this.selectedWallet = walletFromExternalSigner(signer, {
        expectedNetworkId: this.#config.networkId,
      });
      return this;
    },
    fromAddress: (
      address: Address,
      utxos?: readonly MidgardUtxo[],
    ): LucidMidgard => {
      const wallet = readOnlyWalletFromAddress(address, this.#config.networkId);
      let normalizedOverrides: readonly MidgardUtxo[] | undefined;
      if (utxos !== undefined) {
        const normalized = normalizeWalletInputUtxos(
          utxos,
          "instance-override",
          this.#config.networkId,
        );
        for (const utxo of normalized) {
          if (utxoAddress(utxo) !== address) {
            throw new BuilderInvariantError(
              "fromAddress UTxO does not belong to the selected address",
              outRefLabel(utxo),
            );
          }
        }
        normalizedOverrides = normalized;
      }
      this.selectedWallet = wallet;
      if (normalizedOverrides !== undefined) {
        this.setUtxoOverrides(normalizedOverrides);
      }
      return this;
    },
  };

  private constructor({
    providerSnapshot,
    configSnapshot,
    baseConfig,
  }: {
    readonly providerSnapshot: ProviderSnapshot;
    readonly configSnapshot: LucidMidgardConfigSnapshot;
    readonly baseConfig: LucidMidgardConfig;
  }) {
    this.#providerSnapshot = providerSnapshot;
    this.#config = configSnapshot;
    this.#baseConfig = baseConfig;
  }

  static async new(
    provider: MidgardProvider,
    networkOrConfig?: Network | LucidMidgardConfig,
  ): Promise<LucidMidgard> {
    const baseConfig = normalizeLucidMidgardConfig(networkOrConfig);
    const { snapshot, config } = await readProviderSnapshot({
      provider,
      generation: 0,
      config: baseConfig,
    });
    return new LucidMidgard({
      providerSnapshot: snapshot,
      configSnapshot: config,
      baseConfig,
    });
  }

  get provider(): MidgardProvider {
    return this.#providerSnapshot.provider;
  }

  config(): LucidMidgardConfigSnapshot {
    return this.#config;
  }

  private refreshConfigSnapshot(): void {
    this.#config = buildConfigSnapshot({
      input: this.#baseConfig,
      protocolInfo: this.#providerSnapshot.protocolInfo,
      diagnostics: this.#providerSnapshot.diagnostics,
      providerGeneration: this.#providerSnapshot.generation,
      utxoOverrideGeneration: this.#utxoOverrideGeneration,
      hasUtxoOverrides: this.#utxoOverrides !== undefined,
    });
  }

  private setUtxoOverrides(utxos: readonly MidgardUtxo[]): void {
    this.#utxoOverrideGeneration += 1;
    this.#utxoOverrides = {
      generation: this.#utxoOverrideGeneration,
      utxos: utxos.map(cloneUtxo),
    };
    this.refreshConfigSnapshot();
  }

  async switchProvider(
    provider: MidgardProvider,
    options: SwitchProviderOptions = {},
  ): Promise<LucidMidgard> {
    const nextGeneration = this.#providerSnapshot.generation + 1;
    const { snapshot, config } = await readProviderSnapshot({
      provider,
      generation: nextGeneration,
      config: this.#baseConfig,
      currentConfig: this.#config,
      options,
    });
    await validateSelectedWalletForConfig(this.selectedWallet, config);
    this.#providerSnapshot = snapshot;
    this.#config = buildConfigSnapshot({
      input: this.#baseConfig,
      protocolInfo: snapshot.protocolInfo,
      diagnostics: snapshot.diagnostics,
      providerGeneration: snapshot.generation,
      utxoOverrideGeneration: this.#utxoOverrideGeneration,
      hasUtxoOverrides: this.#utxoOverrides !== undefined,
    });
    return this;
  }

  overrideUTxOs(utxos: readonly MidgardUtxo[]): LucidMidgard {
    this.setUtxoOverrides(
      normalizeWalletInputUtxos(
        utxos,
        "instance-override",
        this.#config.networkId,
      ),
    );
    return this;
  }

  clearUTxOOverrides(): LucidMidgard {
    this.#utxoOverrideGeneration += 1;
    this.#utxoOverrides = undefined;
    this.refreshConfigSnapshot();
    return this;
  }

  wallet(): MidgardWallet {
    if (this.selectedWallet === undefined) {
      throw new BuilderInvariantError("No Midgard wallet selected");
    }
    return this.selectedWallet;
  }

  txStatus(txId: string): Promise<TxStatus> {
    const normalizedTxId = normalizeTxIdHex(txId);
    return this.provider
      .getTxStatus(normalizedTxId)
      .then((status) => assertTxStatusMatches(normalizedTxId, status));
  }

  txStatusProgram(txId: string): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.txStatus(txId));
  }

  txStatusSafe(
    txId: string,
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.txStatus(txId));
  }

  currentSlot(): Promise<bigint> {
    return this.provider.getCurrentSlot();
  }

  async utxosAt(address: Address): Promise<readonly MidgardUtxo[]> {
    const endpoint = "/utxos";
    const normalizedAddress = normalizeAddressQuery(
      address,
      this.#config.networkId,
      endpoint,
    );
    const utxos = normalizeProviderUtxos(
      await this.provider.getUtxos(normalizedAddress),
      endpoint,
    );
    for (const utxo of utxos) {
      if (utxoAddress(utxo) !== normalizedAddress) {
        throw new ProviderPayloadError(
          endpoint,
          "Provider returned a UTxO for a different address",
          `expected=${normalizedAddress} actual=${utxoAddress(utxo)}`,
        );
      }
    }
    return [...utxos].sort(compareOutRefs).map(cloneUtxo);
  }

  async utxosAtWithUnit(
    address: Address,
    unit: AssetUnit,
  ): Promise<readonly MidgardUtxo[]> {
    const normalizedUnit = normalizeAssetUnit(unit);
    return (await this.utxosAt(address)).filter(
      (utxo) => assetQuantity(utxoOutputAssets(utxo), normalizedUnit) > 0n,
    );
  }

  utxosByOutRef(
    outRefs: readonly OutRef[],
    options?: UtxosByOutRefOptions,
  ): Promise<readonly MidgardUtxo[]> {
    return orderedUtxosByOutRef(this.provider, outRefs, options);
  }

  async utxoByUnit(unit: AssetUnit): Promise<MidgardUtxo> {
    const endpoint = "/utxo-by-unit";
    const normalizedUnit = normalizeAssetUnit(unit);
    if (this.provider.getUtxosByUnit === undefined) {
      throw new ProviderCapabilityError(
        endpoint,
        "Midgard provider does not expose a native unit index",
      );
    }
    const hits = [
      ...normalizeProviderUtxos(
        await this.provider.getUtxosByUnit(normalizedUnit),
        endpoint,
      ),
    ].sort(compareOutRefs);
    for (const hit of hits) {
      if (assetQuantity(utxoOutputAssets(hit), normalizedUnit) <= 0n) {
        throw new ProviderPayloadError(
          endpoint,
          "Provider unit index returned a UTxO without the requested unit",
          outRefLabel(hit),
        );
      }
    }
    if (hits.length === 0) {
      throw new ProviderPayloadError(
        endpoint,
        "No UTxO found for requested unit",
        normalizedUnit,
      );
    }
    if (hits.length > 1) {
      throw new ProviderPayloadError(
        endpoint,
        "Multiple UTxOs found for requested unit",
        normalizedUnit,
      );
    }
    return cloneUtxo(hits[0]!);
  }

  async datumOf(utxo: MidgardUtxo, options?: DatumOfOptions): Promise<Buffer> {
    return inlineDatumFromUtxo(utxo, options);
  }

  awaitTx(txId: string, options: AwaitTxOptions = {}): Promise<TxStatus> {
    return pollTxStatus(
      options.provider ?? this.provider,
      normalizeTxIdHex(txId),
      options,
    );
  }

  awaitTxProgram(
    txId: string,
    options: AwaitTxOptions = {},
  ): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.awaitTx(txId, options));
  }

  awaitTxSafe(
    txId: string,
    options: AwaitTxOptions = {},
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.awaitTx(txId, options));
  }

  private completeTxContext(): CompleteTxContext {
    return {
      provider: this.#providerSnapshot.provider,
      wallet: () => this.selectedWallet,
      networkId: configNetworkId(this.#config),
      maxSubmitTxCborBytes: this.#config.submissionLimits.maxSubmitTxCborBytes,
    };
  }

  newTx(): TxBuilder {
    const providerSnapshot = {
      ...this.#providerSnapshot,
      protocolInfo: cloneProtocolInfo(this.#providerSnapshot.protocolInfo),
      diagnostics: cloneProviderDiagnostics(this.#providerSnapshot.diagnostics),
    };
    return makeTxBuilder(
      {
        provider: providerSnapshot,
        wallet: this.selectedWallet,
        config: this.#config,
        utxoOverrides: cloneUtxoOverrideSnapshot(this.#utxoOverrides),
      },
      emptyState(configNetworkId(this.#config)),
    );
  }

  fromTx(
    input: FromTxInput,
    options: FromTxOptions & { readonly partial: true },
  ): PartiallySignedTx;
  fromTx(input: FromTxInput, options?: FromTxOptions): CompleteTx;
  fromTx(
    input: FromTxInput,
    options: FromTxOptions = {},
  ): CompleteTx | PartiallySignedTx {
    const expectedNetworkId = configNetworkId(this.#config);
    if (input instanceof CompleteTx) {
      const tx = input.tx;
      assertTxNetworkMatchesExpected(
        tx,
        expectedNetworkId,
        "Imported transaction",
      );
      const metadata = {
        ...importedTxMetadata(tx, options),
        providerGeneration: this.#providerSnapshot.generation,
        providerDiagnostics: cloneProviderDiagnostics(
          this.#providerSnapshot.diagnostics,
        ),
      };
      return options.partial === true
        ? makePartiallySignedTx(tx, metadata, this.completeTxContext())
        : makeCompleteTx(tx, metadata, this.completeTxContext());
    }
    const tx = decodeFromTxInput(input);
    assertTxNetworkMatchesExpected(
      tx,
      expectedNetworkId,
      "Imported transaction",
    );
    const metadata = {
      ...importedTxMetadata(tx, options),
      providerGeneration: this.#providerSnapshot.generation,
      providerDiagnostics: cloneProviderDiagnostics(
        this.#providerSnapshot.diagnostics,
      ),
    };
    return options.partial === true
      ? makePartiallySignedTx(tx, metadata, this.completeTxContext())
      : makeCompleteTx(tx, metadata, this.completeTxContext());
  }

  async walletAddress(): Promise<Address> {
    return this.wallet().address();
  }
}
