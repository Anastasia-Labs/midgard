import { readFile, realpath } from "node:fs/promises";
import { isAbsolute } from "node:path";

import {
  CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY_V1,
  classifyProductionHeaderV1,
  COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY_V1,
  type CompleteCanonicalReplayContextV1,
  createCanonicalDecodabilityProductionWorkflowRunnerV1,
  createCommittedFieldShapeProductionWorkflowRunnerV1,
  createCompleteCanonicalReplayUnionV1,
  createDaHashPreimageProductionWorkflowRunnerV1,
  createDoubleSpendProductionWorkflowRunnerV1,
  createDoubleWithdrawProductionWorkflowRunnerV1,
  createFabricatedDepositProductionWorkflowRunnerV1,
  createFabricatedWithdrawalProductionWorkflowRunnerV1,
  createHttpStateQueueMutationLeaseCoordinator,
  createInputNoIdxProductionWorkflowRunnerV1,
  createInputSetUniquenessProductionWorkflowRunnerV1,
  createInvalidRangeProductionWorkflowRunnerV1,
  createInvalidSignatureProductionWorkflowRunnerV1,
  createL2TxMistagProductionWorkflowRunnerV1,
  createManifestBoundCanonicalDecodabilityWorkflowV1,
  createManifestBoundCommittedFieldShapeWorkflowV1,
  createManifestBoundDaHashPreimageWorkflowV1,
  createManifestBoundDoubleSpendWorkflowV1,
  createManifestBoundDoubleWithdrawWorkflowV1,
  createManifestBoundFabricatedDepositWorkflowV1,
  createManifestBoundFabricatedWithdrawalWorkflowV1,
  createManifestBoundInputNoIdxWorkflowV1,
  createManifestBoundInputSetUniquenessWorkflowV1,
  createManifestBoundInvalidRangeWorkflowV1,
  createManifestBoundInvalidSignatureWorkflowV1,
  createManifestBoundL2TxMistagWorkflowV1,
  createManifestBoundMinAdaWorkflowV1,
  createManifestBoundMinFeeWorkflowV1,
  createManifestBoundMissingNativeScriptTxWorkflowV1,
  createManifestBoundMissingNativeScriptUtxoWorkflowV1,
  createManifestBoundMissingSignatureWorkflowV1,
  createManifestBoundNativeScriptInvalidWorkflowV1,
  createManifestBoundNetworkIdWorkflowV1,
  createManifestBoundNonExistentInputWorkflowV1,
  createManifestBoundNoReferenceInputWorkflowV1,
  createManifestBoundReferenceInputNoIdxWorkflowV1,
  createManifestBoundWithdrawnInputWorkflowV1,
  createManifestBoundWithdrawnReferenceInputWorkflowV1,
  createManifestBoundZeroInputWorkflowV1,
  createMinAdaProductionWorkflowRunnerV1,
  createMinFeeProductionWorkflowRunnerV1,
  createMissingNativeScriptTxProductionWorkflowRunnerV1,
  createMissingNativeScriptUtxoProductionWorkflowRunnerV1,
  createMissingSignatureProductionWorkflowRunnerV1,
  createNativeScriptInvalidProductionWorkflowRunnerV1,
  createNetworkIdProductionWorkflowRunnerV1,
  createNonExistentInputProductionWorkflowRunnerV1,
  createNoReferenceInputProductionWorkflowRunnerV1,
  createProductionExternalHistoricalNativeScriptSourceRosterV1,
  createProductionHeaderClassifierV1,
  createProductionHistoricalNativeScriptHistorySourceV1,
  createProductionHistoricalNativeScriptProviderRosterV1,
  createReferenceInputNoIdxProductionWorkflowRunnerV1,
  createWithdrawnInputProductionWorkflowRunnerV1,
  createWithdrawnReferenceInputProductionWorkflowRunnerV1,
  createZeroInputProductionWorkflowRunnerV1,
  DA_HASH_PREIMAGE_COMPLETE_CANONICAL_REPLAY_V1,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
  DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY_V1,
  type HistoricalNativeScriptSourceRosterV1,
  INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1,
  INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1,
  installProductionWorkflowApplicationRegistryV1,
  INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1,
  INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1,
  L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY_V1,
  makeLucidForSubmit,
  type ManifestBoundCanonicalDecodabilityWorkflowConfigV1,
  type ManifestBoundCommittedFieldShapeWorkflowConfigV1,
  type ManifestBoundDaHashPreimageWorkflowConfigV1,
  type ManifestBoundDoubleSpendWorkflowConfigV1,
  type ManifestBoundDoubleWithdrawWorkflowConfigV1,
  type ManifestBoundFabricatedDepositWorkflowConfigV1,
  type ManifestBoundFabricatedWithdrawalWorkflowConfigV1,
  type ManifestBoundInputNoIdxWorkflowConfigV1,
  type ManifestBoundInputSetUniquenessWorkflowConfigV1,
  type ManifestBoundInvalidRangeWorkflowConfigV1,
  type ManifestBoundInvalidSignatureWorkflowConfigV1,
  type ManifestBoundL2TxMistagWorkflowConfigV1,
  type ManifestBoundMinAdaWorkflowConfigV1,
  type ManifestBoundMinFeeWorkflowConfigV1,
  type ManifestBoundMissingNativeScriptTxWorkflowConfigV1,
  type ManifestBoundMissingNativeScriptUtxoWorkflowConfigV1,
  type ManifestBoundMissingSignatureWorkflowConfigV1,
  type ManifestBoundNativeScriptInvalidWorkflowConfigV1,
  type ManifestBoundNetworkIdWorkflowConfigV1,
  type ManifestBoundNonExistentInputWorkflowConfigV1,
  type ManifestBoundNoReferenceInputWorkflowConfigV1,
  type ManifestBoundReferenceInputNoIdxWorkflowConfigV1,
  type ManifestBoundWithdrawnInputWorkflowConfigV1,
  type ManifestBoundWithdrawnReferenceInputWorkflowConfigV1,
  type ManifestBoundZeroInputWorkflowConfigV1,
  MIN_ADA_COMPLETE_CANONICAL_REPLAY_V1,
  MIN_FEE_COMPLETE_CANONICAL_REPLAY_V1,
  MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY_V1,
  MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1,
  NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY_V1,
  NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
  parseContractDeploymentInfo,
  productionHeaderDecisionReplayContextV1,
  type ProductionHeaderDecisionV1,
  type ProductionHistoricalNativeScriptCheckpointStoreV1,
  type ProductionHistoricalNativeScriptHistorySourceV1,
  type ProductionHistoricalNativeScriptProviderRosterV1,
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerInputV1,
  type ProductionWorkflowAdapterRunnerV1,
  type ProductionWorkflowApplicationRegistryV1,
  type ProductionWorkflowRuntimeConfigLoaderV1,
  REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1,
  requireDeploymentReferenceScript,
  requireProductionHistoricalNativeScriptHistoryAuthorityV1,
  resolveProverSigner,
  restrictProductionWorkflowFundingSignerV1,
  runProductionFraudProofWorkflowCliV1,
  type StateQueueMutationLeaseCoordinator,
  WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
  WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
  ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedStateQueueHeaderObservationV1,
  CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  HeaderV1,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { WatcherConfig, WatcherWalletKeySource } from "./config.js";
import {
  assertVerifiedWatcherDeploymentIdentityV1,
  type VerifiedWatcherDeploymentIdentityV1,
  watcherDeploymentReleaseFinalityAuthorityV1,
} from "./deployment-identity.js";
import {
  createWatcherProductionRetainedDaRuntimeV1,
  createWatcherProductionWorkflowRuntimeLoaderV1,
  type WatcherProductionRetainedDaRuntimeOptionsV1,
} from "./production-retained-da-runtime-v1.js";
import {
  assertWatcherProductionStateQueueHeaderObservationV1,
  type WatcherProductionStateQueueHeaderObservationV1,
} from "./production-state-queue-observation-v1.js";
import {
  assertWatcherProductionWorkflowFundingProfileOverlayV1,
  productionWorkflowFundingProfileFromOverlayV1,
  type WatcherProductionWorkflowFundingProfileOverlayV1,
} from "./production-workflow-funding-profile-overlay-v1.js";

export const WATCHER_FAULT_PROOF_PRODUCTION_APPLICATION_V1 =
  "midgard-watcher-fault-proof-production-application-v1" as const;
export const WATCHER_FAULT_PROOF_STARTUP_READINESS_V1 =
  "midgard-watcher-fault-proof-startup-readiness-v1" as const;

export type WatcherHistoricalNativeScriptHistoryOverlayV1 = Readonly<{
  sourceMode: "external_provider_quorum";
  consistencyPolicy: "exact_bytes_all_providers_v1";
  providers: readonly Readonly<{
    sourceId: string;
    operatorIdentitySha256: string;
    authorityEndpoint: string;
  }>[];
}>;

export const WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1 =
  Object.freeze([
    "doubleSpend",
    "nonExistentInput",
    "nonExistentInputNoIndex",
    "invalidRange",
    "zeroInput",
    "daHashPreimage",
    "noReferenceInput",
    "referenceInputNoIdx",
    "invalidSignature",
    "fabricatedDeposit",
    "fabricatedWithdrawal",
    "missingSignature",
    "missingNativeScriptTx",
    "withdrawnReferenceInput",
    "canonicalDecodability",
    "committedFieldShape",
    "minFee",
    "doubleWithdraw",
    "l2TxMistag",
    "withdrawnInput",
    "inputSetUniqueness",
    "networkId",
    "missingNativeScriptUtxo",
    "nativeScriptInvalid",
    "minAda",
  ] as const);

export type WatcherInstalledProductionWorkflowCategoryV1 =
  (typeof WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1)[number];

export const WATCHER_MISSING_PRODUCTION_WORKFLOW_CATEGORIES_V1 = Object.freeze([
  "transitionTrace",
  "validationTraceDispute",
  "nativeScriptDecoding",
  "withdrawalMistag",
  "crossBlockDuplicateEvent",
  "valueNotPreserved",
  "mintAuthorization",
] as const);

const watcherProductionWorkflowCoverageV1 = new Set([
  ...WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
  ...WATCHER_MISSING_PRODUCTION_WORKFLOW_CATEGORIES_V1,
]);
if (
  watcherProductionWorkflowCoverageV1.size !==
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length ||
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
    (category) => !watcherProductionWorkflowCoverageV1.has(category),
  )
) {
  throw new Error(
    "watcher production workflow coverage does not partition the catalogue",
  );
}

export type WatcherFaultProofInfrastructureAuthorityV1 = Readonly<{
  manifestPath: string;
  blueprintPath: string;
  deploymentInfoPath: string;
  midgardNodeUrl: string;
  midgardNodeAdminKeySource: WatcherWalletKeySource;
  historicalNativeScriptHistory: WatcherHistoricalNativeScriptHistoryOverlayV1;
  stateQueueLeaseTtlMs?: number;
}>;

export type WatcherFaultProofProductionApplicationOptionsV1 = Readonly<{
  deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  infrastructure: WatcherFaultProofInfrastructureAuthorityV1;
  historicalNativeScriptCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
  fundingProfileOverlay: WatcherProductionWorkflowFundingProfileOverlayV1;
}>;

type WatcherFaultProofApplicationConstructionOptionsV1 = Omit<
  WatcherFaultProofProductionApplicationOptionsV1,
  "fundingProfileOverlay" | "historicalNativeScriptCheckpointStore"
> &
  Readonly<{
    historicalNativeScriptCheckpointStore?: ProductionHistoricalNativeScriptCheckpointStoreV1;
    fundingProfileOverlay?: WatcherProductionWorkflowFundingProfileOverlayV1;
    unsafeTransportOptionsForTest?: WatcherProductionRetainedDaRuntimeOptionsV1["unsafeTransportOptionsForTest"];
    unsafeTransportFactoryForTest?: WatcherProductionRetainedDaRuntimeOptionsV1["unsafeTransportFactoryForTest"];
  }>;

export type WatcherFaultProofStartupReadinessV1 = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_PROOF_STARTUP_READINESS_V1;
  ready: true;
  category: WatcherInstalledProductionWorkflowCategoryV1;
  deploymentFingerprint: string;
  headerHash: string;
  referenceScriptOutRefs: Readonly<Record<string, string>>;
}>;

export type WatcherFaultProofHeaderClassificationInputV1 = Readonly<{
  runtimeConfigPath: string;
  observation: AuthenticatedStateQueueHeaderObservationV1;
  authenticatedObservationDigest: string;
  /** Opaque local-node predecessor; its public retained DA is fetched here. */
  predecessor?: WatcherProductionStateQueueHeaderObservationV1;
  retries?: number;
}>;

export type WatcherFaultProofProductionApplicationV1 = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_PROOF_PRODUCTION_APPLICATION_V1;
  deploymentFingerprint: string;
  installedCategories: readonly WatcherInstalledProductionWorkflowCategoryV1[];
  runners: Readonly<
    Record<
      WatcherInstalledProductionWorkflowCategoryV1,
      ProductionWorkflowAdapterRunnerV1
    >
  >;
  applicationRegistry: ProductionWorkflowApplicationRegistryV1;
  classifyHeader(
    input: WatcherFaultProofHeaderClassificationInputV1,
  ): Promise<ProductionHeaderDecisionV1>;
  assertStartupReady(
    invocation: ProductionWorkflowAdapterReadinessInputV1,
  ): Promise<WatcherFaultProofStartupReadinessV1>;
  runOrResume(
    invocation: ProductionWorkflowAdapterRunnerInputV1,
  ): Promise<unknown>;
}>;

type TaggedWorkflowConfigV1 =
  | Readonly<{
      category: "doubleSpend";
      config: ManifestBoundDoubleSpendWorkflowConfigV1;
    }>
  | Readonly<{
      category: "nonExistentInput";
      config: ManifestBoundNonExistentInputWorkflowConfigV1;
    }>
  | Readonly<{
      category: "invalidRange";
      config: ManifestBoundInvalidRangeWorkflowConfigV1;
    }>
  | Readonly<{
      category: "nonExistentInputNoIndex";
      config: ManifestBoundInputNoIdxWorkflowConfigV1;
    }>
  | Readonly<{
      category: "zeroInput";
      config: ManifestBoundZeroInputWorkflowConfigV1;
    }>
  | Readonly<{
      category: "daHashPreimage";
      config: ManifestBoundDaHashPreimageWorkflowConfigV1;
    }>
  | Readonly<{
      category: "noReferenceInput";
      config: ManifestBoundNoReferenceInputWorkflowConfigV1;
    }>
  | Readonly<{
      category: "referenceInputNoIdx";
      config: ManifestBoundReferenceInputNoIdxWorkflowConfigV1;
    }>
  | Readonly<{
      category: "invalidSignature";
      config: ManifestBoundInvalidSignatureWorkflowConfigV1;
    }>
  | Readonly<{
      category: "fabricatedDeposit";
      config: ManifestBoundFabricatedDepositWorkflowConfigV1;
    }>
  | Readonly<{
      category: "fabricatedWithdrawal";
      config: ManifestBoundFabricatedWithdrawalWorkflowConfigV1;
    }>
  | Readonly<{
      category: "canonicalDecodability";
      config: ManifestBoundCanonicalDecodabilityWorkflowConfigV1;
    }>
  | Readonly<{
      category: "withdrawnReferenceInput";
      config: ManifestBoundWithdrawnReferenceInputWorkflowConfigV1;
    }>
  | Readonly<{
      category: "committedFieldShape";
      config: ManifestBoundCommittedFieldShapeWorkflowConfigV1;
    }>
  | Readonly<{
      category: "doubleWithdraw";
      config: ManifestBoundDoubleWithdrawWorkflowConfigV1;
    }>
  | Readonly<{
      category: "minFee";
      config: ManifestBoundMinFeeWorkflowConfigV1;
    }>
  | Readonly<{
      category: "missingSignature";
      config: ManifestBoundMissingSignatureWorkflowConfigV1;
    }>
  | Readonly<{
      category: "missingNativeScriptTx";
      config: ManifestBoundMissingNativeScriptTxWorkflowConfigV1;
    }>
  | Readonly<{
      category: "l2TxMistag";
      config: ManifestBoundL2TxMistagWorkflowConfigV1;
    }>
  | Readonly<{
      category: "withdrawnInput";
      config: ManifestBoundWithdrawnInputWorkflowConfigV1;
    }>
  | Readonly<{
      category: "inputSetUniqueness";
      config: ManifestBoundInputSetUniquenessWorkflowConfigV1;
    }>
  | Readonly<{
      category: "networkId";
      config: ManifestBoundNetworkIdWorkflowConfigV1;
    }>
  | Readonly<{
      category: "nativeScriptInvalid";
      config: ManifestBoundNativeScriptInvalidWorkflowConfigV1;
    }>
  | Readonly<{
      category: "missingNativeScriptUtxo";
      config: ManifestBoundMissingNativeScriptUtxoWorkflowConfigV1;
    }>
  | Readonly<{
      category: "minAda";
      config: ManifestBoundMinAdaWorkflowConfigV1;
    }>;

type TaggedWorkflowConfigForV1<
  Category extends WatcherInstalledProductionWorkflowCategoryV1,
> = Extract<TaggedWorkflowConfigV1, { readonly category: Category }>;

type TaggedWorkflowLoaderForV1<
  Category extends WatcherInstalledProductionWorkflowCategoryV1,
> = ProductionWorkflowRuntimeConfigLoaderV1<
  TaggedWorkflowConfigForV1<Category>
>;

type ConstructedWorkflowIdentityV1 = Readonly<{
  binding: Readonly<{
    deploymentFingerprint: string;
    definition: Readonly<{
      category: WatcherInstalledProductionWorkflowCategoryV1;
      headerHash: string;
    }>;
  }>;
}>;

type WatcherHistoricalNativeScriptAuthorityV1 = Readonly<{
  checkpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
  providerRoster: ProductionHistoricalNativeScriptProviderRosterV1;
  historySource: ProductionHistoricalNativeScriptHistorySourceV1;
  l1SourceRoster: Promise<HistoricalNativeScriptSourceRosterV1>;
}>;

type ResolvedWatcherHistoricalNativeScriptAuthorityV1 = Omit<
  WatcherHistoricalNativeScriptAuthorityV1,
  "l1SourceRoster"
> &
  Readonly<{ l1SourceRoster: HistoricalNativeScriptSourceRosterV1 }>;

export type WatcherFaultProofApplicationDependenciesV1 = Readonly<{
  readText(path: string): Promise<string>;
  canonicalPath(path: string): Promise<string>;
  makeLucid(input: {
    readonly network: WatcherConfig["targetNetwork"];
    readonly kupoHttpUrl: string;
    readonly ogmiosUrl: string;
  }): Promise<LucidEvolution>;
  resolveSigner(input: {
    readonly network: WatcherConfig["targetNetwork"];
    readonly secret: string;
  }): ReturnType<typeof resolveProverSigner>;
  resolveReferenceScript(input: {
    readonly lucid: LucidEvolution;
    readonly deploymentInfo: ReturnType<typeof parseContractDeploymentInfo>;
    readonly contractName: string;
  }): Promise<UTxO>;
  createLeaseCoordinator(input: {
    readonly midgardNodeUrl: string;
    readonly adminKey: string;
    readonly ttlMs?: number;
  }): StateQueueMutationLeaseCoordinator;
  constructWorkflow(
    input: TaggedWorkflowConfigV1,
  ): Promise<ConstructedWorkflowIdentityV1>;
}>;

const constructProductionWorkflow = async (
  input: TaggedWorkflowConfigV1,
): Promise<ConstructedWorkflowIdentityV1> => {
  switch (input.category) {
    case "doubleSpend":
      return await createManifestBoundDoubleSpendWorkflowV1(input.config);
    case "nonExistentInput":
      return await createManifestBoundNonExistentInputWorkflowV1(input.config);
    case "nonExistentInputNoIndex":
      return await createManifestBoundInputNoIdxWorkflowV1(input.config);
    case "invalidRange":
      return await createManifestBoundInvalidRangeWorkflowV1(input.config);
    case "zeroInput":
      return await createManifestBoundZeroInputWorkflowV1(input.config);
    case "daHashPreimage":
      return await createManifestBoundDaHashPreimageWorkflowV1(input.config);
    case "noReferenceInput":
      return await createManifestBoundNoReferenceInputWorkflowV1(input.config);
    case "referenceInputNoIdx":
      return await createManifestBoundReferenceInputNoIdxWorkflowV1(
        input.config,
      );
    case "invalidSignature":
      return await createManifestBoundInvalidSignatureWorkflowV1(input.config);
    case "fabricatedDeposit":
      return await createManifestBoundFabricatedDepositWorkflowV1(input.config);
    case "fabricatedWithdrawal":
      return await createManifestBoundFabricatedWithdrawalWorkflowV1(
        input.config,
      );
    case "withdrawnReferenceInput":
      return await createManifestBoundWithdrawnReferenceInputWorkflowV1(
        input.config,
      );
    case "canonicalDecodability":
      return await createManifestBoundCanonicalDecodabilityWorkflowV1(
        input.config,
      );
    case "committedFieldShape":
      return await createManifestBoundCommittedFieldShapeWorkflowV1(
        input.config,
      );
    case "doubleWithdraw":
      return await createManifestBoundDoubleWithdrawWorkflowV1(input.config);
    case "minFee":
      return await createManifestBoundMinFeeWorkflowV1(input.config);
    case "missingSignature":
      return await createManifestBoundMissingSignatureWorkflowV1(input.config);
    case "missingNativeScriptTx":
      return await createManifestBoundMissingNativeScriptTxWorkflowV1(
        input.config,
      );
    case "l2TxMistag":
      return await createManifestBoundL2TxMistagWorkflowV1(input.config);
    case "withdrawnInput":
      return await createManifestBoundWithdrawnInputWorkflowV1(input.config);
    case "inputSetUniqueness":
      return await createManifestBoundInputSetUniquenessWorkflowV1(
        input.config,
      );
    case "networkId":
      return await createManifestBoundNetworkIdWorkflowV1(input.config);
    case "nativeScriptInvalid":
      return await createManifestBoundNativeScriptInvalidWorkflowV1(
        input.config,
      );
    case "missingNativeScriptUtxo":
      return await createManifestBoundMissingNativeScriptUtxoWorkflowV1(
        input.config,
      );
    case "minAda":
      return await createManifestBoundMinAdaWorkflowV1(input.config);
  }
};

const productionDependencies: WatcherFaultProofApplicationDependenciesV1 =
  Object.freeze({
    readText: async (path) => await readFile(path, "utf8"),
    canonicalPath: realpath,
    makeLucid: async ({ network, kupoHttpUrl, ogmiosUrl }) =>
      await makeLucidForSubmit(
        {
          network,
          provider: "Kupmios",
          kupoUrl: kupoHttpUrl,
          ogmiosUrl,
        },
        Object.freeze({}),
      ),
    resolveSigner: ({ network, secret }) =>
      resolveProverSigner(
        secret.startsWith("ed25519_sk")
          ? { network, walletPrivateKey: secret }
          : { network, walletSeedPhrase: secret },
        Object.freeze({}),
      ),
    resolveReferenceScript: async ({ lucid, deploymentInfo, contractName }) =>
      await requireDeploymentReferenceScript({
        lucid,
        deploymentInfo,
        name: contractName,
      }),
    createLeaseCoordinator: ({ midgardNodeUrl, adminKey, ttlMs }) =>
      createHttpStateQueueMutationLeaseCoordinator({
        midgardNodeUrl,
        adminKey,
        ...(ttlMs === undefined ? {} : { ttlMs }),
      }),
    constructWorkflow: constructProductionWorkflow,
  });

const plainRecord = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  const result = value as Readonly<Record<string, unknown>>;
  for (const key of Object.keys(result)) {
    const descriptor = Object.getOwnPropertyDescriptor(result, key);
    if (
      descriptor === undefined ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      throw new Error(`${label} must not contain accessors`);
    }
  }
  return result;
};

const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  keys: readonly string[],
  label: string,
): void => {
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
};

const canonicalAbsolutePath = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.trim() !== value ||
    !isAbsolute(value)
  ) {
    throw new Error(`${label} must be a canonical absolute path`);
  }
  return value;
};

const canonicalLoopbackUrl = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.trim() !== value) {
    throw new Error(`${label} must be a canonical URL`);
  }
  const parsed = new URL(value);
  const hostname = parsed.hostname.toLowerCase();
  if (
    hostname !== "127.0.0.1" &&
    hostname !== "localhost" &&
    hostname !== "::1" &&
    hostname !== "[::1]"
  ) {
    throw new Error(`${label} must be a loopback endpoint`);
  }
  if (!/^(?:http|https|ws|wss):$/u.test(parsed.protocol)) {
    throw new Error(`${label} has an unsupported protocol`);
  }
  parsed.hash = "";
  return parsed.toString().replace(/\/$/u, "");
};

const historicalProviderEndpoint = (value: unknown): string => {
  if (typeof value !== "string" || value.trim() !== value) {
    throw new Error("historical native-script provider endpoint is invalid");
  }
  let endpoint: URL;
  try {
    endpoint = new URL(value);
  } catch {
    throw new Error("historical native-script provider endpoint is invalid");
  }
  endpoint.pathname = endpoint.pathname.replace(/\/+$/u, "") || "/";
  if (
    endpoint.protocol !== "https:" ||
    endpoint.username.length !== 0 ||
    endpoint.password.length !== 0 ||
    endpoint.search.length !== 0 ||
    endpoint.hash.length !== 0 ||
    ["127.0.0.1", "localhost", "::1", "[::1]"].includes(
      endpoint.hostname.toLowerCase(),
    )
  ) {
    throw new Error(
      "historical native-script provider endpoint must be fixed external HTTPS",
    );
  }
  return endpoint.toString().replace(/\/$/u, "");
};

const admitHistoricalNativeScriptHistory = (
  value: unknown,
): WatcherHistoricalNativeScriptHistoryOverlayV1 => {
  const input = plainRecord(value, "historical native-script history overlay");
  exactKeys(
    input,
    ["sourceMode", "consistencyPolicy", "providers"],
    "historical native-script history overlay",
  );
  if (
    input.sourceMode !== "external_provider_quorum" ||
    input.consistencyPolicy !== "exact_bytes_all_providers_v1" ||
    !Array.isArray(input.providers) ||
    input.providers.length < 2 ||
    input.providers.length > 4
  ) {
    throw new Error("historical native-script history overlay is invalid");
  }
  const sourceIds = new Set<string>();
  const operators = new Set<string>();
  const endpoints = new Set<string>();
  const providers = input.providers.map((value, index) => {
    const provider = plainRecord(
      value,
      `historical native-script provider ${index.toString()}`,
    );
    exactKeys(
      provider,
      ["sourceId", "operatorIdentitySha256", "authorityEndpoint"],
      `historical native-script provider ${index.toString()}`,
    );
    const endpoint = historicalProviderEndpoint(provider.authorityEndpoint);
    if (
      typeof provider.sourceId !== "string" ||
      provider.sourceId.trim() !== provider.sourceId ||
      provider.sourceId.length === 0 ||
      typeof provider.operatorIdentitySha256 !== "string" ||
      !/^[0-9a-f]{64}$/u.test(provider.operatorIdentitySha256) ||
      sourceIds.has(provider.sourceId) ||
      operators.has(provider.operatorIdentitySha256) ||
      endpoints.has(endpoint)
    ) {
      throw new Error(
        "historical native-script providers must have distinct canonical identities and endpoints",
      );
    }
    sourceIds.add(provider.sourceId);
    operators.add(provider.operatorIdentitySha256);
    endpoints.add(endpoint);
    return Object.freeze({
      sourceId: provider.sourceId,
      operatorIdentitySha256: provider.operatorIdentitySha256,
      authorityEndpoint: endpoint,
    });
  });
  return Object.freeze({
    sourceMode: "external_provider_quorum",
    consistencyPolicy: "exact_bytes_all_providers_v1",
    providers: Object.freeze(providers),
  });
};

const secretSource = (
  value: unknown,
  label: string,
): WatcherWalletKeySource => {
  const source = plainRecord(value, label);
  if (source.kind === "environment") {
    exactKeys(source, ["kind", "variable"], label);
    if (
      typeof source.variable !== "string" ||
      !/^[A-Z][A-Z0-9_]{2,127}$/u.test(source.variable)
    ) {
      throw new Error(`${label}.variable is not a canonical environment name`);
    }
    return Object.freeze({ kind: "environment", variable: source.variable });
  }
  if (source.kind === "file") {
    exactKeys(source, ["kind", "path"], label);
    return Object.freeze({
      kind: "file",
      path: canonicalAbsolutePath(source.path, `${label}.path`),
    });
  }
  throw new Error(`${label}.kind is unsupported`);
};

const admitInfrastructure = (
  value: unknown,
): WatcherFaultProofInfrastructureAuthorityV1 => {
  const input = plainRecord(value, "fault-proof infrastructure authority");
  exactKeys(
    input,
    [
      "manifestPath",
      "blueprintPath",
      "deploymentInfoPath",
      "midgardNodeUrl",
      "midgardNodeAdminKeySource",
      "historicalNativeScriptHistory",
      ...(input.stateQueueLeaseTtlMs === undefined
        ? []
        : ["stateQueueLeaseTtlMs"]),
    ],
    "fault-proof infrastructure authority",
  );
  const ttl = input.stateQueueLeaseTtlMs;
  if (
    ttl !== undefined &&
    (!Number.isSafeInteger(ttl) || (ttl as number) <= 0)
  ) {
    throw new Error("state-queue lease TTL must be a positive integer");
  }
  return Object.freeze({
    manifestPath: canonicalAbsolutePath(input.manifestPath, "manifestPath"),
    blueprintPath: canonicalAbsolutePath(input.blueprintPath, "blueprintPath"),
    deploymentInfoPath: canonicalAbsolutePath(
      input.deploymentInfoPath,
      "deploymentInfoPath",
    ),
    midgardNodeUrl: canonicalLoopbackUrl(
      input.midgardNodeUrl,
      "Midgard node URL",
    ),
    midgardNodeAdminKeySource: secretSource(
      input.midgardNodeAdminKeySource,
      "midgardNodeAdminKeySource",
    ),
    historicalNativeScriptHistory: admitHistoricalNativeScriptHistory(
      input.historicalNativeScriptHistory,
    ),
    ...(ttl === undefined ? {} : { stateQueueLeaseTtlMs: ttl as number }),
  });
};

const requireCanonicalFile = async (
  path: string,
  dependencies: WatcherFaultProofApplicationDependenciesV1,
): Promise<string> => {
  const canonical = await dependencies.canonicalPath(path);
  if (canonical !== path) {
    throw new Error(
      `production fault-proof authority path must not traverse a symlink or non-canonical segment: ${path}`,
    );
  }
  return path;
};

const readSecret = async ({
  source,
  dependencies,
  environment,
  label,
}: {
  readonly source: WatcherWalletKeySource;
  readonly dependencies: WatcherFaultProofApplicationDependenciesV1;
  readonly environment: NodeJS.ProcessEnv;
  readonly label: string;
}): Promise<string> => {
  const raw =
    source.kind === "environment"
      ? environment[source.variable]
      : await dependencies.readText(
          await requireCanonicalFile(source.path, dependencies),
        );
  const secret = raw?.trim() ?? "";
  if (secret.length === 0) {
    throw new Error(`${label} secret source is empty`);
  }
  return secret;
};

const referenceContracts = (
  category: WatcherInstalledProductionWorkflowCategoryV1,
): Readonly<Record<string, string>> => {
  const base = {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
  };
  switch (category) {
    case "doubleSpend":
      return Object.freeze({
        step01: "fraudProofDoubleSpend",
        step02: "fraudProofDoubleSpendStep02",
        step03: "fraudProofDoubleSpendStep03",
        step04: "fraudProofDoubleSpendStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "nonExistentInput":
      return Object.freeze({
        step01: "fraudProofNonExistentInput",
        step02: "fraudProofNonExistentInputStep02",
        step03: "fraudProofNonExistentInputStep03",
        step04: "fraudProofNonExistentInputStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "nonExistentInputNoIndex":
      return Object.freeze({
        step01: "fraudProofNonExistentInputNoIndex",
        step02: "fraudProofNonExistentInputNoIndexStep02",
        step03: "fraudProofNonExistentInputNoIndexStep03",
        step04: "fraudProofNonExistentInputNoIndexStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "invalidRange":
      return Object.freeze({
        step01: "fraudProofInvalidRange",
        step02: "fraudProofInvalidRangeStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
      });
    case "zeroInput":
      return Object.freeze({
        step01: "fraudProofZeroInput",
        step02: "fraudProofZeroInputStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
      });
    case "daHashPreimage":
      return Object.freeze({
        step01: "fraudProofDaHashPreimage",
        step02: "fraudProofDaHashPreimageStep02",
        ...base,
      });
    case "noReferenceInput":
      return Object.freeze({
        step01: "fraudProofNoReferenceInput",
        step02: "fraudProofNoReferenceInputStep02",
        step03: "fraudProofNoReferenceInputStep03",
        step04: "fraudProofNoReferenceInputStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "referenceInputNoIdx":
      return Object.freeze({
        step01: "fraudProofReferenceInputNoIdx",
        step02: "fraudProofReferenceInputNoIdxStep02",
        step03: "fraudProofReferenceInputNoIdxStep03",
        step04: "fraudProofReferenceInputNoIdxStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "invalidSignature":
      return Object.freeze({
        step01: "fraudProofInvalidSignature",
        step02: "fraudProofInvalidSignatureStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "fabricatedDeposit":
      return Object.freeze({
        step01: "fraudProofFabricatedDeposit",
        step02: "fraudProofFabricatedDepositStep02",
        step03: "fraudProofFabricatedDepositStep03",
        step04: "fraudProofFabricatedDepositStep04",
        computationThreadMint: "computationThreadMint",
        fraudProofMint: "fraudProofMint",
      });
    case "fabricatedWithdrawal":
      return Object.freeze({
        step01: "fraudProofFabricatedWithdrawal",
        step02: "fraudProofFabricatedWithdrawalStep02",
        step03: "fraudProofFabricatedWithdrawalStep03",
        step04: "fraudProofFabricatedWithdrawalStep04",
        computationThreadMint: "computationThreadMint",
        fraudProofMint: "fraudProofMint",
      });
    case "canonicalDecodability":
      return Object.freeze({
        step01: "fraudProofCanonicalDecodability",
        step02: "fraudProofCanonicalDecodabilityStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "committedFieldShape":
      return Object.freeze({
        step01: "fraudProofCommittedFieldShape",
        step02: "fraudProofCommittedFieldShapeStep02",
        ...base,
      });
    case "minFee":
      return Object.freeze({
        step01: "fraudProofMinFee",
        step02: "fraudProofMinFeeStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "doubleWithdraw":
      return Object.freeze({
        step01: "fraudProofDoubleWithdraw",
        step02: "fraudProofDoubleWithdrawStep02",
        ...base,
      });
    case "missingSignature":
      return Object.freeze({
        step01: "fraudProofMissingSignature",
        step02: "fraudProofMissingSignatureStep02",
        step03: "fraudProofMissingSignatureStep03",
        step04: "fraudProofMissingSignatureStep04",
        ...base,
      });
    case "missingNativeScriptTx":
      return Object.freeze({
        step01: "fraudProofMissingNativeScriptTx",
        step02: "fraudProofMissingNativeScriptTxStep02",
        step03: "fraudProofMissingNativeScriptTxStep03",
        step04: "fraudProofMissingNativeScriptTxStep04",
        step05: "fraudProofMissingNativeScriptTxStep05",
        step06: "fraudProofMissingNativeScriptTxStep06",
        step07: "fraudProofMissingNativeScriptTxStep07",
        step08: "fraudProofMissingNativeScriptTxStep08",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "withdrawnReferenceInput":
      return Object.freeze({
        step01: "fraudProofWithdrawnReferenceInput",
        step02: "fraudProofWithdrawnReferenceInputStep02",
        step03: "fraudProofWithdrawnReferenceInputStep03",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "l2TxMistag":
      return Object.freeze({
        step01: "fraudProofL2TxMistag",
        step02: "fraudProofL2TxMistagStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
      });
    case "withdrawnInput":
      return Object.freeze({
        step01: "fraudProofWithdrawnInput",
        step02: "fraudProofWithdrawnInputStep02",
        step03: "fraudProofWithdrawnInputStep03",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "inputSetUniqueness":
      return Object.freeze({
        step01: "fraudProofInputSetUniqueness",
        step02: "fraudProofInputSetUniquenessStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "networkId":
      return Object.freeze({
        step01: "fraudProofNetworkId",
        step02: "fraudProofNetworkIdStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "nativeScriptInvalid":
      return Object.freeze({
        step01: "fraudProofNativeScriptInvalid",
        step02: "fraudProofNativeScriptInvalidStep02",
        step03: "fraudProofNativeScriptInvalidStep03",
        step04: "fraudProofNativeScriptInvalidStep04",
        step05: "fraudProofNativeScriptInvalidStep05",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "missingNativeScriptUtxo":
      return Object.freeze({
        step01: "fraudProofMissingNativeScriptUtxo",
        step02: "fraudProofMissingNativeScriptUtxoStep02",
        step03: "fraudProofMissingNativeScriptUtxoStep03",
        step04: "fraudProofMissingNativeScriptUtxoStep04",
        step05: "fraudProofMissingNativeScriptUtxoStep05",
        step06: "fraudProofMissingNativeScriptUtxoStep06",
        step07: "fraudProofMissingNativeScriptUtxoStep07",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "minAda":
      return Object.freeze({
        step01: "fraudProofMinAda",
        step02: "fraudProofMinAdaStep02",
        step03: "fraudProofMinAdaStep03",
        step04: "fraudProofMinAdaStep04",
        step05: "fraudProofMinAdaStep05",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
  }
};

type CommonInfrastructureV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ReturnType<typeof resolveProverSigner>;
  source: Readonly<{
    sourceId: string;
    kupoHttpUrl: string;
    ogmiosUrl: string;
    timeoutMs: number;
  }>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  references: Readonly<Record<string, UTxO>>;
  historicalNativeScriptAuthority: ResolvedWatcherHistoricalNativeScriptAuthorityV1;
  replayContext?: CompleteCanonicalReplayContextV1;
}>;

const buildCommonInfrastructure = async ({
  category,
  watcherConfig,
  invocation,
  infrastructure,
  deploymentIdentity,
  historicalNativeScriptAuthority,
  replayContexts,
  dependencies,
  environment,
}: {
  readonly category: WatcherInstalledProductionWorkflowCategoryV1;
  readonly watcherConfig: WatcherConfig;
  readonly invocation: ProductionWorkflowAdapterReadinessInputV1;
  readonly infrastructure: WatcherFaultProofInfrastructureAuthorityV1;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  readonly historicalNativeScriptAuthority: WatcherHistoricalNativeScriptAuthorityV1;
  readonly replayContexts: ReadonlyMap<
    string,
    CompleteCanonicalReplayContextV1
  >;
  readonly dependencies: WatcherFaultProofApplicationDependenciesV1;
  readonly environment: NodeJS.ProcessEnv;
}): Promise<CommonInfrastructureV1> => {
  if (invocation.category !== category) {
    throw new Error(
      `watcher production workflow refuses category ${invocation.category}; expected ${category}`,
    );
  }
  if (invocation.deploymentFingerprint !== deploymentIdentity.manifestId) {
    throw new Error(
      "watcher production workflow invocation differs from deployment authority",
    );
  }
  if (watcherConfig.l1.source.sourceMode !== "local_node") {
    throw new Error(
      "watcher production workflows require the admitted local-node L1 source",
    );
  }
  const kupo = watcherConfig.l1.source.queryServices.find(
    (service) => service.kind === "kupo",
  );
  const ogmios = watcherConfig.l1.source.queryServices.find(
    (service) => service.kind === "ogmios",
  );
  if (kupo === undefined || ogmios === undefined) {
    throw new Error("watcher local-node authority omitted Kupo or Ogmios");
  }
  const [manifestPath, blueprintPath, deploymentInfoPath] = await Promise.all([
    requireCanonicalFile(infrastructure.manifestPath, dependencies),
    requireCanonicalFile(infrastructure.blueprintPath, dependencies),
    requireCanonicalFile(infrastructure.deploymentInfoPath, dependencies),
  ]);
  const [
    manifestJson,
    blueprintJson,
    deploymentInfoJson,
    proverSecret,
    adminKey,
  ] = await Promise.all([
    dependencies.readText(manifestPath),
    dependencies.readText(blueprintPath),
    dependencies.readText(deploymentInfoPath),
    readSecret({
      source: watcherConfig.proverWallet.keySource,
      dependencies,
      environment,
      label: "watcher prover wallet",
    }),
    readSecret({
      source: infrastructure.midgardNodeAdminKeySource,
      dependencies,
      environment,
      label: "Midgard node admin",
    }),
  ]);
  let manifest: unknown;
  let deploymentInfoValue: unknown;
  try {
    manifest = JSON.parse(manifestJson) as unknown;
    deploymentInfoValue = JSON.parse(deploymentInfoJson) as unknown;
  } catch {
    throw new Error("watcher manifest/deployment-info input is not JSON");
  }
  const manifestRecord = plainRecord(manifest, "deployment manifest");
  const manifestFinality = plainRecord(
    manifestRecord.l1Finality,
    "deployment manifest l1Finality",
  );
  if (
    manifestFinality.confirmationDepth !== watcherConfig.l1.finality.depth ||
    manifestFinality.automaticRecoveryMaxDepth !==
      watcherConfig.l1.finality.rollback.postFinalityRecoveryMaxDepth
  ) {
    throw new Error(
      "watcher configured finality differs from the deployment manifest",
    );
  }
  const deploymentInfo = parseContractDeploymentInfo(deploymentInfoValue);
  const lucid = await dependencies.makeLucid({
    network: watcherConfig.targetNetwork,
    kupoHttpUrl: kupo.endpoint,
    ogmiosUrl: ogmios.endpoint,
  });
  const resolvedSigner = dependencies.resolveSigner({
    network: watcherConfig.targetNetwork,
    secret: proverSecret,
  });
  const executionInvocation =
    invocation as Partial<ProductionWorkflowAdapterRunnerInputV1>;
  const replayContext =
    executionInvocation.decisionDigest === undefined
      ? undefined
      : replayContexts.get(executionInvocation.decisionDigest);
  if (
    executionInvocation.decisionDigest !== undefined &&
    (category === "nonExistentInput" || category === "noReferenceInput") &&
    replayContext === undefined
  ) {
    throw new Error(
      `${category} execution omitted its classifier-admitted predecessor context`,
    );
  }
  const signer =
    executionInvocation.fundingReservationPermit === undefined
      ? resolvedSigner
      : restrictProductionWorkflowFundingSignerV1({
          signer: resolvedSigner,
          permit: executionInvocation.fundingReservationPermit,
        });
  const references = Object.freeze(
    Object.fromEntries(
      await Promise.all(
        Object.entries(referenceContracts(category)).map(
          async ([role, contractName]) => [
            role,
            await dependencies.resolveReferenceScript({
              lucid,
              deploymentInfo,
              contractName,
            }),
          ],
        ),
      ),
    ) as Record<string, UTxO>,
  );
  return Object.freeze({
    manifest,
    blueprintJson,
    deploymentInfo: deploymentInfoValue,
    headerHash: invocation.headerHash,
    lucid,
    signer,
    source: Object.freeze({
      sourceId: [
        "watcher-fault-proof",
        category,
        deploymentIdentity.manifestId,
        watcherConfig.l1.source.authorityNodeId,
        watcherConfig.l1.source.chainSync.genesisIdentitySha256,
      ].join("/"),
      kupoHttpUrl: kupo.endpoint,
      ogmiosUrl: ogmios.endpoint,
      timeoutMs: watcherConfig.l1.requestTimeoutMs,
    }),
    stateQueueMutationLeaseCoordinator: dependencies.createLeaseCoordinator({
      midgardNodeUrl: infrastructure.midgardNodeUrl,
      adminKey,
      ...(infrastructure.stateQueueLeaseTtlMs === undefined
        ? {}
        : { ttlMs: infrastructure.stateQueueLeaseTtlMs }),
    }),
    references,
    historicalNativeScriptAuthority: Object.freeze({
      ...historicalNativeScriptAuthority,
      l1SourceRoster: await historicalNativeScriptAuthority.l1SourceRoster,
    }),
    ...(replayContext === undefined ? {} : { replayContext }),
  });
};

const requiredReference = (
  references: Readonly<Record<string, UTxO>>,
  name: string,
): UTxO => {
  const reference = references[name];
  if (reference === undefined) {
    throw new Error(`workflow infrastructure omitted ${name}`);
  }
  return reference;
};

const baseWitnesses = (references: Readonly<Record<string, UTxO>>) => ({
  computationThreadMint: requiredReference(references, "computationThreadMint"),
  fraudProofMint: requiredReference(references, "fraudProofMint"),
  phasMembershipWithdraw: requiredReference(
    references,
    "phasMembershipWithdraw",
  ),
});

function taggedConfig(
  category: "doubleSpend",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "doubleSpend" }>;
function taggedConfig(
  category: "nonExistentInput",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "nonExistentInput" }>;
function taggedConfig(
  category: "nonExistentInputNoIndex",
  common: CommonInfrastructureV1,
): Extract<
  TaggedWorkflowConfigV1,
  { readonly category: "nonExistentInputNoIndex" }
>;
function taggedConfig(
  category: "invalidRange",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "invalidRange" }>;
function taggedConfig(
  category: "zeroInput",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "zeroInput" }>;
function taggedConfig(
  category: "daHashPreimage",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "daHashPreimage" }>;
function taggedConfig(
  category: "noReferenceInput",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "noReferenceInput" }>;
function taggedConfig(
  category: "referenceInputNoIdx",
  common: CommonInfrastructureV1,
): Extract<
  TaggedWorkflowConfigV1,
  { readonly category: "referenceInputNoIdx" }
>;
function taggedConfig(
  category: "invalidSignature",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "invalidSignature" }>;
function taggedConfig(
  category: "fabricatedDeposit",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "fabricatedDeposit" }>;
function taggedConfig(
  category: "fabricatedWithdrawal",
  common: CommonInfrastructureV1,
): Extract<
  TaggedWorkflowConfigV1,
  { readonly category: "fabricatedWithdrawal" }
>;
function taggedConfig(
  category: "withdrawnReferenceInput",
  common: CommonInfrastructureV1,
): Extract<
  TaggedWorkflowConfigV1,
  { readonly category: "withdrawnReferenceInput" }
>;
function taggedConfig(
  category: "canonicalDecodability",
  common: CommonInfrastructureV1,
): Extract<
  TaggedWorkflowConfigV1,
  { readonly category: "canonicalDecodability" }
>;
function taggedConfig(
  category: "committedFieldShape",
  common: CommonInfrastructureV1,
): Extract<
  TaggedWorkflowConfigV1,
  { readonly category: "committedFieldShape" }
>;
function taggedConfig(
  category: "minFee",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "minFee" }>;
function taggedConfig(
  category: "doubleWithdraw",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "doubleWithdraw" }>;
function taggedConfig(
  category: "missingSignature",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "missingSignature" }>;
function taggedConfig(
  category: "missingNativeScriptTx",
  common: CommonInfrastructureV1,
): Extract<
  TaggedWorkflowConfigV1,
  { readonly category: "missingNativeScriptTx" }
>;
function taggedConfig(
  category: "l2TxMistag",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "l2TxMistag" }>;
function taggedConfig(
  category: "withdrawnInput",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "withdrawnInput" }>;
function taggedConfig(
  category: "inputSetUniqueness",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "inputSetUniqueness" }>;
function taggedConfig(
  category: "networkId",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "networkId" }>;
function taggedConfig(
  category: "nativeScriptInvalid",
  common: CommonInfrastructureV1,
): Extract<
  TaggedWorkflowConfigV1,
  { readonly category: "nativeScriptInvalid" }
>;
function taggedConfig(
  category: "missingNativeScriptUtxo",
  common: CommonInfrastructureV1,
): Extract<
  TaggedWorkflowConfigV1,
  { readonly category: "missingNativeScriptUtxo" }
>;
function taggedConfig(
  category: "minAda",
  common: CommonInfrastructureV1,
): Extract<TaggedWorkflowConfigV1, { readonly category: "minAda" }>;
function taggedConfig(
  category: WatcherInstalledProductionWorkflowCategoryV1,
  common: CommonInfrastructureV1,
): TaggedWorkflowConfigV1;
function taggedConfig(
  category: WatcherInstalledProductionWorkflowCategoryV1,
  common: CommonInfrastructureV1,
): TaggedWorkflowConfigV1 {
  const base = {
    manifest: common.manifest,
    blueprintJson: common.blueprintJson,
    deploymentInfo: common.deploymentInfo,
    headerHash: common.headerHash,
    lucid: common.lucid,
    signer: common.signer,
    source: common.source,
    stateQueueMutationLeaseCoordinator:
      common.stateQueueMutationLeaseCoordinator,
  };
  const reference = (name: string) =>
    requiredReference(common.references, name);
  switch (category) {
    case "doubleSpend":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            }),
          }),
          fieldPreimageCertificateReferenceScript: reference(
            "fieldPreimageCertificateMint",
          ),
        }),
      });
    case "nonExistentInput":
    case "noReferenceInput":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          ...(common.replayContext === undefined
            ? {}
            : { replayContext: common.replayContext }),
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
        }),
      });
    case "fabricatedDeposit":
    case "fabricatedWithdrawal":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
            ] as const),
            witnesses: Object.freeze({
              computationThreadMint: reference("computationThreadMint"),
              fraudProofMint: reference("fraudProofMint"),
            }),
          }),
        }),
      });
    case "nonExistentInputNoIndex":
    case "referenceInputNoIdx":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
        }),
      });
    case "invalidRange":
    case "zeroInput":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            }),
          }),
        }),
      });
    case "daHashPreimage":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "invalidSignature":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
        }),
      });
    case "canonicalDecodability":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            }),
          }),
        }),
      });
    case "committedFieldShape":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "minFee":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
        }),
      });
    case "doubleWithdraw":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "l2TxMistag":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            }),
          }),
        }),
      });
    case "missingSignature":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
            ] as const),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "missingNativeScriptTx":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
              reference("step05"),
              reference("step06"),
              reference("step07"),
              reference("step08"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
          historicalNativeScriptCheckpointStore:
            common.historicalNativeScriptAuthority.checkpointStore,
          historicalNativeScriptHistorySource:
            common.historicalNativeScriptAuthority.historySource,
          historicalNativeScriptL1Roster:
            common.historicalNativeScriptAuthority.l1SourceRoster,
        }),
      });
    case "withdrawnReferenceInput":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
            ] as const),
            witnesses: Object.freeze(baseWitnesses(common.references)),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
        }),
      });
    case "withdrawnInput":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
        }),
      });
    case "inputSetUniqueness":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
        }),
      });
    case "networkId":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          stepReferenceScripts: Object.freeze([
            reference("step01"),
            reference("step02"),
          ] as const),
          fieldPreimageCertificateReferenceScript: reference(
            "fieldPreimageCertificateMint",
          ),
          witnessReferenceScripts: Object.freeze({
            ...baseWitnesses(common.references),
            chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            pexcludesWithdraw: reference("pexcludesWithdraw"),
          }),
          removal: Object.freeze({
            stateQueueMutationLeaseCoordinator:
              common.stateQueueMutationLeaseCoordinator,
          }),
        }),
      });
    case "nativeScriptInvalid":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
              reference("step05"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
        }),
      });
    case "missingNativeScriptUtxo":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
              reference("step05"),
              reference("step06"),
              reference("step07"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
          historicalNativeScriptCheckpointStore:
            common.historicalNativeScriptAuthority.checkpointStore,
          historicalNativeScriptHistorySource:
            common.historicalNativeScriptAuthority.historySource,
        }),
      });
    case "minAda":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
              reference("step05"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
          }),
          historicalNativeScriptCheckpointStore:
            common.historicalNativeScriptAuthority.checkpointStore,
          historicalNativeScriptHistorySource:
            common.historicalNativeScriptAuthority.historySource,
        }),
      });
  }
}

const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const taggedReferenceOutRefs = (
  tagged: TaggedWorkflowConfigV1,
): Readonly<Record<string, string>> => {
  const references = referenceContracts(tagged.category);
  const utxos: readonly UTxO[] = (() => {
    switch (tagged.category) {
      case "doubleSpend":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.fieldPreimageCertificateReferenceScript,
        ];
      case "minFee":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "canonicalDecodability":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "invalidSignature":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "fabricatedDeposit":
      case "fabricatedWithdrawal":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
        ];
      case "nonExistentInput":
      case "noReferenceInput":
      case "nonExistentInputNoIndex":
      case "referenceInputNoIdx":
      case "inputSetUniqueness":
      case "withdrawnInput":
      case "withdrawnReferenceInput":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "networkId":
        return [
          ...tagged.config.stepReferenceScripts,
          ...Object.values(tagged.config.witnessReferenceScripts),
          tagged.config.fieldPreimageCertificateReferenceScript,
        ];
      case "missingNativeScriptTx":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "nativeScriptInvalid":
      case "missingNativeScriptUtxo":
      case "minAda":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "invalidRange":
      case "zeroInput":
      case "daHashPreimage":
      case "committedFieldShape":
      case "doubleWithdraw":
      case "l2TxMistag":
      case "missingSignature":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
        ];
    }
  })();
  const roles = Object.keys(references);
  if (roles.length !== utxos.length) {
    throw new Error("workflow reference roster changed after admission");
  }
  return Object.freeze(
    Object.fromEntries(
      roles.map((role, index) => [role, outRef(utxos[index]!)]),
    ),
  );
};

const admittedApplications = new WeakSet<object>();

/** Retains the exact runner/classifier authority captured by this module. */
export const assertWatcherFaultProofProductionApplicationV1 = (
  application: WatcherFaultProofProductionApplicationV1,
): void => {
  if (!admittedApplications.has(application)) {
    throw new Error(
      "watcher fault-proof production application is not module-admitted",
    );
  }
};

const predecessorObservationForClassifierV1 = ({
  current,
  predecessor,
}: {
  readonly current: AuthenticatedStateQueueHeaderObservationV1;
  readonly predecessor: WatcherProductionStateQueueHeaderObservationV1;
}): AuthenticatedStateQueueHeaderObservationV1 => {
  assertWatcherProductionStateQueueHeaderObservationV1(predecessor);
  const header = Data.from(predecessor.headerCborHex, HeaderV1);
  if (
    Data.to(header, HeaderV1) !== predecessor.headerCborHex ||
    predecessor.headerHash !== current.header.prevHeaderHash
  ) {
    throw new Error(
      "watcher predecessor observation differs from the challenged HeaderV1 link",
    );
  }
  if (
    !/^(?:0|[1-9][0-9]*)$/u.test(predecessor.observedSlot) ||
    !/^(?:0|[1-9][0-9]*)$/u.test(predecessor.finalityDepth)
  ) {
    throw new Error("watcher predecessor chain point is malformed");
  }
  const confirmationDepth = BigInt(predecessor.finalityDepth);
  if (confirmationDepth > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("watcher predecessor finality depth exceeds safe range");
  }
  return Object.freeze({
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
    sourceMode: current.sourceMode,
    provenance: current.provenance,
    chainPoint: Object.freeze({
      slot: BigInt(predecessor.observedSlot),
      blockHash: predecessor.observedBlockHash,
    }),
    confirmationDepth: Number(confirmationDepth),
    headerHash: predecessor.headerHash,
    header,
  });
};

const WATCHER_INSTALLED_COMPLETE_REPLAY_V1 =
  createCompleteCanonicalReplayUnionV1([
    DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
    NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
    INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1,
    INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1,
    ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
    DA_HASH_PREIMAGE_COMPLETE_CANONICAL_REPLAY_V1,
    NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
    REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1,
    INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1,
    MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1,
    MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY_V1,
    WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
    CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY_V1,
    COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY_V1,
    MIN_FEE_COMPLETE_CANONICAL_REPLAY_V1,
    DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY_V1,
    L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY_V1,
    WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
    INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1,
    NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
    NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY_V1,
    MIN_ADA_COMPLETE_CANONICAL_REPLAY_V1,
  ]);

const createApplication = ({
  options,
  dependencies,
  environment,
  allowExecution,
}: {
  readonly options: WatcherFaultProofApplicationConstructionOptionsV1;
  readonly dependencies: WatcherFaultProofApplicationDependenciesV1;
  readonly environment: NodeJS.ProcessEnv;
  readonly allowExecution: boolean;
}): WatcherFaultProofProductionApplicationV1 => {
  const deploymentIdentity = options.deploymentIdentity;
  assertVerifiedWatcherDeploymentIdentityV1(deploymentIdentity);
  const infrastructure = admitInfrastructure(options.infrastructure);
  if (options.historicalNativeScriptCheckpointStore === undefined) {
    throw new Error(
      "watcher application requires its historical native-script checkpoint store",
    );
  }
  if (allowExecution && options.fundingProfileOverlay === undefined) {
    throw new Error(
      "watcher application requires its signed funding-profile overlay",
    );
  }
  if (options.fundingProfileOverlay !== undefined) {
    assertWatcherProductionWorkflowFundingProfileOverlayV1(
      options.fundingProfileOverlay,
    );
    if (
      options.fundingProfileOverlay.deploymentFingerprint !==
        deploymentIdentity.manifestId ||
      options.fundingProfileOverlay.releaseEvidenceDigest !==
        deploymentIdentity.releaseEvidenceDigest
    ) {
      throw new Error(
        "watcher funding-profile overlay changed deployment identity",
      );
    }
  }
  const fundingProfile = (
    category: WatcherInstalledProductionWorkflowCategoryV1,
  ) =>
    options.fundingProfileOverlay === undefined
      ? undefined
      : productionWorkflowFundingProfileFromOverlayV1({
          overlay: options.fundingProfileOverlay,
          category,
        });
  const providerRoster = createProductionHistoricalNativeScriptProviderRosterV1(
    {
      deploymentFingerprint: deploymentIdentity.manifestId,
      providers: infrastructure.historicalNativeScriptHistory.providers,
    },
  );
  const historySource = createProductionHistoricalNativeScriptHistorySourceV1({
    providerRoster,
  });
  if (allowExecution) {
    requireProductionHistoricalNativeScriptHistoryAuthorityV1({
      deploymentFingerprint: deploymentIdentity.manifestId,
      checkpointStore: options.historicalNativeScriptCheckpointStore,
      historySource,
    });
  }
  const historicalNativeScriptAuthority: WatcherHistoricalNativeScriptAuthorityV1 =
    Object.freeze({
      checkpointStore: options.historicalNativeScriptCheckpointStore,
      providerRoster,
      historySource,
      l1SourceRoster: watcherDeploymentReleaseFinalityAuthorityV1(
        deploymentIdentity,
      )
        .verifyForWorkflow({
          deploymentFingerprint: deploymentIdentity.manifestId,
        })
        .then((releaseFinality) =>
          createProductionExternalHistoricalNativeScriptSourceRosterV1({
            providerRoster,
            releaseFinality,
          }),
        ),
    });
  const environmentSnapshot = Object.freeze({ ...environment });
  const replayContexts = new Map<string, CompleteCanonicalReplayContextV1>();
  const loaderOptions = {
    deploymentIdentity,
    ...(options.unsafeTransportOptionsForTest === undefined
      ? {}
      : {
          unsafeTransportOptionsForTest: options.unsafeTransportOptionsForTest,
        }),
    ...(options.unsafeTransportFactoryForTest === undefined
      ? {}
      : {
          unsafeTransportFactoryForTest: options.unsafeTransportFactoryForTest,
        }),
  };
  function makeTaggedLoader(
    category: "doubleSpend",
  ): TaggedWorkflowLoaderForV1<"doubleSpend">;
  function makeTaggedLoader(
    category: "nonExistentInput",
  ): TaggedWorkflowLoaderForV1<"nonExistentInput">;
  function makeTaggedLoader(
    category: "nonExistentInputNoIndex",
  ): TaggedWorkflowLoaderForV1<"nonExistentInputNoIndex">;
  function makeTaggedLoader(
    category: "invalidRange",
  ): TaggedWorkflowLoaderForV1<"invalidRange">;
  function makeTaggedLoader(
    category: "zeroInput",
  ): TaggedWorkflowLoaderForV1<"zeroInput">;
  function makeTaggedLoader(
    category: "daHashPreimage",
  ): TaggedWorkflowLoaderForV1<"daHashPreimage">;
  function makeTaggedLoader(
    category: "noReferenceInput",
  ): TaggedWorkflowLoaderForV1<"noReferenceInput">;
  function makeTaggedLoader(
    category: "referenceInputNoIdx",
  ): TaggedWorkflowLoaderForV1<"referenceInputNoIdx">;
  function makeTaggedLoader(
    category: "invalidSignature",
  ): TaggedWorkflowLoaderForV1<"invalidSignature">;
  function makeTaggedLoader(
    category: "fabricatedDeposit",
  ): TaggedWorkflowLoaderForV1<"fabricatedDeposit">;
  function makeTaggedLoader(
    category: "fabricatedWithdrawal",
  ): TaggedWorkflowLoaderForV1<"fabricatedWithdrawal">;
  function makeTaggedLoader(
    category: "withdrawnReferenceInput",
  ): TaggedWorkflowLoaderForV1<"withdrawnReferenceInput">;
  function makeTaggedLoader(
    category: "canonicalDecodability",
  ): TaggedWorkflowLoaderForV1<"canonicalDecodability">;
  function makeTaggedLoader(
    category: "committedFieldShape",
  ): TaggedWorkflowLoaderForV1<"committedFieldShape">;
  function makeTaggedLoader(
    category: "minFee",
  ): TaggedWorkflowLoaderForV1<"minFee">;
  function makeTaggedLoader(
    category: "doubleWithdraw",
  ): TaggedWorkflowLoaderForV1<"doubleWithdraw">;
  function makeTaggedLoader(
    category: "missingSignature",
  ): TaggedWorkflowLoaderForV1<"missingSignature">;
  function makeTaggedLoader(
    category: "missingNativeScriptTx",
  ): TaggedWorkflowLoaderForV1<"missingNativeScriptTx">;
  function makeTaggedLoader(
    category: "l2TxMistag",
  ): TaggedWorkflowLoaderForV1<"l2TxMistag">;
  function makeTaggedLoader(
    category: "withdrawnInput",
  ): TaggedWorkflowLoaderForV1<"withdrawnInput">;
  function makeTaggedLoader(
    category: "inputSetUniqueness",
  ): TaggedWorkflowLoaderForV1<"inputSetUniqueness">;
  function makeTaggedLoader(
    category: "networkId",
  ): TaggedWorkflowLoaderForV1<"networkId">;
  function makeTaggedLoader(
    category: "nativeScriptInvalid",
  ): TaggedWorkflowLoaderForV1<"nativeScriptInvalid">;
  function makeTaggedLoader(
    category: "missingNativeScriptUtxo",
  ): TaggedWorkflowLoaderForV1<"missingNativeScriptUtxo">;
  function makeTaggedLoader(
    category: "minAda",
  ): TaggedWorkflowLoaderForV1<"minAda">;
  function makeTaggedLoader(
    category: WatcherInstalledProductionWorkflowCategoryV1,
  ): ProductionWorkflowRuntimeConfigLoaderV1<TaggedWorkflowConfigV1> {
    return createWatcherProductionWorkflowRuntimeLoaderV1({
      ...loaderOptions,
      buildInfrastructureConfig: async ({ watcherConfig, invocation }) => {
        const common = await buildCommonInfrastructure({
          category,
          watcherConfig,
          invocation,
          infrastructure,
          deploymentIdentity,
          historicalNativeScriptAuthority,
          replayContexts,
          dependencies,
          environment: environmentSnapshot,
        });
        return taggedConfig(category, common);
      },
    });
  }

  const taggedLoaders = {
    doubleSpend: makeTaggedLoader("doubleSpend"),
    nonExistentInput: makeTaggedLoader("nonExistentInput"),
    nonExistentInputNoIndex: makeTaggedLoader("nonExistentInputNoIndex"),
    invalidRange: makeTaggedLoader("invalidRange"),
    zeroInput: makeTaggedLoader("zeroInput"),
    daHashPreimage: makeTaggedLoader("daHashPreimage"),
    noReferenceInput: makeTaggedLoader("noReferenceInput"),
    referenceInputNoIdx: makeTaggedLoader("referenceInputNoIdx"),
    invalidSignature: makeTaggedLoader("invalidSignature"),
    fabricatedDeposit: makeTaggedLoader("fabricatedDeposit"),
    fabricatedWithdrawal: makeTaggedLoader("fabricatedWithdrawal"),
    withdrawnReferenceInput: makeTaggedLoader("withdrawnReferenceInput"),
    canonicalDecodability: makeTaggedLoader("canonicalDecodability"),
    committedFieldShape: makeTaggedLoader("committedFieldShape"),
    minFee: makeTaggedLoader("minFee"),
    doubleWithdraw: makeTaggedLoader("doubleWithdraw"),
    missingSignature: makeTaggedLoader("missingSignature"),
    missingNativeScriptTx: makeTaggedLoader("missingNativeScriptTx"),
    l2TxMistag: makeTaggedLoader("l2TxMistag"),
    withdrawnInput: makeTaggedLoader("withdrawnInput"),
    inputSetUniqueness: makeTaggedLoader("inputSetUniqueness"),
    networkId: makeTaggedLoader("networkId"),
    missingNativeScriptUtxo: makeTaggedLoader("missingNativeScriptUtxo"),
    nativeScriptInvalid: makeTaggedLoader("nativeScriptInvalid"),
    minAda: makeTaggedLoader("minAda"),
  } as const;

  const doubleSpendLoader = async (
    input: Parameters<(typeof taggedLoaders)["doubleSpend"]>[0],
  ) => {
    const loaded = await taggedLoaders.doubleSpend(input);
    if (loaded.config.category !== "doubleSpend") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const nonExistentInputLoader = async (
    input: Parameters<(typeof taggedLoaders)["nonExistentInput"]>[0],
  ) => {
    const loaded = await taggedLoaders.nonExistentInput(input);
    if (loaded.config.category !== "nonExistentInput") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const inputNoIdxLoader = async (
    input: Parameters<(typeof taggedLoaders)["nonExistentInputNoIndex"]>[0],
  ) => {
    const loaded = await taggedLoaders.nonExistentInputNoIndex(input);
    if (loaded.config.category !== "nonExistentInputNoIndex") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const invalidRangeLoader = async (
    input: Parameters<(typeof taggedLoaders)["invalidRange"]>[0],
  ) => {
    const loaded = await taggedLoaders.invalidRange(input);
    if (loaded.config.category !== "invalidRange") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const zeroInputLoader = async (
    input: Parameters<(typeof taggedLoaders)["zeroInput"]>[0],
  ) => {
    const loaded = await taggedLoaders.zeroInput(input);
    if (loaded.config.category !== "zeroInput") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const daHashPreimageLoader = async (
    input: Parameters<(typeof taggedLoaders)["daHashPreimage"]>[0],
  ) => {
    const loaded = await taggedLoaders.daHashPreimage(input);
    if (loaded.config.category !== "daHashPreimage") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const noReferenceInputLoader = async (
    input: Parameters<(typeof taggedLoaders)["noReferenceInput"]>[0],
  ) => {
    const loaded = await taggedLoaders.noReferenceInput(input);
    if (loaded.config.category !== "noReferenceInput") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const referenceInputNoIdxLoader = async (
    input: Parameters<(typeof taggedLoaders)["referenceInputNoIdx"]>[0],
  ) => {
    const loaded = await taggedLoaders.referenceInputNoIdx(input);
    if (loaded.config.category !== "referenceInputNoIdx") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const invalidSignatureLoader = async (
    input: Parameters<(typeof taggedLoaders)["invalidSignature"]>[0],
  ) => {
    const loaded = await taggedLoaders.invalidSignature(input);
    if (loaded.config.category !== "invalidSignature") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const fabricatedDepositLoader = async (
    input: Parameters<(typeof taggedLoaders)["fabricatedDeposit"]>[0],
  ) => {
    const loaded = await taggedLoaders.fabricatedDeposit(input);
    if (loaded.config.category !== "fabricatedDeposit") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const fabricatedWithdrawalLoader = async (
    input: Parameters<(typeof taggedLoaders)["fabricatedWithdrawal"]>[0],
  ) => {
    const loaded = await taggedLoaders.fabricatedWithdrawal(input);
    if (loaded.config.category !== "fabricatedWithdrawal") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const withdrawnReferenceInputLoader = async (
    input: Parameters<(typeof taggedLoaders)["withdrawnReferenceInput"]>[0],
  ) => {
    const loaded = await taggedLoaders.withdrawnReferenceInput(input);
    if (loaded.config.category !== "withdrawnReferenceInput") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const committedFieldShapeLoader = async (
    input: Parameters<(typeof taggedLoaders)["committedFieldShape"]>[0],
  ) => {
    const loaded = await taggedLoaders.committedFieldShape(input);
    if (loaded.config.category !== "committedFieldShape") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const canonicalDecodabilityLoader = async (
    input: Parameters<(typeof taggedLoaders)["canonicalDecodability"]>[0],
  ) => {
    const loaded = await taggedLoaders.canonicalDecodability(input);
    if (loaded.config.category !== "canonicalDecodability") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const minFeeLoader = async (
    input: Parameters<(typeof taggedLoaders)["minFee"]>[0],
  ) => {
    const loaded = await taggedLoaders.minFee(input);
    if (loaded.config.category !== "minFee") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const doubleWithdrawLoader = async (
    input: Parameters<(typeof taggedLoaders)["doubleWithdraw"]>[0],
  ) => {
    const loaded = await taggedLoaders.doubleWithdraw(input);
    if (loaded.config.category !== "doubleWithdraw") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const missingSignatureLoader = async (
    input: Parameters<(typeof taggedLoaders)["missingSignature"]>[0],
  ) => {
    const loaded = await taggedLoaders.missingSignature(input);
    if (loaded.config.category !== "missingSignature") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const missingNativeScriptTxLoader = async (
    input: Parameters<(typeof taggedLoaders)["missingNativeScriptTx"]>[0],
  ) => {
    const loaded = await taggedLoaders.missingNativeScriptTx(input);
    if (loaded.config.category !== "missingNativeScriptTx") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const l2TxMistagLoader = async (
    input: Parameters<(typeof taggedLoaders)["l2TxMistag"]>[0],
  ) => {
    const loaded = await taggedLoaders.l2TxMistag(input);
    if (loaded.config.category !== "l2TxMistag") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const withdrawnInputLoader = async (
    input: Parameters<(typeof taggedLoaders)["withdrawnInput"]>[0],
  ) => {
    const loaded = await taggedLoaders.withdrawnInput(input);
    if (loaded.config.category !== "withdrawnInput") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const inputSetUniquenessLoader = async (
    input: Parameters<(typeof taggedLoaders)["inputSetUniqueness"]>[0],
  ) => {
    const loaded = await taggedLoaders.inputSetUniqueness(input);
    if (loaded.config.category !== "inputSetUniqueness") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const networkIdLoader = async (
    input: Parameters<(typeof taggedLoaders)["networkId"]>[0],
  ) => {
    const loaded = await taggedLoaders.networkId(input);
    if (loaded.config.category !== "networkId") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const nativeScriptInvalidLoader = async (
    input: Parameters<(typeof taggedLoaders)["nativeScriptInvalid"]>[0],
  ) => {
    const loaded = await taggedLoaders.nativeScriptInvalid(input);
    if (loaded.config.category !== "nativeScriptInvalid") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const missingNativeScriptUtxoLoader = async (
    input: Parameters<(typeof taggedLoaders)["missingNativeScriptUtxo"]>[0],
  ) => {
    const loaded = await taggedLoaders.missingNativeScriptUtxo(input);
    if (loaded.config.category !== "missingNativeScriptUtxo") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const minAdaLoader = async (
    input: Parameters<(typeof taggedLoaders)["minAda"]>[0],
  ) => {
    const loaded = await taggedLoaders.minAda(input);
    if (loaded.config.category !== "minAda") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };

  const runners = Object.freeze({
    doubleSpend: createDoubleSpendProductionWorkflowRunnerV1(
      doubleSpendLoader,
      fundingProfile("doubleSpend"),
    ),
    nonExistentInput: createNonExistentInputProductionWorkflowRunnerV1(
      nonExistentInputLoader,
      fundingProfile("nonExistentInput"),
    ),
    nonExistentInputNoIndex: createInputNoIdxProductionWorkflowRunnerV1(
      inputNoIdxLoader,
      fundingProfile("nonExistentInputNoIndex"),
    ),
    invalidRange: createInvalidRangeProductionWorkflowRunnerV1(
      invalidRangeLoader,
      fundingProfile("invalidRange"),
    ),
    zeroInput: createZeroInputProductionWorkflowRunnerV1(
      zeroInputLoader,
      fundingProfile("zeroInput"),
    ),
    daHashPreimage: createDaHashPreimageProductionWorkflowRunnerV1(
      daHashPreimageLoader,
      fundingProfile("daHashPreimage"),
    ),
    noReferenceInput: createNoReferenceInputProductionWorkflowRunnerV1(
      noReferenceInputLoader,
      fundingProfile("noReferenceInput"),
    ),
    referenceInputNoIdx: createReferenceInputNoIdxProductionWorkflowRunnerV1(
      referenceInputNoIdxLoader,
      fundingProfile("referenceInputNoIdx"),
    ),
    invalidSignature: createInvalidSignatureProductionWorkflowRunnerV1(
      invalidSignatureLoader,
      fundingProfile("invalidSignature"),
    ),
    fabricatedDeposit: createFabricatedDepositProductionWorkflowRunnerV1(
      fabricatedDepositLoader,
      fundingProfile("fabricatedDeposit"),
    ),
    fabricatedWithdrawal: createFabricatedWithdrawalProductionWorkflowRunnerV1(
      fabricatedWithdrawalLoader,
      fundingProfile("fabricatedWithdrawal"),
    ),
    withdrawnReferenceInput:
      createWithdrawnReferenceInputProductionWorkflowRunnerV1(
        withdrawnReferenceInputLoader,
        fundingProfile("withdrawnReferenceInput"),
      ),
    canonicalDecodability:
      createCanonicalDecodabilityProductionWorkflowRunnerV1(
        canonicalDecodabilityLoader,
        fundingProfile("canonicalDecodability"),
      ),
    committedFieldShape: createCommittedFieldShapeProductionWorkflowRunnerV1(
      committedFieldShapeLoader,
      fundingProfile("committedFieldShape"),
    ),
    minFee: createMinFeeProductionWorkflowRunnerV1(
      minFeeLoader,
      fundingProfile("minFee"),
    ),
    doubleWithdraw: createDoubleWithdrawProductionWorkflowRunnerV1(
      doubleWithdrawLoader,
      fundingProfile("doubleWithdraw"),
    ),
    missingSignature: createMissingSignatureProductionWorkflowRunnerV1(
      missingSignatureLoader,
      fundingProfile("missingSignature"),
    ),
    missingNativeScriptTx:
      createMissingNativeScriptTxProductionWorkflowRunnerV1(
        missingNativeScriptTxLoader,
        fundingProfile("missingNativeScriptTx"),
      ),
    l2TxMistag: createL2TxMistagProductionWorkflowRunnerV1(
      l2TxMistagLoader,
      fundingProfile("l2TxMistag"),
    ),
    withdrawnInput: createWithdrawnInputProductionWorkflowRunnerV1(
      withdrawnInputLoader,
      fundingProfile("withdrawnInput"),
    ),
    inputSetUniqueness: createInputSetUniquenessProductionWorkflowRunnerV1(
      inputSetUniquenessLoader,
      fundingProfile("inputSetUniqueness"),
    ),
    networkId: createNetworkIdProductionWorkflowRunnerV1(
      networkIdLoader,
      fundingProfile("networkId"),
    ),
    missingNativeScriptUtxo:
      createMissingNativeScriptUtxoProductionWorkflowRunnerV1(
        missingNativeScriptUtxoLoader,
        fundingProfile("missingNativeScriptUtxo"),
      ),
    nativeScriptInvalid: createNativeScriptInvalidProductionWorkflowRunnerV1(
      nativeScriptInvalidLoader,
      fundingProfile("nativeScriptInvalid"),
    ),
    minAda: createMinAdaProductionWorkflowRunnerV1(
      minAdaLoader,
      fundingProfile("minAda"),
    ),
  });
  const applicationRegistry = installProductionWorkflowApplicationRegistryV1({
    deploymentFingerprint: deploymentIdentity.manifestId,
    requiredInstalledCategories:
      WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
    installations: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1.map(
      (category) => ({
        category,
        deploymentFingerprint: deploymentIdentity.manifestId,
        runner: runners[category],
      }),
    ),
  });
  let classifierPromise:
    | ReturnType<typeof createProductionHeaderClassifierV1>
    | undefined;
  const loadClassifier = () => {
    classifierPromise ??= createProductionHeaderClassifierV1({
      deploymentFingerprint: deploymentIdentity.manifestId,
      replayer: WATCHER_INSTALLED_COMPLETE_REPLAY_V1,
      releaseFinalityAuthority:
        watcherDeploymentReleaseFinalityAuthorityV1(deploymentIdentity),
    });
    return classifierPromise;
  };
  const application: WatcherFaultProofProductionApplicationV1 = Object.freeze({
    schemaVersion: WATCHER_FAULT_PROOF_PRODUCTION_APPLICATION_V1,
    deploymentFingerprint: deploymentIdentity.manifestId,
    installedCategories: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
    runners,
    applicationRegistry,
    classifyHeader: async (input) => {
      if (!admittedApplications.has(application)) {
        throw new Error(
          "watcher fault-proof production application is not admitted",
        );
      }
      const runtimeConfigPath = await requireCanonicalFile(
        input.runtimeConfigPath,
        dependencies,
      );
      const watcherConfigJson = await dependencies.readText(runtimeConfigPath);
      let watcherConfig: unknown;
      try {
        watcherConfig = JSON.parse(watcherConfigJson) as unknown;
      } catch {
        throw new Error("watcher runtime configuration is not JSON");
      }
      const retainedDa = await createWatcherProductionRetainedDaRuntimeV1({
        watcherConfig,
        ...loaderOptions,
      });
      try {
        if (
          retainedDa.deploymentFingerprint !== deploymentIdentity.manifestId
        ) {
          throw new Error(
            "watcher retained-DA runtime changed deployment identity",
          );
        }
        const decision = await classifyProductionHeaderV1({
          classifier: await loadClassifier(),
          observation: input.observation,
          authenticatedObservationDigest: input.authenticatedObservationDigest,
          sources: retainedDa.sources,
          ...(input.predecessor === undefined
            ? {}
            : {
                predecessorObservation: predecessorObservationForClassifierV1({
                  current: input.observation,
                  predecessor: input.predecessor,
                }),
              }),
          ...(input.retries === undefined ? {} : { retries: input.retries }),
        });
        const replayContext = productionHeaderDecisionReplayContextV1(decision);
        if (
          decision.decision === "fault_detected" &&
          (decision.category === "nonExistentInput" ||
            decision.category === "noReferenceInput") &&
          replayContext === undefined
        ) {
          throw new Error(
            `${decision.category} classifier decision omitted predecessor authority`,
          );
        }
        if (replayContext !== undefined) {
          replayContexts.set(decision.decisionDigest, replayContext);
        }
        return decision;
      } finally {
        await retainedDa.close();
      }
    },
    assertStartupReady: async (invocation) => {
      if (
        !WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1.includes(
          invocation.category as WatcherInstalledProductionWorkflowCategoryV1,
        )
      ) {
        throw new Error(
          `watcher has no installed production workflow for ${invocation.category}`,
        );
      }
      const category =
        invocation.category as WatcherInstalledProductionWorkflowCategoryV1;
      const loaded = await taggedLoaders[category]({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      try {
        const workflow = await dependencies.constructWorkflow(loaded.config);
        if (
          workflow.binding.definition.category !== category ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.binding.deploymentFingerprint !==
            deploymentIdentity.manifestId
        ) {
          throw new Error(
            "watcher startup workflow differs from its admitted invocation",
          );
        }
        return Object.freeze({
          schemaVersion: WATCHER_FAULT_PROOF_STARTUP_READINESS_V1,
          ready: true,
          category,
          deploymentFingerprint: deploymentIdentity.manifestId,
          headerHash: invocation.headerHash,
          referenceScriptOutRefs: taggedReferenceOutRefs(loaded.config),
        });
      } finally {
        await loaded.close();
      }
    },
    runOrResume: async (invocation) => {
      if (!allowExecution) {
        throw new Error(
          "unsafe watcher fault-proof test application cannot execute transactions",
        );
      }
      if (!admittedApplications.has(application)) {
        throw new Error(
          "watcher fault-proof production application is not admitted",
        );
      }
      return await runProductionFraudProofWorkflowCliV1({
        ...invocation,
        applicationRegistry,
      });
    },
  });
  admittedApplications.add(application);
  return application;
};

export const createWatcherFaultProofProductionApplicationV1 = (
  options: WatcherFaultProofProductionApplicationOptionsV1,
): WatcherFaultProofProductionApplicationV1 =>
  createApplication({
    options: Object.freeze({
      deploymentIdentity: options.deploymentIdentity,
      infrastructure: options.infrastructure,
      historicalNativeScriptCheckpointStore:
        options.historicalNativeScriptCheckpointStore,
      fundingProfileOverlay: options.fundingProfileOverlay,
    }),
    dependencies: productionDependencies,
    environment: process.env,
    allowExecution: true,
  });

/** Narrow test-only dependency seam. It cannot execute transactions. */
export const unsafeCreateWatcherFaultProofProductionApplicationForTestV1 = (
  options: WatcherFaultProofApplicationConstructionOptionsV1,
  dependencies: WatcherFaultProofApplicationDependenciesV1,
  environment: NodeJS.ProcessEnv = {},
): WatcherFaultProofProductionApplicationV1 =>
  createApplication({
    options,
    dependencies,
    environment,
    allowExecution: false,
  });
