import { readFile, realpath } from "node:fs/promises";
import { isAbsolute } from "node:path";

import {
  createManifestBoundTransitionTraceWorkflow,
  createManifestBoundValueConservationWorkflow,
  createTransitionTraceEventAuthority,
  createValueConservationWorkflowRunner,
  type ManifestBoundTransitionTraceWorkflowConfig,
  type ManifestBoundValueConservationWorkflowConfig,
  TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
  TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES,
  WORKFLOW_RUNNER_FACTORIES,
} from "@al-ft/midgard-fault-proofs";
import {
  bindFraudProofTerminalDeployment,
  bindFraudProofWorkflowDeployment,
  classifyHeader as classifyProductionHeaderV1,
  type CompleteCanonicalReplayContext,
  createCatalogueCompleteCanonicalReplay,
  createCrossBlockSettlementAuthority,
  createDoubleSpendWorkflowRunner,
  createExternalHistoricalNativeScriptSourceRoster,
  createHeaderClassifier,
  createHistoricalNativeScriptHistorySource,
  createHistoricalNativeScriptProviderRoster,
  createHttpStateQueueMutationLeaseCoordinator,
  createLocalKupmiosFraudProofRawL1SnapshotAuthority,
  createLocalKupmiosHttpOgmiosRawSource,
  createManifestBoundCanonicalDecodabilityWorkflow,
  createManifestBoundCommittedFieldShapeWorkflow,
  createManifestBoundCrossBlockDuplicateEventWorkflow,
  createManifestBoundDaHashPreimageWorkflow,
  createManifestBoundDistinctAssetAccumulationWorkflow,
  createManifestBoundDoubleSpendWorkflow,
  createManifestBoundDoubleWithdrawWorkflow,
  createManifestBoundExecutionSourceScriptDecodingWorkflow,
  createManifestBoundFabricatedDepositWorkflow,
  createManifestBoundFabricatedWithdrawalWorkflow,
  createManifestBoundFieldItemWidthIllegalWorkflow,
  createManifestBoundFieldPreimageLengthWorkflow,
  createManifestBoundInputNoIdxWorkflow,
  createManifestBoundInputSetUniquenessWorkflow,
  createManifestBoundInvalidRangeWorkflow,
  createManifestBoundInvalidSignatureWorkflow,
  createManifestBoundL2TxMistagWorkflow,
  createManifestBoundMinAdaWorkflow,
  createManifestBoundMinFeeWorkflow,
  createManifestBoundMintAuthorizationWorkflow,
  createManifestBoundMintDeclaredAssetLimitWorkflow,
  createManifestBoundMintItemNonCanonicalWorkflow,
  createManifestBoundMissingNativeScriptTxWorkflow,
  createManifestBoundMissingNativeScriptUtxoWorkflow,
  createManifestBoundMissingRedeemerWorkflow,
  createManifestBoundMissingScriptSourceWorkflow,
  createManifestBoundMissingSignatureWorkflow,
  createManifestBoundNativeScriptDecodingWorkflow,
  createManifestBoundNativeScriptInvalidWorkflow,
  createManifestBoundNetworkIdWorkflow,
  createManifestBoundNonExistentInputWorkflow,
  createManifestBoundNoReferenceInputWorkflow,
  createManifestBoundObserverOrderInvalidWorkflow,
  createManifestBoundObserversForbiddenWorkflow,
  createManifestBoundOutputReferenceScriptDecodingWorkflow,
  createManifestBoundProtectedOutputSignerMissingWorkflow,
  createManifestBoundReceivePurposeLanguageWorkflow,
  createManifestBoundRedeemerCanonicityWorkflow,
  createManifestBoundReferenceInputNoIdxWorkflow,
  createManifestBoundResolvedOutputNonCanonicalWorkflow,
  createManifestBoundScriptIntegrityHashMismatchWorkflow,
  createManifestBoundScriptIntegrityHashMissingWorkflow,
  createManifestBoundSpendInputSignerMissingWorkflow,
  createManifestBoundTransactionOutputNonCanonicalWorkflow,
  createManifestBoundUnusedRedeemerWorkflow,
  createManifestBoundUnusedScriptWitnessWorkflow,
  createManifestBoundValidationTraceDisputeWorkflow,
  createManifestBoundWithdrawalMistagWorkflow,
  createManifestBoundWithdrawnInputWorkflow,
  createManifestBoundWithdrawnReferenceInputWorkflow,
  createManifestBoundWitnessScriptDecodingWorkflow,
  createManifestBoundZeroInputWorkflow,
  createMissingSignatureWorkflowRunner,
  createNetworkIdWorkflowRunner,
  createValidationTraceDisputeWorkflowRunner,
  executionNativeScriptInvalid as executionNativeScriptInvalidV1,
  type FraudProofCompletedVerification,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  type HeaderDecision,
  headerDecisionReplayContext,
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptHistorySource,
  type HistoricalNativeScriptProviderRoster,
  type HistoricalNativeScriptSourceRoster,
  installWorkflowApplicationRegistry,
  journalJsonDigest,
  LocalKupmiosCheckpointChangedError,
  LocalKupmiosExactPointNotCanonicalError,
  makeLucidForSubmit,
  type ManifestBoundCanonicalDecodabilityWorkflowConfig,
  type ManifestBoundCommittedFieldShapeWorkflowConfig,
  type ManifestBoundCrossBlockDuplicateEventWorkflowConfig,
  type ManifestBoundDaHashPreimageWorkflowConfig,
  type ManifestBoundDistinctAssetAccumulationWorkflowConfig,
  type ManifestBoundDoubleSpendWorkflowConfig,
  type ManifestBoundDoubleWithdrawWorkflowConfig,
  type ManifestBoundExecutionSourceScriptDecodingWorkflowConfig,
  type ManifestBoundFabricatedDepositWorkflowConfig,
  type ManifestBoundFabricatedWithdrawalWorkflowConfig,
  type ManifestBoundFieldItemWidthIllegalWorkflowConfig,
  type ManifestBoundFieldPreimageLengthWorkflowConfig,
  type ManifestBoundInputNoIdxWorkflowConfig,
  type ManifestBoundInputSetUniquenessWorkflowConfig,
  type ManifestBoundInvalidRangeWorkflowConfig,
  type ManifestBoundInvalidSignatureWorkflowConfig,
  type ManifestBoundL2TxMistagWorkflowConfig,
  type ManifestBoundMinAdaWorkflowConfig,
  type ManifestBoundMinFeeWorkflowConfig,
  type ManifestBoundMintAuthorizationWorkflowConfig,
  type ManifestBoundMintDeclaredAssetLimitWorkflowConfig,
  type ManifestBoundMintItemNonCanonicalWorkflowConfig,
  type ManifestBoundMissingNativeScriptTxWorkflowConfig,
  type ManifestBoundMissingNativeScriptUtxoWorkflowConfig,
  type ManifestBoundMissingRedeemerWorkflowConfig,
  type ManifestBoundMissingScriptSourceWorkflowConfig,
  type ManifestBoundMissingSignatureWorkflowConfig,
  type ManifestBoundNativeScriptDecodingWorkflowConfig,
  type ManifestBoundNativeScriptInvalidWorkflowConfig,
  type ManifestBoundNetworkIdWorkflowConfig,
  type ManifestBoundNonExistentInputWorkflowConfig,
  type ManifestBoundNoReferenceInputWorkflowConfig,
  type ManifestBoundObserverOrderInvalidWorkflowConfig,
  type ManifestBoundObserversForbiddenWorkflowConfig,
  type ManifestBoundOutputReferenceScriptDecodingWorkflowConfig,
  type ManifestBoundProtectedOutputSignerMissingWorkflowConfig,
  type ManifestBoundReceivePurposeLanguageWorkflowConfig,
  type ManifestBoundRedeemerCanonicityWorkflowConfig,
  type ManifestBoundReferenceInputNoIdxWorkflowConfig,
  type ManifestBoundResolvedOutputNonCanonicalWorkflowConfig,
  type ManifestBoundScriptIntegrityHashMismatchWorkflowConfig,
  type ManifestBoundScriptIntegrityHashMissingWorkflowConfig,
  type ManifestBoundSpendInputSignerMissingWorkflowConfig,
  type ManifestBoundTransactionOutputNonCanonicalWorkflowConfig,
  type ManifestBoundUnusedRedeemerWorkflowConfig,
  type ManifestBoundUnusedScriptWitnessWorkflowConfig,
  type ManifestBoundValidationTraceDisputeWorkflowConfig,
  type ManifestBoundWithdrawalMistagWorkflowConfig,
  type ManifestBoundWithdrawnInputWorkflowConfig,
  type ManifestBoundWithdrawnReferenceInputWorkflowConfig,
  type ManifestBoundWitnessScriptDecodingWorkflowConfig,
  type ManifestBoundZeroInputWorkflowConfig,
  normalizeJournalJson,
  parseContractDeploymentInfo,
  requireDeploymentReferenceScript,
  requireHistoricalNativeScriptHistoryAuthority,
  resolveProverSigner,
  restrictWorkflowFundingSigner,
  runFraudProofWorkflowCli,
  type StateQueueMutationLeaseCoordinator,
  VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES,
  VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES,
  VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES,
  verifyCompletedFraudProofWorkflow,
  workflowActuationPermitIsReconciliationOnly,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
  type WorkflowAdapterRunnerInput,
  type WorkflowApplicationRegistry,
  type WorkflowRuntimeConfigLoader,
} from "@al-ft/midgard-fault-proofs";
import {
  CrossBlockDuplicateEventStep02DatumSchema,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  Header,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  assertWatcherWorkflowFundingProfileOverlay,
  type WatcherWorkflowFundingProfileOverlay,
  workflowFundingProfileFromOverlay,
} from "../funding/workflow-funding-profile-overlay.js";
import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type {
  WatcherConfig,
  WatcherWalletKeySource,
} from "../runtime/config.js";
import { parseWatcherConfig } from "../runtime/config.js";
import {
  assertWatcherVerifiedDeploymentAuthority,
  type VerifiedWatcherDeploymentAuthority,
} from "../runtime/deployment-authority.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentProtocolScriptAuthority,
  watcherDeploymentReleaseFinalityAuthority,
} from "../runtime/deployment-identity.js";
import {
  assertWatcherUserEventRuntime,
  type WatcherUserEventRuntime,
} from "../runtime/user-event-runtime.js";
import type { WatcherReplayTranscriptStore } from "../storage/replay-transcript-store.js";
import {
  createWatcherRetainedDaRuntimeOwner,
  createWatcherWorkflowRuntimeLoader,
  type WatcherRetainedDaRuntimeOptions,
} from "../storage/retained-da-runtime.js";
import {
  assertWatcherValidationReplayCaptureCurrent,
  captureWatcherValidationReplayTranscript,
  refreshWatcherValidationReplayCapture,
} from "./replay-transcript-capture.js";

export const WATCHER_FAULT_PROOF_APPLICATION =
  "midgard-watcher-fault-proof-production-application-v1" as const;
export const WATCHER_FAULT_PROOF_STARTUP_READINESS =
  "midgard-watcher-fault-proof-startup-readiness-v1" as const;

export type WatcherHistoricalNativeScriptHistoryOverlay = Readonly<{
  sourceMode: "external_provider_quorum";
  consistencyPolicy: "exact_bytes_all_providers_v1";
  providers: readonly Readonly<{
    sourceId: string;
    operatorIdentitySha256: string;
    authorityEndpoint: string;
  }>[];
}>;

export const WATCHER_INSTALLED_WORKFLOW_CATEGORIES = Object.freeze([
  "doubleSpend",
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "invalidRange",
  "transitionTrace",
  "zeroInput",
  "validationTraceDispute",
  "daHashPreimage",
  "noReferenceInput",
  "referenceInputNoIdx",
  "invalidSignature",
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "nativeScriptDecoding",
  "missingSignature",
  "missingNativeScriptTx",
  "withdrawnReferenceInput",
  "canonicalDecodability",
  "committedFieldShape",
  "minFee",
  "withdrawalMistag",
  "doubleWithdraw",
  "crossBlockDuplicateEvent",
  "l2TxMistag",
  "withdrawnInput",
  "valueNotPreserved",
  "inputSetUniqueness",
  "mintAuthorization",
  "networkId",
  "missingNativeScriptUtxo",
  "nativeScriptInvalid",
  "minAda",
  "fieldPreimageLengthMismatch",
  "fieldItemWidthIllegal",
  "witnessScriptDecoding",
  "scriptIntegrityHashMissing",
  "transactionOutputNonCanonical",
  "resolvedOutputNonCanonical",
  "mintDeclaredAssetLimit",
  "spendInputSignerMissing",
  "protectedOutputSignerMissing",
  "observersForbiddenOnUntaggedNetwork",
  "observerOrderInvalid",
  "redeemerCanonicity",
  "outputReferenceScriptDecoding",
  "executionSourceScriptDecoding",
  "receivePurposeLanguage",
  "unusedScriptWitness",
  "missingScriptSource",
  "missingRedeemer",
  "unusedRedeemer",
  "executionNativeScriptInvalid",
  "scriptIntegrityHashMismatch",
  "distinctAssetAccumulationLimit",
  "mintItemNonCanonical",
] as const);

export type WatcherInstalledWorkflowCategory =
  (typeof WATCHER_INSTALLED_WORKFLOW_CATEGORIES)[number];

export const WATCHER_MISSING_WORKFLOW_CATEGORIES = Object.freeze([] as const);

const watcherWorkflowCoverage = new Set<string>([
  ...WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  ...WATCHER_MISSING_WORKFLOW_CATEGORIES,
]);
if (
  watcherWorkflowCoverage.size !==
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length ||
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
    (category) => !watcherWorkflowCoverage.has(category),
  )
) {
  throw new Error(
    "watcher production workflow coverage does not partition the catalogue",
  );
}

export type WatcherFaultProofInfrastructureAuthority = Readonly<{
  manifestPath: string;
  blueprintPath: string;
  deploymentInfoPath: string;
  midgardNodeUrl: string;
  midgardNodeAdminKeySource: WatcherWalletKeySource;
  historicalNativeScriptHistory: WatcherHistoricalNativeScriptHistoryOverlay;
  stateQueueLeaseTtlMs?: number;
}>;

export type WatcherFaultProofApplicationOptions = Readonly<{
  deploymentAuthority: VerifiedWatcherDeploymentAuthority;
  replayTranscriptStore: WatcherReplayTranscriptStore;
  userEventRuntime: WatcherUserEventRuntime;
  infrastructure: WatcherFaultProofInfrastructureAuthority;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  fundingProfileOverlay: WatcherWorkflowFundingProfileOverlay;
}>;

type WatcherFaultProofApplicationConstructionOptions = Omit<
  WatcherFaultProofApplicationOptions,
  | "fundingProfileOverlay"
  | "historicalNativeScriptCheckpointStore"
  | "deploymentAuthority"
  | "replayTranscriptStore"
  | "userEventRuntime"
> &
  Readonly<{
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    deploymentAuthority?: VerifiedWatcherDeploymentAuthority;
    replayTranscriptStore?: WatcherReplayTranscriptStore;
    userEventRuntime?: WatcherUserEventRuntime;
    historicalNativeScriptCheckpointStore?: HistoricalNativeScriptCheckpointStore;
    fundingProfileOverlay?: WatcherWorkflowFundingProfileOverlay;
    unsafeTransportOptionsForTest?: WatcherRetainedDaRuntimeOptions["unsafeTransportOptionsForTest"];
    unsafeTransportFactoryForTest?: WatcherRetainedDaRuntimeOptions["unsafeTransportFactoryForTest"];
  }>;

export type WatcherFaultProofStartupReadiness = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_PROOF_STARTUP_READINESS;
  ready: true;
  category: WatcherInstalledWorkflowCategory;
  deploymentFingerprint: string;
  headerHash: string;
  referenceScriptOutRefs: Readonly<Record<string, string>>;
}>;

export type WatcherFaultProofHeaderClassificationInput = Readonly<{
  runtimeConfigPath: string;
  observation: AuthenticatedStateQueueHeaderObservation;
  stateQueueObservation: WatcherAuthenticatedStateQueueObservation;
  header: WatcherStateQueueHeaderObservation;
  authenticatedObservationDigest: string;
  /** Opaque local-node predecessor; its public retained DA is fetched here. */
  predecessor?: WatcherStateQueueHeaderObservation;
  retries?: number;
}>;

export type WatcherCompletedFaultProofInput = Readonly<{
  runtimeConfigPath: string;
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
  decisionDigest: string;
  entries: readonly FraudProofWorkflowJournalEntry[];
  terminal: FraudProofWorkflowTerminal;
}>;
export type WatcherCompletedFaultProofVerification =
  FraudProofCompletedVerification;

export type WatcherFaultProofApplication = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_PROOF_APPLICATION;
  deploymentFingerprint: string;
  installedCategories: readonly WatcherInstalledWorkflowCategory[];
  runners: Readonly<
    Record<WatcherInstalledWorkflowCategory, WorkflowAdapterRunner>
  >;
  applicationRegistry: WorkflowApplicationRegistry;
  classifyHeader(
    input: WatcherFaultProofHeaderClassificationInput,
  ): Promise<HeaderDecision>;
  /** Retire private replay authority after target selection or invalidation. */
  retainDecisionAuthorities(decisionDigest: string | null): void;
  /** Whether an admitted decision retains capabilities of the live event head. */
  decisionUsesLocalEventHistory(decisionDigest: string): boolean;
  assertStartupReady(
    invocation: WorkflowAdapterReadinessInput,
  ): Promise<WatcherFaultProofStartupReadiness>;
  verifyCompleted(
    input: WatcherCompletedFaultProofInput,
  ): Promise<WatcherCompletedFaultProofVerification>;
  runOrResume(invocation: WorkflowAdapterRunnerInput): Promise<unknown>;
  close(): Promise<void>;
}>;

type TaggedWorkflowConfig =
  | Readonly<{
      category: "doubleSpend";
      config: ManifestBoundDoubleSpendWorkflowConfig;
    }>
  | Readonly<{
      category: "nonExistentInput";
      config: ManifestBoundNonExistentInputWorkflowConfig;
    }>
  | Readonly<{
      category: "invalidRange";
      config: ManifestBoundInvalidRangeWorkflowConfig;
    }>
  | Readonly<{
      category: "nonExistentInputNoIndex";
      config: ManifestBoundInputNoIdxWorkflowConfig;
    }>
  | Readonly<{
      category: "zeroInput";
      config: ManifestBoundZeroInputWorkflowConfig;
    }>
  | Readonly<{
      category: "daHashPreimage";
      config: ManifestBoundDaHashPreimageWorkflowConfig;
    }>
  | Readonly<{
      category: "noReferenceInput";
      config: ManifestBoundNoReferenceInputWorkflowConfig;
    }>
  | Readonly<{
      category: "referenceInputNoIdx";
      config: ManifestBoundReferenceInputNoIdxWorkflowConfig;
    }>
  | Readonly<{
      category: "invalidSignature";
      config: ManifestBoundInvalidSignatureWorkflowConfig;
    }>
  | Readonly<{
      category: "fabricatedDeposit";
      config: ManifestBoundFabricatedDepositWorkflowConfig;
    }>
  | Readonly<{
      category: "fabricatedWithdrawal";
      config: ManifestBoundFabricatedWithdrawalWorkflowConfig;
    }>
  | Readonly<{
      category: "canonicalDecodability";
      config: ManifestBoundCanonicalDecodabilityWorkflowConfig;
    }>
  | Readonly<{
      category: "withdrawnReferenceInput";
      config: ManifestBoundWithdrawnReferenceInputWorkflowConfig;
    }>
  | Readonly<{
      category: "committedFieldShape";
      config: ManifestBoundCommittedFieldShapeWorkflowConfig;
    }>
  | Readonly<{
      category: "doubleWithdraw";
      config: ManifestBoundDoubleWithdrawWorkflowConfig;
    }>
  | Readonly<{
      category: "minFee";
      config: ManifestBoundMinFeeWorkflowConfig;
    }>
  | Readonly<{
      category: "missingSignature";
      config: ManifestBoundMissingSignatureWorkflowConfig;
    }>
  | Readonly<{
      category: "missingNativeScriptTx";
      config: ManifestBoundMissingNativeScriptTxWorkflowConfig;
    }>
  | Readonly<{
      category: "l2TxMistag";
      config: ManifestBoundL2TxMistagWorkflowConfig;
    }>
  | Readonly<{
      category: "withdrawnInput";
      config: ManifestBoundWithdrawnInputWorkflowConfig;
    }>
  | Readonly<{
      category: "inputSetUniqueness";
      config: ManifestBoundInputSetUniquenessWorkflowConfig;
    }>
  | Readonly<{
      category: "networkId";
      config: ManifestBoundNetworkIdWorkflowConfig;
    }>
  | Readonly<{
      category: "mintAuthorization";
      config: ManifestBoundMintAuthorizationWorkflowConfig;
    }>
  | Readonly<{
      category: "nativeScriptInvalid";
      config: ManifestBoundNativeScriptInvalidWorkflowConfig;
    }>
  | Readonly<{
      category: "nativeScriptDecoding";
      config: ManifestBoundNativeScriptDecodingWorkflowConfig;
    }>
  | Readonly<{
      category: "crossBlockDuplicateEvent";
      config: ManifestBoundCrossBlockDuplicateEventWorkflowConfig;
    }>
  | Readonly<{
      category: "withdrawalMistag";
      config: ManifestBoundWithdrawalMistagWorkflowConfig;
    }>
  | Readonly<{
      category: "missingNativeScriptUtxo";
      config: ManifestBoundMissingNativeScriptUtxoWorkflowConfig;
    }>
  | Readonly<
      | {
          category: "valueNotPreserved";
          config: ManifestBoundValueConservationWorkflowConfig;
        }
      | {
          category: "transitionTrace";
          config: ManifestBoundTransitionTraceWorkflowConfig;
        }
      | {
          category: "minAda";
          config: ManifestBoundMinAdaWorkflowConfig;
        }
    >
  | Readonly<{
      category: "fieldPreimageLengthMismatch";
      config: ManifestBoundFieldPreimageLengthWorkflowConfig;
    }>
  | Readonly<{
      category: "fieldItemWidthIllegal";
      config: ManifestBoundFieldItemWidthIllegalWorkflowConfig;
    }>
  | Readonly<{
      category: "scriptIntegrityHashMissing";
      config: ManifestBoundScriptIntegrityHashMissingWorkflowConfig;
    }>
  | Readonly<{
      category: "transactionOutputNonCanonical";
      config: ManifestBoundTransactionOutputNonCanonicalWorkflowConfig;
    }>
  | Readonly<{
      category: "mintItemNonCanonical";
      config: ManifestBoundMintItemNonCanonicalWorkflowConfig;
    }>
  | Readonly<{
      category: "resolvedOutputNonCanonical";
      config: ManifestBoundResolvedOutputNonCanonicalWorkflowConfig;
    }>
  | Readonly<{
      category: "mintDeclaredAssetLimit";
      config: ManifestBoundMintDeclaredAssetLimitWorkflowConfig;
    }>
  | Readonly<{
      category: "spendInputSignerMissing";
      config: ManifestBoundSpendInputSignerMissingWorkflowConfig;
    }>
  | Readonly<{
      category: "protectedOutputSignerMissing";
      config: ManifestBoundProtectedOutputSignerMissingWorkflowConfig;
    }>
  | Readonly<{
      category: "observersForbiddenOnUntaggedNetwork";
      config: ManifestBoundObserversForbiddenWorkflowConfig;
    }>
  | Readonly<{
      category: "witnessScriptDecoding";
      config: ManifestBoundWitnessScriptDecodingWorkflowConfig;
    }>
  | Readonly<{
      category: "outputReferenceScriptDecoding";
      config: ManifestBoundOutputReferenceScriptDecodingWorkflowConfig;
    }>
  | Readonly<{
      category: "executionSourceScriptDecoding";
      config: ManifestBoundExecutionSourceScriptDecodingWorkflowConfig;
    }>
  | Readonly<{
      category: "executionNativeScriptInvalid";
      config: executionNativeScriptInvalidV1.ManifestBoundExecutionNativeScriptInvalidWorkflowConfig;
    }>
  | Readonly<{
      category: "observerOrderInvalid";
      config: ManifestBoundObserverOrderInvalidWorkflowConfig;
    }>
  | Readonly<{
      category: "redeemerCanonicity";
      config: ManifestBoundRedeemerCanonicityWorkflowConfig;
    }>
  | Readonly<{
      category: "receivePurposeLanguage";
      config: ManifestBoundReceivePurposeLanguageWorkflowConfig;
    }>
  | Readonly<{
      category: "unusedScriptWitness";
      config: ManifestBoundUnusedScriptWitnessWorkflowConfig;
    }>
  | Readonly<{
      category: "missingScriptSource";
      config: ManifestBoundMissingScriptSourceWorkflowConfig;
    }>
  | Readonly<{
      category: "missingRedeemer";
      config: ManifestBoundMissingRedeemerWorkflowConfig;
    }>
  | Readonly<{
      category: "unusedRedeemer";
      config: ManifestBoundUnusedRedeemerWorkflowConfig;
    }>
  | Readonly<{
      category: "scriptIntegrityHashMismatch";
      config: ManifestBoundScriptIntegrityHashMismatchWorkflowConfig;
    }>
  | Readonly<{
      category: "distinctAssetAccumulationLimit";
      config: ManifestBoundDistinctAssetAccumulationWorkflowConfig;
    }>
  | Readonly<{
      category: "validationTraceDispute";
      config: ManifestBoundValidationTraceDisputeWorkflowConfig;
    }>;

type TaggedWorkflowConfigFor<
  Category extends WatcherInstalledWorkflowCategory,
> = Extract<TaggedWorkflowConfig, { readonly category: Category }>;

type TaggedWorkflowLoaderFor<
  Category extends WatcherInstalledWorkflowCategory,
> = WorkflowRuntimeConfigLoader<TaggedWorkflowConfigFor<Category>>;

type ConstructedWorkflowIdentity = Readonly<{
  binding: Readonly<{
    deploymentFingerprint: string;
    definition: Readonly<{
      category: WatcherInstalledWorkflowCategory;
      headerHash: string;
    }>;
  }>;
}>;

type WatcherHistoricalNativeScriptAuthority = Readonly<{
  checkpointStore: HistoricalNativeScriptCheckpointStore;
  providerRoster: HistoricalNativeScriptProviderRoster;
  historySource: HistoricalNativeScriptHistorySource;
  l1SourceRoster: Promise<HistoricalNativeScriptSourceRoster>;
}>;

type ResolvedWatcherHistoricalNativeScriptAuthority = Omit<
  WatcherHistoricalNativeScriptAuthority,
  "l1SourceRoster"
> &
  Readonly<{ l1SourceRoster: HistoricalNativeScriptSourceRoster }>;

export type WatcherFaultProofApplicationDependencies = Readonly<{
  readText(path: string): Promise<string>;
  canonicalPath(path: string): Promise<string>;
  makeLucid(input: {
    readonly network: WatcherConfig["targetNetwork"];
    readonly kupoHttpUrl: string;
    readonly ogmiosUrl: string;
    readonly slotConfig?: NonNullable<
      WatcherConfig["customNetwork"]
    >["slotConfig"];
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
    input: TaggedWorkflowConfig,
  ): Promise<ConstructedWorkflowIdentity>;
}>;

const constructProductionWorkflow = async (
  input: TaggedWorkflowConfig,
): Promise<ConstructedWorkflowIdentity> => {
  switch (input.category) {
    case "doubleSpend":
      return await createManifestBoundDoubleSpendWorkflow(input.config);
    case "nonExistentInput":
      return await createManifestBoundNonExistentInputWorkflow(input.config);
    case "nonExistentInputNoIndex":
      return await createManifestBoundInputNoIdxWorkflow(input.config);
    case "invalidRange":
      return await createManifestBoundInvalidRangeWorkflow(input.config);
    case "zeroInput":
      return await createManifestBoundZeroInputWorkflow(input.config);
    case "daHashPreimage":
      return await createManifestBoundDaHashPreimageWorkflow(input.config);
    case "noReferenceInput":
      return await createManifestBoundNoReferenceInputWorkflow(input.config);
    case "referenceInputNoIdx":
      return await createManifestBoundReferenceInputNoIdxWorkflow(input.config);
    case "invalidSignature":
      return await createManifestBoundInvalidSignatureWorkflow(input.config);
    case "fieldPreimageLengthMismatch":
      return await createManifestBoundFieldPreimageLengthWorkflow(input.config);
    case "fieldItemWidthIllegal":
      return await createManifestBoundFieldItemWidthIllegalWorkflow(
        input.config,
      );
    case "scriptIntegrityHashMissing":
      return await createManifestBoundScriptIntegrityHashMissingWorkflow(
        input.config,
      );
    case "transactionOutputNonCanonical":
      return await createManifestBoundTransactionOutputNonCanonicalWorkflow(
        input.config,
      );
    case "mintItemNonCanonical":
      return await createManifestBoundMintItemNonCanonicalWorkflow(
        input.config,
      );
    case "resolvedOutputNonCanonical":
      return await createManifestBoundResolvedOutputNonCanonicalWorkflow(
        input.config,
      );
    case "mintDeclaredAssetLimit":
      return await createManifestBoundMintDeclaredAssetLimitWorkflow(
        input.config,
      );
    case "spendInputSignerMissing":
      return await createManifestBoundSpendInputSignerMissingWorkflow(
        input.config,
      );
    case "protectedOutputSignerMissing":
      return await createManifestBoundProtectedOutputSignerMissingWorkflow(
        input.config,
      );
    case "observersForbiddenOnUntaggedNetwork":
      return await createManifestBoundObserversForbiddenWorkflow(input.config);
    case "witnessScriptDecoding":
      return await createManifestBoundWitnessScriptDecodingWorkflow(
        input.config,
      );
    case "outputReferenceScriptDecoding":
      return await createManifestBoundOutputReferenceScriptDecodingWorkflow(
        input.config,
      );
    case "executionSourceScriptDecoding":
      return await createManifestBoundExecutionSourceScriptDecodingWorkflow(
        input.config,
      );
    case "executionNativeScriptInvalid":
      return await executionNativeScriptInvalidV1.createManifestBoundExecutionNativeScriptInvalidWorkflow(
        input.config,
      );
    case "observerOrderInvalid":
      return await createManifestBoundObserverOrderInvalidWorkflow(
        input.config,
      );
    case "redeemerCanonicity":
      return await createManifestBoundRedeemerCanonicityWorkflow(input.config);
    case "receivePurposeLanguage":
      return await createManifestBoundReceivePurposeLanguageWorkflow(
        input.config,
      );
    case "unusedScriptWitness":
      return await createManifestBoundUnusedScriptWitnessWorkflow(input.config);
    case "missingScriptSource":
      return await createManifestBoundMissingScriptSourceWorkflow(input.config);
    case "missingRedeemer":
      return await createManifestBoundMissingRedeemerWorkflow(input.config);
    case "unusedRedeemer":
      return await createManifestBoundUnusedRedeemerWorkflow(input.config);
    case "scriptIntegrityHashMismatch":
      return await createManifestBoundScriptIntegrityHashMismatchWorkflow(
        input.config,
      );
    case "distinctAssetAccumulationLimit":
      return await createManifestBoundDistinctAssetAccumulationWorkflow(
        input.config,
      );
    case "validationTraceDispute":
      return await createManifestBoundValidationTraceDisputeWorkflow(
        input.config,
      );
    case "fabricatedDeposit":
      return await createManifestBoundFabricatedDepositWorkflow(input.config);
    case "fabricatedWithdrawal":
      return await createManifestBoundFabricatedWithdrawalWorkflow(
        input.config,
      );
    case "withdrawnReferenceInput":
      return await createManifestBoundWithdrawnReferenceInputWorkflow(
        input.config,
      );
    case "canonicalDecodability":
      return await createManifestBoundCanonicalDecodabilityWorkflow(
        input.config,
      );
    case "committedFieldShape":
      return await createManifestBoundCommittedFieldShapeWorkflow(input.config);
    case "doubleWithdraw":
      return await createManifestBoundDoubleWithdrawWorkflow(input.config);
    case "minFee":
      return await createManifestBoundMinFeeWorkflow(input.config);
    case "missingSignature":
      return await createManifestBoundMissingSignatureWorkflow(input.config);
    case "missingNativeScriptTx":
      return await createManifestBoundMissingNativeScriptTxWorkflow(
        input.config,
      );
    case "l2TxMistag":
      return await createManifestBoundL2TxMistagWorkflow(input.config);
    case "withdrawnInput":
      return await createManifestBoundWithdrawnInputWorkflow(input.config);
    case "inputSetUniqueness":
      return await createManifestBoundInputSetUniquenessWorkflow(input.config);
    case "networkId":
      return await createManifestBoundNetworkIdWorkflow(input.config);
    case "mintAuthorization":
      return await createManifestBoundMintAuthorizationWorkflow(input.config);
    case "nativeScriptInvalid":
      return await createManifestBoundNativeScriptInvalidWorkflow(input.config);
    case "nativeScriptDecoding":
      return await createManifestBoundNativeScriptDecodingWorkflow(
        input.config,
      );
    case "withdrawalMistag":
      return await createManifestBoundWithdrawalMistagWorkflow(input.config);
    case "crossBlockDuplicateEvent":
      return await createManifestBoundCrossBlockDuplicateEventWorkflow(
        input.config,
      );
    case "missingNativeScriptUtxo":
      return await createManifestBoundMissingNativeScriptUtxoWorkflow(
        input.config,
      );
    case "valueNotPreserved":
      return await createManifestBoundValueConservationWorkflow(input.config);
    case "transitionTrace":
      return await createManifestBoundTransitionTraceWorkflow(input.config);
    case "minAda":
      return await createManifestBoundMinAdaWorkflow(input.config);
  }
};

const productionDependencies: WatcherFaultProofApplicationDependencies =
  Object.freeze({
    readText: async (path) => await readFile(path, "utf8"),
    canonicalPath: realpath,
    makeLucid: async ({ network, kupoHttpUrl, ogmiosUrl, slotConfig }) =>
      await makeLucidForSubmit(
        {
          network,
          provider: "Kupmios",
          slotConfig,
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
): WatcherHistoricalNativeScriptHistoryOverlay => {
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
): WatcherFaultProofInfrastructureAuthority => {
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
  dependencies: WatcherFaultProofApplicationDependencies,
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
  readonly dependencies: WatcherFaultProofApplicationDependencies;
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
  category: WatcherInstalledWorkflowCategory,
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
    case "validationTraceDispute":
      // Exact family roster (ruling R4): the six dispute control scripts, the
      // three witness scripts the submitters attach by reference, and the nine
      // shared cursor-removal contracts — derived from the family constants,
      // never re-typed here.
      return Object.freeze({
        ...VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES,
        ...VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES,
        ...VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES,
      });
    case "daHashPreimage":
      return Object.freeze({
        step01: "fraudProofDaHashPreimage",
        step02: "fraudProofDaHashPreimageStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
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
        ...base,
      });
    case "fabricatedWithdrawal":
      return Object.freeze({
        step01: "fraudProofFabricatedWithdrawal",
        step02: "fraudProofFabricatedWithdrawalStep02",
        step03: "fraudProofFabricatedWithdrawalStep03",
        step04: "fraudProofFabricatedWithdrawalStep04",
        ...base,
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
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
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
        forcedStep: "fraudProofMissingSignatureForcedStep",
        forcedSigner: "fraudProofMissingSignatureForcedSigner",
        forcedWitness: "fraudProofMissingSignatureForcedWitness",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
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
        step03: "fraudProofInputSetUniquenessStep03",
        step04: "fraudProofInputSetUniquenessStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "networkId":
      return Object.freeze({
        step01: "fraudProofNetworkId",
        step02: "fraudProofNetworkIdStep02",
        forcedStep: "fraudProofNetworkIdForcedStep",
        forcedScan: "fraudProofNetworkIdForcedScan",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "mintAuthorization":
      return Object.freeze({
        step01: "fraudProofMintAuthorization",
        step02: "fraudProofMintAuthorizationStep02",
        step03: "fraudProofMintAuthorizationStep03",
        step04: "fraudProofMintAuthorizationStep04",
        step05: "fraudProofMintAuthorizationStep05",
        step06: "fraudProofMintAuthorizationStep06",
        step07: "fraudProofMintAuthorizationStep07",
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
    case "crossBlockDuplicateEvent":
      return Object.freeze({
        phasMembershipWithdraw: "phasMembershipWithdraw",
        step01: "fraudProofCrossBlockDuplicateEvent",
        step02: "fraudProofCrossBlockDuplicateEventStep02",
        computationThreadMint: "computationThreadMint",
        fraudProofMint: "fraudProofMint",
      });
    case "nativeScriptDecoding":
      return Object.freeze({
        step01: "fraudProofNativeScriptDecoding",
        step02: "fraudProofNativeScriptDecodingStep02",
        step03: "fraudProofNativeScriptDecodingStep03OpenSubject",
        step04: "fraudProofNativeScriptDecodingStep03BindDescriptor",
        step05: "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
        step06: "fraudProofNativeScriptDecodingStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "withdrawalMistag":
      return Object.freeze({
        step01: "fraudProofWithdrawalMistag",
        step02: "fraudProofWithdrawalMistagStep02",
        step03: "fraudProofWithdrawalMistagStep03",
        step04: "fraudProofWithdrawalMistagStep04",
        step05: "fraudProofWithdrawalMistagStep05",
        ...base,
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
    case "valueNotPreserved":
      return Object.freeze({
        step01: "fraudProofValueNotPreserved",
        step02: "fraudProofValueNotPreservedStep02",
        step03: "fraudProofValueNotPreservedStep03",
        step04: "fraudProofValueNotPreservedStep04",
        unionAcceptedSource: "fraudProofValueNotPreservedUnionAcceptedSource",
        unionForcedSource: "fraudProofValueNotPreservedUnionForcedSource",
        unionEvent: "fraudProofValueNotPreservedUnionEvent",
        unionPreState: "fraudProofValueNotPreservedUnionPreState",
        unionInputs: "fraudProofValueNotPreservedUnionInputs",
        unionInputValue: "fraudProofValueNotPreservedUnionInputValue",
        unionAssets: "fraudProofValueNotPreservedUnionAssets",
        unionFieldGrammar: "fraudProofValueNotPreservedUnionFieldGrammar",
        unionOutputs: "fraudProofValueNotPreservedUnionOutputs",
        unionOutputScan: "fraudProofValueNotPreservedUnionOutputScan",
        unionMint: "fraudProofValueNotPreservedUnionMint",
        unionUpdate: "fraudProofValueNotPreservedUnionUpdate",
        unionTerminal: "fraudProofValueNotPreservedUnionTerminal",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "transitionTrace":
      return Object.freeze(
        Object.fromEntries(
          TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES.map((name) => [
            name,
            name,
          ]),
        ),
      );
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
        txYield: "fraudProofMinAdaStep02TxWithdraw",
        utxoYield: "fraudProofMinAdaStep02UtxoWithdraw",
      });
    case "fieldPreimageLengthMismatch":
      return Object.freeze({
        step01: "fraudProofFieldPreimageLengthMismatch",
        step02Accepted: "fraudProofFieldPreimageLengthMismatchStep02Accepted",
        step02Forced: "fraudProofFieldPreimageLengthMismatchStep02Forced",
        step03: "fraudProofFieldPreimageLengthMismatchStep03",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "fieldItemWidthIllegal":
      return Object.freeze({
        step01: "fraudProofFieldItemWidthIllegal",
        step02: "fraudProofFieldItemWidthIllegalStep02",
        step03: "fraudProofFieldItemWidthIllegalStep03",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "scriptIntegrityHashMissing":
      return Object.freeze({
        step01: "fraudProofScriptIntegrityHashMissing",
        step02: "fraudProofScriptIntegrityHashMissingStep02",
        step03: "fraudProofScriptIntegrityHashMissingStep03",
        step04: "fraudProofScriptIntegrityHashMissingScriptGrammar",
        step05: "fraudProofScriptIntegrityHashMissingScriptScan",
        step06: "fraudProofScriptIntegrityHashMissingRedeemerGrammar",
        step07: "fraudProofScriptIntegrityHashMissingStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "transactionOutputNonCanonical":
      return Object.freeze({
        step01: "fraudProofTransactionOutputNonCanonical",
        step02: "fraudProofTransactionOutputNonCanonicalStep02",
        step03: "fraudProofTransactionOutputNonCanonicalStep03",
        step04: "fraudProofTransactionOutputNonCanonicalStep04",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "mintItemNonCanonical":
      return Object.freeze({
        step01: "fraudProofMintItemNonCanonical",
        step02: "fraudProofMintItemNonCanonicalStep02",
        step03: "fraudProofMintItemNonCanonicalStep03",
        step04: "fraudProofMintItemNonCanonicalStep04",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "resolvedOutputNonCanonical":
      return Object.freeze({
        step01: "fraudProofResolvedOutputNonCanonical",
        step02: "fraudProofResolvedOutputNonCanonicalStep02",
        step03: "fraudProofResolvedOutputNonCanonicalStep03",
        step04: "fraudProofResolvedOutputNonCanonicalStep04",
        step05: "fraudProofResolvedOutputNonCanonicalStep05",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "mintDeclaredAssetLimit":
      return Object.freeze({
        step01: "fraudProofMintDeclaredAssetLimit",
        step02: "fraudProofMintDeclaredAssetLimitStep02",
        step03: "fraudProofMintDeclaredAssetLimitStep03",
        step04: "fraudProofMintDeclaredAssetLimitStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "spendInputSignerMissing":
      return Object.freeze({
        step01: "fraudProofSpendInputSignerMissing",
        step02: "fraudProofSpendInputSignerMissingStep02",
        step03: "fraudProofSpendInputSignerMissingStep03",
        step04: "fraudProofSpendInputSignerMissingStep04",
        step05: "fraudProofSpendInputSignerMissingStep05",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "protectedOutputSignerMissing":
      return Object.freeze({
        step01: "fraudProofProtectedOutputSignerMissing",
        step02: "fraudProofProtectedOutputSignerMissingStep02",
        step03: "fraudProofProtectedOutputSignerMissingStep03",
        step04: "fraudProofProtectedOutputSignerMissingStep04",
        step05: "fraudProofProtectedOutputSignerMissingStep05",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "observersForbiddenOnUntaggedNetwork":
      return Object.freeze({
        step01: "fraudProofObserversForbiddenOnUntaggedNetwork",
        step02: "fraudProofObserversForbiddenOnUntaggedNetworkStep02",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "observerOrderInvalid":
      return Object.freeze({
        step01: "fraudProofObserverOrderInvalid",
        step02: "fraudProofObserverOrderInvalidStep02",
        step03: "fraudProofObserverOrderInvalidStep03",
        step04: "fraudProofObserverOrderInvalidStep04",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "redeemerCanonicity":
      return Object.freeze({
        step01: "fraudProofRedeemerCanonicity",
        step02: "fraudProofRedeemerCanonicityStep02",
        step03: "fraudProofRedeemerCanonicityStep03",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "witnessScriptDecoding":
      return Object.freeze({
        step01: "fraudProofWitnessScriptDecoding",
        step02: "fraudProofWitnessScriptDecodingStep02",
        step03: "fraudProofWitnessScriptDecodingStep03",
        step04: "fraudProofWitnessScriptDecodingStep04",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "outputReferenceScriptDecoding":
      return Object.freeze({
        step01: "fraudProofOutputReferenceScriptDecoding",
        step02: "fraudProofOutputReferenceScriptDecodingStep02",
        step03: "fraudProofOutputReferenceScriptDecodingStep03",
        step04: "fraudProofOutputReferenceScriptDecodingStep04",
        step05: "fraudProofOutputReferenceScriptDecodingStep05",
        step06: "fraudProofOutputReferenceScriptDecodingStep06",
        ...base,
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
      });
    case "executionSourceScriptDecoding":
      return Object.freeze({
        step01: "fraudProofExecutionSourceScriptDecoding",
        step02: "fraudProofExecutionSourceScriptDecodingStep02",
        step03: "fraudProofExecutionSourceScriptDecodingStep03",
        step04: "fraudProofExecutionSourceScriptDecodingStep04",
        step05: "fraudProofExecutionSourceScriptDecodingStep05",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "executionNativeScriptInvalid":
      return Object.freeze({
        step01: "fraudProofExecutionNativeScriptInvalid",
        step02: "fraudProofExecutionNativeScriptInvalidStep02",
        step03: "fraudProofExecutionNativeScriptInvalidStep03",
        step04: "fraudProofExecutionNativeScriptInvalidStep04",
        step05: "fraudProofExecutionNativeScriptInvalidStep05",
        step06: "fraudProofExecutionNativeScriptInvalidStep06",
        step07:
          "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
        step08: "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
        step09: "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
        step10: "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
        step11: "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
        step12: "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
        step13: "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "receivePurposeLanguage":
      return Object.freeze({
        step01: "fraudProofReceivePurposeLanguage",
        step02: "fraudProofReceivePurposeLanguageStep02",
        step03: "fraudProofReceivePurposeLanguageStep03",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "unusedScriptWitness":
      return Object.freeze({
        step01: "fraudProofUnusedScriptWitness",
        step02: "fraudProofUnusedScriptWitnessStep02",
        step03: "fraudProofUnusedScriptWitnessStep03",
        step04: "fraudProofUnusedScriptWitnessStep04",
        step05: "fraudProofUnusedScriptWitnessStep05",
        step06: "fraudProofUnusedScriptWitnessStep06",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "missingScriptSource":
      return Object.freeze({
        step01: "fraudProofMissingScriptSource",
        step02: "fraudProofMissingScriptSourceStep02",
        step03: "fraudProofMissingScriptSourceStep03",
        step04: "fraudProofMissingScriptSourceStep04",
        step05: "fraudProofMissingScriptSourceStep05",
        step06: "fraudProofMissingScriptSourceStep06",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "missingRedeemer":
      return Object.freeze({
        step01: "fraudProofMissingRedeemer",
        step02: "fraudProofMissingRedeemerStep02",
        step02a: "fraudProofMissingRedeemerStep02a",
        step02b: "fraudProofMissingRedeemerStep02b",
        step03: "fraudProofMissingRedeemerStep03",
        step04: "fraudProofMissingRedeemerStep04",
        step05: "fraudProofMissingRedeemerStep05",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "unusedRedeemer":
      return Object.freeze({
        step01: "fraudProofUnusedRedeemer",
        step02: "fraudProofUnusedRedeemerStep02",
        step02a: "fraudProofUnusedRedeemerStep02a",
        step02b: "fraudProofUnusedRedeemerStep02b",
        step02c: "fraudProofUnusedRedeemerStep02c",
        step03: "fraudProofUnusedRedeemerStep03",
        step04: "fraudProofUnusedRedeemerStep04",
        step05: "fraudProofUnusedRedeemerStep05",
        step06: "fraudProofUnusedRedeemerStep06",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "scriptIntegrityHashMismatch":
      return Object.freeze({
        step01: "fraudProofScriptIntegrityHashMismatch",
        step02: "fraudProofScriptIntegrityHashMismatchStep02",
        step03: "fraudProofScriptIntegrityHashMismatchStep03",
        step04: "fraudProofScriptIntegrityHashMismatchStep04",
        step05: "fraudProofScriptIntegrityHashMismatchStep05",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
    case "distinctAssetAccumulationLimit":
      return Object.freeze({
        step01: "fraudProofDistinctAssetAccumulationLimit",
        step02: "fraudProofDistinctAssetAccumulationLimitStep02",
        step03: "fraudProofDistinctAssetAccumulationLimitStep03",
        step04: "fraudProofDistinctAssetAccumulationLimitStep04",
        step05: "fraudProofDistinctAssetAccumulationLimitStep05",
        step06: "fraudProofDistinctAssetAccumulationLimitStep06",
        ...base,
        chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
        pexcludesWithdraw: "pexcludesWithdraw",
        correctionLockSpend: "correctionLockSpend",
        stateQueueSpend: "stateQueueSpend",
        stateQueueMint: "stateQueueMint",
        stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
        activeOperatorsSpend: "activeOperatorsSpend",
        activeOperatorsMint: "activeOperatorsMint",
        retiredOperatorsSpend: "retiredOperatorsSpend",
        retiredOperatorsMint: "retiredOperatorsMint",
        schedulerSpend: "schedulerSpend",
      });
  }
};

type CommonInfrastructure = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  decisionDigest: string;
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
  historicalNativeScriptAuthority: ResolvedWatcherHistoricalNativeScriptAuthority;
  replayContext?: CompleteCanonicalReplayContext;
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
  readonly category: WatcherInstalledWorkflowCategory;
  readonly watcherConfig: WatcherConfig;
  readonly invocation: WorkflowAdapterReadinessInput;
  readonly infrastructure: WatcherFaultProofInfrastructureAuthority;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly historicalNativeScriptAuthority: WatcherHistoricalNativeScriptAuthority;
  readonly replayContexts: ReadonlyMap<string, CompleteCanonicalReplayContext>;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
  readonly environment: NodeJS.ProcessEnv;
}): Promise<CommonInfrastructure> => {
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
    slotConfig: watcherConfig.customNetwork?.slotConfig,
    kupoHttpUrl: kupo.endpoint,
    ogmiosUrl: ogmios.endpoint,
  });
  const resolvedSigner = dependencies.resolveSigner({
    network: watcherConfig.targetNetwork,
    secret: proverSecret,
  });
  const executionInvocation = invocation as Partial<WorkflowAdapterRunnerInput>;
  const replayContext =
    executionInvocation.decisionDigest === undefined
      ? undefined
      : replayContexts.get(executionInvocation.decisionDigest);
  if (
    executionInvocation.decisionDigest !== undefined &&
    !(
      executionInvocation.actuationPermit !== undefined &&
      workflowActuationPermitIsReconciliationOnly(
        executionInvocation.actuationPermit,
      )
    ) &&
    (category === "nonExistentInput" ||
      category === "noReferenceInput" ||
      category === "nativeScriptDecoding" ||
      category === "withdrawalMistag" ||
      category === "mintAuthorization") &&
    replayContext === undefined
  ) {
    throw new Error(
      `${category} execution omitted its classifier-admitted predecessor context`,
    );
  }
  const signer =
    executionInvocation.fundingReservationPermit === undefined
      ? resolvedSigner
      : restrictWorkflowFundingSigner({
          signer: resolvedSigner,
          permit: executionInvocation.fundingReservationPermit,
        });
  signer.selectWallet(lucid);
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
    decisionDigest: executionInvocation.decisionDigest ?? "00".repeat(32),
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
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "doubleSpend" }>;
function taggedConfig(
  category: "nonExistentInput",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "nonExistentInput" }>;
function taggedConfig(
  category: "nonExistentInputNoIndex",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "nonExistentInputNoIndex" }
>;
function taggedConfig(
  category: "invalidRange",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "invalidRange" }>;
function taggedConfig(
  category: "zeroInput",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "zeroInput" }>;
function taggedConfig(
  category: "daHashPreimage",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "daHashPreimage" }>;
function taggedConfig(
  category: "noReferenceInput",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "noReferenceInput" }>;
function taggedConfig(
  category: "referenceInputNoIdx",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "referenceInputNoIdx" }>;
function taggedConfig(
  category: "invalidSignature",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "invalidSignature" }>;
function taggedConfig(
  category: "fabricatedDeposit",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "fabricatedDeposit" }>;
function taggedConfig(
  category: "fabricatedWithdrawal",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "fabricatedWithdrawal" }>;
function taggedConfig(
  category: "withdrawnReferenceInput",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "withdrawnReferenceInput" }
>;
function taggedConfig(
  category: "canonicalDecodability",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "canonicalDecodability" }
>;
function taggedConfig(
  category: "committedFieldShape",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "committedFieldShape" }>;
function taggedConfig(
  category: "minFee",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "minFee" }>;
function taggedConfig(
  category: "doubleWithdraw",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "doubleWithdraw" }>;
function taggedConfig(
  category: "missingSignature",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "missingSignature" }>;
function taggedConfig(
  category: "missingNativeScriptTx",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "missingNativeScriptTx" }
>;
function taggedConfig(
  category: "l2TxMistag",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "l2TxMistag" }>;
function taggedConfig(
  category: "withdrawnInput",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "withdrawnInput" }>;
function taggedConfig(
  category: "inputSetUniqueness",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "inputSetUniqueness" }>;
function taggedConfig(
  category: "networkId",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "networkId" }>;
function taggedConfig(
  category: "mintAuthorization",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "mintAuthorization" }>;
function taggedConfig(
  category: "nativeScriptInvalid",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "nativeScriptInvalid" }>;
function taggedConfig(
  category: "nativeScriptDecoding",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "nativeScriptDecoding" }>;
function taggedConfig(
  category: "withdrawalMistag",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "withdrawalMistag" }>;
function taggedConfig(
  category: "crossBlockDuplicateEvent",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "crossBlockDuplicateEvent" }
>;
function taggedConfig(
  category: "missingNativeScriptUtxo",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "missingNativeScriptUtxo" }
>;
function taggedConfig(
  category: "valueNotPreserved",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "valueNotPreserved" }>;
function taggedConfig(
  category: "transitionTrace",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "transitionTrace" }>;
function taggedConfig(
  category: "minAda",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "minAda" }>;
function taggedConfig(
  category: "fieldPreimageLengthMismatch",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "fieldPreimageLengthMismatch" }
>;
function taggedConfig(
  category: "fieldItemWidthIllegal",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "fieldItemWidthIllegal" }
>;
function taggedConfig(
  category: "scriptIntegrityHashMissing",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "scriptIntegrityHashMissing" }
>;
function taggedConfig(
  category: "transactionOutputNonCanonical",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "transactionOutputNonCanonical" }
>;
function taggedConfig(
  category: "mintItemNonCanonical",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "mintItemNonCanonical" }>;
function taggedConfig(
  category: "resolvedOutputNonCanonical",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "resolvedOutputNonCanonical" }
>;
function taggedConfig(
  category: "mintDeclaredAssetLimit",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "mintDeclaredAssetLimit" }
>;
function taggedConfig(
  category: "spendInputSignerMissing",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "spendInputSignerMissing" }
>;
function taggedConfig(
  category: "protectedOutputSignerMissing",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "protectedOutputSignerMissing" }
>;
function taggedConfig(
  category: "observersForbiddenOnUntaggedNetwork",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "observersForbiddenOnUntaggedNetwork" }
>;
function taggedConfig(
  category: "witnessScriptDecoding",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "witnessScriptDecoding" }
>;
function taggedConfig(
  category: "outputReferenceScriptDecoding",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "outputReferenceScriptDecoding" }
>;
function taggedConfig(
  category: "executionSourceScriptDecoding",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "executionSourceScriptDecoding" }
>;
function taggedConfig(
  category: "executionNativeScriptInvalid",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "executionNativeScriptInvalid" }
>;
function taggedConfig(
  category: "observerOrderInvalid",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "observerOrderInvalid" }>;
function taggedConfig(
  category: "redeemerCanonicity",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "redeemerCanonicity" }>;
function taggedConfig(
  category: "receivePurposeLanguage",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "receivePurposeLanguage" }
>;
function taggedConfig(
  category: "unusedScriptWitness",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "unusedScriptWitness" }>;
function taggedConfig(
  category: "missingScriptSource",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "missingScriptSource" }>;
function taggedConfig(
  category: "missingRedeemer",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "missingRedeemer" }>;
function taggedConfig(
  category: "unusedRedeemer",
  common: CommonInfrastructure,
): Extract<TaggedWorkflowConfig, { readonly category: "unusedRedeemer" }>;
function taggedConfig(
  category: "scriptIntegrityHashMismatch",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "scriptIntegrityHashMismatch" }
>;
function taggedConfig(
  category: "distinctAssetAccumulationLimit",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "distinctAssetAccumulationLimit" }
>;
function taggedConfig(
  category: "validationTraceDispute",
  common: CommonInfrastructure,
): Extract<
  TaggedWorkflowConfig,
  { readonly category: "validationTraceDispute" }
>;
function taggedConfig(
  category: WatcherInstalledWorkflowCategory,
  common: CommonInfrastructure,
): TaggedWorkflowConfig;
function taggedConfig(
  category: WatcherInstalledWorkflowCategory,
  common: CommonInfrastructure,
): TaggedWorkflowConfig {
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
    case "scriptIntegrityHashMismatch":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
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
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "validationTraceDispute":
      // The admitted W25 challenge is deliberately absent here: startup
      // readiness constructs the workflow challenge-free (binding, signer,
      // roster), and execution inside the workflow fail-closes until the
      // classifier detection authority supplies a freshly admitted
      // validation-trace challenge for the decision.
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            control: Object.freeze({
              opener: reference("opener"),
              source: reference("source"),
              game: reference("game"),
              boundary: reference("boundary"),
              timeout: reference("timeout"),
              award: reference("award"),
            }),
            witnesses: Object.freeze({
              computationThreadMint: reference("computationThreadMint"),
              fraudProofMint: reference("fraudProofMint"),
              phasMembershipWithdraw: reference("phasMembershipWithdraw"),
            }),
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "distinctAssetAccumulationLimit":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
              reference("step05"),
              reference("step06"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
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
            witnesses: Object.freeze(baseWitnesses(common.references)),
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
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
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
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
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
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            forced: Object.freeze({
              bind: reference("forcedStep"),
              signer: reference("forcedSigner"),
              witness: reference("forcedWitness"),
            }),
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
    case "networkId":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          stepReferenceScripts: Object.freeze([
            reference("step01"),
            reference("step02"),
          ] as const),
          forcedStepReferenceScript: reference("forcedStep"),
          forcedScanReferenceScript: reference("forcedScan"),
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
    case "mintAuthorization":
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
    case "crossBlockDuplicateEvent":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          historySource: common.historicalNativeScriptAuthority.historySource,
          checkpointStore:
            common.historicalNativeScriptAuthority.checkpointStore,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze({
              phasMembershipWithdraw: reference("phasMembershipWithdraw"),
              computationThreadMint: reference("computationThreadMint"),
              fraudProofMint: reference("fraudProofMint"),
            }),
          }),
        }),
      });
    case "nativeScriptDecoding":
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
              reference("step05"),
              reference("step06"),
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
    case "withdrawalMistag":
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
              reference("step05"),
            ] as const),
            witnesses: Object.freeze(baseWitnesses(common.references)),
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
    case "valueNotPreserved":
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
            union: Object.freeze({
              unionAcceptedSource: reference("unionAcceptedSource"),
              unionForcedSource: reference("unionForcedSource"),
              unionEvent: reference("unionEvent"),
              unionPreState: reference("unionPreState"),
              unionInputs: reference("unionInputs"),
              unionInputValue: reference("unionInputValue"),
              unionAssets: reference("unionAssets"),
              unionFieldGrammar: reference("unionFieldGrammar"),
              unionOutputs: reference("unionOutputs"),
              unionOutputScan: reference("unionOutputScan"),
              unionMint: reference("unionMint"),
              unionUpdate: reference("unionUpdate"),
              unionTerminal: reference("unionTerminal"),
            }),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "transitionTrace":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          referenceScripts: Object.freeze(
            Object.fromEntries(
              TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES.map((name) => [
                name,
                reference(name),
              ]),
            ),
          ),
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
            yields: Object.freeze({
              tx: reference("txYield"),
              utxo: reference("utxoYield"),
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
    case "fieldPreimageLengthMismatch":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          stateQueueMutationLeaseCoordinator:
            common.stateQueueMutationLeaseCoordinator,
          referenceScripts: Object.freeze({
            step01: reference("step01"),
            step02Accepted: reference("step02Accepted"),
            step02Forced: reference("step02Forced"),
            step03: reference("step03"),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "fieldItemWidthIllegal":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          stateQueueMutationLeaseCoordinator:
            common.stateQueueMutationLeaseCoordinator,
          referenceScripts: Object.freeze({
            step01: reference("step01"),
            step02: reference("step02"),
            step03: reference("step03"),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "scriptIntegrityHashMissing":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          stateQueueMutationLeaseCoordinator:
            common.stateQueueMutationLeaseCoordinator,
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
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "transactionOutputNonCanonical":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          stateQueueMutationLeaseCoordinator:
            common.stateQueueMutationLeaseCoordinator,
          referenceScripts: Object.freeze({
            step01: reference("step01"),
            step02: reference("step02"),
            step03: reference("step03"),
            step04: reference("step04"),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "mintItemNonCanonical":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          stateQueueMutationLeaseCoordinator:
            common.stateQueueMutationLeaseCoordinator,
          referenceScripts: Object.freeze({
            step01: reference("step01"),
            step02: reference("step02"),
            step03: reference("step03"),
            step04: reference("step04"),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "resolvedOutputNonCanonical":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          stateQueueMutationLeaseCoordinator:
            common.stateQueueMutationLeaseCoordinator,
          historicalCheckpointStore:
            common.historicalNativeScriptAuthority.checkpointStore,
          historicalSource:
            common.historicalNativeScriptAuthority.historySource,
          referenceScripts: Object.freeze({
            step01: reference("step01"),
            step02: reference("step02"),
            step03: reference("step03"),
            step04: reference("step04"),
            step05: reference("step05"),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "mintDeclaredAssetLimit":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          stateQueueMutationLeaseCoordinator:
            common.stateQueueMutationLeaseCoordinator,
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
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "spendInputSignerMissing":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          historicalCheckpointStore:
            common.historicalNativeScriptAuthority.checkpointStore,
          historicalSource:
            common.historicalNativeScriptAuthority.historySource,
          referenceScripts: Object.freeze({
            step01: reference("step01"),
            step02: reference("step02"),
            step03: reference("step03"),
            step04: reference("step04"),
            step05: reference("step05"),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "protectedOutputSignerMissing":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            step01: reference("step01"),
            step02: reference("step02"),
            step03: reference("step03"),
            step04: reference("step04"),
            step05: reference("step05"),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "observersForbiddenOnUntaggedNetwork":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "witnessScriptDecoding":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            step01: reference("step01"),
            step02: reference("step02"),
            step03: reference("step03"),
            step04: reference("step04"),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "outputReferenceScriptDecoding":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            step01: reference("step01"),
            step02: reference("step02"),
            step03: reference("step03"),
            step04: reference("step04"),
            step05: reference("step05"),
            step06: reference("step06"),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            witnesses: Object.freeze(baseWitnesses(common.references)),
          }),
        }),
      });
    case "executionSourceScriptDecoding":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
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
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "executionNativeScriptInvalid":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          historicalCheckpointStore:
            common.historicalNativeScriptAuthority.checkpointStore,
          historicalSource:
            common.historicalNativeScriptAuthority.historySource,
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
              reference("step09"),
              reference("step10"),
              reference("step11"),
              reference("step12"),
              reference("step13"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "observerOrderInvalid":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
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
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "redeemerCanonicity":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            fieldPreimageCertificateMint: reference(
              "fieldPreimageCertificateMint",
            ),
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "receivePurposeLanguage":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "unusedScriptWitness":
    case "missingScriptSource":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step03"),
              reference("step04"),
              reference("step05"),
              reference("step06"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "missingRedeemer":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step02a"),
              reference("step02b"),
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
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
    case "unusedRedeemer":
      return Object.freeze({
        category,
        config: Object.freeze({
          ...base,
          decisionDigest: common.decisionDigest,
          referenceScripts: Object.freeze({
            steps: Object.freeze([
              reference("step01"),
              reference("step02"),
              reference("step02a"),
              reference("step02b"),
              reference("step02c"),
              reference("step03"),
              reference("step04"),
              reference("step05"),
              reference("step06"),
            ] as const),
            witnesses: Object.freeze({
              ...baseWitnesses(common.references),
              chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
              pexcludesWithdraw: reference("pexcludesWithdraw"),
            }),
            removal: Object.freeze({
              correctionLockSpend: reference("correctionLockSpend"),
              stateQueueSpend: reference("stateQueueSpend"),
              stateQueueMint: reference("stateQueueMint"),
              stateQueueFraudRemovalWithdraw: reference(
                "stateQueueFraudRemovalWithdraw",
              ),
              activeOperatorsSpend: reference("activeOperatorsSpend"),
              activeOperatorsMint: reference("activeOperatorsMint"),
              retiredOperatorsSpend: reference("retiredOperatorsSpend"),
              retiredOperatorsMint: reference("retiredOperatorsMint"),
              schedulerSpend: reference("schedulerSpend"),
            }),
          }),
        }),
      });
  }
}

const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const taggedReferenceOutRefs = (
  tagged: TaggedWorkflowConfig,
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
          ...(tagged.config.forcedStepReferenceScript === undefined
            ? []
            : [tagged.config.forcedStepReferenceScript]),
          ...(tagged.config.forcedScanReferenceScript === undefined
            ? []
            : [tagged.config.forcedScanReferenceScript]),
          ...Object.values(tagged.config.witnessReferenceScripts),
          tagged.config.fieldPreimageCertificateReferenceScript,
        ];
      case "missingNativeScriptTx":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "crossBlockDuplicateEvent":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
        ];
      case "withdrawalMistag":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
        ];
      case "mintAuthorization":
      case "nativeScriptInvalid":
      case "nativeScriptDecoding":
      case "missingNativeScriptUtxo":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "valueNotPreserved":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.union),
          ...Object.values(tagged.config.referenceScripts.witnesses),
          ...Object.values(tagged.config.referenceScripts.removal),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "transitionTrace":
        return Object.values(tagged.config.referenceScripts);
      case "minAda":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
          ...Object.values(tagged.config.referenceScripts.yields),
        ];
      case "fieldPreimageLengthMismatch":
        return [
          tagged.config.referenceScripts.step01,
          tagged.config.referenceScripts.step02Accepted,
          tagged.config.referenceScripts.step02Forced,
          tagged.config.referenceScripts.step03,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "fieldItemWidthIllegal":
        return [
          tagged.config.referenceScripts.step01,
          tagged.config.referenceScripts.step02,
          tagged.config.referenceScripts.step03,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "scriptIntegrityHashMissing":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
          ...Object.values(tagged.config.referenceScripts.removal),
        ];
      case "transactionOutputNonCanonical":
        return [
          tagged.config.referenceScripts.step01,
          tagged.config.referenceScripts.step02,
          tagged.config.referenceScripts.step03,
          tagged.config.referenceScripts.step04,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "mintItemNonCanonical":
        return [
          tagged.config.referenceScripts.step01,
          tagged.config.referenceScripts.step02,
          tagged.config.referenceScripts.step03,
          tagged.config.referenceScripts.step04,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "resolvedOutputNonCanonical":
        return [
          tagged.config.referenceScripts.step01,
          tagged.config.referenceScripts.step02,
          tagged.config.referenceScripts.step03,
          tagged.config.referenceScripts.step04,
          tagged.config.referenceScripts.step05,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "mintDeclaredAssetLimit":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
          ...Object.values(tagged.config.referenceScripts.removal),
        ];
      case "spendInputSignerMissing":
      case "protectedOutputSignerMissing":
        return [
          tagged.config.referenceScripts.step01,
          tagged.config.referenceScripts.step02,
          tagged.config.referenceScripts.step03,
          tagged.config.referenceScripts.step04,
          tagged.config.referenceScripts.step05,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
        ];
      case "observersForbiddenOnUntaggedNetwork":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
          ...Object.values(tagged.config.referenceScripts.removal),
        ];
      case "witnessScriptDecoding":
        return [
          tagged.config.referenceScripts.step01,
          tagged.config.referenceScripts.step02,
          tagged.config.referenceScripts.step03,
          tagged.config.referenceScripts.step04,
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
          ...Object.values(tagged.config.referenceScripts.witnesses),
        ];
      case "outputReferenceScriptDecoding":
        return [
          tagged.config.referenceScripts.step01,
          tagged.config.referenceScripts.step02,
          tagged.config.referenceScripts.step03,
          tagged.config.referenceScripts.step04,
          tagged.config.referenceScripts.step05,
          tagged.config.referenceScripts.step06,
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
          ...Object.values(tagged.config.referenceScripts.witnesses),
        ];
      case "executionSourceScriptDecoding":
      case "receivePurposeLanguage":
      case "unusedScriptWitness":
      case "missingScriptSource":
      case "unusedRedeemer":
      case "scriptIntegrityHashMismatch":
      case "distinctAssetAccumulationLimit":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          ...Object.values(tagged.config.referenceScripts.removal),
        ];
      case "validationTraceDispute":
        return [
          tagged.config.referenceScripts.control.opener,
          tagged.config.referenceScripts.control.source,
          tagged.config.referenceScripts.control.game,
          tagged.config.referenceScripts.control.boundary,
          tagged.config.referenceScripts.control.timeout,
          tagged.config.referenceScripts.control.award,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          ...Object.values(tagged.config.referenceScripts.removal),
        ];
      case "executionNativeScriptInvalid":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
          ...Object.values(tagged.config.referenceScripts.removal),
        ];
      case "missingRedeemer":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
          ...Object.values(tagged.config.referenceScripts.removal),
        ];
      case "observerOrderInvalid":
      case "redeemerCanonicity":
        return [
          ...tagged.config.referenceScripts.steps,
          ...Object.values(tagged.config.referenceScripts.witnesses),
          tagged.config.referenceScripts.fieldPreimageCertificateMint,
          ...Object.values(tagged.config.referenceScripts.removal),
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
          ...(tagged.category === "missingSignature" &&
          tagged.config.referenceScripts.forced !== undefined
            ? [
                ...Object.values(tagged.config.referenceScripts.forced),
                tagged.config.referenceScripts.fieldPreimageCertificateMint!,
              ]
            : []),
          ...Object.values(tagged.config.referenceScripts.witnesses),
        ];
    }
  })();
  const roles = Object.keys(references);
  if (roles.length !== utxos.length) {
    throw new Error(
      `workflow reference roster changed after admission for ${tagged.category}: admitted ${roles.length.toString()}, configured ${utxos.length.toString()}`,
    );
  }
  return Object.freeze(
    Object.fromEntries(
      roles.map((role, index) => [role, outRef(utxos[index]!)]),
    ),
  );
};

const admittedApplications = new WeakSet<object>();

/** Retains the exact runner/classifier authority captured by this module. */
export const assertWatcherFaultProofApplication = (
  application: WatcherFaultProofApplication,
): void => {
  if (!admittedApplications.has(application)) {
    throw new Error(
      "watcher fault-proof production application is not module-admitted",
    );
  }
};

const predecessorObservationForClassifier = ({
  current,
  predecessor,
}: {
  readonly current: AuthenticatedStateQueueHeaderObservation;
  readonly predecessor: WatcherStateQueueHeaderObservation;
}): AuthenticatedStateQueueHeaderObservation => {
  assertWatcherStateQueueHeaderObservation(predecessor);
  const header = Data.from(predecessor.headerCborHex, Header);
  if (
    Data.to(header, Header) !== predecessor.headerCborHex ||
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
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
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

const createApplication = ({
  options,
  dependencies,
  environment,
  allowExecution,
}: {
  readonly options: WatcherFaultProofApplicationConstructionOptions;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
  readonly environment: NodeJS.ProcessEnv;
  readonly allowExecution: boolean;
}): WatcherFaultProofApplication => {
  const deploymentIdentity = options.deploymentIdentity;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const deploymentAuthority = options.deploymentAuthority;
  const replayTranscriptStore = options.replayTranscriptStore;
  const userEventRuntime = options.userEventRuntime;
  if (allowExecution) {
    if (
      deploymentAuthority === undefined ||
      replayTranscriptStore === undefined ||
      userEventRuntime === undefined
    ) {
      throw new Error(
        "watcher execution requires deployment/rule authority and durable replay transcripts",
      );
    }
    assertWatcherVerifiedDeploymentAuthority(deploymentAuthority);
    assertWatcherUserEventRuntime(userEventRuntime);
    if (
      userEventRuntime.deploymentFingerprint !==
        deploymentIdentity.manifestId ||
      userEventRuntime.blueprintHash !== deploymentIdentity.blueprintHash
    ) {
      throw new Error("watcher event runtime deployment authority differs");
    }
    if (deploymentAuthority.deploymentIdentity !== deploymentIdentity) {
      throw new Error("watcher application deployment authorities differ");
    }
  }
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
    assertWatcherWorkflowFundingProfileOverlay(options.fundingProfileOverlay);
    if (
      options.fundingProfileOverlay.deploymentFingerprint !==
        deploymentIdentity.manifestId ||
      options.fundingProfileOverlay.blueprintHash !==
        deploymentIdentity.blueprintHash
    ) {
      throw new Error(
        "watcher funding-profile overlay changed deployment identity",
      );
    }
  }
  const fundingProfile = (category: WatcherInstalledWorkflowCategory) => {
    const overlay = options.fundingProfileOverlay;
    // Install every runner at startup. Funding is required when a category
    // requests a reservation, so missing measurements do not stop observation.
    if (overlay === undefined || overlay.profiles[category] === undefined) {
      return undefined;
    }
    return workflowFundingProfileFromOverlay({ overlay, category });
  };
  const providerRoster = createHistoricalNativeScriptProviderRoster({
    deploymentFingerprint: deploymentIdentity.manifestId,
    providers: infrastructure.historicalNativeScriptHistory.providers,
  });
  const historySource = createHistoricalNativeScriptHistorySource({
    providerRoster,
  });
  if (allowExecution) {
    requireHistoricalNativeScriptHistoryAuthority({
      deploymentFingerprint: deploymentIdentity.manifestId,
      checkpointStore: options.historicalNativeScriptCheckpointStore,
      historySource,
    });
  }
  const historicalNativeScriptAuthority: WatcherHistoricalNativeScriptAuthority =
    Object.freeze({
      checkpointStore: options.historicalNativeScriptCheckpointStore,
      providerRoster,
      historySource,
      l1SourceRoster: watcherDeploymentReleaseFinalityAuthority(
        deploymentIdentity,
      )
        .verifyForWorkflow({
          deploymentFingerprint: deploymentIdentity.manifestId,
        })
        .then((releaseFinality) =>
          createExternalHistoricalNativeScriptSourceRoster({
            providerRoster,
            releaseFinality,
          }),
        ),
    });
  const environmentSnapshot = Object.freeze({ ...environment });
  const replayContexts = new Map<string, CompleteCanonicalReplayContext>();
  let authorityGeneration = 0;
  const validationCaptures = new Map<
    string,
    Awaited<ReturnType<typeof captureWatcherValidationReplayTranscript>>
  >();
  const retainedDaOptions = {
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
  const retainedDaOwner =
    createWatcherRetainedDaRuntimeOwner(retainedDaOptions);
  const loaderOptions = { ...retainedDaOptions, runtimeOwner: retainedDaOwner };
  function makeTaggedLoader(
    category: "doubleSpend",
  ): TaggedWorkflowLoaderFor<"doubleSpend">;
  function makeTaggedLoader(
    category: "nonExistentInput",
  ): TaggedWorkflowLoaderFor<"nonExistentInput">;
  function makeTaggedLoader(
    category: "nonExistentInputNoIndex",
  ): TaggedWorkflowLoaderFor<"nonExistentInputNoIndex">;
  function makeTaggedLoader(
    category: "invalidRange",
  ): TaggedWorkflowLoaderFor<"invalidRange">;
  function makeTaggedLoader(
    category: "zeroInput",
  ): TaggedWorkflowLoaderFor<"zeroInput">;
  function makeTaggedLoader(
    category: "daHashPreimage",
  ): TaggedWorkflowLoaderFor<"daHashPreimage">;
  function makeTaggedLoader(
    category: "noReferenceInput",
  ): TaggedWorkflowLoaderFor<"noReferenceInput">;
  function makeTaggedLoader(
    category: "referenceInputNoIdx",
  ): TaggedWorkflowLoaderFor<"referenceInputNoIdx">;
  function makeTaggedLoader(
    category: "invalidSignature",
  ): TaggedWorkflowLoaderFor<"invalidSignature">;
  function makeTaggedLoader(
    category: "fabricatedDeposit",
  ): TaggedWorkflowLoaderFor<"fabricatedDeposit">;
  function makeTaggedLoader(
    category: "fabricatedWithdrawal",
  ): TaggedWorkflowLoaderFor<"fabricatedWithdrawal">;
  function makeTaggedLoader(
    category: "withdrawnReferenceInput",
  ): TaggedWorkflowLoaderFor<"withdrawnReferenceInput">;
  function makeTaggedLoader(
    category: "canonicalDecodability",
  ): TaggedWorkflowLoaderFor<"canonicalDecodability">;
  function makeTaggedLoader(
    category: "committedFieldShape",
  ): TaggedWorkflowLoaderFor<"committedFieldShape">;
  function makeTaggedLoader(
    category: "minFee",
  ): TaggedWorkflowLoaderFor<"minFee">;
  function makeTaggedLoader(
    category: "doubleWithdraw",
  ): TaggedWorkflowLoaderFor<"doubleWithdraw">;
  function makeTaggedLoader(
    category: "missingSignature",
  ): TaggedWorkflowLoaderFor<"missingSignature">;
  function makeTaggedLoader(
    category: "missingNativeScriptTx",
  ): TaggedWorkflowLoaderFor<"missingNativeScriptTx">;
  function makeTaggedLoader(
    category: "l2TxMistag",
  ): TaggedWorkflowLoaderFor<"l2TxMistag">;
  function makeTaggedLoader(
    category: "withdrawnInput",
  ): TaggedWorkflowLoaderFor<"withdrawnInput">;
  function makeTaggedLoader(
    category: "inputSetUniqueness",
  ): TaggedWorkflowLoaderFor<"inputSetUniqueness">;
  function makeTaggedLoader(
    category: "networkId",
  ): TaggedWorkflowLoaderFor<"networkId">;
  function makeTaggedLoader(
    category: "mintAuthorization",
  ): TaggedWorkflowLoaderFor<"mintAuthorization">;
  function makeTaggedLoader(
    category: "nativeScriptInvalid",
  ): TaggedWorkflowLoaderFor<"nativeScriptInvalid">;
  function makeTaggedLoader(
    category: "nativeScriptDecoding",
  ): TaggedWorkflowLoaderFor<"nativeScriptDecoding">;
  function makeTaggedLoader(
    category: "crossBlockDuplicateEvent",
  ): TaggedWorkflowLoaderFor<"crossBlockDuplicateEvent">;
  function makeTaggedLoader(
    category: "withdrawalMistag",
  ): TaggedWorkflowLoaderFor<"withdrawalMistag">;
  function makeTaggedLoader(
    category: "missingNativeScriptUtxo",
  ): TaggedWorkflowLoaderFor<"missingNativeScriptUtxo">;
  function makeTaggedLoader(
    category: "transitionTrace",
  ): TaggedWorkflowLoaderFor<"transitionTrace">;
  function makeTaggedLoader(
    category: "minAda",
  ): TaggedWorkflowLoaderFor<"minAda">;
  function makeTaggedLoader(
    category: "valueNotPreserved",
  ): TaggedWorkflowLoaderFor<"valueNotPreserved">;
  function makeTaggedLoader(
    category: "fieldPreimageLengthMismatch",
  ): TaggedWorkflowLoaderFor<"fieldPreimageLengthMismatch">;
  function makeTaggedLoader(
    category: "fieldItemWidthIllegal",
  ): TaggedWorkflowLoaderFor<"fieldItemWidthIllegal">;
  function makeTaggedLoader(
    category: "scriptIntegrityHashMissing",
  ): TaggedWorkflowLoaderFor<"scriptIntegrityHashMissing">;
  function makeTaggedLoader(
    category: "transactionOutputNonCanonical",
  ): TaggedWorkflowLoaderFor<"transactionOutputNonCanonical">;
  function makeTaggedLoader(
    category: "mintItemNonCanonical",
  ): TaggedWorkflowLoaderFor<"mintItemNonCanonical">;
  function makeTaggedLoader(
    category: "resolvedOutputNonCanonical",
  ): TaggedWorkflowLoaderFor<"resolvedOutputNonCanonical">;
  function makeTaggedLoader(
    category: "mintDeclaredAssetLimit",
  ): TaggedWorkflowLoaderFor<"mintDeclaredAssetLimit">;
  function makeTaggedLoader(
    category: "spendInputSignerMissing",
  ): TaggedWorkflowLoaderFor<"spendInputSignerMissing">;
  function makeTaggedLoader(
    category: "protectedOutputSignerMissing",
  ): TaggedWorkflowLoaderFor<"protectedOutputSignerMissing">;
  function makeTaggedLoader(
    category: "observersForbiddenOnUntaggedNetwork",
  ): TaggedWorkflowLoaderFor<"observersForbiddenOnUntaggedNetwork">;
  function makeTaggedLoader(
    category: "witnessScriptDecoding",
  ): TaggedWorkflowLoaderFor<"witnessScriptDecoding">;
  function makeTaggedLoader(
    category: "outputReferenceScriptDecoding",
  ): TaggedWorkflowLoaderFor<"outputReferenceScriptDecoding">;
  function makeTaggedLoader(
    category: "executionSourceScriptDecoding",
  ): TaggedWorkflowLoaderFor<"executionSourceScriptDecoding">;
  function makeTaggedLoader(
    category: "executionNativeScriptInvalid",
  ): TaggedWorkflowLoaderFor<"executionNativeScriptInvalid">;
  function makeTaggedLoader(
    category: "observerOrderInvalid",
  ): TaggedWorkflowLoaderFor<"observerOrderInvalid">;
  function makeTaggedLoader(
    category: "redeemerCanonicity",
  ): TaggedWorkflowLoaderFor<"redeemerCanonicity">;
  function makeTaggedLoader(
    category: "receivePurposeLanguage",
  ): TaggedWorkflowLoaderFor<"receivePurposeLanguage">;
  function makeTaggedLoader(
    category: "unusedScriptWitness",
  ): TaggedWorkflowLoaderFor<"unusedScriptWitness">;
  function makeTaggedLoader(
    category: "missingScriptSource",
  ): TaggedWorkflowLoaderFor<"missingScriptSource">;
  function makeTaggedLoader(
    category: "missingRedeemer",
  ): TaggedWorkflowLoaderFor<"missingRedeemer">;
  function makeTaggedLoader(
    category: "unusedRedeemer",
  ): TaggedWorkflowLoaderFor<"unusedRedeemer">;
  function makeTaggedLoader(
    category: "scriptIntegrityHashMismatch",
  ): TaggedWorkflowLoaderFor<"scriptIntegrityHashMismatch">;
  function makeTaggedLoader(
    category: "distinctAssetAccumulationLimit",
  ): TaggedWorkflowLoaderFor<"distinctAssetAccumulationLimit">;
  function makeTaggedLoader(
    category: "validationTraceDispute",
  ): TaggedWorkflowLoaderFor<"validationTraceDispute">;
  function makeTaggedLoader(
    category: WatcherInstalledWorkflowCategory,
  ): WorkflowRuntimeConfigLoader<TaggedWorkflowConfig> {
    return createWatcherWorkflowRuntimeLoader({
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
    mintAuthorization: makeTaggedLoader("mintAuthorization"),
    nativeScriptInvalid: makeTaggedLoader("nativeScriptInvalid"),
    nativeScriptDecoding: makeTaggedLoader("nativeScriptDecoding"),
    crossBlockDuplicateEvent: makeTaggedLoader("crossBlockDuplicateEvent"),
    withdrawalMistag: makeTaggedLoader("withdrawalMistag"),
    minAda: makeTaggedLoader("minAda"),
    transitionTrace: makeTaggedLoader("transitionTrace"),
    valueNotPreserved: makeTaggedLoader("valueNotPreserved"),
    fieldPreimageLengthMismatch: makeTaggedLoader(
      "fieldPreimageLengthMismatch",
    ),
    fieldItemWidthIllegal: makeTaggedLoader("fieldItemWidthIllegal"),
    scriptIntegrityHashMissing: makeTaggedLoader("scriptIntegrityHashMissing"),
    transactionOutputNonCanonical: makeTaggedLoader(
      "transactionOutputNonCanonical",
    ),
    mintItemNonCanonical: makeTaggedLoader("mintItemNonCanonical"),
    resolvedOutputNonCanonical: makeTaggedLoader("resolvedOutputNonCanonical"),
    mintDeclaredAssetLimit: makeTaggedLoader("mintDeclaredAssetLimit"),
    spendInputSignerMissing: makeTaggedLoader("spendInputSignerMissing"),
    protectedOutputSignerMissing: makeTaggedLoader(
      "protectedOutputSignerMissing",
    ),
    observersForbiddenOnUntaggedNetwork: makeTaggedLoader(
      "observersForbiddenOnUntaggedNetwork",
    ),
    witnessScriptDecoding: makeTaggedLoader("witnessScriptDecoding"),
    outputReferenceScriptDecoding: makeTaggedLoader(
      "outputReferenceScriptDecoding",
    ),
    executionSourceScriptDecoding: makeTaggedLoader(
      "executionSourceScriptDecoding",
    ),
    executionNativeScriptInvalid: makeTaggedLoader(
      "executionNativeScriptInvalid",
    ),
    observerOrderInvalid: makeTaggedLoader("observerOrderInvalid"),
    redeemerCanonicity: makeTaggedLoader("redeemerCanonicity"),
    receivePurposeLanguage: makeTaggedLoader("receivePurposeLanguage"),
    unusedScriptWitness: makeTaggedLoader("unusedScriptWitness"),
    missingScriptSource: makeTaggedLoader("missingScriptSource"),
    missingRedeemer: makeTaggedLoader("missingRedeemer"),
    unusedRedeemer: makeTaggedLoader("unusedRedeemer"),
    scriptIntegrityHashMismatch: makeTaggedLoader(
      "scriptIntegrityHashMismatch",
    ),
    distinctAssetAccumulationLimit: makeTaggedLoader(
      "distinctAssetAccumulationLimit",
    ),
    validationTraceDispute: makeTaggedLoader("validationTraceDispute"),
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
  const mintAuthorizationLoader = async (
    input: Parameters<(typeof taggedLoaders)["mintAuthorization"]>[0],
  ) => {
    const loaded = await taggedLoaders.mintAuthorization(input);
    if (loaded.config.category !== "mintAuthorization") {
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
  const nativeScriptDecodingLoader = async (
    input: Parameters<(typeof taggedLoaders)["nativeScriptDecoding"]>[0],
  ) => {
    const loaded = await taggedLoaders.nativeScriptDecoding(input);
    if (loaded.config.category !== "nativeScriptDecoding") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const withdrawalMistagLoader = async (
    input: Parameters<(typeof taggedLoaders)["withdrawalMistag"]>[0],
  ) => {
    const loaded = await taggedLoaders.withdrawalMistag(input);
    if (loaded.config.category !== "withdrawalMistag") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const crossBlockDuplicateEventLoader = async (
    input: Parameters<(typeof taggedLoaders)["crossBlockDuplicateEvent"]>[0],
  ) => {
    const loaded = await taggedLoaders.crossBlockDuplicateEvent(input);
    if (loaded.config.category !== "crossBlockDuplicateEvent") {
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
  const transitionTraceLoader = async (
    input: Parameters<(typeof taggedLoaders)["transitionTrace"]>[0],
  ) => {
    const loaded = await taggedLoaders.transitionTrace(input);
    if (loaded.config.category !== "transitionTrace") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const valueNotPreservedLoader = async (
    input: Parameters<(typeof taggedLoaders)["valueNotPreserved"]>[0],
  ) => {
    const loaded = await taggedLoaders.valueNotPreserved(input);
    if (loaded.config.category !== "valueNotPreserved") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const fieldPreimageLengthMismatchLoader = async (
    input: Parameters<(typeof taggedLoaders)["fieldPreimageLengthMismatch"]>[0],
  ) => {
    const loaded = await taggedLoaders.fieldPreimageLengthMismatch(input);
    if (loaded.config.category !== "fieldPreimageLengthMismatch") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const fieldItemWidthIllegalLoader = async (
    input: Parameters<(typeof taggedLoaders)["fieldItemWidthIllegal"]>[0],
  ) => {
    const loaded = await taggedLoaders.fieldItemWidthIllegal(input);
    if (loaded.config.category !== "fieldItemWidthIllegal") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const scriptIntegrityHashMissingLoader = async (
    input: Parameters<(typeof taggedLoaders)["scriptIntegrityHashMissing"]>[0],
  ) => {
    const loaded = await taggedLoaders.scriptIntegrityHashMissing(input);
    if (loaded.config.category !== "scriptIntegrityHashMissing") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const transactionOutputNonCanonicalLoader = async (
    input: Parameters<
      (typeof taggedLoaders)["transactionOutputNonCanonical"]
    >[0],
  ) => {
    const loaded = await taggedLoaders.transactionOutputNonCanonical(input);
    if (loaded.config.category !== "transactionOutputNonCanonical") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const mintItemNonCanonicalLoader = async (
    input: Parameters<(typeof taggedLoaders)["mintItemNonCanonical"]>[0],
  ) => {
    const loaded = await taggedLoaders.mintItemNonCanonical(input);
    if (loaded.config.category !== "mintItemNonCanonical") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const resolvedOutputNonCanonicalLoader = async (
    input: Parameters<(typeof taggedLoaders)["resolvedOutputNonCanonical"]>[0],
  ) => {
    const loaded = await taggedLoaders.resolvedOutputNonCanonical(input);
    if (loaded.config.category !== "resolvedOutputNonCanonical") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const mintDeclaredAssetLimitLoader = async (
    input: Parameters<(typeof taggedLoaders)["mintDeclaredAssetLimit"]>[0],
  ) => {
    const loaded = await taggedLoaders.mintDeclaredAssetLimit(input);
    if (loaded.config.category !== "mintDeclaredAssetLimit") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const spendInputSignerMissingLoader = async (
    input: Parameters<(typeof taggedLoaders)["spendInputSignerMissing"]>[0],
  ) => {
    const loaded = await taggedLoaders.spendInputSignerMissing(input);
    if (loaded.config.category !== "spendInputSignerMissing") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const protectedOutputSignerMissingLoader = async (
    input: Parameters<
      (typeof taggedLoaders)["protectedOutputSignerMissing"]
    >[0],
  ) => {
    const loaded = await taggedLoaders.protectedOutputSignerMissing(input);
    if (loaded.config.category !== "protectedOutputSignerMissing") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const observersForbiddenOnUntaggedNetworkLoader = async (
    input: Parameters<
      (typeof taggedLoaders)["observersForbiddenOnUntaggedNetwork"]
    >[0],
  ) => {
    const loaded =
      await taggedLoaders.observersForbiddenOnUntaggedNetwork(input);
    if (loaded.config.category !== "observersForbiddenOnUntaggedNetwork") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const witnessScriptDecodingLoader = async (
    input: Parameters<(typeof taggedLoaders)["witnessScriptDecoding"]>[0],
  ) => {
    const loaded = await taggedLoaders.witnessScriptDecoding(input);
    if (loaded.config.category !== "witnessScriptDecoding") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const outputReferenceScriptDecodingLoader = async (
    input: Parameters<
      (typeof taggedLoaders)["outputReferenceScriptDecoding"]
    >[0],
  ) => {
    const loaded = await taggedLoaders.outputReferenceScriptDecoding(input);
    if (loaded.config.category !== "outputReferenceScriptDecoding") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const executionSourceScriptDecodingLoader = async (
    input: Parameters<
      (typeof taggedLoaders)["executionSourceScriptDecoding"]
    >[0],
  ) => {
    const loaded = await taggedLoaders.executionSourceScriptDecoding(input);
    if (loaded.config.category !== "executionSourceScriptDecoding") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const executionNativeScriptInvalidLoader = async (
    input: Parameters<
      (typeof taggedLoaders)["executionNativeScriptInvalid"]
    >[0],
  ) => {
    const loaded = await taggedLoaders.executionNativeScriptInvalid(input);
    if (loaded.config.category !== "executionNativeScriptInvalid") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const observerOrderInvalidLoader = async (
    input: Parameters<(typeof taggedLoaders)["observerOrderInvalid"]>[0],
  ) => {
    const loaded = await taggedLoaders.observerOrderInvalid(input);
    if (loaded.config.category !== "observerOrderInvalid") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const redeemerCanonicityLoader = async (
    input: Parameters<(typeof taggedLoaders)["redeemerCanonicity"]>[0],
  ) => {
    const loaded = await taggedLoaders.redeemerCanonicity(input);
    if (loaded.config.category !== "redeemerCanonicity") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const receivePurposeLanguageLoader = async (
    input: Parameters<(typeof taggedLoaders)["receivePurposeLanguage"]>[0],
  ) => {
    const loaded = await taggedLoaders.receivePurposeLanguage(input);
    if (loaded.config.category !== "receivePurposeLanguage") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const unusedScriptWitnessLoader = async (
    input: Parameters<(typeof taggedLoaders)["unusedScriptWitness"]>[0],
  ) => {
    const loaded = await taggedLoaders.unusedScriptWitness(input);
    if (loaded.config.category !== "unusedScriptWitness") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const missingScriptSourceLoader = async (
    input: Parameters<(typeof taggedLoaders)["missingScriptSource"]>[0],
  ) => {
    const loaded = await taggedLoaders.missingScriptSource(input);
    if (loaded.config.category !== "missingScriptSource") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const missingRedeemerLoader = async (
    input: Parameters<(typeof taggedLoaders)["missingRedeemer"]>[0],
  ) => {
    const loaded = await taggedLoaders.missingRedeemer(input);
    if (loaded.config.category !== "missingRedeemer") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const unusedRedeemerLoader = async (
    input: Parameters<(typeof taggedLoaders)["unusedRedeemer"]>[0],
  ) => {
    const loaded = await taggedLoaders.unusedRedeemer(input);
    if (loaded.config.category !== "unusedRedeemer") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const scriptIntegrityHashMismatchLoader = async (
    input: Parameters<(typeof taggedLoaders)["scriptIntegrityHashMismatch"]>[0],
  ) => {
    const loaded = await taggedLoaders.scriptIntegrityHashMismatch(input);
    if (loaded.config.category !== "scriptIntegrityHashMismatch") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const distinctAssetAccumulationLimitLoader = async (
    input: Parameters<
      (typeof taggedLoaders)["distinctAssetAccumulationLimit"]
    >[0],
  ) => {
    const loaded = await taggedLoaders.distinctAssetAccumulationLimit(input);
    if (loaded.config.category !== "distinctAssetAccumulationLimit") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    return Object.freeze({
      ...loaded,
      config: loaded.config.config,
      tagged: loaded.config,
    });
  };
  const validationTraceDisputeLoader = async (
    input: Parameters<(typeof taggedLoaders)["validationTraceDispute"]>[0],
  ) => {
    const loaded = await taggedLoaders.validationTraceDispute(input);
    if (loaded.config.category !== "validationTraceDispute") {
      await loaded.close();
      throw new Error("workflow loader changed its fixed category");
    }
    const decisionDigest = (
      input.invocation as Partial<WorkflowAdapterRunnerInput>
    ).decisionDigest;
    if (decisionDigest === undefined) {
      return Object.freeze({
        ...loaded,
        config: loaded.config.config,
        tagged: loaded.config,
      });
    }
    try {
      const capture = validationCaptures.get(decisionDigest);
      if (
        capture === undefined ||
        capture.transcript.headerHash !== input.invocation.headerHash
      ) {
        throw new Error(
          "validation execution has no freshly captured classifier transcript",
        );
      }
      await refreshWatcherValidationReplayCapture(capture);
      assertWatcherValidationReplayCaptureCurrent(capture);
      if (validationCaptures.get(decisionDigest) !== capture) {
        throw new Error(
          "validation decision authority was retired during workflow loading",
        );
      }
      return Object.freeze({
        ...loaded,
        config: Object.freeze({
          ...loaded.config.config,
          challenge: capture.challenge,
        }),
        tagged: loaded.config,
      });
    } catch (error) {
      await loaded.close();
      throw error;
    }
  };

  const runners = Object.freeze({
    doubleSpend: createDoubleSpendWorkflowRunner(
      doubleSpendLoader,
      fundingProfile("doubleSpend"),
    ),
    nonExistentInput: WORKFLOW_RUNNER_FACTORIES.nonExistentInput(
      nonExistentInputLoader,
      fundingProfile("nonExistentInput"),
    ),
    nonExistentInputNoIndex: WORKFLOW_RUNNER_FACTORIES.nonExistentInputNoIndex(
      inputNoIdxLoader,
      fundingProfile("nonExistentInputNoIndex"),
    ),
    invalidRange: WORKFLOW_RUNNER_FACTORIES.invalidRange(
      invalidRangeLoader,
      fundingProfile("invalidRange"),
    ),
    zeroInput: WORKFLOW_RUNNER_FACTORIES.zeroInput(
      zeroInputLoader,
      fundingProfile("zeroInput"),
    ),
    daHashPreimage: WORKFLOW_RUNNER_FACTORIES.daHashPreimage(
      daHashPreimageLoader,
      fundingProfile("daHashPreimage"),
    ),
    noReferenceInput: WORKFLOW_RUNNER_FACTORIES.noReferenceInput(
      noReferenceInputLoader,
      fundingProfile("noReferenceInput"),
    ),
    referenceInputNoIdx: WORKFLOW_RUNNER_FACTORIES.referenceInputNoIdx(
      referenceInputNoIdxLoader,
      fundingProfile("referenceInputNoIdx"),
    ),
    invalidSignature: WORKFLOW_RUNNER_FACTORIES.invalidSignature(
      invalidSignatureLoader,
      fundingProfile("invalidSignature"),
    ),
    fabricatedDeposit: WORKFLOW_RUNNER_FACTORIES.fabricatedDeposit(
      fabricatedDepositLoader,
      fundingProfile("fabricatedDeposit"),
    ),
    fabricatedWithdrawal: WORKFLOW_RUNNER_FACTORIES.fabricatedWithdrawal(
      fabricatedWithdrawalLoader,
      fundingProfile("fabricatedWithdrawal"),
    ),
    withdrawnReferenceInput: WORKFLOW_RUNNER_FACTORIES.withdrawnReferenceInput(
      withdrawnReferenceInputLoader,
      fundingProfile("withdrawnReferenceInput"),
    ),
    canonicalDecodability: WORKFLOW_RUNNER_FACTORIES.canonicalDecodability(
      canonicalDecodabilityLoader,
      fundingProfile("canonicalDecodability"),
    ),
    committedFieldShape: WORKFLOW_RUNNER_FACTORIES.committedFieldShape(
      committedFieldShapeLoader,
      fundingProfile("committedFieldShape"),
    ),
    minFee: WORKFLOW_RUNNER_FACTORIES.minFee(
      minFeeLoader,
      fundingProfile("minFee"),
    ),
    doubleWithdraw: WORKFLOW_RUNNER_FACTORIES.doubleWithdraw(
      doubleWithdrawLoader,
      fundingProfile("doubleWithdraw"),
    ),
    missingSignature: createMissingSignatureWorkflowRunner(
      missingSignatureLoader,
      fundingProfile("missingSignature"),
    ),
    missingNativeScriptTx: WORKFLOW_RUNNER_FACTORIES.missingNativeScriptTx(
      missingNativeScriptTxLoader,
      fundingProfile("missingNativeScriptTx"),
    ),
    l2TxMistag: WORKFLOW_RUNNER_FACTORIES.l2TxMistag(
      l2TxMistagLoader,
      fundingProfile("l2TxMistag"),
    ),
    withdrawnInput: WORKFLOW_RUNNER_FACTORIES.withdrawnInput(
      withdrawnInputLoader,
      fundingProfile("withdrawnInput"),
    ),
    inputSetUniqueness: WORKFLOW_RUNNER_FACTORIES.inputSetUniqueness(
      inputSetUniquenessLoader,
      fundingProfile("inputSetUniqueness"),
    ),
    networkId: createNetworkIdWorkflowRunner(
      networkIdLoader,
      fundingProfile("networkId"),
    ),
    missingNativeScriptUtxo: WORKFLOW_RUNNER_FACTORIES.missingNativeScriptUtxo(
      missingNativeScriptUtxoLoader,
      fundingProfile("missingNativeScriptUtxo"),
    ),
    mintAuthorization: WORKFLOW_RUNNER_FACTORIES.mintAuthorization(
      mintAuthorizationLoader,
      fundingProfile("mintAuthorization"),
    ),
    nativeScriptInvalid: WORKFLOW_RUNNER_FACTORIES.nativeScriptInvalid(
      nativeScriptInvalidLoader,
      fundingProfile("nativeScriptInvalid"),
    ),
    nativeScriptDecoding: WORKFLOW_RUNNER_FACTORIES.nativeScriptDecoding(
      nativeScriptDecodingLoader,
      fundingProfile("nativeScriptDecoding"),
    ),
    withdrawalMistag: WORKFLOW_RUNNER_FACTORIES.withdrawalMistag(
      withdrawalMistagLoader,
      fundingProfile("withdrawalMistag"),
    ),
    crossBlockDuplicateEvent:
      WORKFLOW_RUNNER_FACTORIES.crossBlockDuplicateEvent(
        crossBlockDuplicateEventLoader,
        fundingProfile("crossBlockDuplicateEvent"),
      ),
    minAda: WORKFLOW_RUNNER_FACTORIES.minAda(
      minAdaLoader,
      fundingProfile("minAda"),
    ),
    transitionTrace: WORKFLOW_RUNNER_FACTORIES.transitionTrace(
      transitionTraceLoader,
      fundingProfile("transitionTrace"),
    ),
    valueNotPreserved: createValueConservationWorkflowRunner(
      valueNotPreservedLoader,
      fundingProfile("valueNotPreserved"),
    ),
    fieldPreimageLengthMismatch:
      WORKFLOW_RUNNER_FACTORIES.fieldPreimageLengthMismatch(
        fieldPreimageLengthMismatchLoader,
        fundingProfile("fieldPreimageLengthMismatch"),
      ),
    fieldItemWidthIllegal: WORKFLOW_RUNNER_FACTORIES.fieldItemWidthIllegal(
      fieldItemWidthIllegalLoader,
      fundingProfile("fieldItemWidthIllegal"),
    ),
    scriptIntegrityHashMissing:
      WORKFLOW_RUNNER_FACTORIES.scriptIntegrityHashMissing(
        scriptIntegrityHashMissingLoader,
        fundingProfile("scriptIntegrityHashMissing"),
      ),
    transactionOutputNonCanonical:
      WORKFLOW_RUNNER_FACTORIES.transactionOutputNonCanonical(
        transactionOutputNonCanonicalLoader,
        fundingProfile("transactionOutputNonCanonical"),
      ),
    mintItemNonCanonical: WORKFLOW_RUNNER_FACTORIES.mintItemNonCanonical(
      mintItemNonCanonicalLoader,
      fundingProfile("mintItemNonCanonical"),
    ),
    resolvedOutputNonCanonical:
      WORKFLOW_RUNNER_FACTORIES.resolvedOutputNonCanonical(
        resolvedOutputNonCanonicalLoader,
        fundingProfile("resolvedOutputNonCanonical"),
      ),
    mintDeclaredAssetLimit: WORKFLOW_RUNNER_FACTORIES.mintDeclaredAssetLimit(
      mintDeclaredAssetLimitLoader,
      fundingProfile("mintDeclaredAssetLimit"),
    ),
    spendInputSignerMissing: WORKFLOW_RUNNER_FACTORIES.spendInputSignerMissing(
      spendInputSignerMissingLoader,
      fundingProfile("spendInputSignerMissing"),
    ),
    protectedOutputSignerMissing:
      WORKFLOW_RUNNER_FACTORIES.protectedOutputSignerMissing(
        protectedOutputSignerMissingLoader,
        fundingProfile("protectedOutputSignerMissing"),
      ),
    observersForbiddenOnUntaggedNetwork:
      WORKFLOW_RUNNER_FACTORIES.observersForbiddenOnUntaggedNetwork(
        observersForbiddenOnUntaggedNetworkLoader,
        fundingProfile("observersForbiddenOnUntaggedNetwork"),
      ),
    witnessScriptDecoding: WORKFLOW_RUNNER_FACTORIES.witnessScriptDecoding(
      witnessScriptDecodingLoader,
      fundingProfile("witnessScriptDecoding"),
    ),
    outputReferenceScriptDecoding:
      WORKFLOW_RUNNER_FACTORIES.outputReferenceScriptDecoding(
        outputReferenceScriptDecodingLoader,
        fundingProfile("outputReferenceScriptDecoding"),
      ),
    executionSourceScriptDecoding:
      WORKFLOW_RUNNER_FACTORIES.executionSourceScriptDecoding(
        executionSourceScriptDecodingLoader,
        fundingProfile("executionSourceScriptDecoding"),
      ),
    executionNativeScriptInvalid:
      WORKFLOW_RUNNER_FACTORIES.executionNativeScriptInvalid(
        executionNativeScriptInvalidLoader,
        fundingProfile("executionNativeScriptInvalid"),
      ),
    observerOrderInvalid: WORKFLOW_RUNNER_FACTORIES.observerOrderInvalid(
      observerOrderInvalidLoader,
      fundingProfile("observerOrderInvalid"),
    ),
    redeemerCanonicity: WORKFLOW_RUNNER_FACTORIES.redeemerCanonicity(
      redeemerCanonicityLoader,
      fundingProfile("redeemerCanonicity"),
    ),
    receivePurposeLanguage: WORKFLOW_RUNNER_FACTORIES.receivePurposeLanguage(
      receivePurposeLanguageLoader,
      fundingProfile("receivePurposeLanguage"),
    ),
    unusedScriptWitness: WORKFLOW_RUNNER_FACTORIES.unusedScriptWitness(
      unusedScriptWitnessLoader,
      fundingProfile("unusedScriptWitness"),
    ),
    missingScriptSource: WORKFLOW_RUNNER_FACTORIES.missingScriptSource(
      missingScriptSourceLoader,
      fundingProfile("missingScriptSource"),
    ),
    missingRedeemer: WORKFLOW_RUNNER_FACTORIES.missingRedeemer(
      missingRedeemerLoader,
      fundingProfile("missingRedeemer"),
    ),
    unusedRedeemer: WORKFLOW_RUNNER_FACTORIES.unusedRedeemer(
      unusedRedeemerLoader,
      fundingProfile("unusedRedeemer"),
    ),
    scriptIntegrityHashMismatch:
      WORKFLOW_RUNNER_FACTORIES.scriptIntegrityHashMismatch(
        scriptIntegrityHashMismatchLoader,
        fundingProfile("scriptIntegrityHashMismatch"),
      ),
    distinctAssetAccumulationLimit:
      WORKFLOW_RUNNER_FACTORIES.distinctAssetAccumulationLimit(
        distinctAssetAccumulationLimitLoader,
        fundingProfile("distinctAssetAccumulationLimit"),
      ),
    validationTraceDispute: createValidationTraceDisputeWorkflowRunner(
      validationTraceDisputeLoader,
      fundingProfile("validationTraceDispute"),
    ),
  });
  const applicationRegistry = installWorkflowApplicationRegistry({
    deploymentFingerprint: deploymentIdentity.manifestId,
    requiredInstalledCategories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    installations: WATCHER_INSTALLED_WORKFLOW_CATEGORIES.map((category) => ({
      category,
      deploymentFingerprint: deploymentIdentity.manifestId,
      runner: runners[category],
    })),
  });
  let classifierPromise: ReturnType<typeof createHeaderClassifier> | undefined;
  const loadClassifier = (watcherConfigValue: unknown, headerHash: string) => {
    classifierPromise ??= (async () => {
      const watcherConfig = parseWatcherConfig(watcherConfigValue);
      if (watcherConfig.l1.source.sourceMode !== "local_node")
        throw new Error(
          "cross-block settlement authority requires local-node source",
        );
      const kupo = watcherConfig.l1.source.queryServices.find(
        (service) => service.kind === "kupo",
      );
      const ogmios = watcherConfig.l1.source.queryServices.find(
        (service) => service.kind === "ogmios",
      );
      if (kupo === undefined || ogmios === undefined)
        throw new Error(
          "cross-block settlement authority requires Kupo and Ogmios",
        );
      const [manifestJson, blueprintJson, deploymentInfoJson] =
        await Promise.all(
          [
            infrastructure.manifestPath,
            infrastructure.blueprintPath,
            infrastructure.deploymentInfoPath,
          ].map(
            async (path) =>
              await dependencies.readText(
                await requireCanonicalFile(path, dependencies),
              ),
          ),
        );
      const binding = await bindFraudProofWorkflowDeployment({
        manifest: JSON.parse(manifestJson!),
        blueprintJson: blueprintJson!,
        deploymentInfo: JSON.parse(deploymentInfoJson!),
        category: "crossBlockDuplicateEvent",
        headerHash,
        proverCredential: "00".repeat(28),
        stepDatumSchemas: [
          FraudProofComputationThreadStepDatum,
          CrossBlockDuplicateEventStep02DatumSchema,
        ],
      });
      if (binding.deploymentFingerprint !== deploymentIdentity.manifestId)
        throw new Error("cross-block settlement classifier changed deployment");
      const settlementAuthority = createCrossBlockSettlementAuthority({
        binding,
        source: {
          sourceId: `watcher-settlement-history/${deploymentIdentity.manifestId}`,
          kupoHttpUrl: kupo.endpoint,
          ogmiosUrl: ogmios.endpoint,
          timeoutMs: watcherConfig.l1.requestTimeoutMs,
        },
        historySource: historicalNativeScriptAuthority.historySource,
        checkpointStore: historicalNativeScriptAuthority.checkpointStore,
      });
      const transitionBinding = await bindFraudProofWorkflowDeployment({
        manifest: JSON.parse(manifestJson!),
        blueprintJson: blueprintJson!,
        deploymentInfo: JSON.parse(deploymentInfoJson!),
        category: "transitionTrace",
        headerHash,
        proverCredential: "00".repeat(28),
        stepDatumSchemas: TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
      });
      if (
        transitionBinding.deploymentFingerprint !==
        deploymentIdentity.manifestId
      )
        throw new Error("transition trace classifier changed deployment");
      const transitionTraceEventAuthority = createTransitionTraceEventAuthority(
        {
          binding: transitionBinding,
          source: {
            sourceId: `watcher-transition-events/${deploymentIdentity.manifestId}`,
            kupoHttpUrl: kupo.endpoint,
            ogmiosUrl: ogmios.endpoint,
            timeoutMs: watcherConfig.l1.requestTimeoutMs,
          },
        },
      );
      await watcherDeploymentReleaseFinalityAuthority(
        deploymentIdentity,
      ).verifyForWorkflow({
        deploymentFingerprint: deploymentIdentity.manifestId,
      });
      const lucid = await dependencies.makeLucid({
        network: watcherConfig.targetNetwork,
        slotConfig: watcherConfig.customNetwork?.slotConfig,
        kupoHttpUrl: kupo.endpoint,
        ogmiosUrl: ogmios.endpoint,
      });
      const proverSecret = await readSecret({
        source: watcherConfig.proverWallet.keySource,
        dependencies,
        environment: environmentSnapshot,
        label: "watcher prover wallet",
      });
      const signer = dependencies.resolveSigner({
        network: watcherConfig.targetNetwork,
        secret: proverSecret,
      });
      const replayer = createCatalogueCompleteCanonicalReplay({
        lucid,
        network: watcherConfig.targetNetwork,
        hubOraclePolicyId:
          watcherDeploymentProtocolScriptAuthority(deploymentIdentity)
            .protocolScriptHashes.hubOracleMint,
        minimumConfirmationDepth: 1,
        owner: signer.paymentKeyHash,
      });
      if (
        replayer.launchScope.length !==
          WATCHER_INSTALLED_WORKFLOW_CATEGORIES.length ||
        replayer.launchScope.some(
          (category, index) =>
            category !== WATCHER_INSTALLED_WORKFLOW_CATEGORIES[index],
        )
      )
        throw new Error(
          "Watcher classifier differs from its exact installed workflow catalogue",
        );
      return await createHeaderClassifier({
        transitionTraceEventAuthority,
        deploymentFingerprint: deploymentIdentity.manifestId,
        replayer,
        releaseFinalityAuthority:
          watcherDeploymentReleaseFinalityAuthority(deploymentIdentity),
        settlementAuthority,
        historicalReplayAuthority: Object.freeze({
          checkpointStore: historicalNativeScriptAuthority.checkpointStore,
          historySource: historicalNativeScriptAuthority.historySource,
        }),
      });
    })();
    return classifierPromise;
  };
  const application: WatcherFaultProofApplication = Object.freeze({
    close: async () => {
      admittedApplications.delete(application);
      authorityGeneration += 1;
      replayContexts.clear();
      validationCaptures.clear();
      await retainedDaOwner.close();
    },
    schemaVersion: WATCHER_FAULT_PROOF_APPLICATION,
    deploymentFingerprint: deploymentIdentity.manifestId,
    installedCategories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    runners,
    applicationRegistry,
    decisionUsesLocalEventHistory: (decisionDigest) =>
      validationCaptures.has(decisionDigest),
    retainDecisionAuthorities: (decisionDigest) => {
      authorityGeneration += 1;
      for (const digest of replayContexts.keys()) {
        if (digest !== decisionDigest) replayContexts.delete(digest);
      }
      for (const digest of validationCaptures.keys()) {
        if (digest !== decisionDigest) validationCaptures.delete(digest);
      }
    },
    classifyHeader: async (request) => {
      const generation = authorityGeneration;
      const input = Object.freeze({
        ...request,
        observation: structuredClone(request.observation),
      });
      assertWatcherStateQueueObservation(input.stateQueueObservation);
      assertWatcherStateQueueHeaderObservation(input.header);
      if (
        input.stateQueueObservation.deploymentIdentityDigest !==
          deploymentIdentity.manifestId ||
        !input.stateQueueObservation.finalizedHeaders.includes(input.header) ||
        input.observation.sourceMode !== "local_node" ||
        input.observation.provenance.sourceId !==
          input.stateQueueObservation.sourceId ||
        input.observation.headerHash !== input.header.headerHash ||
        Data.to(input.observation.header, Header) !==
          input.header.headerCborHex ||
        input.observation.chainPoint.slot.toString() !==
          input.header.observedSlot ||
        input.observation.chainPoint.blockHash !==
          input.header.observedBlockHash ||
        input.observation.confirmationDepth.toString() !==
          input.header.finalityDepth
      ) {
        throw new Error(
          "classifier observation differs from its authenticated queue header",
        );
      }
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
      const retainedDa = await retainedDaOwner.createRuntime(watcherConfig);
      let completedDecision: HeaderDecision;
      let pendingCapture:
        | Awaited<ReturnType<typeof captureWatcherValidationReplayTranscript>>
        | undefined;
      try {
        if (
          retainedDa.deploymentFingerprint !== deploymentIdentity.manifestId
        ) {
          throw new Error(
            "watcher retained-DA runtime changed deployment identity",
          );
        }
        const decision = await classifyProductionHeaderV1({
          classifier: await loadClassifier(
            watcherConfig,
            input.observation.headerHash,
          ),
          observation: input.observation,
          authenticatedObservationDigest: input.authenticatedObservationDigest,
          sources: retainedDa.sources,
          ...(input.predecessor === undefined
            ? {}
            : {
                predecessorObservation: predecessorObservationForClassifier({
                  current: input.observation,
                  predecessor: input.predecessor,
                }),
              }),
          ...(input.retries === undefined ? {} : { retries: input.retries }),
        });
        const replayContext = headerDecisionReplayContext(decision);
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
        if (
          decision.decision === "fault_detected" &&
          decision.category === "validationTraceDispute"
        ) {
          if (
            deploymentAuthority === undefined ||
            replayTranscriptStore === undefined ||
            userEventRuntime === undefined
          ) {
            throw new Error(
              "validation classification requires live deployment authority and transcript storage",
            );
          }
          const identity = {
            deploymentFingerprint: deploymentIdentity.manifestId,
            headerHash: input.header.headerHash,
            inclusionPoint: {
              transactionHash: input.header.observedTransactionHash,
              blockHash: input.header.observedBlockHash,
              blockNo: input.header.observedBlockNo,
              slot: input.header.observedSlot,
              chainPointId: input.header.observedChainPointId,
            },
          };
          const archived = await replayTranscriptStore.read(identity);
          const capture = await captureWatcherValidationReplayTranscript({
            deploymentAuthority,
            stateQueueObservation: input.stateQueueObservation,
            header: input.header,
            decision,
            userEventRuntime,
            ...(archived === null
              ? {}
              : {
                  persistedTranscriptCborHex:
                    archived.persistedTranscriptCborHex,
                }),
          });
          if (
            !(await replayTranscriptStore.compareAndSwap({
              expectedTranscriptDigest: archived?.headTranscriptDigest ?? null,
              transcript: capture.transcript,
            }))
          ) {
            throw new Error(
              "validation transcript head changed during capture; classify again",
            );
          }
          pendingCapture = capture;
        }
        completedDecision = decision;
      } finally {
        await retainedDa.close();
      }
      if (pendingCapture !== undefined) {
        await refreshWatcherValidationReplayCapture(pendingCapture);
        assertWatcherValidationReplayCaptureCurrent(pendingCapture);
      }
      if (generation !== authorityGeneration) {
        throw new Error(
          "decision authority was invalidated during classification",
        );
      }
      if (pendingCapture !== undefined) {
        validationCaptures.set(
          completedDecision.decisionDigest,
          pendingCapture,
        );
      }
      const replayContext = headerDecisionReplayContext(completedDecision);
      if (replayContext !== undefined) {
        replayContexts.set(completedDecision.decisionDigest, replayContext);
      }
      return completedDecision;
    },
    assertStartupReady: async (invocation) => {
      if (
        !WATCHER_INSTALLED_WORKFLOW_CATEGORIES.includes(
          invocation.category as WatcherInstalledWorkflowCategory,
        )
      ) {
        throw new Error(
          `watcher has no installed production workflow for ${invocation.category}`,
        );
      }
      const category = invocation.category as WatcherInstalledWorkflowCategory;
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
          schemaVersion: WATCHER_FAULT_PROOF_STARTUP_READINESS,
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
    verifyCompleted: async (input) => {
      if (!admittedApplications.has(application))
        throw new Error("watcher fault-proof application is not admitted");
      if (!WATCHER_INSTALLED_WORKFLOW_CATEGORIES.includes(input.category))
        throw new Error("completed workflow category is not installed");
      const [runtimeJson, manifestJson, blueprintJson, deploymentJson] =
        await Promise.all(
          [
            input.runtimeConfigPath,
            infrastructure.manifestPath,
            infrastructure.blueprintPath,
            infrastructure.deploymentInfoPath,
          ].map(async (path) =>
            dependencies.readText(
              await requireCanonicalFile(path, dependencies),
            ),
          ),
        );
      const config = parseWatcherConfig(JSON.parse(runtimeJson!));
      if (config.l1.source.sourceMode !== "local_node")
        throw new Error(
          "completed workflow verification requires local-node authority",
        );
      const kupo = config.l1.source.queryServices.find(
        ({ kind }) => kind === "kupo",
      );
      const ogmios = config.l1.source.queryServices.find(
        ({ kind }) => kind === "ogmios",
      );
      if (kupo === undefined || ogmios === undefined)
        throw new Error("completed workflow authority omitted Kupo or Ogmios");
      const binding = await bindFraudProofTerminalDeployment({
        manifest: JSON.parse(manifestJson!),
        blueprintJson: blueprintJson!,
        deploymentInfo: JSON.parse(deploymentJson!),
        category: input.category,
        headerHash: input.headerHash,
        proverCredential: input.terminal.economics.proverCredential,
      });
      const finality = await watcherDeploymentReleaseFinalityAuthority(
        deploymentIdentity,
      ).verifyForWorkflow({
        deploymentFingerprint: deploymentIdentity.manifestId,
      });
      if (
        binding.deploymentFingerprint !== deploymentIdentity.manifestId ||
        journalJsonDigest(normalizeJournalJson(binding.releaseFinality)) !==
          journalJsonDigest(normalizeJournalJson(finality))
      )
        throw new Error(
          "completed workflow changed its verified deployment release",
        );
      const source = createLocalKupmiosHttpOgmiosRawSource({
        sourceId: [
          "watcher-fault-proof",
          input.category,
          deploymentIdentity.manifestId,
          config.l1.source.authorityNodeId,
          config.l1.source.chainSync.genesisIdentitySha256,
        ].join("/"),
        kupoHttpUrl: kupo.endpoint,
        ogmiosUrl: ogmios.endpoint,
        timeoutMs: config.l1.requestTimeoutMs,
        releaseFinality: binding.releaseFinality,
        observationDepth: "inclusion",
      });
      const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
        source,
        releaseFinality: binding.releaseFinality,
        observationDepth: "inclusion",
      });
      try {
        return await verifyCompletedFraudProofWorkflow({
          binding,
          authority,
          entries: input.entries,
          terminal: input.terminal,
          decisionDigest: input.decisionDigest,
        });
      } catch (error) {
        if (
          error instanceof LocalKupmiosCheckpointChangedError ||
          error instanceof LocalKupmiosExactPointNotCanonicalError
        )
          return { kind: "pending", reason: "checkpoint_changed" };
        throw error;
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
      return await runFraudProofWorkflowCli({
        ...invocation,
        applicationRegistry,
      });
    },
  });
  admittedApplications.add(application);
  return application;
};

export const createWatcherFaultProofApplication = (
  options: WatcherFaultProofApplicationOptions,
): WatcherFaultProofApplication =>
  createApplication({
    options: Object.freeze({
      deploymentIdentity: options.deploymentAuthority.deploymentIdentity,
      deploymentAuthority: options.deploymentAuthority,
      replayTranscriptStore: options.replayTranscriptStore,
      userEventRuntime: options.userEventRuntime,
      infrastructure: options.infrastructure,
      historicalNativeScriptCheckpointStore:
        options.historicalNativeScriptCheckpointStore,
      fundingProfileOverlay: options.fundingProfileOverlay,
    }),
    dependencies: productionDependencies,
    environment: process.env,
    allowExecution: true,
  });

/** Bind installed production workflows without exposing execution or classification. */
export const createWatcherFaultProofReadinessApplication = (
  options: Pick<
    WatcherFaultProofApplicationOptions,
    | "deploymentAuthority"
    | "infrastructure"
    | "historicalNativeScriptCheckpointStore"
    | "fundingProfileOverlay"
  >,
): Pick<
  WatcherFaultProofApplication,
  "installedCategories" | "assertStartupReady" | "close"
> => {
  assertWatcherVerifiedDeploymentAuthority(options.deploymentAuthority);
  const application = createApplication({
    options: {
      ...options,
      deploymentIdentity: options.deploymentAuthority.deploymentIdentity,
    },
    dependencies: productionDependencies,
    environment: process.env,
    allowExecution: false,
  });
  return Object.freeze({
    installedCategories: application.installedCategories,
    assertStartupReady: application.assertStartupReady,
    close: application.close,
  });
};

/** Narrow test-only dependency seam. It cannot execute transactions. */
export const unsafeCreateWatcherFaultProofApplicationForTest = (
  options: WatcherFaultProofApplicationConstructionOptions,
  dependencies: WatcherFaultProofApplicationDependencies,
  environment: NodeJS.ProcessEnv = {},
): WatcherFaultProofApplication =>
  createApplication({
    options,
    dependencies,
    environment,
    allowExecution: false,
  });
