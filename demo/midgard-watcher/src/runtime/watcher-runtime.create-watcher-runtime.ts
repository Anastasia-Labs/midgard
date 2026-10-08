import { readFile } from "node:fs/promises";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";

import {
  createWatcherAvailabilityRuntime,
  type WatcherAvailabilityRuntime,
  type WatcherAvailabilityStatusTransition,
} from "../availability/runtime.js";
import {
  createWatcherFaultDecisionBridge,
  type WatcherFaultDecisionBridge,
} from "../fault-proofs/fault-decision-bridge.js";
import { type WatcherFaultProofApplication } from "../fault-proofs/fault-proof-application.js";
import { createWatcherFaultProofExecution } from "../fault-proofs/fault-proof-execution.js";
import {
  createWatcherFaultProofSupervisor,
  type WatcherFaultProofSupervisor,
} from "../fault-proofs/fault-proof-supervisor.js";
import { createWatcherProtocolParameterRuntimeAuthority } from "../funding/prover-funding.js";
import type { WatcherSqliteProverFundingReservationStoreRuntime } from "../funding/sqlite-prover-funding-reservation-store.js";
import { createWatcherLocalKupmiosNativeObservationRuntime } from "../l1/local-kupmios-native-observation.js";
import {
  startWatcherNativeChainSyncWithRetry,
  type WatcherNativeChainSyncRuntime,
} from "../l1/native-chain-sync.js";
import { createWatcherDurableRuntime } from "../storage/durable-runtime.js";
import {
  bindWatcherRetainedDaOperations,
  type WatcherRetainedDaOperationsBinding,
} from "../storage/retained-da-runtime.js";
import {
  createWatcherChainCoordinator,
  type WatcherChainCoordinator,
} from "./chain-coordinator.js";
import { oldestRetainedCanonicalHint } from "./chain-coordinator.retained-canonical-prefix.js";
import { createWatcherHistoryRecovery } from "./history-recovery.js";
import {
  startWatcherOperationsHttpServer,
  type WatcherOperationsHttpServer,
} from "./operations-http.js";
import {
  createWatcherOperationsObservability,
  watcherDaBondPoolReadFailureReporter,
  watcherDaBondPoolReporter,
} from "./operations-observability.js";
import {
  loadWatcherSecretText,
  type WatcherProcessConfig,
} from "./process-config.js";
import { watcherReplayTranscriptRetirementHooks } from "./replay-transcript-retirement.js";
import {
  createWatcherStartupProgress,
  type WatcherStartupProgress,
} from "./startup-progress.js";
import {
  createWatcherUserEventRuntime,
  type WatcherUserEventRuntime,
} from "./user-event-runtime.js";
import { openWatcherProverFundingRuntime } from "./watcher-prover-funding-runtime.js";
import { closeWatcherAllocatedResources } from "./watcher-runtime.close-allocated-resources.js";
import {
  createWatcherRuntimeLifecycle,
  createWatcherRuntimeSignals,
} from "./watcher-runtime.create-lifecycle.js";
import {
  readWatcherNativeRecoveryBoundary,
  watcherRestartIntersectionCandidates,
  type WatcherRuntime,
} from "./watcher-runtime.create-watcher-native-event-handler.js";
import {
  observeWatcherNativeReadLifetime,
  watcherRuntimeNativeStartupOptions,
} from "./watcher-runtime.native-startup-options.js";
import {
  createWatcherRuntimeProverWallet,
  prepareWatcherRuntimeAuthority,
} from "./watcher-runtime.prepare-authority.js";
import {
  prepareWatcherRuntimeWorkflows,
  restoreWatcherRuntimeQueue,
} from "./watcher-runtime.prepare-services.js";
import { attemptWatcherRestartQuarantineRecovery } from "./watcher-runtime.restart-quarantine.js";
/** Compose production startup and replay through the durable coordinator. */
export const createWatcherRuntime = async (input: {
  readonly config: WatcherProcessConfig;
  readonly onStartupProgress?: (progress: WatcherStartupProgress) => void;
  readonly onAvailabilityStatusTransition?: (
    event: WatcherAvailabilityStatusTransition,
  ) => void;
}): Promise<WatcherRuntime> => {
  const startup = createWatcherStartupProgress(input.onStartupProgress);
  const {
    deploymentAuthority,
    deploymentIdentity,
    policy,
    localL1Source,
    trusted,
    historicalNativeScriptCheckpointStore,
    fundingProfileOverlay,
    sqlite,
  } = await prepareWatcherRuntimeAuthority(input, startup);

  let queueReadScopes:
    | Parameters<typeof observeWatcherNativeReadLifetime>[1]
    | undefined;
  let activeCoordinator: WatcherChainCoordinator | undefined;
  let native: WatcherNativeChainSyncRuntime | undefined;
  let userEventRuntime: WatcherUserEventRuntime | undefined;
  let observation:
    | Awaited<
        ReturnType<typeof createWatcherLocalKupmiosNativeObservationRuntime>
      >
    | undefined;
  let allocatedFaultProofApplication: WatcherFaultProofApplication | undefined;
  let faultProofSupervisor: WatcherFaultProofSupervisor | undefined;
  let faultDecisionBridge: WatcherFaultDecisionBridge | undefined;
  let availability: WatcherAvailabilityRuntime | undefined;
  let operationsHttp: WatcherOperationsHttpServer | undefined;
  let retainedDaOperationsBinding:
    | WatcherRetainedDaOperationsBinding
    | undefined;
  let proverFundingStore:
    | WatcherSqliteProverFundingReservationStoreRuntime
    | undefined;
  const closeAllocatedResources = () =>
    closeWatcherAllocatedResources({
      readScopes: () => queueReadScopes,
      historyRecovery: () => historyRecovery,
      activeCoordinator: () => activeCoordinator,
      faultDecisionBridge: () => faultDecisionBridge,
      availability: () => availability,
      retainedDaOperationsBinding: () => retainedDaOperationsBinding,
      operationsHttp: () => operationsHttp,
      native: () => native,
      faultProofSupervisor: () => faultProofSupervisor,
      allocatedFaultProofApplication: () => allocatedFaultProofApplication,
      observation: () => observation,
      proverFundingStore: () => proverFundingStore,
      userEventRuntime: () => userEventRuntime,
      sqlite: () => sqlite,
    });

  let historyRecovery:
    | ReturnType<typeof createWatcherHistoryRecovery>
    | undefined;
  const {
    coordinatorReady,
    resolveCoordinator,
    rejectCoordinator,
    nativeCaughtUp,
    resolveCaughtUp,
    rejectCaughtUp,
  } = createWatcherRuntimeSignals();
  try {
    const durable = await createWatcherDurableRuntime({
      backend: sqlite.backend,
      userEventArchive: sqlite.userEventArchive,
      policy,
      authenticationKey: trusted.rollbackAuthenticationKey,
      client: trusted.client,
    });
    const blockProgress = sqlite.openBlockProgress(
      trusted.rollbackAuthenticationKey,
    );
    const restoreQueue = () =>
      restoreWatcherRuntimeQueue(input, {
        sqlite,
        deploymentIdentity,
        startup,
        onReadScopesAllocated: (scopes) => {
          queueReadScopes?.close();
          queueReadScopes = scopes;
        },
      });
    const earlyQueue =
      durable.readFinality().phase === "quarantined"
        ? await restoreQueue()
        : null;
    if (earlyQueue !== null) {
      await startup("post_finality_recovery", () =>
        attemptWatcherRestartQuarantineRecovery({
          durable,
          blockProgress,
          stateQueueCursor: earlyQueue.stateQueueRuntime.replayIntersection,
          binaryPath: input.config.nativeChainSyncBinaryPath,
          watcherConfig: input.config.watcherConfig,
        }),
      );
    }
    const blueprintBytes = await readFile(
      input.config.faultProofInfrastructure.blueprintPath,
    );
    const eventHistory = await startup("user_event_runtime", async () =>
      createWatcherUserEventRuntime({
        watcherConfig: input.config.watcherConfig,
        deploymentAuthority,
        blueprintBytes,
        nativeChainSyncBinaryPath: input.config.nativeChainSyncBinaryPath,
        runtime: durable,
        archive: sqlite.userEventArchive,
        coverage: sqlite.openUserEventCoverage(
          trusted.rollbackAuthenticationKey,
        ),
      }),
    );
    userEventRuntime = eventHistory;
    const retireEventHistory = () =>
      faultDecisionBridge?.invalidateForHistoryChange();
    void eventHistory.done.then(retireEventHistory, retireEventHistory);
    const { faultProofApplication, faultProofReadiness } =
      await prepareWatcherRuntimeWorkflows(input, {
        deploymentAuthority,
        deploymentIdentity,
        sqlite,
        historicalNativeScriptCheckpointStore,
        fundingProfileOverlay,
        startup,
        eventHistory,
        onAllocated: (application) => {
          allocatedFaultProofApplication = application;
        },
      });

    const {
      rawSource,
      inclusionRawSource,
      stateQueueSource,
      stateQueueRuntime,
    } = earlyQueue ?? (await restoreQueue());
    const kupoService = localL1Source.queryServices.find(
      ({ kind }) => kind === "kupo",
    );
    const ogmiosService = localL1Source.queryServices.find(
      ({ kind }) => kind === "ogmios",
    );
    if (kupoService === undefined || ogmiosService === undefined) {
      throw new Error(
        "watcher production runtime omitted its Kupo or Ogmios query authority",
      );
    }
    const fundingRuntime = await openWatcherProverFundingRuntime({
      path: input.config.watcherConfig.storage.path,
      authenticationKey: trusted.rollbackAuthenticationKey,
      deploymentIdentity,
      createProtocolParameters: () =>
        startup("protocol_parameters", ({ retryL1Read }) =>
          retryL1Read(() =>
            createWatcherProtocolParameterRuntimeAuthority({
              deploymentIdentity,
              ogmiosUrl: ogmiosService.endpoint,
              timeoutMs: input.config.watcherConfig.l1.requestTimeoutMs,
            }),
          ),
        ),
      launchScope: faultProofApplication.installedCategories,
      journalRoot: input.config.workflowJournalDirectory,
    });
    proverFundingStore = fundingRuntime.store;
    const proverFundingAuthorityFactory = fundingRuntime.factory;

    // Address derivation only; the runtime never holds a live signer. The
    // executing runner re-resolves the same secret source itself.
    const proverSecret = await loadWatcherSecretText(
      input.config.watcherConfig.proverWallet.keySource,
    );
    const { proverWalletAddress, proverUtxoProvider } =
      createWatcherRuntimeProverWallet(
        input,
        proverSecret,
        kupoService,
        ogmiosService,
      );
    faultProofSupervisor = createWatcherFaultProofSupervisor({
      journalRoot: input.config.workflowJournalDirectory,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deadlineAlertHeadroomMs: Math.max(
        input.config.watcherConfig.deadlines.daFetchMs,
        input.config.watcherConfig.deadlines.daPublishMs,
        input.config.watcherConfig.deadlines.proofConstructMs,
        input.config.watcherConfig.deadlines.proofSubmitMs,
      ),
      queueAuthenticationKey: trusted.rollbackAuthenticationKey,
      execution: createWatcherFaultProofExecution({
        application: faultProofApplication,
        fundingFactory: proverFundingAuthorityFactory,
        walletAddress: proverWalletAddress,
        provider: proverUtxoProvider,
        journalRoot: input.config.workflowJournalDirectory,
        runtimeConfigPath: input.config.watcherRuntimeConfigPath,
        deploymentFingerprint: deploymentIdentity.manifestId,
        operationsSink: () => operations.sink,
      }),
    });
    const operations = createWatcherOperationsObservability({
      deploymentFingerprint: deploymentIdentity.manifestId,
      supervisor: faultProofSupervisor,
      launchScopeStatus: () => ({
        installedCategoryCount:
          faultProofApplication.installedCategories.length,
        requiredCategoryCount: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      }),
      durableProofQueueStatus: faultProofSupervisor.durableQueueStatus,
      retainedDaTransportStatus:
        faultProofApplication.retainedDaTransportStatus,
      coordinatorStatus: () => activeCoordinator?.status() ?? null,
    });
    retainedDaOperationsBinding = bindWatcherRetainedDaOperations({
      deploymentIdentity,
      sink: operations.sink,
    });
    availability = await createWatcherAvailabilityRuntime({
      config: input.config,
      identity: deploymentIdentity,
      rawSource,
      faultProofObservation: {
        rawSource: inclusionRawSource,
        currentObservation: stateQueueRuntime.current,
      },
      mergedHeaders: async (observation) =>
        (await stateQueueSource.resolveMergedHeaders?.({ observation })) ??
        new Map(),
      proverWalletAddress,
      onStatusTransition: input.onAvailabilityStatusTransition,
      // E5: the pool readout and its alerts reach /v1/status; they never make
      // the watcher not_ready.
      onDaBondPool: watcherDaBondPoolReporter(
        operations.sink,
        deploymentIdentity.manifestId,
      ),
      onDaBondPoolReadFailure: watcherDaBondPoolReadFailureReporter(
        operations.sink,
      ),
    });
    await startup("availability_reconciliation", () =>
      availability!.reconcile(stateQueueRuntime.current(), false),
    );
    faultDecisionBridge = await createWatcherFaultDecisionBridge({
      application: faultProofApplication,
      supervisor: faultProofSupervisor,
      stateQueueSource,
      journalDirectory: input.config.workflowJournalDirectory,
      authenticationKey: trusted.rollbackAuthenticationKey,
      runtimeConfigPath: input.config.watcherRuntimeConfigPath,
      maximumClassificationConcurrency: 16,
      operationsSink: operations.sink,
      pendingAvailabilityHeaders: availability.pendingAvailabilityHeaders,
    });
    // Capture the full finalized backlog in bounded batches before recovering
    // older queue headers. Their event views remain scoped to each header.
    const restoredQueuePoint = stateQueueRuntime.catchupBoundary;
    await startup("user_event_catchup", () =>
      eventHistory.advanceThrough({
        blockHash: restoredQueuePoint.blockHash,
        blockNo: restoredQueuePoint.blockNo,
        slot: restoredQueuePoint.slot,
        pointId: computeFraudProofRawL1PointId(restoredQueuePoint),
      }),
    );
    await startup("header_classification", () =>
      faultDecisionBridge!.prepareForRecovery(stateQueueRuntime.current()),
    );
    const recoveredFaultProofWorkflowCount = await startup(
      "workflow_recovery",
      () => faultDecisionBridge!.recoverExisting(),
    );
    operationsHttp = await startWatcherOperationsHttpServer({
      endpoint: input.config.operationsEndpoint,
      observability: operations,
    });
    const retainedFinality = durable.readFinality();
    const intersectionCandidates = watcherRestartIntersectionCandidates({
      oldestAuthenticatedHint: oldestRetainedCanonicalHint(durable),
      progressHead: blockProgress.readHead(),
      progressCandidates: blockProgress.readCandidates(),
      authorityFinalized:
        retainedFinality.finalized !== null ? retainedFinality.finalized : null,
      stateQueueCursor: stateQueueRuntime.replayIntersection,
    });
    native = await startWatcherNativeChainSyncWithRetry(
      watcherRuntimeNativeStartupOptions(input, {
        intersectionCandidates,
        coordinator: coordinatorReady,
        onCaughtUp: resolveCaughtUp,
        operationsSink: operations.sink,
        sourceIdentityDigest: localL1Source.chainSync.genesisIdentitySha256,
        readScopes: queueReadScopes!,
      }),
    );
    observeWatcherNativeReadLifetime(
      native.done,
      queueReadScopes!,
      rejectCaughtUp,
    );
    observation = await createWatcherLocalKupmiosNativeObservationRuntime({
      watcherConfig: input.config.watcherConfig,
      deploymentIdentity,
      nativeAuthority: native.authority,
      rawSource,
    });
    const details = readWatcherNativeRecoveryBoundary({
      nativeAuthority: native.authority,
      admittedIntersections: intersectionCandidates,
    });
    const queueHooks = stateQueueRuntime.bindFaultDecisionBridge(
      faultDecisionBridge,
      availability,
    );
    const activeUserEventRuntime = eventHistory;
    const activeBridge = faultDecisionBridge;
    const activeAvailability = availability;
    const recovery = createWatcherHistoryRecovery({
      history: activeUserEventRuntime,
      queue: queueHooks,
      bridge: activeBridge,
      availability: activeAvailability,
      quarantined: () => durable.readFinality().phase === "quarantined",
      resume: async () => (await coordinatorReady).resume(),
      retryDelayMs: input.config.watcherConfig.l1.requestTimeoutMs,
      onPending: (pending) =>
        operations.sink.setAlert({
          code: "chain_rollback",
          subjectDigest: deploymentIdentity.manifestId,
          active: pending,
          observedAtMs: BigInt(Date.now()).toString(),
        }),
    });
    historyRecovery = recovery;
    const coordinator = createWatcherChainCoordinator({
      policy,
      durable,
      observation,
      restartIntersection: details.selectedIntersection,
      progress: blockProgress,
      // One predicate decides relevance for every component: the user-event
      // runtime's deployment policy plus its active event outrefs, extended
      // with the queue nodes and correction lock the state queue follows.
      relevance: (block) => {
        const current = stateQueueRuntime.current();
        return recovery.classify(block, [
          ...current.finalizedQueue.map(({ outRef }) => outRef),
          ...current.finalizedHeaders.map(({ queueOutRef }) => queueOutRef),
          ...(current.finalizedCorrectionLock === null
            ? []
            : [current.finalizedCorrectionLock.outRef]),
        ]);
      },
      hooks: watcherReplayTranscriptRetirementHooks({
        recovery,
        durable,
        supervisor: faultProofSupervisor,
        localObservationRuntime: observation,
        stateQueueSource,
        stateQueueRuntime,
        store: sqlite.replayTranscripts,
        config: input.config.watcherConfig,
      }),
    });
    activeCoordinator = coordinator;
    resolveCoordinator(coordinator);
    if (
      details.currentTip.kind === "point" &&
      details.selectedIntersection.blockHash === details.currentTip.blockHash &&
      details.selectedIntersection.slot === details.currentTip.slot
    ) {
      resolveCaughtUp();
    }
    return createWatcherRuntimeLifecycle({
      deploymentAuthority,
      policy,
      coordinator,
      faultProofApplication,
      faultProofReadiness,
      faultProofSupervisor,
      operations,
      operationsHttp,
      recoveredFaultProofWorkflowCount,
      availability,
      recovery,
      native,
      activeUserEventRuntime,
      nativeCaughtUp,
      stateQueueRuntime,
      closeAllocatedResources,
    });
  } catch (error) {
    if (native !== undefined) {
      rejectCoordinator(
        error instanceof Error ? error : new Error(String(error)),
      );
    }
    try {
      await closeAllocatedResources();
    } catch (shutdownError) {
      throw new AggregateError(
        [error, shutdownError],
        "watcher production startup and cleanup failed",
      );
    }
    throw error;
  }
};
