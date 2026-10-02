import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  computeFraudProofRawL1PointId,
  createSqliteHistoricalNativeScriptCheckpointStore,
  resolveProverSigner,
} from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { Kupmios } from "@lucid-evolution/lucid";

import {
  createWatcherAvailabilityRuntime,
  type WatcherAvailabilityRuntime,
  type WatcherAvailabilityStatusTransition,
} from "../availability/runtime.js";
import {
  createWatcherFaultDecisionBridge,
  type WatcherFaultDecisionBridge,
} from "../fault-proofs/fault-decision-bridge.js";
import {
  createWatcherFaultProofApplication,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_STARTUP_READINESS_HEADER_HASH,
  type WatcherFaultProofApplication,
  type WatcherFaultProofStartupReadiness,
} from "../fault-proofs/fault-proof-application.js";
import { createWatcherFaultProofExecution } from "../fault-proofs/fault-proof-execution.js";
import {
  createWatcherFaultProofSupervisor,
  type WatcherFaultProofSupervisor,
} from "../fault-proofs/fault-proof-supervisor.js";
import { createWatcherProtocolParameterRuntimeAuthority } from "../funding/prover-funding.js";
import { createWatcherProverFundingAuthorityFactory } from "../funding/prover-funding-authority.js";
import {
  openWatcherSqliteProverFundingReservationStore,
  type WatcherSqliteProverFundingReservationStoreRuntime,
} from "../funding/sqlite-prover-funding-reservation-store.js";
import { loadWatcherWorkflowFundingProfileOverlay } from "../funding/workflow-funding-profile-overlay.js";
import { createWatcherStateQueueObservationSource } from "../indexers/authenticated-state-queue-observation.js";
import { makeWatcherFinalityPolicy } from "../l1/finality-engine.js";
import { createWatcherLocalKupmiosNativeObservationRuntime } from "../l1/local-kupmios-native-observation.js";
import { createWatcherLocalKupmiosRawSource } from "../l1/local-kupmios-raw-source.js";
import {
  startWatcherNativeChainSyncWithRetry,
  type WatcherNativeChainSyncPoint,
  type WatcherNativeChainSyncRuntime,
  watcherNativeChainSyncStartupTimeoutMs,
} from "../l1/native-chain-sync.js";
import { createWatcherDurableRuntime } from "../storage/durable-runtime.js";
import {
  bindWatcherRetainedDaOperations,
  type WatcherRetainedDaOperationsBinding,
} from "../storage/retained-da-runtime.js";
import { openWatcherSqliteDurableBackend } from "../storage/sqlite-durable-backend.js";
import {
  createWatcherChainCoordinator,
  type WatcherChainCoordinator,
} from "./chain-coordinator.js";
import { loadWatcherVerifiedDeploymentAuthority } from "./deployment-authority.js";
import { watcherDeploymentReleaseFinalityPolicy } from "./deployment-identity.js";
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
  refusePermanently,
  WatcherPermanentRefusalError,
} from "./permanent-refusal.js";
import {
  loadWatcherSecretText,
  type WatcherProcessConfig,
} from "./process-config.js";
import {
  createWatcherStartupProgress,
  type WatcherStartupProgress,
} from "./startup-progress.js";
import { createWatcherStateQueueRuntime } from "./state-queue-runtime.js";
import { createWatcherTrustedHeadClientRuntime } from "./trusted-head-runtime.js";
import {
  createWatcherUserEventRuntime,
  type WatcherUserEventRuntime,
} from "./user-event-runtime.js";
import {
  assertWatcherFaultProofLaunchScope,
  createWatcherNativeEventHandler,
  prepareJournalDirectory,
  readWatcherNativeRecoveryBoundary,
  requireWatcherRuntimeConfig,
  WATCHER_RUNTIME_SCHEMA_VERSION,
  watcherRestartIntersectionCandidates,
  type WatcherRuntime,
} from "./watcher-runtime.create-watcher-native-event-handler.js";
import { attemptWatcherRestartQuarantineRecovery } from "./watcher-runtime.restart-quarantine.js";

/**
 * Production start/replay composition. All release, source, secret and proof
 * infrastructure checks complete before the native helper can deliver an L1
 * event. Every admitted event is then serialized through the sidecar-backed
 * durable coordinator.
 */
export const createWatcherRuntime = async (input: {
  readonly config: WatcherProcessConfig;
  readonly onStartupProgress?: (progress: WatcherStartupProgress) => void;
  readonly onAvailabilityStatusTransition?: (
    event: WatcherAvailabilityStatusTransition,
  ) => void;
}): Promise<WatcherRuntime> => {
  const startup = createWatcherStartupProgress(input.onStartupProgress);
  await startup("runtime_configuration", () =>
    refusePermanently("runtime_configuration", () =>
      requireWatcherRuntimeConfig(input.config),
    ),
  );
  await prepareJournalDirectory(input.config.workflowJournalDirectory);
  const deploymentAuthority = await startup("deployment_authority", () =>
    refusePermanently("deployment_authority", () =>
      loadWatcherVerifiedDeploymentAuthority({
        path: input.config.deploymentAuthorityPath,
        ruleBundlePath: input.config.ruleBundlePath,
      }),
    ),
  );
  const { deploymentIdentity } = deploymentAuthority;
  const policy = makeWatcherFinalityPolicy(
    input.config.watcherConfig,
    deploymentIdentity,
  );
  const releaseDepth = String(
    watcherDeploymentReleaseFinalityPolicy(deploymentIdentity).policy
      .confirmationDepth,
  );
  if (
    policy === null ||
    (policy.network !== "Preprod" && policy.network !== "Custom") ||
    policy.sourceMode !== "local_node" ||
    policy.confirmationDepth !== releaseDepth ||
    policy.maximumPreFinalityRollbackDepth !== releaseDepth ||
    policy.maximumPostFinalityRecoveryDepth !== "2160"
  ) {
    throw new WatcherPermanentRefusalError(
      "finality_policy",
      new Error(
        "watcher production finality differs from the verified release",
      ),
    );
  }
  const localL1Source = input.config.watcherConfig.l1.source;
  if (localL1Source.sourceMode !== "local_node") {
    throw new Error("watcher production runtime requires local-node authority");
  }
  const trusted = await createWatcherTrustedHeadClientRuntime({
    config: input.config,
    policy,
    additionalSecretSources: [input.config.availability.keySource],
  });
  const historicalNativeScriptCheckpointStore =
    createSqliteHistoricalNativeScriptCheckpointStore({
      path: input.config.watcherConfig.storage.path,
      rollbackAuthenticationKey: trusted.rollbackAuthenticationKey,
    });
  const fundingProfileOverlay = await loadWatcherWorkflowFundingProfileOverlay({
    bundlePath: input.config.fundingProfileBundlePath,
    deploymentIdentity,
  });
  const sqlite = await openWatcherSqliteDurableBackend({
    path: input.config.watcherConfig.storage.path,
  });

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
  const closeAllocatedResources = async (): Promise<void> => {
    historyRecovery?.close();
    const coordinatorStopped = activeCoordinator?.stop();
    const failures: unknown[] = [];
    faultDecisionBridge?.invalidateForShutdown();
    availability?.invalidateForShutdown();
    try {
      retainedDaOperationsBinding?.close();
    } catch (error) {
      failures.push(error);
    }
    if (operationsHttp !== undefined) {
      try {
        await operationsHttp.close();
      } catch (error) {
        failures.push(error);
      }
    }
    if (native !== undefined) {
      try {
        await native.close();
      } catch (error) {
        failures.push(error);
      }
    }
    try {
      await coordinatorStopped;
    } catch (error) {
      failures.push(error);
    }
    if (faultProofSupervisor !== undefined) {
      try {
        await faultProofSupervisor.close();
      } catch (error) {
        failures.push(error);
      }
    }
    if (allocatedFaultProofApplication !== undefined) {
      try {
        await allocatedFaultProofApplication.close();
      } catch (error) {
        failures.push(error);
      }
    }
    if (availability !== undefined) {
      try {
        await availability.close();
      } catch (error) {
        failures.push(error);
      }
    }
    try {
      observation?.close();
    } catch (error) {
      failures.push(error);
    }
    try {
      proverFundingStore?.close();
    } catch (error) {
      failures.push(error);
    }
    if (userEventRuntime !== undefined) {
      try {
        await userEventRuntime.close();
      } catch (error) {
        failures.push(error);
      }
    }
    try {
      sqlite.close();
    } catch (error) {
      failures.push(error);
    }
    if (failures.length > 0) {
      throw new AggregateError(
        failures,
        "watcher production runtime shutdown failed",
      );
    }
  };
  let historyRecovery:
    | ReturnType<typeof createWatcherHistoryRecovery>
    | undefined;
  let resolveCoordinator!: (value: WatcherChainCoordinator) => void;
  let rejectCoordinator!: (reason: Error) => void;
  const coordinatorReady = new Promise<WatcherChainCoordinator>(
    (resolve, reject) => {
      resolveCoordinator = resolve;
      rejectCoordinator = reject;
    },
  );
  // Startup can fail before the event handler awaits this promise. Observe
  // rejection immediately while leaving the original promise rejecting.
  void coordinatorReady.catch(() => undefined);
  let resolveCaughtUp!: () => void;
  let rejectCaughtUp!: (reason: Error) => void;
  const nativeCaughtUp = new Promise<void>((resolve, reject) => {
    resolveCaughtUp = resolve;
    rejectCaughtUp = reject;
  });
  void nativeCaughtUp.catch(() => undefined);
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
    const restoreQueue = async () => {
      const rawSource = createWatcherLocalKupmiosRawSource({
        watcherConfig: input.config.watcherConfig,
        deploymentIdentity,
      });
      const inclusionRawSource = createWatcherLocalKupmiosRawSource({
        watcherConfig: input.config.watcherConfig,
        deploymentIdentity,
        observationDepth: "inclusion",
      });
      const stateQueueSource = createWatcherStateQueueObservationSource({
        deploymentIdentity,
        rawSource,
        inclusionRawSource,
      });
      const stateQueueRuntime = await startup("state_queue_recovery", () =>
        createWatcherStateQueueRuntime({
          store: sqlite.stateQueueObservations,
          source: stateQueueSource,
        }),
      );
      return {
        rawSource,
        inclusionRawSource,
        stateQueueSource,
        stateQueueRuntime,
      };
    };
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
    const { faultProofApplication, faultProofReadiness } = await startup(
      "workflow_readiness",
      async ({ retryL1Read }) => {
        const faultProofApplication = createWatcherFaultProofApplication({
          deploymentAuthority,
          replayTranscriptStore: sqlite.replayTranscripts,
          userEventRuntime: eventHistory,
          infrastructure: input.config.faultProofInfrastructure,
          historicalNativeScriptCheckpointStore,
          fundingProfileOverlay,
        });
        allocatedFaultProofApplication = faultProofApplication;
        assertWatcherFaultProofLaunchScope(
          faultProofApplication.installedCategories,
        );
        const faultProofReadiness: WatcherFaultProofStartupReadiness[] = [];
        for (const category of WATCHER_INSTALLED_WORKFLOW_CATEGORIES) {
          const journalDirectory = join(
            input.config.workflowJournalDirectory,
            "readiness",
            category,
          );
          await prepareJournalDirectory(journalDirectory);
          // Readiness only reads L1 and builds nothing: an L1 transient
          // repeats this read, never the allocation above.
          faultProofReadiness.push(
            await retryL1Read(() =>
              faultProofApplication.assertStartupReady({
                mode: "resume",
                category,
                deploymentFingerprint: deploymentIdentity.manifestId,
                headerHash: WATCHER_STARTUP_READINESS_HEADER_HASH,
                journalDirectory,
                runtimeConfigPath: input.config.watcherRuntimeConfigPath,
              }),
            ),
          );
        }
        return { faultProofApplication, faultProofReadiness };
      },
    );

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
    proverFundingStore = await openWatcherSqliteProverFundingReservationStore({
      path: input.config.watcherConfig.storage.path,
    });
    const proverFundingProtocolParameters = await startup(
      "protocol_parameters",
      ({ retryL1Read }) =>
        retryL1Read(() =>
          createWatcherProtocolParameterRuntimeAuthority({
            deploymentIdentity,
            ogmiosUrl: ogmiosService.endpoint,
            timeoutMs: input.config.watcherConfig.l1.requestTimeoutMs,
          }),
        ),
    );
    const proverFundingAuthorityFactory =
      createWatcherProverFundingAuthorityFactory({
        launchScope: faultProofApplication.installedCategories,
        journalRoot: input.config.workflowJournalDirectory,
        deploymentIdentity,
        protocolParameters: proverFundingProtocolParameters,
        store: proverFundingStore.store,
      });
    // No supervisor jobs exist yet; reclaim reservations left before any signed attempt.
    await proverFundingAuthorityFactory.releaseUnused();

    // Address derivation only; the runtime never holds a live signer. The
    // executing runner re-resolves the same secret source itself.
    const proverSecret = await loadWatcherSecretText(
      input.config.watcherConfig.proverWallet.keySource,
    );
    const proverWalletAddress = resolveProverSigner(
      proverSecret.startsWith("ed25519_sk")
        ? {
            network: input.config.watcherConfig.targetNetwork,
            walletPrivateKey: proverSecret,
          }
        : {
            network: input.config.watcherConfig.targetNetwork,
            walletSeedPhrase: proverSecret,
          },
      Object.freeze({}),
    ).address;
    const proverUtxoProvider = new Kupmios(
      kupoService.endpoint,
      ogmiosService.endpoint,
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
      progressHead: blockProgress.readHead(),
      progressCandidates: blockProgress.readCandidates(),
      authorityFinalized:
        retainedFinality.phase === "finalized" &&
        retainedFinality.finalized !== null
          ? retainedFinality.finalized
          : null,
      stateQueueCursor: stateQueueRuntime.replayIntersection,
    });
    native = await startWatcherNativeChainSyncWithRetry({
      binaryPath: input.config.nativeChainSyncBinaryPath,
      watcherConfig: input.config.watcherConfig,
      // The node picks the newest recorded point it still has; anything the
      // watcher recorded above it is rolled back by the coordinator.
      intersectionCandidates: Object.freeze(
        intersectionCandidates.map(
          ({ blockHash, slot }): WatcherNativeChainSyncPoint =>
            Object.freeze({ kind: "point", blockHash, slot }),
        ),
      ),
      startupTimeoutMs: watcherNativeChainSyncStartupTimeoutMs(
        input.config.watcherConfig,
      ),
      onEvent: createWatcherNativeEventHandler({
        coordinator: coordinatorReady,
        onCaughtUp: resolveCaughtUp,
        operationsSink: operations.sink,
        sourceIdentityDigest: localL1Source.chainSync.genesisIdentitySha256,
      }),
    });
    void native.done.catch((error) => {
      rejectCaughtUp(error instanceof Error ? error : new Error(String(error)));
    });
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
      hooks: recovery.hooks,
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
    let phase: "live" | "closing" | "closed" | "failed" = "live";
    let caughtUp = false;
    let closePromise: Promise<void> | undefined;
    const activeFaultProofSupervisor = faultProofSupervisor;
    const runtimeDone = Promise.race([
      recovery.done,
      native.done,
      activeUserEventRuntime.done,
      activeFaultProofSupervisor.done,
      operationsHttp.done,
    ]);
    const caughtUpPromise = Promise.race([
      Promise.all([nativeCaughtUp, stateQueueRuntime.caughtUp]).then(
        async () => {
          do {
            await recovery.waitForRecovery();
            await coordinator.waitForDelivery();
          } while (
            recovery.status().pending ||
            coordinator.status().deliveryHeld ||
            coordinator.status().rollbackPoint !== null
          );
          caughtUp = true;
        },
      ),
      runtimeDone.then(() => {
        throw new Error(
          "watcher production liveness ended before durable catch-up",
        );
      }),
    ]);
    void caughtUpPromise.catch(() => undefined);
    void runtimeDone.then(
      () => {
        if (phase === "live") phase = "failed";
      },
      () => {
        if (phase === "live") phase = "failed";
      },
    );
    const runtime: WatcherRuntime = Object.freeze({
      schemaVersion: WATCHER_RUNTIME_SCHEMA_VERSION,
      deploymentAuthority,
      policy,
      coordinator,
      faultProofApplication,
      faultProofReadiness: Object.freeze(faultProofReadiness),
      faultProofSupervisor: activeFaultProofSupervisor,
      operations,
      operationsEndpoint: operationsHttp.endpoint,
      recoveredFaultProofWorkflowCount,
      availability,
      done: runtimeDone,
      caughtUp: caughtUpPromise,
      status: () => {
        const proofSupervisor = activeFaultProofSupervisor.status();
        const operationsStatus = operations.api.status();
        const availabilityStatus = availability!.status();
        const liveness = phase === "live";
        return Object.freeze({
          phase,
          liveness,
          readiness:
            liveness &&
            caughtUp &&
            !recovery.status().pending &&
            !coordinator.status().deliveryHeld &&
            !coordinator.status().quarantined &&
            proofSupervisor.phase === "accepting" &&
            proofSupervisor.recovered &&
            proofSupervisor.deadlineHealth === "safe" &&
            availabilityStatus.phase !== "blocked" &&
            operationsStatus.readiness === "ready",
          caughtUp,
          historyRecovery: recovery.status(),
          proofSupervisor,
          availability: availabilityStatus,
        });
      },
      close: () => {
        if (closePromise !== undefined) return closePromise;
        phase = "closing";
        closePromise = closeAllocatedResources().then(
          () => {
            phase = "closed";
          },
          (error: unknown) => {
            phase = "failed";
            throw error;
          },
        );
        return closePromise;
      },
    });
    return runtime;
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
