import { readFile } from "node:fs/promises";

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
import { validateWatcherFaultDecisionJournalConfiguration } from "../fault-proofs/fault-decision-journal.js";
import { type WatcherFaultProofApplication } from "../fault-proofs/fault-proof-application.js";
import { createWatcherFaultProofExecution } from "../fault-proofs/fault-proof-execution.js";
import {
  createWatcherFaultProofSupervisor,
  type WatcherFaultProofSupervisor,
} from "../fault-proofs/fault-proof-supervisor.js";
import { createWatcherProtocolParameterRuntimeAuthority } from "../funding/prover-funding.js";
import type { WatcherSqliteProverFundingReservationStoreRuntime } from "../funding/sqlite-prover-funding-reservation-store.js";
import { deriveWatcherNativeGenesisIdentity } from "../l1/native-chain-sync.derive-watcher-native-genesis-identity.js";
import {
  openWatcherDeploymentFollower,
  watcherFollowedScripts,
} from "../l1-follower/deployment-follower.js";
import { WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX } from "../l1-follower/fault-proof-l1-source.js";
import { type WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import { createWatcherQueueHeaderSource } from "../l1-follower/observation.js";
import { createWatcherDurableRuntime } from "../storage/durable-runtime.js";
import {
  bindWatcherRetainedDaOperations,
  type WatcherRetainedDaOperationsBinding,
} from "../storage/retained-da-runtime.js";
import { watcherDeploymentProtocolScriptAuthority } from "./deployment-identity.js";
import {
  watcherDeploymentAppliedScriptHashes,
  watcherDeploymentReleaseFinalityPolicy,
} from "./deployment-identity.watcher-deployment-availability-challenge-authority.js";
import {
  startWatcherOperationsHttpServer,
  type WatcherOperationsHttpServer,
} from "./operations-http.js";
import {
  createWatcherOperationsObservability,
  watcherDaBondPoolReadFailureReporter,
  watcherDaBondPoolReporter,
} from "./operations-observability.js";
import { refusePermanently } from "./permanent-refusal.js";
import {
  loadWatcherSecretText,
  type WatcherProcessConfig,
} from "./process-config.js";
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
import { createWatcherRuntimeLifecycle } from "./watcher-runtime.create-lifecycle.js";
import {
  createWatcherDecisionDriver,
  type WatcherDecisionDriver,
} from "./watcher-runtime.decision-driver.js";
import { createWatcherL1Readiness } from "./watcher-runtime.l1-readiness.js";
import { type WatcherRuntime } from "./watcher-runtime.launch-checks.js";
import {
  prepareWatcherRuntimeAuthority,
  resolveWatcherRuntimeWalletAddress,
} from "./watcher-runtime.prepare-authority.js";
import { prepareWatcherRuntimeWorkflows } from "./watcher-runtime.prepare-services.js";

/**
 * The persisted sourceId of the watcher's state-queue observations, kept
 * byte-identical to the earlier source so persisted records still match.
 */
const watcherObservationSourceId = (
  manifestId: string,
  authorityNodeId: string,
): string =>
  `${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX}${[
    "watcher-native-crosscheck",
    manifestId,
    authorityNodeId,
  ].join("/")}`;

/** How often the cached L1 readiness is refreshed without a follower change. */
const L1_READINESS_REFRESH_MS = 5_000;

/**
 * Compose production startup around the watcher's chain follower (ticket
 * W1): every L1 read goes through the follower's fact store, and the
 * decision driver decides from its projections at the tip and at the
 * release depth.
 *
 * Liveness: the operations server binds before the first decision pass, so
 * `/v1/status` (the liveness probe) answers while the follower syncs; there
 * is no `/healthz` route. Until the first pass completes, and whenever the
 * follower, a pass or the journals hold decisions, `/readyz` names the
 * reason. No L1 condition, journal integrity failure or failed journal open
 * ends the process after the server binds; bad configuration, the journals'
 * directory and key included, exits before it binds.
 */
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
  const watcherConfig = input.config.watcherConfig;

  let follower: WatcherFollowerRuntime | undefined;
  let decisionDriver: WatcherDecisionDriver | undefined;
  let userEventRuntime: WatcherUserEventRuntime | undefined;
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
  let readinessTimer: ReturnType<typeof setInterval> | undefined;
  let unsubscribeL1: (() => void)[] = [];
  const closeAllocatedResources = async () => {
    if (readinessTimer !== undefined) clearInterval(readinessTimer);
    readinessTimer = undefined;
    for (const unsubscribe of unsubscribeL1) unsubscribe();
    unsubscribeL1 = [];
    await closeWatcherAllocatedResources({
      decisionDriver: () => decisionDriver,
      follower: () => follower,
      faultDecisionBridge: () => faultDecisionBridge,
      availability: () => availability,
      retainedDaOperationsBinding: () => retainedDaOperationsBinding,
      operationsHttp: () => operationsHttp,
      faultProofSupervisor: () => faultProofSupervisor,
      allocatedFaultProofApplication: () => allocatedFaultProofApplication,
      proverFundingStore: () => proverFundingStore,
      userEventRuntime: () => userEventRuntime,
      sqlite: () => sqlite,
    });
  };

  try {
    const releaseFinality =
      watcherDeploymentReleaseFinalityPolicy(deploymentIdentity).policy;
    const releaseDepth = releaseFinality.confirmationDepth;
    const authority =
      watcherDeploymentProtocolScriptAuthority(deploymentIdentity);
    const durable = await createWatcherDurableRuntime({
      backend: sqlite.backend,
      userEventArchive: sqlite.userEventArchive,
      policy,
      authenticationKey: trusted.rollbackAuthenticationKey,
      client: trusted.client,
    });
    const blueprintBytes = await readFile(
      input.config.faultProofInfrastructure.blueprintPath,
    );
    const eventHistory = await startup("user_event_runtime", async () =>
      createWatcherUserEventRuntime({
        watcherConfig,
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

    const { networkMagic } = await startup("l1_node_identity", () =>
      deriveWatcherNativeGenesisIdentity({ watcherConfig }),
    );
    // Address derivation only; the runtime never holds a live signer. The
    // executing runner re-resolves the same secret source itself.
    const proverWalletAddress = resolveWatcherRuntimeWalletAddress(
      input,
      await loadWatcherSecretText(watcherConfig.proverWallet.keySource),
    );
    const availabilityWalletAddress = resolveWatcherRuntimeWalletAddress(
      input,
      await loadWatcherSecretText(input.config.availability.keySource),
    );
    const activeFollower = openWatcherDeploymentFollower({
      authority,
      storePath: `${watcherConfig.storage.path}.l1-follower.sqlite`,
      automaticRecoveryMaxDepth: releaseFinality.automaticRecoveryMaxDepth,
      origin: watcherConfig.l1.origin,
      followedScripts: watcherFollowedScripts({
        contractScriptHashes:
          watcherDeploymentAppliedScriptHashes(deploymentIdentity),
        blueprint: JSON.parse(Buffer.from(blueprintBytes).toString("utf8")),
      }),
      node: {
        binaryPath: input.config.nativeChainSyncBinaryPath,
        socketPath: localL1Source.chainSync.socketPath,
        networkMagic,
      },
      walletAddresses: [proverWalletAddress, availabilityWalletAddress],
      log: (line) => process.stderr.write(`${line}\n`),
    });
    follower = activeFollower;
    const { store, rawReads, transport, provider } = activeFollower;

    const { faultProofApplication, faultProofReadiness } =
      await prepareWatcherRuntimeWorkflows(input, {
        deploymentAuthority,
        deploymentIdentity,
        sqlite,
        historicalNativeScriptCheckpointStore,
        fundingProfileOverlay,
        startup,
        eventHistory,
        l1: activeFollower.faultProofL1,
        onAllocated: (application) => {
          allocatedFaultProofApplication = application;
        },
      });

    // A bad journal directory or key exits before the operations server binds.
    await refusePermanently("journal_configuration", () =>
      validateWatcherFaultDecisionJournalConfiguration({
        directory: input.config.workflowJournalDirectory,
        deploymentFingerprint: deploymentIdentity.manifestId,
        launchScope: faultProofApplication.installedCategories,
        authenticationKey: trusted.rollbackAuthenticationKey,
      }),
    );
    const fundingRuntime = await openWatcherProverFundingRuntime({
      path: watcherConfig.storage.path,
      authenticationKey: trusted.rollbackAuthenticationKey,
      deploymentIdentity,
      createProtocolParameters: () =>
        startup("protocol_parameters", ({ retryL1Read }) =>
          retryL1Read(() =>
            createWatcherProtocolParameterRuntimeAuthority({
              deploymentIdentity,
              query: () => transport.query({ query: "protocol_params" }),
            }),
          ),
        ),
      launchScope: faultProofApplication.installedCategories,
      journalRoot: input.config.workflowJournalDirectory,
    });
    proverFundingStore = fundingRuntime.store;

    const activeSupervisor = createWatcherFaultProofSupervisor({
      journalRoot: input.config.workflowJournalDirectory,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deadlineAlertHeadroomMs: Math.max(
        watcherConfig.deadlines.daFetchMs,
        watcherConfig.deadlines.daPublishMs,
        watcherConfig.deadlines.proofConstructMs,
        watcherConfig.deadlines.proofSubmitMs,
      ),
      queueAuthenticationKey: trusted.rollbackAuthenticationKey,
      execution: createWatcherFaultProofExecution({
        application: faultProofApplication,
        fundingFactory: fundingRuntime.factory,
        walletAddress: proverWalletAddress,
        provider,
        journalRoot: input.config.workflowJournalDirectory,
        runtimeConfigPath: input.config.watcherRuntimeConfigPath,
        deploymentFingerprint: deploymentIdentity.manifestId,
        operationsSink: () => operations.sink,
      }),
      proofRetention: activeFollower.proofRetention,
    });
    faultProofSupervisor = activeSupervisor;

    const l1 = createWatcherL1Readiness({
      follower: activeFollower,
      driver: () => decisionDriver,
    });
    const refreshFollowerReadiness = (): void => void l1.refresh();
    const l1Readiness = l1.read;
    const operations = createWatcherOperationsObservability({
      deploymentFingerprint: deploymentIdentity.manifestId,
      supervisor: activeSupervisor,
      launchScopeStatus: () => ({
        installedCategoryCount:
          faultProofApplication.installedCategories.length,
        requiredCategoryCount: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      }),
      durableProofQueueStatus: activeSupervisor.durableQueueStatus,
      retainedDaTransportStatus:
        faultProofApplication.retainedDaTransportStatus,
      l1Readiness,
      l1Degradations: l1.degradations,
    });
    retainedDaOperationsBinding = bindWatcherRetainedDaOperations({
      deploymentIdentity,
      sink: operations.sink,
    });
    const headerSource = createWatcherQueueHeaderSource(store, {
      releaseDepth,
    });
    const activeAvailability = await createWatcherAvailabilityRuntime({
      config: input.config,
      identity: deploymentIdentity,
      l1: { reads: rawReads, store, provider },
      confirmationDepth: releaseDepth,
      faultProofObservation: {
        currentObservation: () => decisionDriver?.inclusion() ?? null,
      },
      mergedHeaders: async (observation) =>
        await headerSource.resolveMergedHeaders({ observation }),
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
    availability = activeAvailability;
    const activeBridge = await createWatcherFaultDecisionBridge({
      application: faultProofApplication,
      supervisor: activeSupervisor,
      stateQueueSource: headerSource,
      journalDirectory: input.config.workflowJournalDirectory,
      authenticationKey: trusted.rollbackAuthenticationKey,
      runtimeConfigPath: input.config.watcherRuntimeConfigPath,
      maximumClassificationConcurrency: 16,
      operationsSink: operations.sink,
      pendingAvailabilityHeaders: activeAvailability.pendingAvailabilityHeaders,
    });
    faultDecisionBridge = activeBridge;
    operationsHttp = await startWatcherOperationsHttpServer({
      endpoint: input.config.operationsEndpoint,
      observability: operations,
    });

    const sourceIdentityDigest = localL1Source.chainSync.genesisIdentitySha256;
    const retirementReady = () => {
      const status = activeSupervisor.status();
      return (
        status.recovered &&
        status.phase === "accepting" &&
        status.unfinishedObjectiveCount === 0 &&
        status.queuedJobCount === 0 &&
        status.activeJob === null &&
        status.blockedJob === null
      );
    };
    const activeDriver = createWatcherDecisionDriver(
      {
        store,
        onFollowerChange: activeFollower.onChange,
        authority,
        sourceId: watcherObservationSourceId(
          deploymentIdentity.manifestId,
          localL1Source.authorityNodeId,
        ),
        releaseDepth,
        bridge: activeBridge,
        availability: activeAvailability,
        history: eventHistory,
        retirement: {
          ready: retirementReady,
          retire: (observation) =>
            sqlite.replayTranscripts.retireExpired({
              observation,
              network: watcherConfig.targetNetwork,
              ...(watcherConfig.customNetwork === undefined
                ? {}
                : { customSlotConfig: watcherConfig.customNetwork.slotConfig }),
            }),
          reset: () => sqlite.replayTranscripts.resetRetirementWitnesses(),
        },
        onDecided: (tip) => {
          const observedAtMs = BigInt(Date.now()).toString();
          operations.sink.recordL1Source({
            sourceIdentityDigest,
            sourceMode: "local_node",
            status: "consistent",
            blockHash: tip.nativePoint.blockHash,
            blockNo: tip.nativePoint.blockNo,
            slot: tip.nativePoint.slot,
            observedAtMs,
          });
          operations.sink.setAlert({
            code: "chain_rollback",
            subjectDigest: sourceIdentityDigest,
            active: false,
            observedAtMs,
          });
          refreshFollowerReadiness();
        },
        retryDelayMs: watcherConfig.l1.requestTimeoutMs,
        log: (line) => process.stderr.write(`watcher decisions: ${line}\n`),
      },
      { atTip: () => activeFollower.status()?.atTip === true },
    );
    decisionDriver = activeDriver;
    unsubscribeL1 = [
      activeFollower.onChange(refreshFollowerReadiness),
      store.onGeneration(() => {
        operations.sink.setAlert({
          code: "chain_rollback",
          subjectDigest: sourceIdentityDigest,
          active: true,
          observedAtMs: BigInt(Date.now()).toString(),
        });
      }),
    ];
    readinessTimer = setInterval(
      refreshFollowerReadiness,
      L1_READINESS_REFRESH_MS,
    );
    readinessTimer.unref();
    refreshFollowerReadiness();
    activeDriver.wake();

    const recoveredFaultProofWorkflowCount = await startup(
      "workflow_recovery",
      () => activeDriver.recovered,
    );
    return createWatcherRuntimeLifecycle({
      deploymentAuthority,
      faultProofApplication,
      faultProofReadiness,
      faultProofSupervisor: activeSupervisor,
      operations,
      operationsHttp,
      recoveredFaultProofWorkflowCount,
      availability: activeAvailability,
      follower: activeFollower,
      decisionDriver: activeDriver,
      closeAllocatedResources,
    });
  } catch (error) {
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
