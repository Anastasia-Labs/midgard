import { readFile } from "node:fs/promises";

import { proofMaterialsAddressOfFundingAddress } from "@al-ft/midgard-fault-proofs/funding/proof-materials-address";
import { createWorkflowInFlight } from "@al-ft/midgard-fault-proofs/workflow/in-flight";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk/core";

import { createWatcherAvailabilityRuntime } from "../availability/runtime.create-watcher-availability-runtime.js";
import type {
  WatcherAvailabilityRuntime,
  WatcherAvailabilityStatusTransition,
} from "../availability/runtime.release-watcher-availability-workflows.js";
import { createWatcherFaultDecisionBridge } from "../fault-proofs/fault-decision-bridge.create-watcher-fault-decision-bridge.js";
import type { WatcherFaultDecisionBridge } from "../fault-proofs/fault-decision-bridge.selected-target.js";
import { validateWatcherFaultDecisionJournalConfiguration } from "../fault-proofs/fault-decision-journal.create-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../fault-proofs/fault-proof-application.js";
import type { WatcherFaultProofApplication } from "../fault-proofs/fault-proof-application.production-dependencies.js";
import { createWatcherFaultProofExecution } from "../fault-proofs/fault-proof-execution.js";
import {
  createWatcherFaultProofSupervisor,
  watcherDeadlineAlertHeadroomMs,
} from "../fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import type { WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.validate-job.js";
import {
  createWatcherOwnThreadSweep,
  watcherOwnThreadCancellerSource,
} from "../fault-proofs/own-thread-sweep.js";
import { deriveWatcherNativeGenesisIdentity } from "../l1/native-chain-sync.derive-watcher-native-genesis-identity.js";
import { watcherAvailabilityTransactionPins } from "../l1-follower/availability-pins.js";
import {
  openWatcherDeploymentFollower,
  watcherComputationThreadPolicyId,
  watcherFollowedScripts,
} from "../l1-follower/deployment-follower.js";
import type { WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import { createWatcherQueueHeaderSource } from "../l1-follower/observation.js";
import { watcherReplayCheckpointPrecedesInit } from "../l1-follower/replay-source.js";
import type { WatcherUserEvents } from "../l1-follower/user-events.js";
import { bindWatcherRetainedDaOperations } from "../storage/retained-da-runtime.js";
import type { WatcherRetainedDaOperationsBinding } from "../storage/retained-da-runtime.retained-da-request-permits.js";
import { assertWatcherL1SlotConfigMatchesDeployment } from "./custom-network.js";
import {
  readWatcherUserEventScriptBinding,
  verifyWatcherUserEventScriptBinding,
} from "./deployment-identity.js";
import { watcherDeploymentProtocolScriptAuthority } from "./deployment-identity.verify-watcher-user-event-script-binding.js";
import {
  watcherDeploymentAppliedScriptHashes,
  watcherDeploymentReleaseFinalityPolicy,
} from "./deployment-identity.watcher-deployment-availability-challenge-authority.js";
import type { WatcherOperationsHttpServer } from "./operations-http.js";
import {
  createWatcherOperationsSink,
  createWatcherOperationsView,
  watcherDaBondPoolReadFailureReporter,
  watcherDaBondPoolReporter,
} from "./operations-observability.js";
import { refusePermanently } from "./permanent-refusal.js";
import type { WatcherProcessConfig } from "./process-config.js";
import type { WatcherStartupOperations } from "./startup-operations.js";
import type { WatcherStartup } from "./startup-progress.js";
import { closeWatcherAllocatedResources } from "./watcher-runtime.close-allocated-resources.js";
import { createWatcherRuntimeLifecycle } from "./watcher-runtime.create-lifecycle.js";
import {
  createWatcherDecisionDriver,
  type WatcherDecisionDriver,
  watcherFollowerStarted,
} from "./watcher-runtime.decision-driver.js";
import { createWatcherL1Readiness } from "./watcher-runtime.l1-readiness.js";
import { watcherL1SourceReports } from "./watcher-runtime.l1-source-reports.js";
import type { WatcherRuntime } from "./watcher-runtime.launch-checks.js";
import { watcherObservationSourceId } from "./watcher-runtime.observation-source-id.js";
import {
  prepareWatcherRuntimeAuthority,
  resolveWatcherRuntimeWalletAddresses,
} from "./watcher-runtime.prepare-authority.js";
import { prepareWatcherRuntimeWorkflows } from "./watcher-runtime.prepare-services.js";
import { createWatcherRuntimeReplayHost } from "./watcher-runtime.replayed-ledger.js";

/** How often the cached L1 readiness is refreshed without a follower change. */
const L1_READINESS_REFRESH_MS = 5_000;

/**
 * Compose production startup around the watcher's chain follower: every L1
 * read goes through the follower's fact store, and the decision driver
 * decides from its projections at the tip and at the release depth.
 *
 * Liveness: the operations server (`bound`) is bound before any stage
 * (`createWatcherRuntime`), so `/v1/status` (the liveness probe) answers
 * while stages wait out an unanswering node and while the follower syncs;
 * there is no `/healthz` route. Until the runtime's observability exists
 * `/readyz` names `startup:<stage>`, the stage most recently begun; after
 * that, until the first pass completes, and whenever the follower, a pass
 * or the journals hold decisions, it names the reason. No L1 condition,
 * journal integrity failure or failed journal open ends the process after
 * startup. On a failure this closes what it opened but the server.
 */
export const startWatcherRuntime = async (
  input: {
    readonly config: WatcherProcessConfig;
    readonly onAvailabilityStatusTransition?: (
      event: WatcherAvailabilityStatusTransition,
    ) => void;
  },
  bound: Readonly<{
    startup: WatcherStartup;
    startupOperations: WatcherStartupOperations;
    operationsHttp: WatcherOperationsHttpServer;
  }>,
): Promise<WatcherRuntime> => {
  const { startup, startupOperations } = bound;
  const {
    deploymentAuthority,
    deploymentIdentity,
    localL1Source,
    rollbackAuthenticationKey,
    fundingProfileOverlay,
  } = await prepareWatcherRuntimeAuthority(input, startup);
  const watcherConfig = input.config.watcherConfig;

  let follower: WatcherFollowerRuntime | undefined;
  let decisionDriver: WatcherDecisionDriver | undefined;
  let userEvents: WatcherUserEvents | undefined;
  let allocatedFaultProofApplication: WatcherFaultProofApplication | undefined;
  let faultProofSupervisor: WatcherFaultProofSupervisor | undefined;
  let faultDecisionBridge: WatcherFaultDecisionBridge | undefined;
  let availability: WatcherAvailabilityRuntime | undefined;
  let operationsHttp: WatcherOperationsHttpServer | undefined =
    bound.operationsHttp;
  let retainedDaOperationsBinding:
    | WatcherRetainedDaOperationsBinding
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
      userEvents: () => userEvents,
    });
  };

  try {
    // Every module below reports here; the view over it comes later.
    const recorder = createWatcherOperationsSink({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const operationsSink = recorder.sink;
    retainedDaOperationsBinding = bindWatcherRetainedDaOperations({
      deploymentIdentity,
      sink: operationsSink,
    });
    const releaseFinality =
      watcherDeploymentReleaseFinalityPolicy(deploymentIdentity).policy;
    const releaseDepth = releaseFinality.confirmationDepth;
    const authority =
      watcherDeploymentProtocolScriptAuthority(deploymentIdentity);
    const blueprintBytes = await readFile(
      input.config.faultProofInfrastructure.blueprintPath,
    );
    await refusePermanently("l1_slot_config", () =>
      assertWatcherL1SlotConfigMatchesDeployment(
        deploymentAuthority.ruleBundle.ruleBundle.l1SlotConfig,
        watcherConfig,
      ),
    );
    const userEventScripts = await refusePermanently("user_event_scripts", () =>
      readWatcherUserEventScriptBinding({
        binding: verifyWatcherUserEventScriptBinding({
          deploymentIdentity,
          blueprintBytes,
        }),
        deploymentIdentity,
      }),
    );

    // Address derivation only; the runtime never holds a live signer. The
    // executing runner re-resolves the same secret source itself.
    const {
      prover: proverWalletAddress,
      availability: availabilityWalletAddress,
    } = await resolveWatcherRuntimeWalletAddresses(input);
    await refusePermanently("journal_configuration", () =>
      validateWatcherFaultDecisionJournalConfiguration({
        directory: input.config.workflowJournalDirectory,
        deploymentFingerprint: deploymentIdentity.manifestId,
        launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
        authenticationKey: rollbackAuthenticationKey,
      }),
    );
    const { networkMagic } = await startup("l1_node_identity", () =>
      deriveWatcherNativeGenesisIdentity({ watcherConfig }),
    );
    const replayHost = createWatcherRuntimeReplayHost(
      deploymentAuthority,
      userEventScripts,
      input.config.workflowJournalDirectory,
      operationsSink,
    );
    const followedDeployment = {
      contractScriptHashes:
        watcherDeploymentAppliedScriptHashes(deploymentIdentity),
      blueprint: JSON.parse(Buffer.from(blueprintBytes).toString("utf8")),
    };
    const activeFollower = openWatcherDeploymentFollower({
      authority,
      storePath: `${watcherConfig.storage.path}.l1-follower.sqlite`,
      automaticRecoveryMaxDepth: releaseFinality.automaticRecoveryMaxDepth,
      origin: watcherConfig.l1.origin,
      followedScripts: watcherFollowedScripts(followedDeployment),
      node: {
        binaryPath: input.config.l1NodeTransportBinaryPath,
        socketPath: localL1Source.chainSync.socketPath,
        networkMagic,
        requestTimeoutMs: watcherConfig.l1.requestTimeoutMs,
      },
      // The prover's proof materials sit at their own address, read by it.
      walletAddresses: [
        proverWalletAddress,
        proofMaterialsAddressOfFundingAddress(proverWalletAddress),
        availabilityWalletAddress,
      ],
      eventProjection: userEventScripts.eventProjection,
      replayPruneFloor: replayHost.retention.floor,
      log: (line) => process.stderr.write(`${line}\n`),
    });
    follower = activeFollower;
    const { store, rawReads, provider } = activeFollower;
    const replay = replayHost.bind(activeFollower);
    userEvents = replay.userEvents;
    unsubscribeL1.push(replay.close);
    const { faultProofApplication, faultProofReadiness, perHeaderAlerts } =
      await prepareWatcherRuntimeWorkflows(input, {
        deploymentAuthority,
        deploymentIdentity,
        fundingProfileOverlay,
        startup,
        replayedLedger: replay.keeper,
        follower: activeFollower,
        l1: activeFollower.faultProofL1,
        onAllocated: (application) => {
          allocatedFaultProofApplication = application;
        },
        operationsSink,
      });

    // What this process sent and has not seen settle, and the wallet coins
    // its builds and its other actions hold: in memory only, one per process.
    const inFlight = createWorkflowInFlight();
    const walletLeases = Object.freeze({
      heldOutRefs: inFlight.heldOutRefs,
      holdWalletInputs: inFlight.holdWalletInputs,
    });
    const execution = createWatcherFaultProofExecution({
      application: faultProofApplication,
      inFlight,
      runtimeConfigPath: input.config.watcherRuntimeConfigPath,
      deploymentFingerprint: deploymentIdentity.manifestId,
      operationsSink,
    });
    const ownThreads = createWatcherOwnThreadSweep({
      store,
      policyId: watcherComputationThreadPolicyId(followedDeployment),
      finalDepth: releaseFinality.automaticRecoveryMaxDepth + 2,
      openCanceller: watcherOwnThreadCancellerSource(
        faultProofApplication,
        walletLeases,
      ),
      log: (line) => process.stderr.write(`watcher own threads: ${line}\n`),
    });
    const activeSupervisor = createWatcherFaultProofSupervisor({
      journalRoot: input.config.workflowJournalDirectory,
      inFlight,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deadlineAlertHeadroomMs: watcherDeadlineAlertHeadroomMs(
        watcherConfig.deadlines,
      ),
      queueAuthenticationKey: rollbackAuthenticationKey,
      l1Clock: {
        slotConfig: deploymentAuthority.ruleBundle.ruleBundle.l1SlotConfig,
        appliedSlot: activeFollower.appliedSlot,
      },
      execution,
      proofRetention: activeFollower.proofRetention,
      journalBusyRequeue: { wake: () => decisionDriver?.wake() },
      ownThreadSweep: ownThreads.pass,
      proofMaterials: faultProofApplication.proofMaterials,
      sweepProofMaterials: async ({ job, ...sweep }) =>
        await faultProofApplication.sweepProofMaterials({
          ...sweep,
          category: job.category,
          headerHash: job.headerHash,
          decisionDigest: job.decisionDigest,
          runtimeConfigPath: input.config.watcherRuntimeConfigPath,
        }),
      ...(faultProofApplication.proofMaterials === undefined
        ? {}
        : {
            proofMaterialsSweepable:
              faultProofApplication.proofMaterials.sweepable,
          }),
      observeSignedTransaction: activeFollower.faultProofL1
        .source("released-in-flight")
        .signedTransactions({
          releaseFinality:
            watcherDeploymentReleaseFinalityPolicy(deploymentIdentity),
        }).observeSignedTransaction,
    });
    faultProofSupervisor = activeSupervisor;
    // The proof-materials first pass completes once the roster's floor is
    // lifted: the follower's own first pass at tip.
    faultProofApplication.proofMaterials?.passAtTip(
      activeFollower.proofRetention.lifted,
    );

    const l1 = createWatcherL1Readiness({
      follower: activeFollower,
      driver: () => decisionDriver,
      proofMaterials: faultProofApplication.proofMaterials,
    });
    const refreshFollowerReadiness = (): void => void l1.refresh();
    const operations = createWatcherOperationsView(recorder, {
      supervisor: activeSupervisor,
      launchScopeStatus: () => ({
        installedCategoryCount:
          faultProofApplication.installedCategories.length,
        requiredCategoryCount: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      }),
      durableProofQueueStatus: activeSupervisor.durableQueueStatus,
      retainedDaTransportStatus:
        faultProofApplication.retainedDaTransportStatus,
      l1Readiness: () => [...l1.read(), ...execution.readiness()],
      l1Degradations: () => [
        ...l1.degradations(),
        ...ownThreads.degradations(),
      ],
      replayRefusals: replay.refusals.status,
    });
    const headerSource = createWatcherQueueHeaderSource(store, {
      releaseDepth,
    });
    const activeAvailability = await createWatcherAvailabilityRuntime({
      config: input.config,
      identity: deploymentIdentity,
      l1: {
        reads: rawReads,
        store,
        provider,
        // Validity lower bounds are measured from the ledger tip, not the
        // wall clock: the ledger judges them against its latest block.
        ledgerTipSlot: async () => (await provider.readTip()).slot,
      },
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
        operationsSink,
        deploymentIdentity.manifestId,
      ),
      onDaBondPoolReadFailure:
        watcherDaBondPoolReadFailureReporter(operationsSink),
      pinJournalTransactions: watcherAvailabilityTransactionPins(store),
    });
    availability = activeAvailability;
    const activeBridge = await createWatcherFaultDecisionBridge({
      application: faultProofApplication,
      supervisor: activeSupervisor,
      stateQueueSource: headerSource,
      journalDirectory: input.config.workflowJournalDirectory,
      authenticationKey: rollbackAuthenticationKey,
      maximumClassificationConcurrency: 16,
      walletLeases,
      operationsSink,
      perHeaderAlerts: [...perHeaderAlerts, retainedDaOperationsBinding],
      daFetchAlert: retainedDaOperationsBinding,
      pendingAvailabilityHeaders: activeAvailability.pendingAvailabilityHeaders,
    });
    faultDecisionBridge = activeBridge;
    startupOperations.attach(operations);

    const sourceIdentityDigest = localL1Source.chainSync.genesisIdentitySha256;
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
        replayedLedger: {
          depth: releaseFinality.automaticRecoveryMaxDepth + 2,
          advance: faultProofApplication.advanceReplayedLedger,
          checkpointPrecedesInit: (depth) =>
            watcherReplayCheckpointPrecedesInit(store, {
              hubOracleOneShotOutRef: authority.hubOracleOneShotOutRef,
              depth,
            }),
        },
        // Once per generation, whether the driver heard the rewind or pulled it
        // from the rollback log (before it subscribed, or left by a stop).
        ...watcherL1SourceReports({
          operationsSink,
          sourceIdentityDigest,
          refresh: refreshFollowerReadiness,
        }),
        retryDelayMs: watcherConfig.l1.requestTimeoutMs,
        log: (line) => process.stderr.write(`watcher decisions: ${line}\n`),
      },
      {
        atTip: () => activeFollower.status()?.atTip === true,
        started: () => watcherFollowerStarted(activeFollower.status()),
      },
    );
    decisionDriver = activeDriver;
    unsubscribeL1.push(activeFollower.onChange(refreshFollowerReadiness));
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
      operationsHttp: bound.operationsHttp,
      recoveredFaultProofWorkflowCount,
      availability: activeAvailability,
      follower: activeFollower,
      decisionDriver: activeDriver,
      closeAllocatedResources,
    });
  } catch (error) {
    // The caller closes the server, or keeps it serving a held startup.
    operationsHttp = undefined;
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
