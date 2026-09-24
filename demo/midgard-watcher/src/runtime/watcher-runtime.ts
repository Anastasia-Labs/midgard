import { mkdir, readFile, realpath } from "node:fs/promises";
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
import {
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import { createWatcherLocalKupmiosNativeObservationRuntime } from "../l1/local-kupmios-native-observation.js";
import { createWatcherLocalKupmiosRawSource } from "../l1/local-kupmios-raw-source.js";
import {
  startWatcherNativeChainSyncWithRetry,
  type WatcherNativeChainSyncAuthority,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncPoint,
  type WatcherNativeChainSyncRuntime,
} from "../l1/native-chain-sync.js";
import { createWatcherDurableRuntime } from "../storage/durable-runtime.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  bindWatcherRetainedDaOperations,
  type WatcherRetainedDaOperationsBinding,
} from "../storage/retained-da-runtime.js";
import { openWatcherSqliteDurableBackend } from "../storage/sqlite-durable-backend.js";
import {
  createWatcherChainCoordinator,
  recoverWatcherCoordinatorAfterRestart,
  type WatcherChainCoordinator,
} from "./chain-coordinator.js";
import { parseWatcherConfigJson } from "./config.js";
import {
  loadWatcherVerifiedDeploymentAuthority,
  type VerifiedWatcherDeploymentAuthority,
} from "./deployment-authority.js";
import { createWatcherHistoryRecovery } from "./history-recovery.js";
import {
  startWatcherOperationsHttpServer,
  type WatcherOperationsHttpServer,
} from "./operations-http.js";
import {
  createWatcherOperationsObservability,
  type WatcherOperationsObservability,
  type WatcherOperationsSink,
} from "./operations-observability.js";
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

export const WATCHER_RUNTIME_SCHEMA_VERSION =
  "midgard-watcher-production-runtime-v1" as const;

export type WatcherRuntime = Readonly<{
  schemaVersion: typeof WATCHER_RUNTIME_SCHEMA_VERSION;
  deploymentAuthority: VerifiedWatcherDeploymentAuthority;
  policy: WatcherFinalityPolicy;
  coordinator: WatcherChainCoordinator;
  faultProofApplication: WatcherFaultProofApplication;
  faultProofReadiness: readonly WatcherFaultProofStartupReadiness[];
  faultProofSupervisor: WatcherFaultProofSupervisor;
  operations: WatcherOperationsObservability;
  operationsEndpoint: string;
  recoveredFaultProofWorkflowCount: number;
  availability: WatcherAvailabilityRuntime;
  done: Promise<void>;
  caughtUp: Promise<void>;
  status(): Readonly<{
    phase: "live" | "closing" | "closed" | "failed";
    liveness: boolean;
    readiness: boolean;
    caughtUp: boolean;
    historyRecovery: ReturnType<
      ReturnType<typeof createWatcherHistoryRecovery>["status"]
    >;
    proofSupervisor: ReturnType<WatcherFaultProofSupervisor["status"]>;
    availability: ReturnType<WatcherAvailabilityRuntime["status"]>;
  }>;
  close(): Promise<void>;
}>;

/**
 * A partial replay/runner union is not a production classifier: an omitted
 * family could otherwise be misreported as healthy. Launch therefore requires
 * the exact canonical catalogue, in canonical order, before native L1 intake.
 */
export const assertWatcherFaultProofLaunchScope = (
  categories: readonly string[],
): void => {
  if (
    categories.length !== FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length ||
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
      (category, index) => categories[index] !== category,
    )
  ) {
    throw new Error(
      "watcher production fault-proof application does not cover the exact canonical catalogue",
    );
  }
};

export type WatcherRestartIntersectionCandidate = Readonly<{
  blockHash: string;
  blockNo: string;
  slot: string;
}>;

/**
 * Check that the native stream intersected one of the recorded resume points
 * and that its tip is not behind that point. The replay depth is unbounded:
 * every block between the intersection and the tip is replayed, and only a
 * node whose chain excludes all recorded history is refused.
 */
export const readWatcherNativeRecoveryBoundary = (input: {
  readonly nativeAuthority: WatcherNativeChainSyncAuthority;
  readonly admittedIntersections: readonly WatcherRestartIntersectionCandidate[];
}) => {
  const details = watcherNativeChainSyncAuthorityDetails(input.nativeAuthority);
  if (details === null) {
    throw new Error("native chain-sync authority expired during startup");
  }
  const selected = details.selectedIntersection;
  const admitted =
    selected.kind === "point"
      ? input.admittedIntersections.find(
          (candidate) =>
            candidate.blockHash === selected.blockHash &&
            candidate.slot === selected.slot,
        )
      : undefined;
  if (selected.kind !== "point" || admitted === undefined) {
    throw new Error(
      "native chain-sync selected a point outside the recorded resume history",
    );
  }
  if (
    details.currentTip.kind !== "point" ||
    BigInt(details.currentTip.blockNo) < BigInt(admitted.blockNo)
  ) {
    throw new Error(
      "native chain-sync tip is behind the selected resume point",
    );
  }
  return Object.freeze({
    ...details,
    selectedIntersection: selected,
    selectedBlockNo: admitted.blockNo,
    currentTip: details.currentTip,
  });
};

/**
 * Resume point first, then spaced progress rows, the durable finality
 * authority and the state-queue cursor, so a node that forked below the
 * head still finds a recorded ancestor.
 */
export const watcherRestartIntersectionCandidates = (input: {
  readonly progressHead: WatcherRestartIntersectionCandidate | null;
  readonly progressCandidates: readonly WatcherRestartIntersectionCandidate[];
  readonly authorityFinalized: WatcherRestartIntersectionCandidate | null;
  readonly stateQueueCursor: WatcherRestartIntersectionCandidate;
}): readonly WatcherRestartIntersectionCandidate[] => {
  const ordered = [
    input.progressHead ?? input.authorityFinalized ?? input.stateQueueCursor,
    ...input.progressCandidates,
    ...(input.authorityFinalized === null ? [] : [input.authorityFinalized]),
    input.stateQueueCursor,
  ];
  const seen = new Set<string>();
  const candidates: WatcherRestartIntersectionCandidate[] = [];
  for (const candidate of ordered) {
    const key = `${candidate.blockHash}@${candidate.slot}`;
    if (seen.has(key)) continue;
    seen.add(key);
    candidates.push(
      Object.freeze({
        blockHash: candidate.blockHash,
        blockNo: candidate.blockNo,
        slot: candidate.slot,
      }),
    );
    if (candidates.length === 128) break;
  }
  return Object.freeze(candidates);
};

export {
  mintWatcherProverFundingReservationPermit,
  type WatcherProverFundingUtxoProvider,
} from "../fault-proofs/fault-proof-execution.js";

const sameTipPoint = (event: WatcherNativeChainSyncEvent): boolean => {
  if (event.tip.kind === "origin")
    return event.kind === "roll_backward" && event.point.kind === "origin";
  return event.kind === "roll_forward"
    ? event.blockHash === event.tip.blockHash &&
        event.slot === event.tip.slot &&
        event.blockNo === event.tip.blockNo
    : event.point.kind === "point" &&
        event.point.blockHash === event.tip.blockHash &&
        event.point.slot === event.tip.slot;
};

export const createWatcherNativeEventHandler =
  (input: {
    readonly coordinator: Promise<Pick<WatcherChainCoordinator, "handle">>;
    readonly onCaughtUp: () => void;
    readonly operationsSink?: WatcherOperationsSink;
    readonly sourceIdentityDigest?: string;
    readonly nowMs?: () => bigint;
  }): ((event: WatcherNativeChainSyncEvent) => Promise<void>) =>
  async (event) => {
    const coordinator = await input.coordinator;
    await coordinator.handle(event);
    if (
      input.operationsSink !== undefined &&
      input.sourceIdentityDigest !== undefined
    ) {
      const observedAtMs = (input.nowMs?.() ?? BigInt(Date.now())).toString();
      if (event.kind === "roll_forward") {
        input.operationsSink.recordL1Source({
          sourceIdentityDigest: input.sourceIdentityDigest,
          sourceMode: "local_node",
          status: "consistent",
          blockHash: event.blockHash,
          blockNo: event.blockNo,
          slot: event.slot,
          observedAtMs,
        });
        input.operationsSink.setAlert({
          code: "chain_rollback",
          subjectDigest: input.sourceIdentityDigest,
          active: false,
          observedAtMs,
        });
      } else {
        input.operationsSink.setAlert({
          code: "chain_rollback",
          subjectDigest: input.sourceIdentityDigest,
          active: true,
          observedAtMs,
        });
        if (event.tip.kind === "point") {
          input.operationsSink.recordL1Source({
            sourceIdentityDigest: input.sourceIdentityDigest,
            sourceMode: "local_node",
            status: "consistent",
            blockHash: event.tip.blockHash,
            blockNo: event.tip.blockNo,
            slot: event.tip.slot,
            observedAtMs,
          });
        }
      }
    }
    if (sameTipPoint(event)) input.onCaughtUp();
  };

const requireWatcherRuntimeConfig = async (
  config: WatcherProcessConfig,
): Promise<void> => {
  if (
    (await realpath(config.watcherRuntimeConfigPath)) !==
    config.watcherRuntimeConfigPath
  ) {
    throw new Error("watcher workflow runtime config traverses a symlink");
  }
  const raw = await readFile(config.watcherRuntimeConfigPath, "utf8");
  const parsed = parseWatcherConfigJson(raw);
  if (
    watcherCanonicalJson(parsed) !== watcherCanonicalJson(config.watcherConfig)
  ) {
    throw new Error(
      "watcher process and workflow runtime configurations differ",
    );
  }
};

const prepareJournalDirectory = async (path: string): Promise<void> => {
  await mkdir(path, { recursive: true, mode: 0o700 });
  if ((await realpath(path)) !== path) {
    throw new Error("watcher workflow journal directory traverses a symlink");
  }
};

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
    requireWatcherRuntimeConfig(input.config),
  );
  await prepareJournalDirectory(input.config.workflowJournalDirectory);
  const deploymentAuthority = await startup("deployment_authority", () =>
    loadWatcherVerifiedDeploymentAuthority({
      path: input.config.deploymentAuthorityPath,
      ruleBundlePath: input.config.ruleBundlePath,
    }),
  );
  const { deploymentIdentity } = deploymentAuthority;
  const policy = makeWatcherFinalityPolicy(
    input.config.watcherConfig,
    deploymentIdentity,
  );
  if (
    policy === null ||
    (policy.network !== "Preprod" && policy.network !== "Custom") ||
    policy.sourceMode !== "local_node" ||
    policy.confirmationDepth !== "30" ||
    policy.maximumPreFinalityRollbackDepth !== "30" ||
    policy.maximumPostFinalityRecoveryDepth !== "2160"
  ) {
    throw new Error(
      "watcher production finality differs from the verified release",
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
      const { stateQueueRuntime } = earlyQueue;
      const retained = durable.readFinality();
      const candidates = watcherRestartIntersectionCandidates({
        progressHead: blockProgress.readHead(),
        progressCandidates: blockProgress.readCandidates(),
        authorityFinalized: retained.finalized,
        stateQueueCursor: stateQueueRuntime.replayIntersection,
      });
      const bootstrap = await startWatcherNativeChainSyncWithRetry({
        binaryPath: input.config.nativeChainSyncBinaryPath,
        watcherConfig: input.config.watcherConfig,
        intersectionCandidates: candidates.map(({ blockHash, slot }) => ({
          kind: "point" as const,
          blockHash,
          slot,
        })),
        startupTimeoutMs: input.config.watcherConfig.l1.requestTimeoutMs,
        onEvent: async () => undefined,
      });
      try {
        const boundary = readWatcherNativeRecoveryBoundary({
          nativeAuthority: bootstrap.authority,
          admittedIntersections: candidates,
        });
        if (
          await recoverWatcherCoordinatorAfterRestart({
            durable,
            restartIntersection: boundary.selectedIntersection,
          })
        )
          throw new Error(
            "Watcher restart remains quarantined: authenticated recovery evidence is incomplete",
          );
      } finally {
        await bootstrap.close();
      }
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
      async () => {
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
          faultProofReadiness.push(
            await faultProofApplication.assertStartupReady({
              mode: "resume",
              category,
              deploymentFingerprint: deploymentIdentity.manifestId,
              headerHash: WATCHER_STARTUP_READINESS_HEADER_HASH,
              journalDirectory,
              runtimeConfigPath: input.config.watcherRuntimeConfigPath,
            }),
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
    const proverFundingProtocolParameters =
      await createWatcherProtocolParameterRuntimeAuthority({
        deploymentIdentity,
        ogmiosUrl: ogmiosService.endpoint,
        timeoutMs: input.config.watcherConfig.l1.requestTimeoutMs,
      });
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
      proverWalletAddress,
      onStatusTransition: input.onAvailabilityStatusTransition,
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
      startupTimeoutMs: input.config.watcherConfig.l1.requestTimeoutMs,
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
