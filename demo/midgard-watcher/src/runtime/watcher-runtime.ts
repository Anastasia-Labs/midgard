import { mkdir, readFile, realpath } from "node:fs/promises";
import { join } from "node:path";

import {
  createSqliteHistoricalNativeScriptCheckpointStore,
  isWorkflowActuationRevokedError,
  resolveProverSigner,
  type WorkflowActuationPermit,
  type WorkflowAdapterRunner,
  type WorkflowAdapterRunnerInput,
  type WorkflowFundingReservationPermit,
} from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { Kupmios, type UTxO, utxoToCore } from "@lucid-evolution/lucid";

import {
  createWatcherFaultDecisionBridge,
  type WatcherFaultDecisionBridge,
} from "../fault-proofs/fault-decision-bridge.js";
import {
  createWatcherFaultProofApplication,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherFaultProofApplication,
  type WatcherFaultProofStartupReadiness,
  type WatcherInstalledWorkflowCategory,
} from "../fault-proofs/fault-proof-application.js";
import {
  createWatcherFaultProofSupervisor,
  type WatcherFaultProofSupervisor,
} from "../fault-proofs/fault-proof-supervisor.js";
import { createWatcherProtocolParameterRuntimeAuthority } from "../funding/prover-funding.js";
import {
  assertWatcherProverFundingAuthorityFactory,
  createWatcherProverFundingAuthorityFactory,
  type WatcherProverFundingAuthorityFactory,
} from "../funding/prover-funding-authority.js";
import {
  openWatcherSqliteProverFundingReservationStore,
  type WatcherSqliteProverFundingReservationStoreRuntime,
} from "../funding/sqlite-prover-funding-reservation-store.js";
import {
  loadWatcherWorkflowFundingProfileOverlay,
  type WatcherWorkflowFundingProfileOverlay,
  workflowFundingProfileFromOverlay,
} from "../funding/workflow-funding-profile-overlay.js";
import { createWatcherStateQueueObservationSource } from "../indexers/authenticated-state-queue-observation.js";
import {
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import {
  createWatcherLocalKupmiosNativeObservationRuntime,
  createWatcherLocalKupmiosRawSource,
} from "../l1/local-kupmios-native-observation.js";
import {
  startWatcherNativeChainSyncWithRetry,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncPoint,
  type WatcherNativeChainSyncRuntime,
} from "../l1/native-chain-sync.js";
import { createWatcherDurableRuntime } from "../storage/durable-runtime.js";
import {
  watcherCanonicalJson,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";
import {
  bindWatcherRetainedDaOperations,
  type WatcherRetainedDaOperationsBinding,
} from "../storage/retained-da-runtime.js";
import { openWatcherSqliteDurableBackend } from "../storage/sqlite-durable-backend.js";
import {
  createWatcherChainCoordinator,
  type WatcherChainCoordinator,
} from "./chain-coordinator.js";
import { parseWatcherConfigJson } from "./config.js";
import { loadWatcherVerifiedDeploymentAuthority } from "./deployment-authority.js";
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
import { createWatcherStateQueueRuntime } from "./state-queue-runtime.js";
import { createWatcherTrustedHeadClientRuntime } from "./trusted-head-runtime.js";

export const WATCHER_RUNTIME_SCHEMA_VERSION =
  "midgard-watcher-production-runtime-v1" as const;

export type WatcherRuntime = Readonly<{
  schemaVersion: typeof WATCHER_RUNTIME_SCHEMA_VERSION;
  policy: WatcherFinalityPolicy;
  coordinator: WatcherChainCoordinator;
  faultProofApplication: WatcherFaultProofApplication;
  faultProofReadiness: readonly WatcherFaultProofStartupReadiness[];
  faultProofSupervisor: WatcherFaultProofSupervisor;
  operations: WatcherOperationsObservability;
  operationsEndpoint: string;
  recoveredFaultProofWorkflowCount: number;
  done: Promise<void>;
  caughtUp: Promise<void>;
  status(): Readonly<{
    phase: "live" | "closing" | "closed" | "failed";
    liveness: boolean;
    readiness: boolean;
    caughtUp: boolean;
    proofSupervisor: ReturnType<WatcherFaultProofSupervisor["status"]>;
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

const parseWatcherProverFundingOutRef = (
  outRef: string,
): Readonly<{ txHash: string; outputIndex: number }> => {
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u.exec(outRef);
  if (match === null) {
    throw new Error("prover funding output reference is not canonical");
  }
  return Object.freeze({ txHash: match[1]!, outputIndex: Number(match[2]!) });
};

export type WatcherProverFundingUtxoProvider = Readonly<{
  getUtxos(address: string): Promise<UTxO[]>;
  getUtxosByOutRef(
    outRefs: readonly Readonly<{ txHash: string; outputIndex: number }>[],
  ): Promise<UTxO[]>;
}>;

/**
 * Application-supervisor permit mint. The supervisor binds the exact admitted
 * decision digest, actuation permit, and rollback generation to a fresh
 * atomically reserved slice of the live prover wallet; the fault-proof
 * application can neither mint nor substitute this authority. All wallet and
 * protocol-input resolution goes through the same local-node Kupo/Ogmios
 * authority the runners execute against.
 */
export const mintWatcherProverFundingReservationPermit = async (input: {
  readonly category: WatcherInstalledWorkflowCategory;
  readonly runner: WorkflowAdapterRunner;
  readonly fundingProfileOverlay: WatcherWorkflowFundingProfileOverlay;
  readonly factory: WatcherProverFundingAuthorityFactory;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly rollbackGeneration: string;
  readonly decisionDigest: string;
  readonly walletAddress: string;
  readonly provider: WatcherProverFundingUtxoProvider;
}): Promise<WorkflowFundingReservationPermit> => {
  assertWatcherProverFundingAuthorityFactory(input.factory);
  const fundingRequirements = workflowFundingProfileFromOverlay({
    overlay: input.fundingProfileOverlay,
    category: input.category,
  });
  return await input.factory.create({
    category: input.category,
    runner: input.runner,
    fundingRequirements,
    actuationPermit: input.actuationPermit,
    rollbackGeneration: input.rollbackGeneration,
    decisionDigest: input.decisionDigest,
    walletAddress: input.walletAddress,
    walletUtxos: await input.provider.getUtxos(input.walletAddress),
    resolveInputs: async (outRefs) =>
      await input.provider.getUtxosByOutRef(
        outRefs.map(parseWatcherProverFundingOutRef),
      ),
    resolveProtocolInputAuthority: async ({
      deploymentIdentity,
      outRef,
      semanticRole,
    }) => {
      const resolved = await input.provider.getUtxosByOutRef([
        parseWatcherProverFundingOutRef(outRef),
      ]);
      if (resolved.length !== 1) {
        throw new Error(
          "prover funding protocol input is not a unique live local-node output",
        );
      }
      return Object.freeze({
        deploymentFingerprint: deploymentIdentity.manifestId,
        outRef,
        semanticRole,
        resolvedOutputCborHex: utxoToCore(resolved[0]!)
          .output()
          .to_canonical_cbor_hex(),
      });
    },
  });
};

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
}): Promise<WatcherRuntime> => {
  await requireWatcherRuntimeConfig(input.config);
  await prepareJournalDirectory(input.config.workflowJournalDirectory);
  const deploymentIdentity = await loadWatcherVerifiedDeploymentAuthority({
    path: input.config.deploymentAuthorityPath,
  });
  const policy = makeWatcherFinalityPolicy(
    input.config.watcherConfig,
    deploymentIdentity,
  );
  if (
    policy === null ||
    policy.network !== "Preprod" ||
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
    additionalSecretSources: [
      input.config.faultProofInfrastructure.midgardNodeAdminKeySource,
    ],
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
  const faultProofApplication = createWatcherFaultProofApplication({
    deploymentIdentity,
    infrastructure: input.config.faultProofInfrastructure,
    historicalNativeScriptCheckpointStore,
    fundingProfileOverlay,
  });
  assertWatcherFaultProofLaunchScope(faultProofApplication.installedCategories);
  const faultProofReadiness: WatcherFaultProofStartupReadiness[] = [];
  for (const category of WATCHER_INSTALLED_WORKFLOW_CATEGORIES) {
    const journalDirectory = join(
      input.config.workflowJournalDirectory,
      "readiness",
      category,
      input.config.readinessHeaderHash,
    );
    await prepareJournalDirectory(journalDirectory);
    faultProofReadiness.push(
      await faultProofApplication.assertStartupReady({
        mode: "resume",
        category,
        deploymentFingerprint: deploymentIdentity.manifestId,
        headerHash: input.config.readinessHeaderHash,
        journalDirectory,
        runtimeConfigPath: input.config.watcherRuntimeConfigPath,
      }),
    );
  }

  const sqlite = await openWatcherSqliteDurableBackend({
    path: input.config.watcherConfig.storage.path,
  });
  let native: WatcherNativeChainSyncRuntime | undefined;
  let observation:
    | Awaited<
        ReturnType<typeof createWatcherLocalKupmiosNativeObservationRuntime>
      >
    | undefined;
  let faultProofSupervisor: WatcherFaultProofSupervisor | undefined;
  let faultDecisionBridge: WatcherFaultDecisionBridge | undefined;
  let operationsHttp: WatcherOperationsHttpServer | undefined;
  let retainedDaOperationsBinding:
    | WatcherRetainedDaOperationsBinding
    | undefined;
  let proverFundingStore:
    | WatcherSqliteProverFundingReservationStoreRuntime
    | undefined;
  const closeAllocatedResources = async (): Promise<void> => {
    const failures: unknown[] = [];
    faultDecisionBridge?.invalidateForShutdown();
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
    if (faultProofSupervisor !== undefined) {
      try {
        await faultProofSupervisor.close();
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
  let resolveCoordinator!: (value: WatcherChainCoordinator) => void;
  let rejectCoordinator!: (reason: Error) => void;
  const coordinatorReady = new Promise<WatcherChainCoordinator>(
    (resolve, reject) => {
      resolveCoordinator = resolve;
      rejectCoordinator = reject;
    },
  );
  let resolveCaughtUp!: () => void;
  let rejectCaughtUp!: (reason: Error) => void;
  const nativeCaughtUp = new Promise<void>((resolve, reject) => {
    resolveCaughtUp = resolve;
    rejectCaughtUp = reject;
  });
  try {
    const durable = await createWatcherDurableRuntime({
      backend: sqlite.backend,
      policy,
      authenticationKey: trusted.rollbackAuthenticationKey,
      client: trusted.client,
    });
    const rawSource = createWatcherLocalKupmiosRawSource({
      watcherConfig: input.config.watcherConfig,
      deploymentIdentity,
    });
    const stateQueueSource = createWatcherStateQueueObservationSource({
      deploymentIdentity,
      rawSource,
    });
    const stateQueueRuntime = await createWatcherStateQueueRuntime({
      store: sqlite.stateQueueObservations,
      source: stateQueueSource,
    });
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
        deploymentIdentity,
        protocolParameters: proverFundingProtocolParameters,
        store: proverFundingStore.store,
      });
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
      run: async ({ job, actuationPermit }) => {
        const { mode, category, headerHash, decisionDigest } = job;
        const updatedAtMs = Date.now().toString();
        const actionIdentityDigest = watcherSha256CanonicalJson({
          category,
          headerHash,
          decisionDigest,
          rollbackGeneration: job.rollbackGeneration,
        });
        operations.sink.recordProofStep({
          decisionDigest,
          stage: "prepare",
          actionIdentityDigest,
          status: "preflight",
          updatedAtMs,
        });
        const journalDirectory = join(
          input.config.workflowJournalDirectory,
          "fault-proofs",
          category,
          headerHash,
        );
        await prepareJournalDirectory(journalDirectory);
        try {
          const fundingReservationPermit =
            await mintWatcherProverFundingReservationPermit({
              category,
              runner: faultProofApplication.runners[category],
              fundingProfileOverlay,
              factory: proverFundingAuthorityFactory,
              actuationPermit,
              rollbackGeneration: job.rollbackGeneration,
              decisionDigest,
              walletAddress: proverWalletAddress,
              provider: proverUtxoProvider,
            });
          const invocation: WorkflowAdapterRunnerInput = {
            mode,
            category,
            deploymentFingerprint: deploymentIdentity.manifestId,
            headerHash,
            decisionDigest,
            actuationPermit,
            fundingReservationPermit,
            journalDirectory,
            runtimeConfigPath: input.config.watcherRuntimeConfigPath,
          };
          const result = await faultProofApplication.runOrResume(invocation);
          operations.sink.recordProofStep({
            decisionDigest,
            stage: "terminal",
            actionIdentityDigest,
            status: "completed",
            updatedAtMs: Date.now().toString(),
          });
          return result;
        } catch (error) {
          const cancelled = isWorkflowActuationRevokedError(error);
          operations.sink.recordProofStep({
            decisionDigest,
            stage: "terminal",
            actionIdentityDigest,
            status: cancelled ? "cancelled" : "failed",
            updatedAtMs: Date.now().toString(),
          });
          if (!cancelled) {
            operations.sink.setAlert({
              code: "proof_submission_failure",
              subjectDigest: decisionDigest,
              active: true,
              observedAtMs: Date.now().toString(),
            });
          }
          throw error;
        }
      },
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
    });
    retainedDaOperationsBinding = bindWatcherRetainedDaOperations({
      deploymentIdentity,
      sink: operations.sink,
    });
    faultDecisionBridge = await createWatcherFaultDecisionBridge({
      application: faultProofApplication,
      supervisor: faultProofSupervisor,
      stateQueueSource,
      journalDirectory: input.config.workflowJournalDirectory,
      runtimeConfigPath: input.config.watcherRuntimeConfigPath,
      maximumClassificationConcurrency: 16,
      operationsSink: operations.sink,
    });
    await faultDecisionBridge.prepareForRecovery(stateQueueRuntime.current());
    const recoveredFaultProofWorkflowCount =
      await faultDecisionBridge.recoverExisting();
    operationsHttp = await startWatcherOperationsHttpServer({
      endpoint: input.config.operationsEndpoint,
      observability: operations,
    });
    const replayIntersection: WatcherNativeChainSyncPoint = Object.freeze({
      kind: "point",
      blockHash: stateQueueRuntime.replayIntersection.blockHash,
      slot: stateQueueRuntime.replayIntersection.slot,
    });
    native = await startWatcherNativeChainSyncWithRetry({
      binaryPath: input.config.nativeChainSyncBinaryPath,
      watcherConfig: input.config.watcherConfig,
      // State-queue authority owns this exact intersection. Retrying an older
      // native point while retaining the latest queue cursor would skip or
      // reverse authenticated queue transitions, so there is no fallback.
      intersectionCandidates: Object.freeze([replayIntersection]),
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
    const details = watcherNativeChainSyncAuthorityDetails(native.authority);
    if (details === null) {
      throw new Error("native chain-sync authority expired during startup");
    }
    if (
      details.selectedIntersection.kind !== "point" ||
      details.selectedIntersection.blockHash !==
        stateQueueRuntime.replayIntersection.blockHash ||
      details.selectedIntersection.slot !==
        stateQueueRuntime.replayIntersection.slot
    ) {
      throw new Error(
        "native chain-sync selected a point outside state-queue restore authority",
      );
    }
    if (
      details.currentTip.kind !== "point" ||
      details.currentTip.blockNo !==
        stateQueueRuntime.catchupBoundary.ogmiosTipBlockNo ||
      BigInt(details.currentTip.blockNo) -
        BigInt(stateQueueRuntime.replayIntersection.blockNo) >
        2_160n
    ) {
      throw new Error(
        "native chain-sync tip differs from the admitted state-queue recovery bound",
      );
    }
    const coordinator = createWatcherChainCoordinator({
      policy,
      durable,
      observation,
      restartIntersection: details.selectedIntersection,
      hooks: stateQueueRuntime.bindFaultDecisionBridge(faultDecisionBridge),
    });
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
      native.done,
      activeFaultProofSupervisor.done,
      operationsHttp.done,
    ]);
    const caughtUpPromise = Promise.race([
      Promise.all([nativeCaughtUp, stateQueueRuntime.caughtUp]).then(() => {
        caughtUp = true;
      }),
      runtimeDone.then(() => {
        throw new Error(
          "watcher production liveness ended before durable catch-up",
        );
      }),
    ]);
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
      policy,
      coordinator,
      faultProofApplication,
      faultProofReadiness: Object.freeze(faultProofReadiness),
      faultProofSupervisor: activeFaultProofSupervisor,
      operations,
      operationsEndpoint: operationsHttp.endpoint,
      recoveredFaultProofWorkflowCount,
      done: runtimeDone,
      caughtUp: caughtUpPromise,
      status: () => {
        const proofSupervisor = activeFaultProofSupervisor.status();
        const operationsStatus = operations.api.status();
        const liveness = phase === "live";
        return Object.freeze({
          phase,
          liveness,
          readiness:
            liveness &&
            caughtUp &&
            proofSupervisor.phase === "accepting" &&
            proofSupervisor.recovered &&
            proofSupervisor.deadlineHealth === "safe" &&
            operationsStatus.readiness === "ready",
          caughtUp,
          proofSupervisor,
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
