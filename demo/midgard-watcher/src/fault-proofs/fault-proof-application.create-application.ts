import {
  bindFraudProofTerminalDeployment,
  bindFraudProofWorkflowDeployment,
  type CompleteCanonicalReplayContext,
  createHeaderClassifier,
  createTransitionTraceEventAuthority,
  FAMILY_APPLICATION_REGISTRY,
  FraudProofL1CheckpointChangedError,
  headerDecisionReplayContext,
  installWorkflowApplicationRegistry,
  normalizeWorkflowJson,
  resolveFamilyApplicationReferences,
  runFraudProofWorkflowCli,
  TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
  verifyCompletedFraudProofWorkflow,
  WORKFLOW_RUNNER_FACTORIES,
  type WorkflowAdapterRunner,
  workflowJsonDigest,
} from "@al-ft/midgard-fault-proofs";
import { Header } from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import { admitWatcherWorkflowFundingProfileOverlay } from "../funding/workflow-funding-profile-overlay.js";
import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import { parseWatcherConfig } from "../runtime/config.parse-watcher-config.js";
import { assertWatcherVerifiedDeploymentAuthority } from "../runtime/deployment-authority.js";
import { assertVerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.verify-watcher-user-event-script-binding.js";
import {
  matchesWatcherSignedDeploymentManifest,
  watcherDeploymentReleaseFinalityAuthority,
} from "../runtime/deployment-identity.watcher-deployment-availability-challenge-authority.js";
import {
  createWatcherRetainedDaRuntimeOwner,
  createWatcherWorkflowRuntimeLoader,
} from "../storage/retained-da-runtime.js";
import {
  admitInfrastructure,
  bindWatcherDeploymentAuthority,
  readSecret,
  requireCanonicalFile,
} from "./fault-proof-application.bind-watcher-deployment-authority.js";
import {
  admittedApplications,
  type ApplicationConstruction,
  buildCommonInfrastructure,
  replayContextsPinnedAtLoadStart,
  type WatcherFaultProofApplicationWithLoaderForTest,
  watcherFaultProofSourceId,
} from "./fault-proof-application.build-common-infrastructure.js";
import { createWatcherCatalogueReplayer } from "./fault-proof-application.create-catalogue-replayer.js";
import { driveProvenCorrectionFrom } from "./fault-proof-application.drive-proven-correction.js";
import { openOwnThreadCancellerFrom } from "./fault-proof-application.own-thread-canceller.js";
import {
  WATCHER_FAULT_PROOF_APPLICATION,
  WATCHER_FAULT_PROOF_STARTUP_READINESS,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherFaultProofApplication,
  type WatcherInstalledWorkflowCategory,
} from "./fault-proof-application.production-dependencies.js";
import {
  classifyWatcherReplayedHeader,
  watcherReplayCheckpointAdvance,
} from "./fault-proof-application.replayed-ledger.js";
import { sweepProofMaterialsFrom } from "./fault-proof-application.sweep-proof-materials.js";
import { createWatcherProofMaterialsViews } from "./proof-materials-views.js";

export function createApplication(
  input: ApplicationConstruction &
    Readonly<{ unsafeExposeRuntimeLoaderForTest: true }>,
): WatcherFaultProofApplicationWithLoaderForTest;

export function createApplication(
  input: ApplicationConstruction,
): WatcherFaultProofApplication;

export function createApplication({
  options,
  dependencies,
  environment,
  allowExecution,
  unsafeExposeRuntimeLoaderForTest = false,
}: ApplicationConstruction &
  Readonly<{
    unsafeExposeRuntimeLoaderForTest?: boolean;
  }>): WatcherFaultProofApplication {
  if (unsafeExposeRuntimeLoaderForTest && allowExecution) {
    throw new Error(
      "watcher runtime loader is exposed only to a non-executing test application",
    );
  }
  const deploymentIdentity = options.deploymentIdentity;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const watcherConfig = parseWatcherConfig(options.watcherConfig);
  const l1 = options.l1;
  const deploymentAuthority = options.deploymentAuthority;
  const replayedLedger = options.replayedLedger;
  if (allowExecution && replayedLedger === undefined)
    throw new Error("watcher execution requires a durable replay ledger");
  if (allowExecution) {
    if (deploymentAuthority === undefined) {
      throw new Error("watcher execution requires deployment/rule authority");
    }
    assertWatcherVerifiedDeploymentAuthority(deploymentAuthority);
    if (deploymentAuthority.deploymentIdentity !== deploymentIdentity) {
      throw new Error("watcher application deployment authorities differ");
    }
  }
  const infrastructure = admitInfrastructure(options.infrastructure);
  admitWatcherWorkflowFundingProfileOverlay({
    overlay: options.fundingProfileOverlay,
    deploymentIdentity,
    allowExecution,
  });
  const environmentSnapshot = Object.freeze({ ...environment });
  const replayContexts = new Map<string, CompleteCanonicalReplayContext>();
  let authorityGeneration = 0;
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
  const pinnedReplayContexts = replayContextsPinnedAtLoadStart(replayContexts);
  const proofMaterials = createWatcherProofMaterialsViews(l1.provider);
  /** One loader for all installed families; the record does the rest. */
  const loadRuntime = pinnedReplayContexts.loader(
    createWatcherWorkflowRuntimeLoader({
      ...loaderOptions,
      watcherConfig,
      buildInfrastructure: async ({ watcherConfig, invocation }) =>
        await buildCommonInfrastructure({
          watcherConfig,
          invocation,
          infrastructure,
          deploymentIdentity,
          replayContext: pinnedReplayContexts.started(invocation),
          l1,
          dependencies,
          environment: environmentSnapshot,
          proofMaterials,
        }),
    }),
  );
  const runners: WatcherFaultProofApplication["runners"] = Object.freeze(
    Object.fromEntries(
      WATCHER_INSTALLED_WORKFLOW_CATEGORIES.map(
        (
          category,
        ): readonly [
          WatcherInstalledWorkflowCategory,
          WorkflowAdapterRunner,
        ] => [category, WORKFLOW_RUNNER_FACTORIES[category](loadRuntime)],
      ),
    ) as Record<WatcherInstalledWorkflowCategory, WorkflowAdapterRunner>,
  );
  const applicationRegistry = installWorkflowApplicationRegistry({
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
    requiredInstalledCategories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    installations: WATCHER_INSTALLED_WORKFLOW_CATEGORIES.map((category) => ({
      category,
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      runner: runners[category],
    })),
  });
  let classifierPromise: ReturnType<typeof createHeaderClassifier> | undefined;
  const loadClassifier = (headerHash: string) => {
    classifierPromise ??= (async () => {
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
      const manifest: unknown = JSON.parse(manifestJson!);
      if (!matchesWatcherSignedDeploymentManifest(deploymentIdentity, manifest))
        throw new Error(
          "classifier manifest differs from the signed deployment manifest",
        );
      const transitionBinding = await bindFraudProofWorkflowDeployment({
        manifest,
        blueprintJson: blueprintJson!,
        deploymentInfo: JSON.parse(deploymentInfoJson!),
        category: "transitionTrace",
        headerHash,
        proverCredential: "00".repeat(28),
        stepDatumSchemas: TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
      });
      const transitionTraceEventAuthority = createTransitionTraceEventAuthority(
        {
          binding: transitionBinding,
          l1: l1.source(
            `watcher-transition-events/${deploymentIdentity.hubOraclePolicyId}`,
          ),
        },
      );
      await watcherDeploymentReleaseFinalityAuthority(
        deploymentIdentity,
      ).verifyForWorkflow({
        hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      });
      const lucid = await dependencies.makeLucid({
        network: watcherConfig.targetNetwork,
        slotConfig: watcherConfig.customNetwork?.slotConfig,
        provider: l1.provider,
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
      const replayer = await createWatcherCatalogueReplayer({
        manifestJson: manifestJson!,
        blueprintJson: blueprintJson!,
        deploymentInfoJson: deploymentInfoJson!,
        headerHash,
        owner: signer.paymentKeyHash,
        lucid,
        network: watcherConfig.targetNetwork,
        deploymentIdentity,
        ...(options.onValidationTraceReplayStuck === undefined
          ? {}
          : { onReplayStuck: options.onValidationTraceReplayStuck }),
        ...(options.faultDetectorAlert === undefined
          ? {}
          : { faultDetectorAlert: options.faultDetectorAlert }),
      });
      return await createHeaderClassifier({
        transitionTraceEventAuthority,
        hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
        replayer,
        releaseFinalityAuthority:
          watcherDeploymentReleaseFinalityAuthority(deploymentIdentity),
      });
    })();
    return classifierPromise;
  };
  // The chain-driven operations' authority; they act only while admitted.
  const applicationAuthority = Object.freeze({
    isExecutable: () => allowExecution && admittedApplications.has(application),
    watcherConfig,
    infrastructure,
    deploymentIdentity,
    l1,
    dependencies,
    environment: environmentSnapshot,
    proofMaterials,
  });
  const methods: WatcherFaultProofApplication = {
    advanceReplayedLedger: watcherReplayCheckpointAdvance({
      watcherConfig,
      replayedLedger,
      retainedDaOwner,
      isAdmitted: () => admittedApplications.has(application),
    }),
    close: async () => {
      admittedApplications.delete(application);
      authorityGeneration += 1;
      replayContexts.clear();
      await retainedDaOwner.close();
    },
    schemaVersion: WATCHER_FAULT_PROOF_APPLICATION,
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
    installedCategories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    runners,
    applicationRegistry,
    proofMaterials,
    retainedDaTransportStatus: retainedDaOwner.transportStatus,
    retainDecisionAuthorities: (decisionDigest) => {
      authorityGeneration += 1;
      for (const digest of replayContexts.keys())
        if (digest !== decisionDigest) replayContexts.delete(digest);
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
        input.stateQueueObservation.hubOraclePolicyId !==
          deploymentIdentity.hubOraclePolicyId ||
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
      const completedDecision = await classifyWatcherReplayedHeader({
        request: input,
        watcherConfig,
        retainedDaOwner,
        replayedLedger,
        loadClassifier,
      });
      if (generation !== authorityGeneration) {
        throw new Error(
          "decision authority was invalidated during classification",
        );
      }
      const replayContext = headerDecisionReplayContext(completedDecision);
      if (replayContext !== undefined)
        replayContexts.set(completedDecision.decisionDigest, replayContext);
      return completedDecision;
    },
    assertStartupReady: async (invocation) => {
      if (!admittedApplications.has(application))
        throw new Error("watcher fault-proof application is closed");
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
      // Readiness binds the deployment and resolves the family's whole roster,
      // then stops: it binds no config, constructs no workflow and reads no
      // secret, so it can neither act nor need the optional infrastructure an
      // acting invocation must hold. Runtime startup resolves the prover
      // wallet's secret before the operations server binds, not here.
      const { resolveReferenceScript } = await bindWatcherDeploymentAuthority({
        watcherConfig,
        infrastructure,
        l1,
        dependencies,
      });
      const { referenceScriptOutRefs } =
        await resolveFamilyApplicationReferences({
          record: FAMILY_APPLICATION_REGISTRY[category],
          resolveReferenceScript,
        });
      return Object.freeze({
        schemaVersion: WATCHER_FAULT_PROOF_STARTUP_READINESS,
        ready: true,
        category,
        hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
        headerHash: invocation.headerHash,
        referenceScriptOutRefs,
      });
    },
    verifyCompleted: async (input) => {
      if (!admittedApplications.has(application))
        throw new Error("watcher fault-proof application is not admitted");
      if (!WATCHER_INSTALLED_WORKFLOW_CATEGORIES.includes(input.category))
        throw new Error("completed workflow category is not installed");
      const [manifestJson, blueprintJson, deploymentJson] = await Promise.all(
        [
          infrastructure.manifestPath,
          infrastructure.blueprintPath,
          infrastructure.deploymentInfoPath,
        ].map(async (path) =>
          dependencies.readText(await requireCanonicalFile(path, dependencies)),
        ),
      );
      const manifest: unknown = JSON.parse(manifestJson!);
      if (!matchesWatcherSignedDeploymentManifest(deploymentIdentity, manifest))
        throw new Error(
          "completed workflow manifest differs from the signed deployment manifest",
        );
      // The removal must cite the prover's record: the credential a fresh
      // run's terminal names, else (an observed completion) the wallet's key.
      const proverCredential =
        input.terminal?.economics.proverCredential ??
        dependencies.resolveSigner({
          network: watcherConfig.targetNetwork,
          secret: await readSecret({
            source: watcherConfig.proverWallet.keySource,
            dependencies,
            environment: environmentSnapshot,
            label: "watcher prover wallet",
          }),
        }).paymentKeyHash;
      const binding = await bindFraudProofTerminalDeployment({
        manifest,
        blueprintJson: blueprintJson!,
        deploymentInfo: JSON.parse(deploymentJson!),
        category: input.category,
        headerHash: input.headerHash,
        proverCredential,
      });
      const finality = await watcherDeploymentReleaseFinalityAuthority(
        deploymentIdentity,
      ).verifyForWorkflow({
        hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      });
      if (
        workflowJsonDigest(normalizeWorkflowJson(binding.releaseFinality)) !==
        workflowJsonDigest(normalizeWorkflowJson(finality))
      )
        throw new Error(
          "completed workflow changed its verified deployment release",
        );
      const authority = l1
        .source(
          watcherFaultProofSourceId({
            category: input.category,
            hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
            localL1Source: watcherConfig.l1.source,
          }),
        )
        .snapshotAuthority({
          releaseFinality: binding.releaseFinality,
          observationDepth: "inclusion",
        });
      try {
        return await verifyCompletedFraudProofWorkflow({
          binding,
          authority,
          decisionDigest: input.decisionDigest,
          ...(input.terminal === undefined ? {} : { terminal: input.terminal }),
        });
      } catch (error) {
        if (error instanceof FraudProofL1CheckpointChangedError)
          return { kind: "pending", reason: "checkpoint_changed" };
        throw error;
      }
    },
    driveProvenCorrection: driveProvenCorrectionFrom(applicationAuthority),
    openOwnThreadCanceller: openOwnThreadCancellerFrom(applicationAuthority),
    sweepProofMaterials: sweepProofMaterialsFrom(applicationAuthority),
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
  };
  const application: WatcherFaultProofApplication = Object.freeze(
    unsafeExposeRuntimeLoaderForTest
      ? { ...methods, unsafeLoadRuntimeForTest: loadRuntime }
      : methods,
  );
  admittedApplications.add(application);
  return application;
}
