import {
  createTransitionTraceEventAuthority,
  TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
  WORKFLOW_RUNNER_FACTORIES,
} from "@al-ft/midgard-fault-proofs";
import {
  bindFraudProofTerminalDeployment,
  bindFraudProofWorkflowDeployment,
  classifyHeader as classifyProductionHeaderV1,
  type CompleteCanonicalReplayContext,
  createCatalogueCompleteCanonicalReplay,
  createCrossBlockSettlementAuthority,
  createHeaderClassifier,
  FAMILY_APPLICATION_REGISTRY,
  type FamilyValidationChallengePort,
  FraudProofL1CheckpointChangedError,
  type HeaderDecision,
  headerDecisionReplayContext,
  installWorkflowApplicationRegistry,
  journalJsonDigest,
  normalizeJournalJson,
  resolveFamilyApplicationReferences,
  runFraudProofWorkflowCli,
  type WorkflowAdapterRunner,
} from "@al-ft/midgard-fault-proofs";
import {
  CrossBlockDuplicateEventStep02DatumSchema,
  FabricatedDepositStep02Datum,
  FabricatedDepositStep03Datum,
  FabricatedDepositStep04Datum,
  FabricatedWithdrawalStep02Datum,
  FabricatedWithdrawalStep03Datum,
  FabricatedWithdrawalStep04Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import { EMPTY_MERKLE_TREE_ROOT, Header } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherWorkflowFundingProfileOverlay,
  workflowFundingProfileFromOverlay,
} from "../funding/workflow-funding-profile-overlay.js";
import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import { parseWatcherConfig } from "../runtime/config.js";
import { assertWatcherVerifiedDeploymentAuthority } from "../runtime/deployment-authority.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  watcherDeploymentProtocolScriptAuthority,
  watcherDeploymentReleaseFinalityAuthority,
} from "../runtime/deployment-identity.js";
import {
  verifyCompletedWatcherReplayTranscriptWorkflow,
  watcherReplayTranscriptClassification,
} from "../storage/replay-transcript-completion.js";
import {
  createWatcherRetainedDaRuntimeOwner,
  createWatcherWorkflowRuntimeLoader,
  readAdmittedWatcherRuntimeConfig,
} from "../storage/retained-da-runtime.js";
import { archiveWatcherValidationCapture } from "./fault-proof-application.archive-validation-capture.js";
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
  predecessorObservationForClassifier,
  type WatcherFaultProofApplicationWithLoaderForTest,
  watcherFaultProofSourceId,
} from "./fault-proof-application.build-common-infrastructure.js";
import {
  WATCHER_FAULT_PROOF_APPLICATION,
  WATCHER_FAULT_PROOF_STARTUP_READINESS,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES,
  type WatcherFaultProofApplication,
  type WatcherInstalledWorkflowCategory,
} from "./fault-proof-application.production-dependencies.js";
import {
  assertWatcherValidationReplayCaptureCurrent,
  captureWatcherValidationReplayTranscript,
  refreshWatcherValidationReplayCapture,
} from "./replay-transcript-capture.js";
import { WatcherProofDecisionMissingError } from "./watcher-decision-hold.js";

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
  const l1 = options.l1;
  const deploymentAuthority = options.deploymentAuthority;
  const replayTranscriptStore = options.replayTranscriptStore;
  const userEvents = options.userEvents;
  if (allowExecution) {
    if (
      deploymentAuthority === undefined ||
      replayTranscriptStore === undefined ||
      userEvents === undefined
    ) {
      throw new Error(
        "watcher execution requires deployment/rule authority and durable replay transcripts",
      );
    }
    assertWatcherVerifiedDeploymentAuthority(deploymentAuthority);
    if (
      userEvents.deploymentManifestId !== deploymentIdentity.manifestId ||
      userEvents.blueprintHash !== deploymentIdentity.blueprintHash
    ) {
      throw new Error("watcher user-event reads' deployment authority differs");
    }
    if (deploymentAuthority.deploymentIdentity !== deploymentIdentity) {
      throw new Error("watcher application deployment authorities differ");
    }
  }
  const infrastructure = admitInfrastructure(options.infrastructure);
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
  const environmentSnapshot = Object.freeze({ ...environment });
  const replayContexts = new Map<string, CompleteCanonicalReplayContext>();
  let authorityGeneration = 0;
  const validationCaptures = new Map<
    string,
    Awaited<ReturnType<typeof captureWatcherValidationReplayTranscript>>
  >();
  /** Decisions held over a pre-follower transcript with an open proof. */
  const heldValidationDecisions = new Map<
    string,
    Readonly<{ headerHash: string; detail: string }>
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
  /**
   * How the validation-trace dispute reaches the challenge its classifier
   * captured for this decision. The capture is refreshed and its currency
   * asserted here, at the moment the family binds its config, so a retired
   * or superseded decision cannot be disputed.
   */
  const validationChallenge: FamilyValidationChallengePort = Object.freeze({
    currentChallenge: async ({ headerHash, decisionDigest }) => {
      const held = heldValidationDecisions.get(decisionDigest);
      if (held !== undefined && held.headerHash === headerHash)
        throw new WatcherProofDecisionMissingError({
          kind: "objective",
          category: "validationTraceDispute",
          headerHash,
          decisionDigest,
          detail: held.detail,
          readiness: "validation_transcript_pre_follower",
        });
      const capture = validationCaptures.get(decisionDigest);
      if (
        capture === undefined ||
        capture.transcript.headerHash !== headerHash
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
      if (replayTranscriptStore === undefined)
        throw new Error("validation challenge has no transcript lifecycle");
      await replayTranscriptStore.beginProofOperation(
        watcherReplayTranscriptClassification(capture),
      );
      assertWatcherValidationReplayCaptureCurrent(capture);
      return capture.challenge;
    },
  });
  /** One loader for all installed families; the record does the rest. */
  const loadRuntime = createWatcherWorkflowRuntimeLoader({
    ...loaderOptions,
    buildInfrastructure: async ({ watcherConfig, invocation }) =>
      await buildCommonInfrastructure({
        watcherConfig,
        invocation,
        infrastructure,
        deploymentIdentity,
        replayContexts,
        validationChallenge,
        l1,
        dependencies,
        environment: environmentSnapshot,
      }),
  });
  const runners: WatcherFaultProofApplication["runners"] = Object.freeze(
    Object.fromEntries(
      WATCHER_INSTALLED_WORKFLOW_CATEGORIES.map(
        (
          category,
        ): readonly [
          WatcherInstalledWorkflowCategory,
          WorkflowAdapterRunner,
        ] => [
          category,
          WORKFLOW_RUNNER_FACTORIES[category](
            loadRuntime,
            fundingProfile(category),
          ),
        ],
      ),
    ) as Record<WatcherInstalledWorkflowCategory, WorkflowAdapterRunner>,
  );
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
        l1: l1.source(
          `watcher-settlement-history/${deploymentIdentity.manifestId}`,
        ),
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
          l1: l1.source(
            `watcher-transition-events/${deploymentIdentity.manifestId}`,
          ),
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
      const [depositBinding, withdrawalBinding] = await Promise.all([
        bindFraudProofWorkflowDeployment({
          manifest: JSON.parse(manifestJson!),
          blueprintJson: blueprintJson!,
          deploymentInfo: JSON.parse(deploymentInfoJson!),
          category: "fabricatedDeposit",
          headerHash,
          proverCredential: signer.paymentKeyHash,
          stepDatumSchemas: [
            FraudProofComputationThreadStepDatum,
            FabricatedDepositStep02Datum,
            FabricatedDepositStep03Datum,
            FabricatedDepositStep04Datum,
          ],
        }),
        bindFraudProofWorkflowDeployment({
          manifest: JSON.parse(manifestJson!),
          blueprintJson: blueprintJson!,
          deploymentInfo: JSON.parse(deploymentInfoJson!),
          category: "fabricatedWithdrawal",
          headerHash,
          proverCredential: signer.paymentKeyHash,
          stepDatumSchemas: [
            FraudProofComputationThreadStepDatum,
            FabricatedWithdrawalStep02Datum,
            FabricatedWithdrawalStep03Datum,
            FabricatedWithdrawalStep04Datum,
          ],
        }),
      ]);
      const depositHistory =
        depositBinding.resolvedContracts.contracts.fabricatedDeposit?.history;
      const withdrawalHistory =
        withdrawalBinding.resolvedContracts.contracts.fabricatedWithdrawal
          ?.history;
      if (
        depositBinding.deploymentFingerprint !==
          deploymentIdentity.manifestId ||
        withdrawalBinding.deploymentFingerprint !==
          deploymentIdentity.manifestId ||
        depositHistory === undefined ||
        withdrawalHistory === undefined
      )
        throw new Error(
          "Event replay history differs from the applied deployment",
        );
      const replayer = createCatalogueCompleteCanonicalReplay({
        lucid,
        network: watcherConfig.targetNetwork,
        hubOraclePolicyId:
          watcherDeploymentProtocolScriptAuthority(deploymentIdentity)
            .protocolScriptHashes.hubOracleMint,
        minimumConfirmationDepth: 1,
        owner: signer.paymentKeyHash,
        history: { deposit: depositHistory, withdrawal: withdrawalHistory },
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
      });
    })();
    return classifierPromise;
  };
  const methods: WatcherFaultProofApplication = {
    close: async () => {
      admittedApplications.delete(application);
      authorityGeneration += 1;
      replayContexts.clear();
      validationCaptures.clear();
      heldValidationDecisions.clear();
      await retainedDaOwner.close();
    },
    schemaVersion: WATCHER_FAULT_PROOF_APPLICATION,
    deploymentFingerprint: deploymentIdentity.manifestId,
    installedCategories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    runners,
    applicationRegistry,
    retainedDaTransportStatus: retainedDaOwner.transportStatus,
    decisionUsesLocalEventHistory: (decisionDigest) =>
      validationCaptures.has(decisionDigest) ||
      heldValidationDecisions.has(decisionDigest),
    retainDecisionAuthorities: (decisionDigest) => {
      authorityGeneration += 1;
      for (const digest of replayContexts.keys()) {
        if (digest !== decisionDigest) replayContexts.delete(digest);
      }
      for (const digest of validationCaptures.keys()) {
        if (digest !== decisionDigest) validationCaptures.delete(digest);
      }
      for (const digest of heldValidationDecisions.keys()) {
        if (digest !== decisionDigest) heldValidationDecisions.delete(digest);
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
      let pendingHold: string | undefined;
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
        // A header committing a non-empty previous ledger can only be proved
        // against that ledger; the genesis-ledger header (empty root) has no
        // predecessor and the classifier forbids one.
        if (
          decision.decision === "fault_detected" &&
          WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES.includes(
            decision.category,
          ) &&
          input.observation.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT &&
          replayContext?.predecessor === undefined
        ) {
          throw new Error(
            `${decision.category} classifier decision omitted the authenticated predecessor ledger`,
          );
        }
        if (
          decision.decision === "fault_detected" &&
          decision.category === "validationTraceDispute"
        ) {
          if (
            deploymentAuthority === undefined ||
            replayTranscriptStore === undefined ||
            userEvents === undefined
          ) {
            throw new Error(
              "validation classification requires live deployment authority and transcript storage",
            );
          }
          const archived = await archiveWatcherValidationCapture({
            deploymentAuthority,
            replayTranscriptStore,
            stateQueueObservation: input.stateQueueObservation,
            header: input.header,
            decision,
            userEvents,
          });
          if (archived.kind === "captured") pendingCapture = archived.capture;
          else pendingHold = archived.detail;
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
        if (replayTranscriptStore === undefined)
          throw new Error(
            "validation classification has no transcript lifecycle",
          );
        await replayTranscriptStore.completeClassification(
          watcherReplayTranscriptClassification(pendingCapture),
        );
        assertWatcherValidationReplayCaptureCurrent(pendingCapture);
        heldValidationDecisions.delete(completedDecision.decisionDigest);
        validationCaptures.set(
          completedDecision.decisionDigest,
          pendingCapture,
        );
      }
      if (pendingHold !== undefined) {
        validationCaptures.delete(completedDecision.decisionDigest);
        heldValidationDecisions.set(completedDecision.decisionDigest, {
          headerHash: completedDecision.headerHash,
          detail: pendingHold,
        });
      }
      const replayContext = headerDecisionReplayContext(completedDecision);
      if (replayContext !== undefined) {
        replayContexts.set(completedDecision.decisionDigest, replayContext);
      }
      return completedDecision;
    },
    assertStartupReady: async (invocation) => {
      if (!admittedApplications.has(application)) {
        throw new Error("watcher fault-proof application is closed");
      }
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
      const watcherConfig = await readAdmittedWatcherRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        deploymentFingerprint: invocation.deploymentFingerprint,
        deploymentIdentity,
      });
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
        deploymentFingerprint: deploymentIdentity.manifestId,
        headerHash: invocation.headerHash,
        referenceScriptOutRefs,
      });
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
      const authority = l1
        .source(
          watcherFaultProofSourceId({
            category: input.category,
            manifestId: deploymentIdentity.manifestId,
            localL1Source: config.l1.source,
          }),
        )
        .snapshotAuthority({
          releaseFinality: binding.releaseFinality,
          observationDepth: "inclusion",
        });
      try {
        return await verifyCompletedWatcherReplayTranscriptWorkflow({
          ...(replayTranscriptStore === undefined
            ? {}
            : { replayTranscriptStore }),
          binding,
          authority,
          entries: input.entries,
          terminal: input.terminal,
          decisionDigest: input.decisionDigest,
        });
      } catch (error) {
        if (error instanceof FraudProofL1CheckpointChangedError)
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
  };
  const application: WatcherFaultProofApplication = Object.freeze(
    unsafeExposeRuntimeLoaderForTest
      ? { ...methods, unsafeLoadRuntimeForTest: loadRuntime }
      : methods,
  );
  admittedApplications.add(application);
  return application;
}
