/**
 * The validation-trace dispute through the watcher's installed application,
 * over the Open scene's emulator deployment: the watcher's classifier reads
 * the committed header through the authenticated raw L1 port and the
 * retained payload, and each turn runs the registry's admitted runner
 * (`WORKFLOW_RUNNER_FACTORIES`) over one watcher process's in-flight set;
 * the runner resolves the family's roster, binds its config and constructs
 * its workflow from the deployment the loader hands it.
 *
 * The deployment is the scene's, described by a finalized manifest the
 * watcher's signed identity binds (`installedSceneSignedDeployment`), so the
 * convicted block's removal and its bond-backed slash are the deployment's
 * own. The calling test mocks the dispute's deployment binding
 * with `bindingFor` and the L1 observation port with the recorded raw
 * authority. The roster is the installed scene's.
 */
import { Lucid } from "@lucid-evolution/lucid";
import {
  watcherDeploymentReleaseEconomicsAuthority,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";

import { validationTraceComputationSteps } from "../../src/validation-trace-dispute/removal.js";
import type { ValidationTraceAwaitingCounterparty } from "../../src/validation-trace-dispute/waiting.js";
import { createWorkflowActuationPermitController } from "../../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import type { WorkflowAdapterRunnerInput } from "../../src/workflow/adapters.js";
import { createValidationTraceDisputeCompleteCanonicalReplay } from "../../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import { applyFamilyApplicationRecord } from "../../src/workflow/family-application.apply-family-application-record.js";
import { FAMILY_APPLICATION_REGISTRY } from "../../src/workflow/family-application-registry.family-application-registry.js";
import type { VALIDATION_TRACE_DISPUTE_FAMILY_APPLICATION_RECORD } from "../../src/workflow/family-application-registry.validation-trace-dispute-family-application-record.js";
import { createFraudProofFamilyRawL1ObservationPort } from "../../src/workflow/family-l1-observation.js";
import { authenticatedStateQueueObservationDigest } from "../../src/workflow/header-classifier.authenticated-state-queue-observation-digest.js";
import { classifyHeader } from "../../src/workflow/header-classifier.classify-header.js";
import {
  createHeaderClassifier,
  headerDecisionReplayContext,
} from "../../src/workflow/header-classifier.create-header-classifier.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  createFamilyApplicationWorkflowRunner,
  WORKFLOW_RUNTIME_CONFIG,
} from "../../src/workflow/runtime.js";
import type { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { network } from "./emulator/blueprints.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.build-removal-deployment-info.js";
import { emulatorManifestProtocolParameters } from "./emulator-protocol-parameters.js";
import {
  fixtureLeaseCoordinator,
  FixtureRetainedDaSource,
} from "./script-decode-installed-host.js";
import { l1SlotConfig } from "./validation-trace-claim-material.js";
import { installedSceneSignedDeployment } from "./validation-trace-dispute-installed-manifest.js";
import {
  type InstalledScene,
  sceneRoster,
  scriptHashOf,
} from "./validation-trace-dispute-installed-scene.js";
import type { Block } from "./validation-trace-dispute-open-scene.js";
import {
  createTestWorkflowProcess,
  recordingRecord,
  type SentTransaction,
} from "./workflow-process.js";

const CATEGORY = "validationTraceDispute" as const;
const RUNTIME_CONFIG_PATH = "/installed/validation-trace-runtime.json";

type Recorder = ReturnType<typeof recordCrossBlockRawEmulator>;

/**
 * Binds the scene's deployment as its signed manifest does, classifies the
 * failing header as the watcher's classifier does, and returns the
 * objective's watcher turn, the binding the mocked manifest binding answers
 * for each header, and the installed application's host parts.
 */
export const installValidationTraceApplication = async ({
  installed,
  recorder,
}: {
  readonly installed: InstalledScene;
  readonly recorder: Recorder;
}) => {
  const { scene, removal } = installed;
  const { h, chain } = scene;
  const signed = installedSceneSignedDeployment(
    installed,
    buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
      removalReferenceScripts: removal.published,
    }),
  );
  const identity = signed.result;
  const { hubOraclePolicyId } = identity;
  const releaseFinalityAuthority =
    watcherDeploymentReleaseFinalityAuthority(identity);
  const releaseFinality = await releaseFinalityAuthority.verifyForWorkflow({
    hubOraclePolicyId,
  });
  const releaseEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    identity,
  ).verifyForWorkflow({ hubOraclePolicyId });
  const category = h.catalogue.categories[CATEGORY]!;
  // The finalized manifest the identity binds.
  const deploymentInfo = signed.signedIdentity.manifest as Readonly<{
    contracts: unknown;
  }>;
  const roster = sceneRoster(installed);
  const definitionFor = (headerHash: string) => ({
    category: CATEGORY,
    categoryId: category.categoryId,
    headerHash,
    proverCredential: h.proverSigner.paymentKeyHash,
    stateQueue: {
      policyId: h.contracts.stateQueue.policyId,
      address: h.contracts.stateQueue.spendingScriptAddress,
    },
    computationThread: {
      policyId: h.contracts.computationThread.policyId,
      steps: validationTraceComputationSteps(chain),
    },
    proofToken: {
      policyId: h.contracts.fraudProof.policyId,
      address: h.contracts.fraudProof.spendingScriptAddress,
    },
    operatorDirectory: {
      activePolicyId: h.contracts.activeOperators.policyId,
      activeAddress: h.contracts.activeOperators.spendingScriptAddress,
      retiredPolicyId: h.contracts.retiredOperators.policyId,
      retiredAddress: h.contracts.retiredOperators.spendingScriptAddress,
    },
    schedulerAddress: h.contracts.scheduler.spendingScriptAddress,
  });
  /** What the signed manifest binds for the dispute over `headerHash`. */
  const bindingFor = ({ headerHash }: { readonly headerHash: string }) => ({
    hubOraclePolicyId,
    blueprintHash: releaseFinality.blueprintHash,
    network,
    blueprint: h.realBlueprint,
    deploymentInfo,
    contractEntries: deploymentInfo.contracts,
    releaseFinality,
    releaseEconomics,
    cardanoProtocolParameters: emulatorManifestProtocolParameters(h.emulator),
    catalogue: {
      policyId: h.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        h.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: h.catalogue.root,
    },
    fieldPreimageCertificate: {
      policyId: installed.certificate.policyId,
      mintingScript: installed.certificate.mintingScript,
    },
    referenceScriptsByContract: Object.fromEntries(
      [...roster].map(([name, utxo]) => [
        name,
        {
          outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
          scriptHash: scriptHashOf(utxo),
        },
      ]),
    ),
    definition: definitionFor(headerHash),
    resolvedContracts: {
      hubOraclePolicyId: h.contracts.hubOracle.policyId,
      stateQueuePolicyId: h.contracts.stateQueue.policyId,
      category,
      contracts: {
        validationTraceDispute: chain,
        computationThread: h.contracts.computationThread,
        fraudProof: h.contracts.fraudProof,
      },
    },
  });

  // Classify each committed header as the watcher's classifier does.
  const sources = [
    new FixtureRetainedDaSource(
      hubOraclePolicyId,
      new Map(
        [scene.failing, scene.passing].map((block) => [
          block.evidence.headerHash,
          block.payloadEnvelopeCbor,
        ]),
      ),
    ),
  ];
  const classifier = await createHeaderClassifier({
    hubOraclePolicyId,
    replayer: createValidationTraceDisputeCompleteCanonicalReplay(l1SlotConfig),
    releaseFinalityAuthority,
  });
  const classify = async (block: Block) => {
    const { headerHash } = block.evidence;
    const l1 = createFraudProofFamilyRawL1ObservationPort({
      authority: recorder.authority,
      releaseFinality,
      releaseEconomics,
      definition: definitionFor(headerHash) as never,
    });
    const observation = await l1.observeHeader({ headerHash });
    return await classifyHeader({
      classifier,
      observation,
      authenticatedObservationDigest:
        await authenticatedStateQueueObservationDigest({
          observation,
          minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
        }),
      sources,
      predecessorLedgerAt: async (hash) =>
        hash === block.evidence.header.prevHeaderHash
          ? block.predecessorEntries
          : undefined,
    });
  };
  const decision = await classify(scene.failing);
  if (decision.decision !== "fault_detected")
    throw new Error(`header classified ${decision.decision}`);
  const headerHash = scene.failing.evidence.headerHash;
  const permit = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "0",
  }).permit;

  const slotConfig = h.proverLucid.config().slotConfig;
  const stateQueueMutationLeaseCoordinator = fixtureLeaseCoordinator();
  const signer = h.proverSigner;

  /** The watcher's loader body (`buildCommonInfrastructure`) for one
   * invocation over `headerHash`, with `decisionDigest` when admitted. */
  const load = async ({
    headerHash: loaded,
    decisionDigest,
    replayContext,
  }: {
    readonly headerHash: string;
    readonly decisionDigest?: string;
    readonly replayContext: ReturnType<typeof headerDecisionReplayContext>;
  }) => {
    const lucid = await Lucid(h.emulator, "Custom", { slotConfig });
    signer.selectWallet(lucid);
    return {
      schemaVersion: WORKFLOW_RUNTIME_CONFIG,
      infrastructure: {
        manifest: {},
        blueprintJson: JSON.stringify(h.realBlueprint),
        deploymentInfo,
        headerHash: loaded,
        ...(decisionDigest === undefined ? {} : { decisionDigest }),
        lucid,
        signer,
        // The case history reads the recorded raw L1.
        l1Source: { snapshotAuthority: () => recorder.authority },
        stateQueueMutationLeaseCoordinator,
        ...(replayContext === undefined ? {} : { replayContext }),
      },
      resolveReferenceScript: async ({
        contractName,
      }: {
        readonly contractName: string;
      }) => {
        const utxo = roster.get(contractName);
        if (utxo === undefined)
          throw new Error(`no published reference for ${contractName}`);
        return utxo;
      },
      retainedDaSources: sources,
      close: async () => undefined,
    };
  };

  /** The registry's installed record for the dispute. */
  const record = FAMILY_APPLICATION_REGISTRY[
    CATEGORY
  ] as typeof VALIDATION_TRACE_DISPUTE_FAMILY_APPLICATION_RECORD;
  /** The watcher process the turns run in; `restart` replaces it. */
  let process = createTestWorkflowProcess();
  // The installed runner over the record, with what the dispute submits
  // recorded by the process the turn runs in.
  const runner = createFamilyApplicationWorkflowRunner(
    recordingRecord(record, (adapter) => process.recording(adapter)),
    async ({ invocation }) => {
      const execution = invocation as Partial<WorkflowAdapterRunnerInput>;
      return (await load({
        headerHash: invocation.headerHash,
        ...(execution.decisionDigest === undefined
          ? {}
          : { decisionDigest: execution.decisionDigest }),
        replayContext: headerDecisionReplayContext(decision),
      })) as never;
    },
  );
  const objective = { category: CATEGORY, headerHash } as const;
  /** What earlier processes sent, oldest first. */
  const before: SentTransaction[] = [];
  /** One watcher application turn for the objective. */
  const turn = async () =>
    (await runner.runOrResume({
      category: CATEGORY,
      hubOraclePolicyId,
      headerHash,
      decisionDigest: decision.decisionDigest,
      actuationPermit: permit,
      objectiveWallet: process.objectiveWallet(objective),
      runtimeConfigPath: RUNTIME_CONFIG_PATH,
    })) as FraudProofWorkflowRunResult | ValidationTraceAwaitingCounterparty;
  /** The installed record applied to `block` under its classifier
   * `decided` decision, as the runner applies it, without the permits. */
  const apply = async (
    block: Block,
    decided: Parameters<typeof headerDecisionReplayContext>[0],
  ) => {
    const loaded = await load({
      headerHash: block.evidence.headerHash,
      replayContext: headerDecisionReplayContext(decided),
    });
    return await applyFamilyApplicationRecord({
      record,
      infrastructure: loaded.infrastructure as never,
      resolveReferenceScript: loaded.resolveReferenceScript,
      invocation: {
        hubOraclePolicyId,
        category: CATEGORY,
        headerHash: block.evidence.headerHash,
      },
    });
  };
  return {
    decision,
    record,
    hubOraclePolicyId,
    bindingFor,
    classify,
    apply,
    sources,
    turn,
    /** A watcher restart: the next turn runs in a fresh process. */
    restart: () => {
      before.push(...process.sent());
      process = createTestWorkflowProcess();
    },
    /** Every transaction the watcher sent for the objective, across
     * restarts, oldest first. */
    sent: () => Object.freeze([...before, ...process.sent()]),
    close: async () => undefined,
  } as const;
};

export type InstalledApplication = Awaited<
  ReturnType<typeof installValidationTraceApplication>
>;
