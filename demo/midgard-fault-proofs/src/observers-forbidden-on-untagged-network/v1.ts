import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  workflowActuationAuthorizingDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import { OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../workflow/cursor-family-adapter.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "../workflow/field-carriage-prerequisite.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  resumeRecordedFraudProofWorkflow,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import type { FraudProofRawL1FamilyStage } from "../workflow/raw-l1-family-derivation.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import {
  createObserversForbiddenActuator,
  type ObserversForbiddenActuatorAction,
  observersForbiddenFieldRequirement,
  type ObserversForbiddenWorkflowReferenceScripts,
} from "./actuator.js";
import {
  admitObserversForbiddenArtifact,
  type ObserversForbiddenArtifact,
} from "./artifact.js";
import {
  OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES,
  type ObserversForbiddenContracts,
} from "./contracts.js";
import { OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY } from "./family.js";
import {
  prepareObserversForbiddenAcceptedArtifact,
  prepareObserversForbiddenForcedArtifact,
} from "./replay.js";
import { ObserversForbiddenStep02DatumSchema } from "./schemas.js";

const CATEGORY = OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY;
export const OBSERVERS_FORBIDDEN_WORKFLOW =
  "midgard-observers-forbidden-on-untagged-network-production-workflow-v1" as const;

export type ObserversForbiddenRemovalReferenceScripts = Readonly<{
  correctionLockSpend: UTxO;
  stateQueueSpend: UTxO;
  stateQueueMint: UTxO;
  stateQueueFraudRemovalWithdraw: UTxO;
  activeOperatorsSpend: UTxO;
  activeOperatorsMint: UTxO;
  retiredOperatorsSpend: UTxO;
  retiredOperatorsMint: UTxO;
  schedulerSpend: UTxO;
}>;

export type ManifestBoundObserversForbiddenWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ObserversForbiddenWorkflowReferenceScripts &
    Readonly<{ removal: ObserversForbiddenRemovalReferenceScripts }>;
}>;

type SerialBinding =
  FraudProofWorkflowDeploymentBinding<"observersForbiddenOnUntaggedNetwork">;

export type ManifestBoundObserversForbiddenWorkflow = Readonly<{
  binding: SerialBinding;
  l1: FraudProofFamilyL1ObservationPort<"observersForbiddenOnUntaggedNetwork">;
  actuator: ReturnType<typeof createObserversForbiddenActuator>;
  prerequisite: FieldCarriagePrerequisitePort<"observersForbiddenOnUntaggedNetwork">;
  lucid: LucidEvolution;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

/** Config contains infrastructure and authenticated references only. */
export const createManifestBoundObserversForbiddenWorkflow = async (
  config: ManifestBoundObserversForbiddenWorkflowConfig,
): Promise<ManifestBoundObserversForbiddenWorkflow> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("observersForbidden decision digest is malformed");
  const binding = (await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: CATEGORY as never,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      ObserversForbiddenStep02DatumSchema,
      ObserversForbiddenStep02DatumSchema,
    ],
  })) as SerialBinding;
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const resolved = binding.resolvedContracts as unknown as {
    category: { categoryId: string };
    hubOraclePolicyId: string;
    stateQueuePolicyId?: string;
    contracts: Record<string, unknown> & {
      computationThread: ObserversForbiddenContracts["computationThread"];
      fraudProof: ObserversForbiddenContracts["fraudProof"] & {
        spendingScriptHash: string;
      };
      observersForbiddenOnUntaggedNetwork?: {
        steps: ObserversForbiddenContracts["steps"];
      };
    };
  };
  const chain = resolved.contracts.observersForbiddenOnUntaggedNetwork;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    chain.steps.length !== 2 ||
    certificate === null ||
    resolved.stateQueuePolicyId === undefined
  )
    throw new Error("observersForbidden manifest omitted required contracts");
  const bindReference = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const steps = [
    bindReference(
      "fraudProofObserversForbiddenOnUntaggedNetwork",
      config.referenceScripts.steps[0],
    ),
    bindReference(
      "fraudProofObserversForbiddenOnUntaggedNetworkStep02",
      config.referenceScripts.steps[1],
    ),
  ] as const;
  const witnessNames = {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  } as const;
  const witnesses = Object.fromEntries(
    Object.entries(witnessNames).map(([role, name]) => [
      role,
      bindReference(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScripts
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScripts>;
  const references = Object.freeze({
    steps,
    witnesses: Object.freeze(witnesses),
    fieldPreimageCertificateMint: bindReference(
      "fieldPreimageCertificateMint",
      config.referenceScripts.fieldPreimageCertificateMint,
    ),
  });
  const contracts: ObserversForbiddenContracts = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      blueprintTitle: OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as ObserversForbiddenContracts["steps"],
    computationThread: resolved.contracts.computationThread,
    fraudProof: resolved.contracts.fraudProof,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const actuator = createObserversForbiddenActuator({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const prerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
    category: CATEGORY as never,
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) =>
      observersForbiddenFieldRequirement({
        action: action.input as unknown as ObserversForbiddenActuatorAction,
        artifact,
        owner: config.signer.paymentKeyHash,
        certificate: {
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo: references.fieldPreimageCertificateMint,
        },
      }),
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  const adapter = withFieldCarriagePrerequisite({
    category: CATEGORY,
    prerequisite,
    base: createCursorFamilyWorkflowAdapter({
      spec: {
        category: CATEGORY,
        stepCount: 2,
        successors: { 1: [2], 2: ["proof_token"] },
      },
      l1,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
      refineAction: async ({ observed, artifact }) => {
        const selected = await currentAction({
          workflow: { lucid: config.lucid },
          artifact: admitObserversForbiddenArtifact(artifact).artifact,
          stage: observed.stage,
        });
        if (selected === "removed")
          throw new Error("observersForbidden action became terminal");
        return Object.fromEntries(
          Object.entries(selected).filter(
            ([key]) => key === "action" || key === "walkOrdinal",
          ),
        );
      },
      transactions: {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category: CATEGORY,
        prepareRaw: async (routed) => {
          if (routed.kind !== "observers_forbidden_on_untagged_network")
            throw new Error("raw family route changed");
          return await prepareObserversForbiddenAcceptedArtifact(
            routed.evidence,
          );
        },
        validatePreparedRawArtifact: async ({ routed, artifact }) => {
          if (routed.kind !== "observers_forbidden_on_untagged_network")
            throw new Error("raw family route changed");
          if (
            journalJsonDigest(
              await prepareObserversForbiddenAcceptedArtifact(routed.evidence),
            ) !== journalJsonDigest(artifact)
          )
            throw new Error(
              "prepared raw artifact differs from retained evidence",
            );
        },
        validatePreparedArtifact: async ({ evidence, artifact }) => {
          if (
            journalJsonDigest(
              await prepareObserversForbiddenForcedArtifact(evidence),
            ) !== journalJsonDigest(artifact)
          )
            throw new Error(
              "prepared family artifact differs from retained evidence",
            );
        },
        prepare: async ({ evidence }) =>
          await prepareObserversForbiddenForcedArtifact(evidence),
        capture: async ({ action, artifact }) =>
          await actuator.capture({
            action: action.input as unknown as ObserversForbiddenActuatorAction,
            artifact,
          }),
      },
    }),
  });
  return Object.freeze({
    binding,
    l1,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    actuator,
    prerequisite,
    lucid: config.lucid,
    decisionDigest: config.decisionDigest,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
};

const currentAction = async ({
  workflow,
  artifact,
  stage,
}: {
  readonly workflow: Pick<ManifestBoundObserversForbiddenWorkflow, "lucid">;
  readonly artifact: ObserversForbiddenArtifact;
  readonly stage: FraudProofRawL1FamilyStage;
}): Promise<ObserversForbiddenActuatorAction | "removed"> => {
  if (stage.kind === "not_started")
    return {
      stage: "init",
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
    };
  if (stage.kind === "proof_token")
    return {
      stage: "remove",
      nextRemovalOutRef: stage.nextRemovalOutRef,
      fraudProofOutRef: stage.fraudProofOutRef,
    };
  if (stage.kind === "removed") return "removed";
  if (stage.step === 1)
    return {
      stage: "step_01",
      threadOutRef: stage.threadOutRef,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
    };
  if (stage.step === 2) {
    const [txHash, outputIndex] = stage.threadOutRef.split("#");
    const [utxo] = await workflow.lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (utxo?.datum == null)
      throw new Error("observersForbidden step-02 datum disappeared");
    const state = Data.from(
      utxo.datum,
      ObserversForbiddenStep02DatumSchema as never,
    ) as { data: { subject: { transaction_id: string }; network_id: bigint } };
    if (
      state.data.subject.transaction_id !== artifact.transactionId ||
      state.data.network_id !== BigInt(artifact.networkId)
    )
      throw new Error("observersForbidden restart datum substitution");
    return { stage: "step_02", threadOutRef: stage.threadOutRef };
  }
  throw new Error("observersForbidden observed an impossible step");
};

export type ObserversForbiddenWorkflowRunResult = FraudProofWorkflowRunResult;

export const executeManifestBoundObserversForbiddenWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundObserversForbiddenWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    workflowActuationAuthorizingDecisionDigest(journal) !==
    workflow.decisionDigest
  )
    throw new Error("observersForbidden journal changed decision digest");
  if (workflowJournalIsReconciliationOnly(journal))
    return await resumeRecordedFraudProofWorkflow({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      category: CATEGORY,
      headerHash: workflow.binding.definition.headerHash,
      journal,
      adapter: workflow.adapter,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: [CATEGORY],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export type LoadedObserversForbiddenWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundObserversForbiddenWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadObserversForbiddenWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedObserversForbiddenWorkflow>;

/** Standard runtime loader surface; no evidence, stage, submit, or journal callbacks. */
export const createObserversForbiddenWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadObserversForbiddenWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== CATEGORY)
        throw new Error("observersForbidden runner category changed");
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: CATEGORY as never,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: CATEGORY as never,
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      try {
        if (
          loaded.schemaVersion !==
            "midgard-production-fraud-proof-runtime-config-v1" ||
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        )
          throw new Error(
            "observersForbidden runtime requires concrete public retained DA",
          );
        const workflow = await createManifestBoundObserversForbiddenWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "observersForbidden runtime binding changed invocation",
          );
        return (await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            executeManifestBoundObserversForbiddenWorkflow({
              workflow,
              sources: loaded.retainedDaSources,
              journal,
            }),
        })) as never;
      } finally {
        await loaded.close();
      }
    },
  });
