import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriagePrerequisitePort,
} from "../workflow/field-carriage-prerequisite.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import type { FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import {
  submitCapturedTransaction,
  workflowTransactionInputOutRefs,
} from "../workflow/transaction-boundary.js";
import {
  createObserversForbiddenActuator,
  type ObserversForbiddenActuatorAction,
  observersForbiddenFieldRequirement,
  type ObserversForbiddenWorkflowReferenceScripts,
} from "./actuator.js";
import {
  type ObserversForbiddenArtifact,
  observersForbiddenArtifactDigest,
} from "./artifact.js";
import {
  OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES,
  type ObserversForbiddenContracts,
} from "./contracts.js";
import { OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY } from "./family.js";
import {
  detectObserversForbiddenAcceptedRawReplay,
  observersForbiddenRawBlockEvidenceFromVerifiedPayload,
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
  return Object.freeze({
    binding,
    l1,
    actuator,
    prerequisite,
    lucid: config.lucid,
    decisionDigest: config.decisionDigest,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
};

const appendEvent = async ({
  journal,
  workflowId,
  identity,
  event,
}: {
  readonly journal: FraudProofWorkflowJournalStore;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
  readonly event: FraudProofWorkflowJournalEvent;
}) => {
  const sequence = (await journal.load(workflowId)).length;
  const entry: FraudProofWorkflowJournalEntry = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
    workflowId,
    identity,
    sequence,
    recordedAt: new Date().toISOString(),
    event,
  };
  await journal.append(entry, sequence);
};

export const observersForbiddenActionId = (
  action: ObserversForbiddenActuatorAction,
) =>
  `observersForbidden:${Buffer.from(JSON.stringify(action)).toString("hex")}`;

const workflowAction = (
  action: ObserversForbiddenActuatorAction,
): FraudProofWorkflowAction => ({
  actionId: observersForbiddenActionId(action),
  input: {
    schemaVersion: "midgard-production-cursor-family-action-v1",
    category: CATEGORY,
    ...action,
  },
});

const currentAction = async ({
  workflow,
  artifact,
}: {
  readonly workflow: ManifestBoundObserversForbiddenWorkflow;
  readonly artifact: ObserversForbiddenArtifact;
}): Promise<ObserversForbiddenActuatorAction | "removed"> => {
  const stage = (await workflow.l1.observe({ headerHash: artifact.headerHash }))
    .stage;
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

export const reconcileObserversForbiddenSubmissionIntent = ({
  intendedActionId,
  txHash,
  transactionConfirmed,
  observedAction,
}: {
  readonly intendedActionId: string;
  readonly txHash: string;
  readonly transactionConfirmed: boolean;
  readonly observedAction: ObserversForbiddenActuatorAction | "removed";
}): Readonly<{ kind: "confirmed" | "pending" | "conflict"; txHash: string }> =>
  transactionConfirmed
    ? { kind: "confirmed", txHash }
    : observedAction === "removed" ||
        observersForbiddenActionId(observedAction) !== intendedActionId
      ? { kind: "conflict", txHash }
      : { kind: "pending", txHash };

export const observersForbiddenSubmissionPrelude = ({
  actionId,
  actionInput,
  txHash,
  referenceScripts,
  durableRecovery,
}: {
  readonly actionId: string;
  readonly actionInput: JournalJsonObject;
  readonly txHash: string;
  readonly referenceScripts: Extract<
    FraudProofWorkflowJournalEvent,
    { readonly kind: "preflight_passed" }
  >["referenceScripts"];
  readonly durableRecovery?: JournalJsonObject;
}) =>
  Object.freeze([
    Object.freeze({
      kind: "preflight_passed" as const,
      actionId,
      txHash,
      localEvaluator: "lucid-evolution-local-uplc-v1" as const,
      referenceScripts,
    }),
    Object.freeze({
      kind: "submission_intent" as const,
      actionId,
      actionInput,
      ...(durableRecovery === undefined ? {} : { durableRecovery }),
      attempt: 1,
      txHash,
    }),
  ]);

export type ObserversForbiddenWorkflowRunResult = Readonly<{
  kind: "pending" | "completed";
  workflowId: string;
  txHash?: string;
}>;

/** One locally evaluated, intent-journaled action per invocation. */
export const executeManifestBoundObserversForbiddenWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundObserversForbiddenWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<ObserversForbiddenWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: observation.headerHash,
    sources,
  });
  const raw = await observersForbiddenRawBlockEvidenceFromVerifiedPayload({
    observation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance: fetched.provenance,
  });
  const artifact =
    detectObserversForbiddenAcceptedRawReplay(raw).length > 0
      ? await prepareObserversForbiddenAcceptedArtifact(raw)
      : await prepareObserversForbiddenForcedArtifact(
          await fetchCanonicalBlockEvidence({ observation, sources }),
        );
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: CATEGORY as never,
    target: { kind: "state_queue_header", headerHash: artifact.headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  let entries = await journal.load(workflowId);
  if (entries.length === 0) {
    await appendEvent({
      journal,
      workflowId,
      identity,
      event: { kind: "started" },
    });
    await appendEvent({
      journal,
      workflowId,
      identity,
      event: {
        kind: "prepared",
        artifact,
        artifactDigest: observersForbiddenArtifactDigest(artifact),
      },
    });
    entries = await journal.load(workflowId);
  } else {
    const prepared = entries.find((entry) => entry.event.kind === "prepared");
    if (
      prepared?.event.kind !== "prepared" ||
      prepared.event.artifactDigest !==
        observersForbiddenArtifactDigest(artifact)
    )
      throw new Error("observersForbidden durable artifact substitution");
  }
  const lastIntent = [...entries]
    .reverse()
    .find((entry) => entry.event.kind === "submission_intent");
  if (lastIntent?.event.kind === "submission_intent") {
    const intent = lastIntent.event;
    const confirmed = entries.some(
      (entry) =>
        entry.event.kind === "confirmed" &&
        entry.event.actionId === intent.actionId,
    );
    if (!confirmed) {
      const onChain = await workflow.l1.transactionConfirmed({
        headerHash: artifact.headerHash,
        txHash: intent.txHash,
      });
      if (!onChain) {
        const reconciliation = reconcileObserversForbiddenSubmissionIntent({
          intendedActionId: intent.actionId,
          txHash: intent.txHash,
          transactionConfirmed: false,
          observedAction: await currentAction({ workflow, artifact }),
        });
        if (reconciliation.kind === "conflict")
          throw new Error(
            "observersForbidden transaction substitution changed durable cursor",
          );
        return { kind: "pending", workflowId, txHash: intent.txHash };
      }
      await appendEvent({
        journal,
        workflowId,
        identity,
        event: {
          kind: "reconciled",
          actionId: intent.actionId,
          outcome: "confirmed",
          txHash: intent.txHash,
        },
      });
      await appendEvent({
        journal,
        workflowId,
        identity,
        event: {
          kind: "confirmed",
          actionId: intent.actionId,
          txHash: intent.txHash,
        },
      });
    }
  }
  const action = await currentAction({ workflow, artifact });
  if (action === "removed") {
    const terminalStage = (
      await workflow.l1.observe({ headerHash: artifact.headerHash })
    ).stage;
    if (terminalStage.kind !== "removed")
      throw new Error("observersForbidden terminal observation changed");
    await appendEvent({
      journal,
      workflowId,
      identity,
      event: {
        kind: "completed",
        terminal: terminalStage.terminal,
        terminalDigest: journalJsonDigest(
          terminalStage.terminal as unknown as JournalJsonObject,
        ),
      },
    });
    return { kind: "completed", workflowId };
  }
  const baseAction = workflowAction(action);
  const prerequisite = await workflow.prerequisite.inspect({
    headerHash: artifact.headerHash,
    baseAction,
    artifact,
    entries: await journal.load(workflowId),
  });
  const captured =
    prerequisite.kind === "required"
      ? await workflow.prerequisite.capture({
          headerHash: artifact.headerHash,
          action: prerequisite.action,
          artifact,
        })
      : prerequisite.kind === "pending"
        ? null
        : await workflow.actuator.capture({ action, artifact });
  if (captured === null) return { kind: "pending", workflowId };
  const actionId =
    prerequisite.kind === "required"
      ? prerequisite.action.actionId
      : baseAction.actionId;
  const actionInput =
    prerequisite.kind === "required"
      ? prerequisite.action.input
      : baseAction.input;
  const durableRecovery =
    "mutationLease" in captured && captured.mutationLease !== undefined
      ? {
          stateQueueMutationLease: {
            token: captured.mutationLease.token,
            source: captured.mutationLease.source,
          },
        }
      : "durableRecovery" in captured
        ? captured.durableRecovery
        : undefined;
  for (const event of observersForbiddenSubmissionPrelude({
    actionId,
    actionInput,
    txHash: captured.transaction.txHash,
    referenceScripts: captured.transaction.referenceScripts,
    ...(durableRecovery === undefined ? {} : { durableRecovery }),
  }))
    await appendEvent({ journal, workflowId, identity, event });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("observersForbidden provider substituted transaction");
  await appendEvent({
    journal,
    workflowId,
    identity,
    event: { kind: "submitted", actionId, attempt: 1, txHash: submitted },
  });
  if (
    action.stage === "remove" &&
    !workflowTransactionInputOutRefs(captured.transaction.signed).includes(
      action.nextRemovalOutRef,
    )
  )
    throw new Error("observersForbidden removal changed mutation target");
  return { kind: "pending", workflowId, txHash: submitted };
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
        return (await executeManifestBoundObserversForbiddenWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        })) as never;
      } finally {
        await loaded.close();
      }
    },
  });
