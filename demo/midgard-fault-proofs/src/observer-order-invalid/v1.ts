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
  createObserverOrderInvalidActuator,
  type ObserverOrderInvalidActuatorAction,
  observerOrderInvalidFieldRequirement,
  type ObserverOrderInvalidWorkflowReferenceScripts,
} from "./actuator.js";
import {
  admitObserverOrderInvalidArtifact,
  type ObserverOrderInvalidArtifact,
  observerOrderInvalidArtifactDigest,
} from "./artifact.js";
import {
  OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES,
  type ObserverOrderInvalidContracts,
} from "./contracts.js";
import {
  detectObserverOrderInvalidAcceptedRawReplay,
  observerOrderInvalidRawBlockEvidenceFromVerifiedPayload,
  prepareObserverOrderInvalidAcceptedArtifact,
  prepareObserverOrderInvalidForcedArtifact,
} from "./replay.js";
import {
  ObserverOrderInvalidStep02DatumSchema,
  ObserverOrderInvalidStep03DatumSchema,
  ObserverOrderInvalidStep04DatumSchema,
} from "./schemas.js";
import { hashObserverOrderWalkCheckpoint } from "./staged-plan.js";

export const OBSERVER_ORDER_INVALID_WORKFLOW =
  "midgard-observer-order-invalid-production-workflow-v1" as const;

export type ObserverOrderInvalidRemovalReferenceScripts = Readonly<{
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

export type ManifestBoundObserverOrderInvalidWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ObserverOrderInvalidWorkflowReferenceScripts &
    Readonly<{ removal: ObserverOrderInvalidRemovalReferenceScripts }>;
}>;

export type ManifestBoundObserverOrderInvalidWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"observerOrderInvalid">;
  l1: FraudProofFamilyL1ObservationPort<"observerOrderInvalid">;
  actuator: ReturnType<typeof createObserverOrderInvalidActuator>;
  prerequisite: FieldCarriagePrerequisitePort<"observerOrderInvalid">;
  lucid: LucidEvolution;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const CATEGORY = "observerOrderInvalid" as const;

export const createManifestBoundObserverOrderInvalidWorkflow = async (
  config: ManifestBoundObserverOrderInvalidWorkflowConfig,
): Promise<ManifestBoundObserverOrderInvalidWorkflow> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("observerOrderInvalid decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: CATEGORY,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      ObserverOrderInvalidStep02DatumSchema,
      ObserverOrderInvalidStep02DatumSchema,
      ObserverOrderInvalidStep03DatumSchema,
      ObserverOrderInvalidStep04DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts[CATEGORY];
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 4 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error("observerOrderInvalid manifest omitted required contracts");
  const bindReference = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const steps = OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES.map((_title, index) =>
    bindReference(
      [
        "fraudProofObserverOrderInvalid",
        "fraudProofObserverOrderInvalidStep02",
        "fraudProofObserverOrderInvalidStep03",
        "fraudProofObserverOrderInvalidStep04",
      ][index]!,
      config.referenceScripts.steps[index]!,
    ),
  ) as unknown as ObserverOrderInvalidWorkflowReferenceScripts["steps"];
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
    steps: Object.freeze(steps),
    witnesses: Object.freeze(witnesses),
    fieldPreimageCertificateMint: bindReference(
      "fieldPreimageCertificateMint",
      config.referenceScripts.fieldPreimageCertificateMint,
    ),
  });
  const contracts: ObserverOrderInvalidContracts = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      blueprintTitle: OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as ObserverOrderInvalidContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const actuator = createObserverOrderInvalidActuator({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const prerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
    category: CATEGORY,
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) =>
      observerOrderInvalidFieldRequirement({
        action: action.input as unknown as ObserverOrderInvalidActuatorAction,
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
    lucid: config.lucid,
    actuator,
    prerequisite,
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

export const observerOrderInvalidActionId = (
  action: ObserverOrderInvalidActuatorAction,
): string =>
  `observerOrderInvalid:${Buffer.from(JSON.stringify(action)).toString("hex")}`;

const workflowAction = (
  action: ObserverOrderInvalidActuatorAction,
): FraudProofWorkflowAction => ({
  actionId: observerOrderInvalidActionId(action),
  input: {
    schemaVersion: "midgard-production-cursor-family-action-v1",
    category: "observerOrderInvalid",
    ...action,
  },
});

const currentAction = async ({
  workflow,
  artifact,
}: {
  readonly workflow: ManifestBoundObserverOrderInvalidWorkflow;
  readonly artifact: ObserverOrderInvalidArtifact;
}): Promise<ObserverOrderInvalidActuatorAction | "removed"> => {
  const stage = (await workflow.l1.observe({ headerHash: artifact.headerHash }))
    .stage;
  const admitted = admitObserverOrderInvalidArtifact(artifact);
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
    return {
      stage: "step_02",
      threadOutRef: stage.threadOutRef,
      action: { kind: "authenticate" },
    };
  }
  if (stage.step === 3) {
    const [txHash, outputIndex] = stage.threadOutRef.split("#");
    const [utxo] = await workflow.lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (utxo?.datum == null)
      throw new Error("observerOrderInvalid step-03 datum disappeared");
    const state = Data.from(
      utxo.datum,
      ObserverOrderInvalidStep03DatumSchema as never,
    ) as { data: { checkpoint_hash: string } };
    const hash = state.data.checkpoint_hash;
    const ordinal = [
      admitted.staged.initialWalk,
      ...admitted.staged.walk,
    ].findIndex(
      (checkpoint) => hashObserverOrderWalkCheckpoint(checkpoint) === hash,
    );
    if (ordinal < 0 || ordinal >= admitted.staged.walk.length)
      throw new Error("observerOrderInvalid scan checkpoint substitution");
    return {
      stage: "step_03",
      threadOutRef: stage.threadOutRef,
      walkOrdinal: ordinal,
    };
  }
  if (stage.step === 4)
    return { stage: "step_04", threadOutRef: stage.threadOutRef };
  throw new Error("observerOrderInvalid observed an impossible step");
};

export type ObserverOrderInvalidWorkflowRunResult = Readonly<{
  kind: "pending" | "completed";
  workflowId: string;
  txHash?: string;
}>;

export const reconcileObserverOrderInvalidSubmissionIntent = ({
  intendedActionId,
  txHash,
  transactionConfirmed,
  observedAction,
}: {
  readonly intendedActionId: string;
  readonly txHash: string;
  readonly transactionConfirmed: boolean;
  readonly observedAction: ObserverOrderInvalidActuatorAction | "removed";
}):
  | Readonly<{ kind: "confirmed"; txHash: string }>
  | Readonly<{ kind: "pending"; txHash: string }>
  | Readonly<{ kind: "conflict"; txHash: string }> => {
  if (transactionConfirmed) return { kind: "confirmed", txHash };
  if (
    observedAction === "removed" ||
    observerOrderInvalidActionId(observedAction) !== intendedActionId
  )
    return { kind: "conflict", txHash };
  return { kind: "pending", txHash };
};

/** Deterministic durability prelude shared by prerequisite and proof actions. */
export const observerOrderInvalidSubmissionPrelude = ({
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
}): readonly [
  Extract<
    FraudProofWorkflowJournalEvent,
    { readonly kind: "preflight_passed" }
  >,
  Extract<
    FraudProofWorkflowJournalEvent,
    { readonly kind: "submission_intent" }
  >,
] =>
  Object.freeze([
    Object.freeze({
      kind: "preflight_passed",
      actionId,
      txHash,
      localEvaluator: "lucid-evolution-local-uplc-v1",
      referenceScripts,
    }),
    Object.freeze({
      kind: "submission_intent",
      actionId,
      actionInput,
      ...(durableRecovery === undefined ? {} : { durableRecovery }),
      attempt: 1,
      txHash,
    }),
  ]);

/** One crash-safe action per call; callers resume by invoking the same runner. */
export const executeManifestBoundObserverOrderInvalidWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundObserverOrderInvalidWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<ObserverOrderInvalidWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: observation.headerHash,
    sources,
  });
  const raw = await observerOrderInvalidRawBlockEvidenceFromVerifiedPayload({
    observation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance: fetched.provenance,
  });
  const artifact =
    detectObserverOrderInvalidAcceptedRawReplay(raw).length > 0
      ? await prepareObserverOrderInvalidAcceptedArtifact(raw)
      : await prepareObserverOrderInvalidForcedArtifact(
          await fetchCanonicalBlockEvidence({ observation, sources }),
        );
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: CATEGORY,
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
        artifactDigest: observerOrderInvalidArtifactDigest(artifact),
      },
    });
    entries = await journal.load(workflowId);
  } else {
    const prepared = entries.find((entry) => entry.event.kind === "prepared");
    if (
      prepared?.event.kind !== "prepared" ||
      prepared.event.artifactDigest !==
        observerOrderInvalidArtifactDigest(artifact)
    )
      throw new Error("observerOrderInvalid durable artifact substitution");
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
      const recovery = intent.durableRecovery?.stateQueueMutationLease;
      const resumed =
        typeof recovery === "object" &&
        recovery !== null &&
        !Array.isArray(recovery) &&
        workflow.stateQueueMutationLeaseCoordinator.resume !== undefined
          ? await workflow.stateQueueMutationLeaseCoordinator.resume(
              recovery as { token: string; source: string },
            )
          : undefined;
      if (!onChain) {
        const observedAction = await currentAction({ workflow, artifact });
        const reconciliation = reconcileObserverOrderInvalidSubmissionIntent({
          intendedActionId: intent.actionId,
          txHash: intent.txHash,
          transactionConfirmed: false,
          observedAction,
        });
        if (reconciliation.kind === "conflict") {
          await resumed?.fail(
            "observerOrderInvalid observed a different transaction at the durable cursor",
          );
          throw new Error(
            "observerOrderInvalid transaction substitution changed the durable cursor",
          );
        }
        await resumed?.renew();
        return { kind: "pending", workflowId, txHash: intent.txHash };
      }
      await resumed?.release();
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
      throw new Error("observerOrderInvalid terminal observation changed");
    const complete = (await journal.load(workflowId)).some(
      (entry) => entry.event.kind === "completed",
    );
    if (!complete)
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
  if (prerequisite.kind === "pending") return { kind: "pending", workflowId };
  if (prerequisite.kind === "required") {
    const captured = await workflow.prerequisite.capture({
      headerHash: artifact.headerHash,
      action: prerequisite.action,
      artifact,
    });
    const prelude = observerOrderInvalidSubmissionPrelude({
      actionId: prerequisite.action.actionId,
      actionInput: prerequisite.action.input,
      txHash: captured.transaction.txHash,
      referenceScripts: captured.transaction.referenceScripts,
      durableRecovery: captured.durableRecovery,
    });
    for (const event of prelude)
      await appendEvent({ journal, workflowId, identity, event });
    const submitted = await submitCapturedTransaction(captured.transaction);
    await appendEvent({
      journal,
      workflowId,
      identity,
      event: {
        kind: "submitted",
        actionId: prerequisite.action.actionId,
        attempt: 1,
        txHash: submitted,
      },
    });
    return { kind: "pending", workflowId, txHash: submitted };
  }
  const id = baseAction.actionId;
  const captured = await workflow.actuator.capture({ action, artifact });
  const prelude = observerOrderInvalidSubmissionPrelude({
    actionId: id,
    actionInput: baseAction.input,
    txHash: captured.transaction.txHash,
    referenceScripts: captured.transaction.referenceScripts,
    ...(captured.mutationLease === undefined
      ? {}
      : {
          durableRecovery: {
            stateQueueMutationLease: {
              token: captured.mutationLease.token,
              source: captured.mutationLease.source,
            },
          },
        }),
  });
  for (const event of prelude)
    await appendEvent({ journal, workflowId, identity, event });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("observerOrderInvalid provider substituted transaction");
  await appendEvent({
    journal,
    workflowId,
    identity,
    event: { kind: "submitted", actionId: id, attempt: 1, txHash: submitted },
  });
  if (
    action.stage === "remove" &&
    !workflowTransactionInputOutRefs(captured.transaction.signed).includes(
      action.nextRemovalOutRef,
    )
  )
    throw new Error("observerOrderInvalid removal changed mutation target");
  return { kind: "pending", workflowId, txHash: submitted };
};

export type LoadedObserverOrderInvalidWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundObserverOrderInvalidWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadObserverOrderInvalidWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedObserverOrderInvalidWorkflow>;

/** Standard runtime-loader-compatible package runner; core config has no callbacks. */
export const createObserverOrderInvalidWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadObserverOrderInvalidWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== (CATEGORY as string))
        throw new Error("observerOrderInvalid runner category changed");
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: CATEGORY,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: CATEGORY,
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
            "observerOrderInvalid runtime requires concrete public retained DA",
          );
        const workflow = await createManifestBoundObserverOrderInvalidWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "observerOrderInvalid runtime binding changed invocation",
          );
        return (await executeManifestBoundObserverOrderInvalidWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        })) as never;
      } finally {
        await loaded.close();
      }
    },
  });
