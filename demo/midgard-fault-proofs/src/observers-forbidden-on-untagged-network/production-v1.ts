import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../workflow/family-l1-observation-v1.js";
import {
  computeFraudProofWorkflowIdV1,
  DirectoryFraudProofWorkflowJournalStoreV1,
  FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowIdentityV1,
  type FraudProofWorkflowJournalEntryV1,
  type FraudProofWorkflowJournalEventV1,
  type FraudProofWorkflowJournalStoreV1,
  journalJsonDigestV1,
  type JournalJsonObjectV1,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import type { FraudProofWorkflowActionV1 } from "../workflow/orchestrator-v1.js";
import {
  assertProductionWorkflowJournalActuationV1,
  bindProductionWorkflowActuationJournalV1,
} from "../workflow/production-actuation-permit-v1.js";
import {
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerV1,
} from "../workflow/production-adapters-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriagePrerequisitePortV1,
} from "../workflow/production-field-carriage-prerequisite-v1.js";
import { bindProductionWorkflowFundingReservationJournalV1 } from "../workflow/production-funding-reservation-permit-v1.js";
import {
  submitCapturedTransactionV1,
  workflowTransactionInputOutRefsV1,
} from "../workflow/transaction-boundary-v1.js";
import {
  OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES_V1,
  type ObserversForbiddenContractsV1,
} from "./contracts-v1.js";
import { OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_V1 } from "./family-v1.js";
import {
  createObserversForbiddenActuatorV1,
  type ObserversForbiddenActuatorActionV1,
  observersForbiddenFieldRequirementV1,
  type ObserversForbiddenWorkflowReferenceScriptsV1,
} from "./production-actuator-v1.js";
import {
  productionObserversForbiddenArtifactDigestV1,
  type ProductionObserversForbiddenArtifactV1,
} from "./production-artifact-v1.js";
import {
  detectObserversForbiddenAcceptedRawReplayV1,
  observersForbiddenRawBlockEvidenceFromVerifiedPayloadV1,
  prepareProductionObserversForbiddenAcceptedArtifactV1,
  prepareProductionObserversForbiddenForcedArtifactV1,
} from "./replay-v1.js";
import { ObserversForbiddenStep02DatumV1Schema } from "./schemas-v1.js";

const CATEGORY = OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_V1;
export const OBSERVERS_FORBIDDEN_PRODUCTION_WORKFLOW_V1 =
  "midgard-observers-forbidden-on-untagged-network-production-workflow-v1" as const;

export type ObserversForbiddenRemovalReferenceScriptsV1 = Readonly<{
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

export type ManifestBoundObserversForbiddenWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ObserversForbiddenWorkflowReferenceScriptsV1 &
    Readonly<{ removal: ObserversForbiddenRemovalReferenceScriptsV1 }>;
}>;

type SerialBindingV1 =
  FraudProofWorkflowDeploymentBindingV1<"observersForbiddenOnUntaggedNetwork">;

export type ManifestBoundObserversForbiddenWorkflowV1 = Readonly<{
  binding: SerialBindingV1;
  l1: FraudProofFamilyL1ObservationPortV1<"observersForbiddenOnUntaggedNetwork">;
  actuator: ReturnType<typeof createObserversForbiddenActuatorV1>;
  prerequisite: ProductionFieldCarriagePrerequisitePortV1<"observersForbiddenOnUntaggedNetwork">;
  lucid: LucidEvolution;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

/** Config contains infrastructure and authenticated references only. */
export const createManifestBoundObserversForbiddenWorkflowV1 = async (
  config: ManifestBoundObserversForbiddenWorkflowConfigV1,
): Promise<ManifestBoundObserversForbiddenWorkflowV1> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("observersForbidden decision digest is malformed");
  const binding = (await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: CATEGORY as never,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      ObserversForbiddenStep02DatumV1Schema,
      ObserversForbiddenStep02DatumV1Schema,
    ],
  })) as SerialBindingV1;
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const resolved = binding.resolvedContracts as unknown as {
    category: { categoryId: string };
    hubOraclePolicyId: string;
    stateQueuePolicyId?: string;
    contracts: Record<string, unknown> & {
      computationThread: ObserversForbiddenContractsV1["computationThread"];
      fraudProof: ObserversForbiddenContractsV1["fraudProof"] & {
        spendingScriptHash: string;
      };
      observersForbiddenOnUntaggedNetwork?: {
        steps: ObserversForbiddenContractsV1["steps"];
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
    requireManifestBoundReferenceScriptUtxoV1({
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
          role as keyof FaultProofWitnessReferenceScriptsV1
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScriptsV1>;
  const references = Object.freeze({
    steps,
    witnesses: Object.freeze(witnesses),
    fieldPreimageCertificateMint: bindReference(
      "fieldPreimageCertificateMint",
      config.referenceScripts.fieldPreimageCertificateMint,
    ),
  });
  const contracts: ObserversForbiddenContractsV1 = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      blueprintTitle: OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES_V1[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as ObserversForbiddenContractsV1["steps"],
    computationThread: resolved.contracts.computationThread,
    fraudProof: resolved.contracts.fraudProof,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const actuator = createObserversForbiddenActuatorV1({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const prerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: CATEGORY as never,
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) =>
      observersForbiddenFieldRequirementV1({
        action: action.input as unknown as ObserversForbiddenActuatorActionV1,
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
  readonly journal: FraudProofWorkflowJournalStoreV1;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentityV1;
  readonly event: FraudProofWorkflowJournalEventV1;
}) => {
  const sequence = (await journal.load(workflowId)).length;
  const entry: FraudProofWorkflowJournalEntryV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
    workflowId,
    identity,
    sequence,
    recordedAt: new Date().toISOString(),
    event,
  };
  await journal.append(entry, sequence);
};

export const observersForbiddenActionIdV1 = (
  action: ObserversForbiddenActuatorActionV1,
) =>
  `observersForbidden:${Buffer.from(JSON.stringify(action)).toString("hex")}`;

const workflowAction = (
  action: ObserversForbiddenActuatorActionV1,
): FraudProofWorkflowActionV1 => ({
  actionId: observersForbiddenActionIdV1(action),
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
  readonly workflow: ManifestBoundObserversForbiddenWorkflowV1;
  readonly artifact: ProductionObserversForbiddenArtifactV1;
}): Promise<ObserversForbiddenActuatorActionV1 | "removed"> => {
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
      ObserversForbiddenStep02DatumV1Schema as never,
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

export const reconcileObserversForbiddenSubmissionIntentV1 = ({
  intendedActionId,
  txHash,
  transactionConfirmed,
  observedAction,
}: {
  readonly intendedActionId: string;
  readonly txHash: string;
  readonly transactionConfirmed: boolean;
  readonly observedAction: ObserversForbiddenActuatorActionV1 | "removed";
}): Readonly<{ kind: "confirmed" | "pending" | "conflict"; txHash: string }> =>
  transactionConfirmed
    ? { kind: "confirmed", txHash }
    : observedAction === "removed" ||
        observersForbiddenActionIdV1(observedAction) !== intendedActionId
      ? { kind: "conflict", txHash }
      : { kind: "pending", txHash };

export const observersForbiddenSubmissionPreludeV1 = ({
  actionId,
  actionInput,
  txHash,
  referenceScripts,
  durableRecovery,
}: {
  readonly actionId: string;
  readonly actionInput: JournalJsonObjectV1;
  readonly txHash: string;
  readonly referenceScripts: Extract<
    FraudProofWorkflowJournalEventV1,
    { readonly kind: "preflight_passed" }
  >["referenceScripts"];
  readonly durableRecovery?: JournalJsonObjectV1;
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

export type ObserversForbiddenWorkflowRunResultV1 = Readonly<{
  kind: "pending" | "completed";
  workflowId: string;
  txHash?: string;
}>;

/** One locally evaluated, intent-journaled action per invocation. */
export const executeManifestBoundObserversForbiddenWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundObserversForbiddenWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<ObserversForbiddenWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: observation.headerHash,
    sources,
  });
  const raw = await observersForbiddenRawBlockEvidenceFromVerifiedPayloadV1({
    observation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance: fetched.provenance,
  });
  const artifact =
    detectObserversForbiddenAcceptedRawReplayV1(raw).length > 0
      ? await prepareProductionObserversForbiddenAcceptedArtifactV1(raw)
      : await prepareProductionObserversForbiddenForcedArtifactV1(
          await fetchCanonicalBlockEvidenceV1({ observation, sources }),
        );
  const identity: FraudProofWorkflowIdentityV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: CATEGORY as never,
    target: { kind: "state_queue_header", headerHash: artifact.headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowIdV1(identity);
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
        artifactDigest: productionObserversForbiddenArtifactDigestV1(artifact),
      },
    });
    entries = await journal.load(workflowId);
  } else {
    const prepared = entries.find((entry) => entry.event.kind === "prepared");
    if (
      prepared?.event.kind !== "prepared" ||
      prepared.event.artifactDigest !==
        productionObserversForbiddenArtifactDigestV1(artifact)
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
        const reconciliation = reconcileObserversForbiddenSubmissionIntentV1({
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
        terminalDigest: journalJsonDigestV1(
          terminalStage.terminal as unknown as JournalJsonObjectV1,
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
  for (const event of observersForbiddenSubmissionPreludeV1({
    actionId,
    actionInput,
    txHash: captured.transaction.txHash,
    referenceScripts: captured.transaction.referenceScripts,
    ...(durableRecovery === undefined ? {} : { durableRecovery }),
  }))
    await appendEvent({ journal, workflowId, identity, event });
  const submitted = await submitCapturedTransactionV1(captured.transaction);
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
    !workflowTransactionInputOutRefsV1(captured.transaction.signed).includes(
      action.nextRemovalOutRef,
    )
  )
    throw new Error("observersForbidden removal changed mutation target");
  return { kind: "pending", workflowId, txHash: submitted };
};

export type LoadedObserversForbiddenProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundObserversForbiddenWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadObserversForbiddenProductionWorkflowV1 = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedObserversForbiddenProductionWorkflowV1>;

/** Standard runtime loader surface; no evidence, stage, submit, or journal callbacks. */
export const createObserversForbiddenProductionWorkflowRunnerSurfaceV1 = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadObserversForbiddenProductionWorkflowV1;
}): ProductionWorkflowAdapterRunnerV1 =>
  Object.freeze({
    runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== CATEGORY)
        throw new Error("observersForbidden runner category changed");
      const journal = bindProductionWorkflowFundingReservationJournalV1({
        permit: invocation.fundingReservationPermit,
        journal: bindProductionWorkflowActuationJournalV1({
          journal: new DirectoryFraudProofWorkflowJournalStoreV1(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: CATEGORY as never,
          headerHash: invocation.headerHash,
        }),
      });
      assertProductionWorkflowJournalActuationV1({
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
        const workflow = await createManifestBoundObserversForbiddenWorkflowV1(
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
        return (await executeManifestBoundObserversForbiddenWorkflowV1({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        })) as never;
      } finally {
        await loaded.close();
      }
    },
  });
