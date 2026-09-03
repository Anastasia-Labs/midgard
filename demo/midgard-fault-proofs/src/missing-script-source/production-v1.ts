import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding-v1.js";
import { createFraudProofFamilyLocalKupmiosL1ObservationPort } from "../workflow/family-l1-observation-v1.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
} from "../workflow/production-actuation-permit-v1.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/production-adapters-v1.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/production-funding-reservation-permit-v1.js";
import { submitCapturedTransaction } from "../workflow/transaction-boundary-v1.js";
import {
  MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES,
  type MissingScriptSourceContracts,
} from "./contracts-v1.js";
import {
  type BoundMissingScriptSourceActuatorConfig,
  createMissingScriptSourceActuator,
  type MissingScriptSourceActuatorAction,
  type MissingScriptSourceWorkflowReferences,
} from "./production-actuator-v1.js";
import { prepareMissingScriptSourceArtifact } from "./production-replay-v1.js";
import {
  ExecutionSourceStep02DatumSchema,
  ExecutionSourceStep03DatumSchema,
  ExecutionSourceStep04DatumSchema,
  ExecutionSourceStep05DatumSchema,
  ExecutionSourceStep06DatumSchema,
} from "./schemas-v1.js";

export const MISSING_SCRIPT_SOURCE_WORKFLOW =
  "midgard-missing-script-source-production-workflow-v1" as const;
export const MISSING_SCRIPT_SOURCE_CONFIG_KEYS = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "source",
  "decisionDigest",
  "stateQueueMutationLeaseCoordinator",
  "referenceScripts",
] as const);
export const MISSING_SCRIPT_SOURCE_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  ExecutionSourceStep02DatumSchema,
  ExecutionSourceStep03DatumSchema,
  ExecutionSourceStep04DatumSchema,
  ExecutionSourceStep05DatumSchema,
  ExecutionSourceStep06DatumSchema,
] as const);

export type MissingScriptSourceRemovalReferences = Readonly<{
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

export type ManifestBoundMissingScriptSourceWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: MissingScriptSourceWorkflowReferences &
    Readonly<{
      removal: MissingScriptSourceRemovalReferences;
    }>;
}>;

export type ManifestBoundMissingScriptSourceWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<never> &
    BoundMissingScriptSourceActuatorConfig["binding"];
  actuator: ReturnType<typeof createMissingScriptSourceActuator>;
  lucid: LucidEvolution;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPort>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

/**
 * Manifest-bound family construction. Config contains infrastructure and
 * authenticated references only: no evidence, stage, submit, or journal
 * callbacks are accepted.
 */
export const createManifestBoundMissingScriptSourceWorkflow = async (
  config: ManifestBoundMissingScriptSourceWorkflowConfig,
): Promise<ManifestBoundMissingScriptSourceWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...MISSING_SCRIPT_SOURCE_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "missingScriptSource production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("missingScriptSource decision digest is malformed");
  const rawBinding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "missingScriptSource" as never,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: MISSING_SCRIPT_SOURCE_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
    network: rawBinding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const binding =
    rawBinding as unknown as FraudProofWorkflowDeploymentBinding<never> &
      BoundMissingScriptSourceActuatorConfig["binding"] & {
        resolvedContracts: {
          contracts: {
            computationThread: MissingScriptSourceContracts["computationThread"];
            fraudProof: MissingScriptSourceContracts["fraudProof"] & {
              spendingScriptHash: string;
            };
            missingScriptSource?: {
              steps: MissingScriptSourceContracts["steps"];
            };
          };
        };
      };
  const chain = binding.resolvedContracts.contracts.missingScriptSource;
  const hubOraclePolicyId = rawBinding.deploymentInfo.hubOracleMint?.scriptHash;
  if (
    chain === undefined ||
    chain.steps.length !== 6 ||
    hubOraclePolicyId === undefined
  )
    throw new Error("missingScriptSource manifest omitted six-step chain");
  const bindReference = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding: rawBinding,
      contractName: name,
      utxo,
    });
  const stepNames = [
    "fraudProofMissingScriptSource",
    "fraudProofMissingScriptSourceStep02",
    "fraudProofMissingScriptSourceStep03",
    "fraudProofMissingScriptSourceStep04",
    "fraudProofMissingScriptSourceStep05",
    "fraudProofMissingScriptSourceStep06",
  ] as const;
  const steps = stepNames.map((name, index) =>
    bindReference(name, config.referenceScripts.steps[index]!),
  ) as unknown as MissingScriptSourceWorkflowReferences["steps"];
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
  const contracts: MissingScriptSourceContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as MissingScriptSourceContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId,
  };
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  return Object.freeze({
    binding,
    lucid: config.lucid,
    source: config.source,
    decisionDigest: config.decisionDigest,
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    actuator: createMissingScriptSourceActuator({
      binding,
      lucid: config.lucid,
      signer: config.signer,
      contracts,
      references: { steps, witnesses },
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    }),
  });
};

const appendEvent = async ({
  journal,
  workflowId,
  identity,
  event,
}: {
  journal: FraudProofWorkflowJournalStore;
  workflowId: string;
  identity: FraudProofWorkflowIdentity;
  event: FraudProofWorkflowJournalEvent;
}) => {
  const sequence = (await journal.load(workflowId)).length;
  await journal.append(
    {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence,
      recordedAt: new Date().toISOString(),
      event,
    },
    sequence,
  );
};

const currentAction = async (
  workflow: ManifestBoundMissingScriptSourceWorkflow,
): Promise<MissingScriptSourceActuatorAction | "removed"> => {
  const stage = (
    await workflow.l1.observe({
      headerHash: workflow.binding.definition.headerHash,
    })
  ).stage;
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
  const actions = [
    "step_01",
    "step_02",
    "step_03",
    "scan",
    "prove",
    "finalize",
  ] as const;
  const action = actions[stage.step - 1];
  if (action === undefined)
    throw new Error("missingScriptSource observed impossible step");
  return action === "step_01"
    ? {
        stage: action,
        threadOutRef: stage.threadOutRef,
        stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      }
    : { stage: action, threadOutRef: stage.threadOutRef };
};

export type MissingScriptSourceWorkflowRunResult = Readonly<{
  kind: "pending" | "completed";
  workflowId: string;
  txHash?: string;
}>;

/** One package-owned locally evaluated, intent-journaled action per call. */
export const executeManifestBoundMissingScriptSourceWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundMissingScriptSourceWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<MissingScriptSourceWorkflowRunResult> => {
  const headerHash = workflow.binding.definition.headerHash;
  const evidence = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const artifact = await prepareMissingScriptSourceArtifact(evidence);
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: "missingScriptSource" as never,
    target: { kind: "state_queue_header", headerHash },
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
    entries = await journal.load(workflowId);
  }
  const pending = [...entries]
    .reverse()
    .find(({ event }) => event.kind === "submission_intent");
  const pendingIntent =
    pending?.event.kind === "submission_intent" ? pending.event : undefined;
  if (
    pendingIntent !== undefined &&
    !entries.some(
      ({ event }) =>
        event.kind === "confirmed" && event.actionId === pendingIntent.actionId,
    )
  ) {
    const intent = pendingIntent;
    if (
      !(await workflow.l1.transactionConfirmed({
        headerHash,
        txHash: intent.txHash,
      }))
    )
      return { kind: "pending", workflowId, txHash: intent.txHash };
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
  const action = await currentAction(workflow);
  if (action === "removed") return { kind: "completed", workflowId };
  const captured = await workflow.actuator.capture({ action, artifact });
  const actionId = `missingScriptSource:${action.stage}`;
  const actionInput = {
    schemaVersion: "midgard-production-cursor-family-action-v1" as const,
    category: "missingScriptSource",
    stage: action.stage,
  };
  await appendEvent({
    journal,
    workflowId,
    identity,
    event: {
      kind: "preflight_passed",
      actionId,
      txHash: captured.transaction.txHash,
      localEvaluator: "lucid-evolution-local-uplc-v1",
      referenceScripts: captured.transaction.referenceScripts,
    },
  });
  await appendEvent({
    journal,
    workflowId,
    identity,
    event: {
      kind: "submission_intent",
      actionId,
      actionInput,
      attempt: 1,
      txHash: captured.transaction.txHash,
    },
  });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("missingScriptSource provider substituted transaction");
  await appendEvent({
    journal,
    workflowId,
    identity,
    event: { kind: "submitted", actionId, attempt: 1, txHash: submitted },
  });
  return { kind: "pending", workflowId, txHash: submitted };
};

export const runOrResumeManifestBoundMissingScriptSourceWorkflow =
  async (input: {
    workflow: ManifestBoundMissingScriptSourceWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
      throw new Error(
        "missingScriptSource runner rejects caller-authored evidence",
      );
    return await executeManifestBoundMissingScriptSourceWorkflow(input);
  };

export type LoadedMissingScriptSourceWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundMissingScriptSourceWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadMissingScriptSourceWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedMissingScriptSourceWorkflow>;

export const createMissingScriptSourceWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadMissingScriptSourceWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "missingScriptSource")
        throw new Error("missingScriptSource runner category changed");
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "missingScriptSource" as never,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: "missingScriptSource" as never,
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      try {
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        )
          throw new Error(
            "missingScriptSource requires concrete public retained DA",
          );
        const workflow = await createManifestBoundMissingScriptSourceWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "missingScriptSource runtime binding changed invocation",
          );
        return (await runOrResumeManifestBoundMissingScriptSourceWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        })) as never;
      } finally {
        await loaded.close();
      }
    },
  });
