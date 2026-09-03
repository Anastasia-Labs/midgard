import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
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
import { createFraudProofFamilyLocalKupmiosL1ObservationPort } from "../workflow/family-l1-observation.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { submitCapturedTransaction } from "../workflow/transaction-boundary.js";
import {
  type BoundReceivePurposeLanguageActuatorConfig,
  createReceivePurposeLanguageActuator,
  type ReceivePurposeLanguageActuatorAction,
  type ReceivePurposeLanguageWorkflowReferences,
} from "./actuator.js";
import { prepareReceivePurposeLanguageArtifact } from "./authenticated-replay.js";
import {
  RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES,
  type ReceivePurposeLanguageContracts,
} from "./contracts.js";
import {
  ReceivePurposeStep02DatumSchema,
  ReceivePurposeStep03DatumSchema,
} from "./schemas.js";

export const RECEIVE_PURPOSE_LANGUAGE_WORKFLOW =
  "midgard-receive-purpose-language-production-workflow-v1" as const;
export const RECEIVE_PURPOSE_LANGUAGE_CONFIG_KEYS = Object.freeze([
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
export const RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  ReceivePurposeStep02DatumSchema,
  ReceivePurposeStep03DatumSchema,
] as const);
export type ReceivePurposeLanguageRemovalReferences = Readonly<{
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
export type ManifestBoundReceivePurposeLanguageWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ReceivePurposeLanguageWorkflowReferences &
    Readonly<{ removal: ReceivePurposeLanguageRemovalReferences }>;
}>;
export type ManifestBoundReceivePurposeLanguageWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<never> &
    BoundReceivePurposeLanguageActuatorConfig["binding"];
  actuator: ReturnType<typeof createReceivePurposeLanguageActuator>;
  lucid: LucidEvolution;
  decisionDigest: string;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPort>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

/** Strict manifest/reference binding; its input admits no callback authority. */
export const createManifestBoundReceivePurposeLanguageWorkflow = async (
  config: ManifestBoundReceivePurposeLanguageWorkflowConfig,
): Promise<ManifestBoundReceivePurposeLanguageWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...RECEIVE_PURPOSE_LANGUAGE_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "receivePurposeLanguage production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("receivePurposeLanguage decision digest is malformed");
  const raw = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "receivePurposeLanguage" as never,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
    network: raw.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const binding = raw as unknown as FraudProofWorkflowDeploymentBinding<never> &
    BoundReceivePurposeLanguageActuatorConfig["binding"] & {
      resolvedContracts: {
        contracts: {
          computationThread: ReceivePurposeLanguageContracts["computationThread"];
          fraudProof: ReceivePurposeLanguageContracts["fraudProof"] & {
            spendingScriptHash: string;
          };
          receivePurposeLanguage?: {
            steps: ReceivePurposeLanguageContracts["steps"];
          };
        };
      };
    };
  const chain = binding.resolvedContracts.contracts.receivePurposeLanguage;
  const hubOraclePolicyId = raw.deploymentInfo.hubOracleMint?.scriptHash;
  if (
    chain === undefined ||
    chain.steps.length !== 3 ||
    hubOraclePolicyId === undefined
  )
    throw new Error("receivePurposeLanguage manifest omitted three-step chain");
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding: raw,
      contractName: name,
      utxo,
    });
  const names = [
    "fraudProofReceivePurposeLanguage",
    "fraudProofReceivePurposeLanguageStep02",
    "fraudProofReceivePurposeLanguageStep03",
  ] as const;
  const steps = names.map((name, index) =>
    bind(name, config.referenceScripts.steps[index]!),
  ) as unknown as ReceivePurposeLanguageWorkflowReferences["steps"];
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
      bind(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScripts
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScripts>;
  const contracts: ReceivePurposeLanguageContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as ReceivePurposeLanguageContracts["steps"],
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
    decisionDigest: config.decisionDigest,
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    actuator: createReceivePurposeLanguageActuator({
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
const append = async (
  journal: FraudProofWorkflowJournalStore,
  workflowId: string,
  identity: FraudProofWorkflowIdentity,
  event: FraudProofWorkflowJournalEvent,
) => {
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
const actionFor = async (
  workflow: ManifestBoundReceivePurposeLanguageWorkflow,
): Promise<ReceivePurposeLanguageActuatorAction | "removed"> => {
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
  const action = ["step_01", "step_02", "step_03"] as const;
  const selected = action[stage.step - 1];
  if (selected === undefined)
    throw new Error("receivePurposeLanguage observed impossible step");
  return selected === "step_01"
    ? {
        stage: selected,
        threadOutRef: stage.threadOutRef,
        stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      }
    : { stage: selected, threadOutRef: stage.threadOutRef };
};
export type ReceivePurposeLanguageWorkflowRunResult = Readonly<{
  kind: "pending" | "completed";
  workflowId: string;
  txHash?: string;
}>;
export const executeManifestBoundReceivePurposeLanguageWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundReceivePurposeLanguageWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<ReceivePurposeLanguageWorkflowRunResult> => {
  const headerHash = workflow.binding.definition.headerHash;
  const block = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const artifact = await prepareReceivePurposeLanguageArtifact(block);
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: "receivePurposeLanguage" as never,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  let entries = await journal.load(workflowId);
  if (entries.length === 0) {
    await append(journal, workflowId, identity, { kind: "started" });
    entries = await journal.load(workflowId);
  }
  const pending = [...entries]
    .reverse()
    .find(({ event }) => event.kind === "submission_intent");
  const intent =
    pending?.event.kind === "submission_intent" ? pending.event : undefined;
  if (
    intent !== undefined &&
    !entries.some(
      ({ event }) =>
        event.kind === "confirmed" && event.actionId === intent.actionId,
    )
  ) {
    if (
      !(await workflow.l1.transactionConfirmed({
        headerHash,
        txHash: intent.txHash,
      }))
    )
      return { kind: "pending", workflowId, txHash: intent.txHash };
    await append(journal, workflowId, identity, {
      kind: "confirmed",
      actionId: intent.actionId,
      txHash: intent.txHash,
    });
  }
  const action = await actionFor(workflow);
  if (action === "removed") return { kind: "completed", workflowId };
  const captured = await workflow.actuator.capture({ action, artifact });
  const actionId = `receivePurposeLanguage:${action.stage}`;
  await append(journal, workflowId, identity, {
    kind: "preflight_passed",
    actionId,
    txHash: captured.transaction.txHash,
    localEvaluator: "lucid-evolution-local-uplc-v1",
    referenceScripts: captured.transaction.referenceScripts,
  });
  await append(journal, workflowId, identity, {
    kind: "submission_intent",
    actionId,
    actionInput: {
      schemaVersion: "midgard-production-cursor-family-action-v1",
      category: "receivePurposeLanguage",
      stage: action.stage,
    },
    attempt: 1,
    txHash: captured.transaction.txHash,
  });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("receivePurposeLanguage provider substituted transaction");
  await append(journal, workflowId, identity, {
    kind: "submitted",
    actionId,
    attempt: 1,
    txHash: submitted,
  });
  return { kind: "pending", workflowId, txHash: submitted };
};
export const runOrResumeManifestBoundReceivePurposeLanguageWorkflow =
  async (input: {
    workflow: ManifestBoundReceivePurposeLanguageWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
      throw new Error(
        "receivePurposeLanguage runner rejects caller-authored evidence",
      );
    return await executeManifestBoundReceivePurposeLanguageWorkflow(input);
  };
export type LoadedReceivePurposeLanguageWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundReceivePurposeLanguageWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;
export type LoadReceivePurposeLanguageWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedReceivePurposeLanguageWorkflow>;
export const createReceivePurposeLanguageWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadReceivePurposeLanguageWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "receivePurposeLanguage")
        throw new Error("receivePurposeLanguage runner category changed");
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "receivePurposeLanguage" as never,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: "receivePurposeLanguage" as never,
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
            "receivePurposeLanguage requires concrete public retained DA",
          );
        const workflow =
          await createManifestBoundReceivePurposeLanguageWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "receivePurposeLanguage runtime binding changed invocation",
          );
        return await runOrResumeManifestBoundReceivePurposeLanguageWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
