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
  SCRIPT_INTEGRITY_HASH_MISMATCH_BLUEPRINT_TITLES,
  type ScriptIntegrityHashMismatchContracts,
} from "./contracts-v1.js";
import {
  createScriptIntegrityHashMismatchLucidActuator,
  type ScriptIntegrityHashMismatchLucidAction,
  type ScriptIntegrityHashMismatchWorkflowReferences,
} from "./lucid-actuator-v1.js";
import { prepareScriptIntegrityHashMismatchArtifact } from "./production-replay-v1.js";
import {
  IntegrityStep02DatumSchema,
  IntegrityStep03DatumSchema,
  IntegrityStep04DatumSchema,
  IntegrityStep05DatumSchema,
} from "./schemas-v1.js";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_CONFIG_KEYS = Object.freeze([
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

export const SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  IntegrityStep02DatumSchema,
  IntegrityStep03DatumSchema,
  IntegrityStep04DatumSchema,
  IntegrityStep05DatumSchema,
] as const);

export type ScriptIntegrityHashMismatchRemovalReferenceScripts = Readonly<{
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

export type ScriptIntegrityHashMismatchReferenceScripts =
  ScriptIntegrityHashMismatchWorkflowReferences &
    Readonly<{
      removal: ScriptIntegrityHashMismatchRemovalReferenceScripts;
    }>;

export type ManifestBoundScriptIntegrityHashMismatchWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ScriptIntegrityHashMismatchReferenceScripts;
}>;

export type ManifestBoundScriptIntegrityHashMismatchWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"scriptIntegrityHashMismatch">;
  decisionDigest: string;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPort>;
  actuator: ReturnType<typeof createScriptIntegrityHashMismatchLucidActuator>;
}>;

const contractRoles = Object.freeze({
  steps: [
    "fraudProofScriptIntegrityHashMismatch",
    "fraudProofScriptIntegrityHashMismatchStep02",
    "fraudProofScriptIntegrityHashMismatchStep03",
    "fraudProofScriptIntegrityHashMismatchStep04",
    "fraudProofScriptIntegrityHashMismatchStep05",
  ],
  witnesses: {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  },
  removal: {
    correctionLockSpend: "correctionLockSpend",
    stateQueueSpend: "stateQueueSpend",
    stateQueueMint: "stateQueueMint",
    stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
    activeOperatorsSpend: "activeOperatorsSpend",
    activeOperatorsMint: "activeOperatorsMint",
    retiredOperatorsSpend: "retiredOperatorsSpend",
    retiredOperatorsMint: "retiredOperatorsMint",
    schedulerSpend: "schedulerSpend",
  },
} as const);

export const createManifestBoundScriptIntegrityHashMismatchWorkflow = async (
  config: ManifestBoundScriptIntegrityHashMismatchWorkflowConfig,
): Promise<ManifestBoundScriptIntegrityHashMismatchWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...SCRIPT_INTEGRITY_HASH_MISMATCH_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "scriptIntegrityHashMismatch production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("scriptIntegrityHashMismatch decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "scriptIntegrityHashMismatch",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.scriptIntegrityHashMismatch;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const hubOraclePolicyId = binding.deploymentInfo.hubOracleMint?.scriptHash;
  if (
    chain === undefined ||
    chain.steps.length !== 5 ||
    stateQueuePolicyId === undefined ||
    hubOraclePolicyId === undefined
  )
    throw new Error(
      "scriptIntegrityHashMismatch manifest omitted its exact five-step dependencies",
    );
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const steps = contractRoles.steps.map((name, index) =>
    bind(name, config.referenceScripts.steps[index]!),
  ) as unknown as ScriptIntegrityHashMismatchWorkflowReferences["steps"];
  const witnesses = Object.fromEntries(
    Object.entries(contractRoles.witnesses).map(([role, name]) => [
      role,
      bind(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScripts
        ],
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScripts>;
  Object.entries(contractRoles.removal).forEach(([role, name]) =>
    bind(
      name,
      config.referenceScripts.removal[
        role as keyof ScriptIntegrityHashMismatchRemovalReferenceScripts
      ],
    ),
  );
  const contracts: ScriptIntegrityHashMismatchContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: SCRIPT_INTEGRITY_HASH_MISMATCH_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as ScriptIntegrityHashMismatchContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId,
    stateQueuePolicyId,
  };
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  return Object.freeze({
    binding,
    decisionDigest: config.decisionDigest,
    l1,
    actuator: createScriptIntegrityHashMismatchLucidActuator({
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
  workflow: ManifestBoundScriptIntegrityHashMismatchWorkflow,
): Promise<ScriptIntegrityHashMismatchLucidAction | "removed"> => {
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
  const selected = (
    ["step_01", "step_02", "step_03", "step_04", "step_05"] as const
  )[stage.step - 1];
  if (selected === undefined)
    throw new Error("scriptIntegrityHashMismatch observed impossible step");
  return selected === "step_01"
    ? {
        stage: selected,
        threadOutRef: stage.threadOutRef,
        stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      }
    : { stage: selected, threadOutRef: stage.threadOutRef };
};

export const executeManifestBoundScriptIntegrityHashMismatchWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundScriptIntegrityHashMismatchWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const block = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const artifact = await prepareScriptIntegrityHashMismatchArtifact(block);
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: "scriptIntegrityHashMismatch",
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
      return { kind: "pending" as const, workflowId, txHash: intent.txHash };
    await append(journal, workflowId, identity, {
      kind: "confirmed",
      actionId: intent.actionId,
      txHash: intent.txHash,
    });
  }
  const action = await actionFor(workflow);
  if (action === "removed") return { kind: "completed" as const, workflowId };
  const captured = await workflow.actuator.capture({ action, artifact });
  const actionId = `scriptIntegrityHashMismatch:${action.stage}`;
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
      category: "scriptIntegrityHashMismatch",
      stage: action.stage,
    },
    attempt: 1,
    txHash: captured.transaction.txHash,
  });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error(
      "scriptIntegrityHashMismatch provider substituted transaction",
    );
  await append(journal, workflowId, identity, {
    kind: "submitted",
    actionId,
    attempt: 1,
    txHash: submitted,
  });
  return { kind: "pending" as const, workflowId, txHash: submitted };
};

export type LoadedScriptIntegrityHashMismatchWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundScriptIntegrityHashMismatchWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadScriptIntegrityHashMismatchWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedScriptIntegrityHashMismatchWorkflow>;

export const createScriptIntegrityHashMismatchWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadScriptIntegrityHashMismatchWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== "scriptIntegrityHashMismatch")
        throw new Error(
          `scriptIntegrityHashMismatch production runner category mismatch: ${invocation.category}`,
        );
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "scriptIntegrityHashMismatch",
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: "scriptIntegrityHashMismatch",
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      if (typeof loaded.close !== "function")
        throw new Error(
          "scriptIntegrityHashMismatch runtime omitted its disposer",
        );
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
            "scriptIntegrityHashMismatch requires concrete public retained DA",
          );
        const workflow =
          await createManifestBoundScriptIntegrityHashMismatchWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "scriptIntegrityHashMismatch runtime binding changed invocation",
          );
        return await executeManifestBoundScriptIntegrityHashMismatchWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
