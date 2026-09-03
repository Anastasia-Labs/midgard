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
import { bindWorkflowActuationJournal } from "../workflow/actuation-permit.js";
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
  createUnusedScriptWitnessActuator,
  type UnusedScriptWitnessWorkflowReferences as ActuatorReferences,
} from "./actuator.js";
import {
  UNUSED_SCRIPT_WITNESS_BLUEPRINT_TITLES,
  type UnusedScriptWitnessContracts,
} from "./contracts.js";
import { prepareUnusedScriptWitnessArtifact } from "./replay.js";
import {
  UnusedScriptStep02DatumSchema,
  UnusedScriptStep03DatumSchema,
  UnusedScriptStep04DatumSchema,
  UnusedScriptStep05DatumSchema,
  UnusedScriptStep06DatumSchema,
} from "./schemas.js";

export const UNUSED_SCRIPT_WITNESS_CONFIG_KEYS = Object.freeze([
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

export const UNUSED_SCRIPT_WITNESS_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  UnusedScriptStep02DatumSchema,
  UnusedScriptStep03DatumSchema,
  UnusedScriptStep04DatumSchema,
  UnusedScriptStep05DatumSchema,
  UnusedScriptStep06DatumSchema,
] as const);

export type UnusedScriptWitnessRemovalReferenceScripts = Readonly<{
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

export type UnusedScriptWitnessWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  removal: UnusedScriptWitnessRemovalReferenceScripts;
}>;

export type ManifestBoundUnusedScriptWitnessWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: UnusedScriptWitnessWorkflowReferenceScripts;
}>;

export type ManifestBoundUnusedScriptWitnessWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"unusedScriptWitness">;
  lucid: LucidEvolution;
  decisionDigest: string;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPort>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  actuator: ReturnType<typeof createUnusedScriptWitnessActuator>;
}>;

const contracts = Object.freeze({
  steps: [
    "fraudProofUnusedScriptWitness",
    "fraudProofUnusedScriptWitnessStep02",
    "fraudProofUnusedScriptWitnessStep03",
    "fraudProofUnusedScriptWitnessStep04",
    "fraudProofUnusedScriptWitnessStep05",
    "fraudProofUnusedScriptWitnessStep06",
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

/** Strict infrastructure-only manifest/reference binding. */
export const createManifestBoundUnusedScriptWitnessWorkflow = async (
  config: ManifestBoundUnusedScriptWitnessWorkflowConfig,
): Promise<ManifestBoundUnusedScriptWitnessWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...UNUSED_SCRIPT_WITNESS_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "unusedScriptWitness production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("unusedScriptWitness decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "unusedScriptWitness",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: UNUSED_SCRIPT_WITNESS_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.unusedScriptWitness;
  if (chain === undefined || chain.steps.length !== 6)
    throw new Error("unusedScriptWitness manifest omitted six-step chain");
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const steps = contracts.steps.map((name, index) =>
    bind(name, config.referenceScripts.steps[index]!),
  ) as unknown as ActuatorReferences["steps"];
  const witnesses = Object.fromEntries(
    Object.entries(contracts.witnesses).map(([role, name]) => [
      role,
      bind(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScripts
        ],
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScripts>;
  Object.entries(contracts.removal).forEach(([role, name]) =>
    bind(
      name,
      config.referenceScripts.removal[
        role as keyof UnusedScriptWitnessRemovalReferenceScripts
      ],
    ),
  );
  const hubOraclePolicyId = binding.deploymentInfo.hubOracleMint?.scriptHash;
  if (hubOraclePolicyId === undefined)
    throw new Error("unusedScriptWitness manifest omitted hub oracle");
  const familyContracts: UnusedScriptWitnessContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: UNUSED_SCRIPT_WITNESS_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as UnusedScriptWitnessContracts["steps"],
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
    actuator: createUnusedScriptWitnessActuator({
      binding,
      lucid: config.lucid,
      signer: config.signer,
      contracts: familyContracts,
      references: { steps, witnesses },
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    }),
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
};

export type LoadedUnusedScriptWitnessWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundUnusedScriptWitnessWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadUnusedScriptWitnessWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedUnusedScriptWitnessWorkflow>;

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
  workflow: ManifestBoundUnusedScriptWitnessWorkflow,
) => {
  const stage = (
    await workflow.l1.observe({
      headerHash: workflow.binding.definition.headerHash,
    })
  ).stage;
  if (stage.kind === "not_started")
    return {
      stage: "init" as const,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
    };
  if (stage.kind === "proof_token")
    return {
      stage: "remove" as const,
      nextRemovalOutRef: stage.nextRemovalOutRef,
      fraudProofOutRef: stage.fraudProofOutRef,
    };
  if (stage.kind === "removed") return "removed" as const;
  const selected = (
    ["step_01", "step_02", "step_03", "step_04", "step_05", "step_06"] as const
  )[stage.step - 1];
  if (selected === undefined)
    throw new Error("unusedScriptWitness observed impossible step");
  return selected === "step_01"
    ? {
        stage: selected,
        threadOutRef: stage.threadOutRef,
        stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      }
    : { stage: selected, threadOutRef: stage.threadOutRef };
};

export const executeManifestBoundUnusedScriptWitnessWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundUnusedScriptWitnessWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const block = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const artifact = await prepareUnusedScriptWitnessArtifact(block);
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: "unusedScriptWitness",
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
  const actionId = `unusedScriptWitness:${action.stage}`;
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
      category: "unusedScriptWitness",
      stage: action.stage,
    },
    attempt: 1,
    txHash: captured.transaction.txHash,
  });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("unusedScriptWitness provider substituted transaction");
  await append(journal, workflowId, identity, {
    kind: "submitted",
    actionId,
    attempt: 1,
    txHash: submitted,
  });
  return { kind: "pending" as const, workflowId, txHash: submitted };
};

/** Central-loader-compatible, callback-free production surface. */
export const createUnusedScriptWitnessWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadUnusedScriptWitnessWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "unusedScriptWitness")
        throw new Error("unusedScriptWitness runner category changed");
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
            "unusedScriptWitness requires concrete public retained DA",
          );
        const workflow = await createManifestBoundUnusedScriptWitnessWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "unusedScriptWitness runtime binding changed invocation",
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
            category: "unusedScriptWitness",
            headerHash: invocation.headerHash,
          }),
        });
        return await executeManifestBoundUnusedScriptWitnessWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
