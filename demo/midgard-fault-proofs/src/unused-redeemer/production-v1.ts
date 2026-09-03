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
import { bindWorkflowActuationJournal } from "../workflow/production-actuation-permit-v1.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/production-adapters-v1.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/production-funding-reservation-permit-v1.js";
import { submitCapturedTransaction } from "../workflow/transaction-boundary-v1.js";
import {
  UNUSED_REDEEMER_BLUEPRINT_TITLES,
  type UnusedRedeemerContracts,
} from "./contracts-v1.js";
import {
  createUnusedRedeemerActuator,
  type UnusedRedeemerWorkflowReferences as ActuatorReferences,
} from "./production-actuator-v1.js";
import { prepareUnusedRedeemerArtifact } from "./production-replay-v1.js";
import {
  UnusedRedeemerStep02aDatumSchema,
  UnusedRedeemerStep02bDatumSchema,
  UnusedRedeemerStep02cDatumSchema,
  UnusedRedeemerStep02DatumSchema,
  UnusedRedeemerStep03DatumSchema,
  UnusedRedeemerStep04DatumSchema,
  UnusedRedeemerStep05DatumSchema,
  UnusedRedeemerStep06DatumSchema,
} from "./schemas-v1.js";

export const UNUSED_REDEEMER_CONFIG_KEYS = Object.freeze([
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

export const UNUSED_REDEEMER_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  UnusedRedeemerStep02DatumSchema,
  UnusedRedeemerStep02aDatumSchema,
  UnusedRedeemerStep02bDatumSchema,
  UnusedRedeemerStep02cDatumSchema,
  UnusedRedeemerStep03DatumSchema,
  UnusedRedeemerStep04DatumSchema,
  UnusedRedeemerStep05DatumSchema,
  UnusedRedeemerStep06DatumSchema,
] as const);

export type UnusedRedeemerRemovalReferenceScripts = Readonly<{
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

export type UnusedRedeemerWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  removal: UnusedRedeemerRemovalReferenceScripts;
}>;

export type ManifestBoundUnusedRedeemerWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: UnusedRedeemerWorkflowReferenceScripts;
}>;

export type ManifestBoundUnusedRedeemerWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"unusedRedeemer">;
  lucid: LucidEvolution;
  decisionDigest: string;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPort>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  actuator: ReturnType<typeof createUnusedRedeemerActuator>;
}>;

const contracts = Object.freeze({
  steps: [
    "fraudProofUnusedRedeemer",
    "fraudProofUnusedRedeemerStep02",
    "fraudProofUnusedRedeemerStep02a",
    "fraudProofUnusedRedeemerStep02b",
    "fraudProofUnusedRedeemerStep02c",
    "fraudProofUnusedRedeemerStep03",
    "fraudProofUnusedRedeemerStep04",
    "fraudProofUnusedRedeemerStep05",
    "fraudProofUnusedRedeemerStep06",
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
export const createManifestBoundUnusedRedeemerWorkflow = async (
  config: ManifestBoundUnusedRedeemerWorkflowConfig,
): Promise<ManifestBoundUnusedRedeemerWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...UNUSED_REDEEMER_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "unusedRedeemer production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("unusedRedeemer decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "unusedRedeemer",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: UNUSED_REDEEMER_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.unusedRedeemer;
  if (chain === undefined || chain.steps.length !== 9)
    throw new Error("unusedRedeemer manifest omitted nine-step chain");
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
        role as keyof UnusedRedeemerRemovalReferenceScripts
      ],
    ),
  );
  const hubOraclePolicyId = binding.deploymentInfo.hubOracleMint?.scriptHash;
  if (hubOraclePolicyId === undefined)
    throw new Error("unusedRedeemer manifest omitted hub oracle");
  const stateQueuePolicyId = binding.deploymentInfo.stateQueueMint?.scriptHash;
  if (stateQueuePolicyId === undefined)
    throw new Error("unusedRedeemer manifest omitted state queue");
  const familyContracts: UnusedRedeemerContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: UNUSED_REDEEMER_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as UnusedRedeemerContracts["steps"],
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
    lucid: config.lucid,
    decisionDigest: config.decisionDigest,
    l1,
    actuator: createUnusedRedeemerActuator({
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

export type LoadedUnusedRedeemerWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundUnusedRedeemerWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadUnusedRedeemerWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedUnusedRedeemerWorkflow>;

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

const actionFor = async (workflow: ManifestBoundUnusedRedeemerWorkflow) => {
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
    [
      "step_01",
      "step_02",
      "step_02a",
      "step_02b",
      "step_02c",
      "step_03",
      "step_04",
      "step_05",
      "step_06",
    ] as const
  )[stage.step - 1];
  if (selected === undefined)
    throw new Error("unusedRedeemer observed impossible step");
  return selected === "step_01"
    ? {
        stage: selected,
        threadOutRef: stage.threadOutRef,
        stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      }
    : { stage: selected, threadOutRef: stage.threadOutRef };
};

export const executeManifestBoundUnusedRedeemerWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundUnusedRedeemerWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const block = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const artifact = await prepareUnusedRedeemerArtifact(block);
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: "unusedRedeemer",
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
  const actionId = `unusedRedeemer:${action.stage}`;
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
      category: "unusedRedeemer",
      stage: action.stage,
    },
    attempt: 1,
    txHash: captured.transaction.txHash,
  });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("unusedRedeemer provider substituted transaction");
  await append(journal, workflowId, identity, {
    kind: "submitted",
    actionId,
    attempt: 1,
    txHash: submitted,
  });
  return { kind: "pending" as const, workflowId, txHash: submitted };
};

/** Central-loader-compatible, callback-free production surface. */
export const createUnusedRedeemerWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadUnusedRedeemerWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "unusedRedeemer")
        throw new Error("unusedRedeemer runner category changed");
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
            "unusedRedeemer requires concrete public retained DA",
          );
        const workflow = await createManifestBoundUnusedRedeemerWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error("unusedRedeemer runtime binding changed invocation");
        const journal = bindWorkflowFundingReservationJournal({
          permit: invocation.fundingReservationPermit,
          journal: bindWorkflowActuationJournal({
            journal: new DirectoryFraudProofWorkflowJournalStore(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category: "unusedRedeemer",
            headerHash: invocation.headerHash,
          }),
        });
        return await executeManifestBoundUnusedRedeemerWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
