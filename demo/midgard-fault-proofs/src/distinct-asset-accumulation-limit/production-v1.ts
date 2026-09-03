import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

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
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation-v1.js";
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
  DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES,
  type DistinctAssetAccumulationContracts,
} from "./contracts-v1.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
} from "./family-v1.js";
import {
  createDistinctAssetAccumulationActuator,
  type DistinctAssetAccumulationActuatorAction,
  type DistinctAssetAccumulationWorkflowReferences,
} from "./production-actuator-v1.js";
import { prepareDistinctAssetAccumulationArtifact } from "./production-replay-v1.js";
import { DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS } from "./schemas-v1.js";

export const DISTINCT_ASSET_ACCUMULATION_WORKFLOW =
  "midgard-distinct-asset-accumulation-production-workflow-v1" as const;

export type DistinctAssetAccumulationRemovalReferences = Readonly<{
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

export type DistinctAssetAccumulationReferences =
  DistinctAssetAccumulationWorkflowReferences &
    Readonly<{ removal: DistinctAssetAccumulationRemovalReferences }>;

export const DISTINCT_ASSET_ACCUMULATION_CONFIG_KEYS = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "source",
  "decisionDigest",
  "referenceScripts",
  "stateQueueMutationLeaseCoordinator",
] as const);

export type ManifestBoundDistinctAssetAccumulationWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  referenceScripts: DistinctAssetAccumulationReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundDistinctAssetAccumulationWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"distinctAssetAccumulationLimit">;
  lucid: LucidEvolution;
  decisionDigest: string;
  l1: FraudProofFamilyL1ObservationPort<"distinctAssetAccumulationLimit">;
  actuator: ReturnType<typeof createDistinctAssetAccumulationActuator>;
}>;

const manifestContracts = Object.freeze({
  steps: [
    "fraudProofDistinctAssetAccumulationLimit",
    "fraudProofDistinctAssetAccumulationLimitStep02",
    "fraudProofDistinctAssetAccumulationLimitStep03",
    "fraudProofDistinctAssetAccumulationLimitStep04",
    "fraudProofDistinctAssetAccumulationLimitStep05",
    "fraudProofDistinctAssetAccumulationLimitStep06",
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

/** Manifest/reference/signer-bound workflow construction with no evidence input. */
export const createManifestBoundDistinctAssetAccumulationWorkflow = async (
  config: ManifestBoundDistinctAssetAccumulationWorkflowConfig,
): Promise<ManifestBoundDistinctAssetAccumulationWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...DISTINCT_ASSET_ACCUMULATION_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "distinctAssetAccumulationLimit production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error(
      "distinctAssetAccumulationLimit decision digest is malformed",
    );
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain =
    binding.resolvedContracts.contracts.distinctAssetAccumulationLimit;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 6 ||
    stateQueuePolicyId === undefined
  )
    throw new Error(
      "distinctAssetAccumulationLimit manifest omitted required contracts",
    );
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const steps = manifestContracts.steps.map((name, index) =>
    bind(name, config.referenceScripts.steps[index]!),
  ) as unknown as DistinctAssetAccumulationWorkflowReferences["steps"];
  const witnesses = Object.fromEntries(
    Object.entries(manifestContracts.witnesses).map(([role, name]) => [
      role,
      bind(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScripts
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScripts>;
  for (const [role, name] of Object.entries(manifestContracts.removal))
    bind(
      name,
      config.referenceScripts.removal[
        role as keyof DistinctAssetAccumulationRemovalReferences
      ],
    );
  const contracts: DistinctAssetAccumulationContracts = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      blueprintTitle:
        DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
    })) as unknown as DistinctAssetAccumulationContracts["steps"],
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
  });
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
    actuator: createDistinctAssetAccumulationActuator({
      lucid: config.lucid,
      blueprint: binding.blueprint,
      deploymentInfo: binding.deploymentInfo,
      network: binding.network,
      signer: config.signer,
      categoryId: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
      contracts,
      references: { steps, witnesses },
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
      fraudProofSpendingScriptHash:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptHash,
      fraudProverRewardLovelace: BigInt(
        binding.releaseEconomics.policy.fraudProverRewardLovelace,
      ),
    }),
  });
};

const appendEvent = async (
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

const actionFor = async ({
  workflow,
  headerHash,
}: {
  workflow: ManifestBoundDistinctAssetAccumulationWorkflow;
  headerHash: string;
}): Promise<DistinctAssetAccumulationActuatorAction | "removed"> => {
  const stage = (await workflow.l1.observe({ headerHash })).stage;
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
      stage: "step01",
      threadOutRef: stage.threadOutRef,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
    };
  if (stage.step === 2)
    return { stage: "step02", threadOutRef: stage.threadOutRef };
  if (stage.step >= 3 && stage.step <= 5)
    return {
      stage: "fold",
      stepIndex: (stage.step - 1) as 2 | 3 | 4,
      threadOutRef: stage.threadOutRef,
    };
  if (stage.step === 6)
    return { stage: "step06", threadOutRef: stage.threadOutRef };
  throw new Error("distinctAssetAccumulationLimit observed impossible step");
};

/** One retained-DA-derived, locally evaluated, intent-journaled action. */
export const executeManifestBoundDistinctAssetAccumulationWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundDistinctAssetAccumulationWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const block = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const artifact = await prepareDistinctAssetAccumulationArtifact(block);
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  let entries = await journal.load(workflowId);
  if (entries.length === 0) {
    await appendEvent(journal, workflowId, identity, { kind: "started" });
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
    await appendEvent(journal, workflowId, identity, {
      kind: "confirmed",
      actionId: intent.actionId,
      txHash: intent.txHash,
    });
  }
  const action = await actionFor({ workflow, headerHash });
  if (action === "removed") return { kind: "completed" as const, workflowId };
  const actionId = `distinctAssetAccumulationLimit:${action.stage}:${"stepIndex" in action ? action.stepIndex.toString() : "0"}`;
  const captured = await workflow.actuator.capture({ action, artifact });
  await appendEvent(journal, workflowId, identity, {
    kind: "preflight_passed",
    actionId,
    txHash: captured.transaction.txHash,
    localEvaluator: "lucid-evolution-local-uplc-v1",
    referenceScripts: captured.transaction.referenceScripts,
  });
  await appendEvent(journal, workflowId, identity, {
    kind: "submission_intent",
    actionId,
    actionInput: {
      schemaVersion: "midgard-production-cursor-family-action-v1",
      category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
      stage: action.stage,
    },
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
    attempt: 1,
    txHash: captured.transaction.txHash,
  });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error(
      "distinctAssetAccumulationLimit provider substituted transaction",
    );
  await appendEvent(journal, workflowId, identity, {
    kind: "submitted",
    actionId,
    attempt: 1,
    txHash: submitted,
  });
  return { kind: "pending" as const, workflowId, txHash: submitted };
};

export const runOrResumeManifestBoundDistinctAssetAccumulationWorkflow =
  async (input: {
    workflow: ManifestBoundDistinctAssetAccumulationWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
      throw new Error(
        "distinctAssetAccumulationLimit runner rejects caller-authored evidence",
      );
    return await executeManifestBoundDistinctAssetAccumulationWorkflow(input);
  };

export type LoadedDistinctAssetAccumulationWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundDistinctAssetAccumulationWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadDistinctAssetAccumulationWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedDistinctAssetAccumulationWorkflow>;

/** Standard strict loader-based surface consumed by ProductionWorkflowAdapter. */
export const createDistinctAssetAccumulationWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadDistinctAssetAccumulationWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY)
        throw new Error(
          "distinctAssetAccumulationLimit runner category changed",
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
          category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
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
            "distinctAssetAccumulationLimit requires concrete public retained DA",
          );
        const workflow =
          await createManifestBoundDistinctAssetAccumulationWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "distinctAssetAccumulationLimit runtime binding changed invocation",
          );
        return (await runOrResumeManifestBoundDistinctAssetAccumulationWorkflow(
          { workflow, sources: loaded.retainedDaSources, journal },
        )) as never;
      } finally {
        await loaded.close();
      }
    },
  });
