import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import { createFraudProofFamilyLocalKupmiosL1ObservationPortV1 } from "../workflow/family-l1-observation-v1.js";
import {
  computeFraudProofWorkflowIdV1,
  DirectoryFraudProofWorkflowJournalStoreV1,
  FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowIdentityV1,
  type FraudProofWorkflowJournalEventV1,
  type FraudProofWorkflowJournalStoreV1,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  assertProductionWorkflowJournalActuationV1,
  bindProductionWorkflowActuationJournalV1,
} from "../workflow/production-actuation-permit-v1.js";
import {
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerV1,
} from "../workflow/production-adapters-v1.js";
import { bindProductionWorkflowFundingReservationJournalV1 } from "../workflow/production-funding-reservation-permit-v1.js";
import { submitCapturedTransactionV1 } from "../workflow/transaction-boundary-v1.js";
import {
  MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES_V1,
  type MissingScriptSourceContractsV1,
} from "./contracts-v1.js";
import {
  type BoundMissingScriptSourceActuatorConfigV1,
  createMissingScriptSourceActuatorV1,
  type MissingScriptSourceActuatorActionV1,
  type MissingScriptSourceWorkflowReferencesV1,
} from "./production-actuator-v1.js";
import { prepareProductionMissingScriptSourceArtifactV1 } from "./production-replay-v1.js";
import {
  ExecutionSourceStep02DatumV1Schema,
  ExecutionSourceStep03DatumV1Schema,
  ExecutionSourceStep04DatumV1Schema,
  ExecutionSourceStep05DatumV1Schema,
  ExecutionSourceStep06DatumV1Schema,
} from "./schemas-v1.js";

export const MISSING_SCRIPT_SOURCE_PRODUCTION_WORKFLOW_V1 =
  "midgard-missing-script-source-production-workflow-v1" as const;
export const MISSING_SCRIPT_SOURCE_PRODUCTION_CONFIG_KEYS_V1 = Object.freeze([
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
export const MISSING_SCRIPT_SOURCE_STEP_DATUM_SCHEMAS_V1 = Object.freeze([
  FraudProofComputationThreadStepDatum,
  ExecutionSourceStep02DatumV1Schema,
  ExecutionSourceStep03DatumV1Schema,
  ExecutionSourceStep04DatumV1Schema,
  ExecutionSourceStep05DatumV1Schema,
  ExecutionSourceStep06DatumV1Schema,
] as const);

export type MissingScriptSourceRemovalReferencesV1 = Readonly<{
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

export type ManifestBoundMissingScriptSourceWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: MissingScriptSourceWorkflowReferencesV1 &
    Readonly<{
      removal: MissingScriptSourceRemovalReferencesV1;
    }>;
}>;

export type ManifestBoundMissingScriptSourceWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<never> &
    BoundMissingScriptSourceActuatorConfigV1["binding"];
  actuator: ReturnType<typeof createMissingScriptSourceActuatorV1>;
  lucid: LucidEvolution;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  decisionDigest: string;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPortV1>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

/**
 * Manifest-bound family construction. Config contains infrastructure and
 * authenticated references only: no evidence, stage, submit, or journal
 * callbacks are accepted.
 */
export const createManifestBoundMissingScriptSourceWorkflowV1 = async (
  config: ManifestBoundMissingScriptSourceWorkflowConfigV1,
): Promise<ManifestBoundMissingScriptSourceWorkflowV1> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...MISSING_SCRIPT_SOURCE_PRODUCTION_CONFIG_KEYS_V1].sort().join("\0")
  )
    throw new Error(
      "missingScriptSource production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("missingScriptSource decision digest is malformed");
  const rawBinding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "missingScriptSource" as never,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: MISSING_SCRIPT_SOURCE_STEP_DATUM_SCHEMAS_V1,
  });
  assertManifestBoundWorkflowSignerV1({
    network: rawBinding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const binding =
    rawBinding as unknown as FraudProofWorkflowDeploymentBindingV1<never> &
      BoundMissingScriptSourceActuatorConfigV1["binding"] & {
        resolvedContracts: {
          contracts: {
            computationThread: MissingScriptSourceContractsV1["computationThread"];
            fraudProof: MissingScriptSourceContractsV1["fraudProof"] & {
              spendingScriptHash: string;
            };
            missingScriptSource?: {
              steps: MissingScriptSourceContractsV1["steps"];
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
    requireManifestBoundReferenceScriptUtxoV1({
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
  ) as unknown as MissingScriptSourceWorkflowReferencesV1["steps"];
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
  const contracts: MissingScriptSourceContractsV1 = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES_V1[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as MissingScriptSourceContractsV1["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId,
  };
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
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
    actuator: createMissingScriptSourceActuatorV1({
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
  journal: FraudProofWorkflowJournalStoreV1;
  workflowId: string;
  identity: FraudProofWorkflowIdentityV1;
  event: FraudProofWorkflowJournalEventV1;
}) => {
  const sequence = (await journal.load(workflowId)).length;
  await journal.append(
    {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
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
  workflow: ManifestBoundMissingScriptSourceWorkflowV1,
): Promise<MissingScriptSourceActuatorActionV1 | "removed"> => {
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

export type MissingScriptSourceWorkflowRunResultV1 = Readonly<{
  kind: "pending" | "completed";
  workflowId: string;
  txHash?: string;
}>;

/** One package-owned locally evaluated, intent-journaled action per call. */
export const executeManifestBoundMissingScriptSourceWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundMissingScriptSourceWorkflowV1;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStoreV1;
}): Promise<MissingScriptSourceWorkflowRunResultV1> => {
  const headerHash = workflow.binding.definition.headerHash;
  const evidence = await fetchCanonicalBlockEvidenceV1({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const artifact =
    await prepareProductionMissingScriptSourceArtifactV1(evidence);
  const identity: FraudProofWorkflowIdentityV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: "missingScriptSource" as never,
    target: { kind: "state_queue_header", headerHash },
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
  const submitted = await submitCapturedTransactionV1(captured.transaction);
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

export const runOrResumeManifestBoundMissingScriptSourceWorkflowV1 =
  async (input: {
    workflow: ManifestBoundMissingScriptSourceWorkflowV1;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStoreV1;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
      throw new Error(
        "missingScriptSource runner rejects caller-authored evidence",
      );
    return await executeManifestBoundMissingScriptSourceWorkflowV1(input);
  };

export type LoadedMissingScriptSourceProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundMissingScriptSourceWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadMissingScriptSourceProductionWorkflowV1 = (input: {
  runtimeConfigPath: string;
  invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedMissingScriptSourceProductionWorkflowV1>;

export const createMissingScriptSourceProductionWorkflowRunnerSurfaceV1 = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadMissingScriptSourceProductionWorkflowV1;
}): ProductionWorkflowAdapterRunnerV1 =>
  Object.freeze({
    runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "missingScriptSource")
        throw new Error("missingScriptSource runner category changed");
      const journal = bindProductionWorkflowFundingReservationJournalV1({
        permit: invocation.fundingReservationPermit,
        journal: bindProductionWorkflowActuationJournalV1({
          journal: new DirectoryFraudProofWorkflowJournalStoreV1(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "missingScriptSource" as never,
          headerHash: invocation.headerHash,
        }),
      });
      assertProductionWorkflowJournalActuationV1({
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
        const workflow = await createManifestBoundMissingScriptSourceWorkflowV1(
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
        return (await runOrResumeManifestBoundMissingScriptSourceWorkflowV1({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        })) as never;
      } finally {
        await loaded.close();
      }
    },
  });
