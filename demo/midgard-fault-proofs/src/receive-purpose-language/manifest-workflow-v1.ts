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
  RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES_V1,
  type ReceivePurposeLanguageContractsV1,
} from "./contracts-v1.js";
import {
  type BoundReceivePurposeLanguageActuatorConfigV1,
  createReceivePurposeLanguageActuatorV1,
  type ReceivePurposeLanguageActuatorActionV1,
  type ReceivePurposeLanguageWorkflowReferencesV1,
} from "./production-actuator-v1.js";
import { prepareProductionReceivePurposeLanguageArtifactV1 } from "./production-replay-v1.js";
import {
  ReceivePurposeStep02DatumV1Schema,
  ReceivePurposeStep03DatumV1Schema,
} from "./schemas-v1.js";

export const RECEIVE_PURPOSE_LANGUAGE_PRODUCTION_WORKFLOW_V1 =
  "midgard-receive-purpose-language-production-workflow-v1" as const;
export const RECEIVE_PURPOSE_LANGUAGE_PRODUCTION_CONFIG_KEYS_V1 = Object.freeze(
  [
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
  ] as const,
);
export const RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS_V1 = Object.freeze([
  FraudProofComputationThreadStepDatum,
  ReceivePurposeStep02DatumV1Schema,
  ReceivePurposeStep03DatumV1Schema,
] as const);
export type ReceivePurposeLanguageRemovalReferencesV1 = Readonly<{
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
export type ManifestBoundReceivePurposeLanguageWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ReceivePurposeLanguageWorkflowReferencesV1 &
    Readonly<{ removal: ReceivePurposeLanguageRemovalReferencesV1 }>;
}>;
export type ManifestBoundReceivePurposeLanguageWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<never> &
    BoundReceivePurposeLanguageActuatorConfigV1["binding"];
  actuator: ReturnType<typeof createReceivePurposeLanguageActuatorV1>;
  lucid: LucidEvolution;
  decisionDigest: string;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPortV1>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

/** Strict manifest/reference binding; its input admits no callback authority. */
export const createManifestBoundReceivePurposeLanguageWorkflowV1 = async (
  config: ManifestBoundReceivePurposeLanguageWorkflowConfigV1,
): Promise<ManifestBoundReceivePurposeLanguageWorkflowV1> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...RECEIVE_PURPOSE_LANGUAGE_PRODUCTION_CONFIG_KEYS_V1].sort().join("\0")
  )
    throw new Error(
      "receivePurposeLanguage production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("receivePurposeLanguage decision digest is malformed");
  const raw = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "receivePurposeLanguage" as never,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS_V1,
  });
  assertManifestBoundWorkflowSignerV1({
    network: raw.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const binding =
    raw as unknown as FraudProofWorkflowDeploymentBindingV1<never> &
      BoundReceivePurposeLanguageActuatorConfigV1["binding"] & {
        resolvedContracts: {
          contracts: {
            computationThread: ReceivePurposeLanguageContractsV1["computationThread"];
            fraudProof: ReceivePurposeLanguageContractsV1["fraudProof"] & {
              spendingScriptHash: string;
            };
            receivePurposeLanguage?: {
              steps: ReceivePurposeLanguageContractsV1["steps"];
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
    requireManifestBoundReferenceScriptUtxoV1({
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
  ) as unknown as ReceivePurposeLanguageWorkflowReferencesV1["steps"];
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
          role as keyof FaultProofWitnessReferenceScriptsV1
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScriptsV1>;
  const contracts: ReceivePurposeLanguageContractsV1 = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES_V1[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as ReceivePurposeLanguageContractsV1["steps"],
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
    decisionDigest: config.decisionDigest,
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    actuator: createReceivePurposeLanguageActuatorV1({
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
  journal: FraudProofWorkflowJournalStoreV1,
  workflowId: string,
  identity: FraudProofWorkflowIdentityV1,
  event: FraudProofWorkflowJournalEventV1,
) => {
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
const actionFor = async (
  workflow: ManifestBoundReceivePurposeLanguageWorkflowV1,
): Promise<ReceivePurposeLanguageActuatorActionV1 | "removed"> => {
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
export type ReceivePurposeLanguageWorkflowRunResultV1 = Readonly<{
  kind: "pending" | "completed";
  workflowId: string;
  txHash?: string;
}>;
export const executeManifestBoundReceivePurposeLanguageWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundReceivePurposeLanguageWorkflowV1;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStoreV1;
}): Promise<ReceivePurposeLanguageWorkflowRunResultV1> => {
  const headerHash = workflow.binding.definition.headerHash;
  const block = await fetchCanonicalBlockEvidenceV1({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const artifact =
    await prepareProductionReceivePurposeLanguageArtifactV1(block);
  const identity: FraudProofWorkflowIdentityV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: "receivePurposeLanguage" as never,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowIdV1(identity);
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
  const submitted = await submitCapturedTransactionV1(captured.transaction);
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
export const runOrResumeManifestBoundReceivePurposeLanguageWorkflowV1 =
  async (input: {
    workflow: ManifestBoundReceivePurposeLanguageWorkflowV1;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStoreV1;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
      throw new Error(
        "receivePurposeLanguage runner rejects caller-authored evidence",
      );
    return await executeManifestBoundReceivePurposeLanguageWorkflowV1(input);
  };
export type LoadedReceivePurposeLanguageProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundReceivePurposeLanguageWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;
export type LoadReceivePurposeLanguageProductionWorkflowV1 = (input: {
  runtimeConfigPath: string;
  invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedReceivePurposeLanguageProductionWorkflowV1>;
export const createReceivePurposeLanguageProductionWorkflowRunnerSurfaceV1 = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadReceivePurposeLanguageProductionWorkflowV1;
}): ProductionWorkflowAdapterRunnerV1 =>
  Object.freeze({
    runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "receivePurposeLanguage")
        throw new Error("receivePurposeLanguage runner category changed");
      const journal = bindProductionWorkflowFundingReservationJournalV1({
        permit: invocation.fundingReservationPermit,
        journal: bindProductionWorkflowActuationJournalV1({
          journal: new DirectoryFraudProofWorkflowJournalStoreV1(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "receivePurposeLanguage" as never,
          headerHash: invocation.headerHash,
        }),
      });
      assertProductionWorkflowJournalActuationV1({
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
          await createManifestBoundReceivePurposeLanguageWorkflowV1(
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
        return await runOrResumeManifestBoundReceivePurposeLanguageWorkflowV1({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
