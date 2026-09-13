import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

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
import { RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../workflow/cursor-family-adapter.js";
import {
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import {
  type BoundReceivePurposeLanguageActuatorConfig,
  createReceivePurposeLanguageActuator,
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
export const RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC: CursorFamilySpec<"receivePurposeLanguage"> =
  Object.freeze<CursorFamilySpec<"receivePurposeLanguage">>({
    category: "receivePurposeLanguage",
    stepCount: 3,
    successors: { 1: [2], 2: [3], 3: ["proof_token"] },
  });

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
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
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
  const hubOraclePolicyId = raw.contractEntries.hubOracleMint?.scriptHash;
  const stateQueuePolicyId = raw.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 3 ||
    hubOraclePolicyId === undefined ||
    stateQueuePolicyId === undefined
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
    stateQueuePolicyId,
  };
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const actuator = createReceivePurposeLanguageActuator({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const artifacts = createCanonicalFamilyArtifactPort(({ evidence }) =>
    prepareReceivePurposeLanguageArtifact(evidence),
  );
  const adapter = createCursorFamilyWorkflowAdapter({
    spec: RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC,
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    transactions: {
      portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
      category: "receivePurposeLanguage",
      prepare: artifacts.prepare,
      validatePreparedArtifact: artifacts.validatePreparedArtifact,
      capture: async ({ action, artifact }) => {
        const input = cursorFamilyActionInput({
          category: "receivePurposeLanguage",
          action,
        });
        const restored = artifacts.require(artifact);
        if (input.stage === "init")
          return actuator.capture({
            artifact: restored,
            action: {
              stage: "init",
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
            },
          });
        if (input.stage === "remove")
          return actuator.capture({
            artifact: restored,
            action: {
              stage: "remove",
              nextRemovalOutRef: cursorStringField(input, "nextRemovalOutRef"),
              fraudProofOutRef: cursorStringField(input, "fraudProofOutRef"),
            },
          });
        const stages = ["step_01", "step_02", "step_03"] as const;
        const stage = stages[Number(input.ordinal) - 1];
        if (stage === undefined)
          throw new Error("receivePurposeLanguage cursor ordinal changed");
        const threadOutRef = cursorStringField(input, "threadOutRef");
        return actuator.capture({
          artifact: restored,
          action:
            stage === "step_01"
              ? {
                  stage,
                  threadOutRef,
                  stateQueueBlockOutRef: cursorStringField(
                    input,
                    "stateQueueBlockOutRef",
                  ),
                }
              : { stage, threadOutRef },
        });
      },
    },
  });
  return Object.freeze({
    binding,
    lucid: config.lucid,
    decisionDigest: config.decisionDigest,
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    actuator,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};
export const executeManifestBoundReceivePurposeLanguageWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundReceivePurposeLanguageWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  return executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    replayer: RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  });
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
        return await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            runOrResumeManifestBoundReceivePurposeLanguageWorkflow({
              workflow,
              sources: loaded.retainedDaSources,
              journal,
            }),
        });
      } finally {
        await loaded.close();
      }
    },
  });
