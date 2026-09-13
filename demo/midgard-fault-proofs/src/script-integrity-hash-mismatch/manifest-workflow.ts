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
import { SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
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
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "../workflow/proof-chunk-prerequisite.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import {
  SCRIPT_INTEGRITY_HASH_MISMATCH_BLUEPRINT_TITLES,
  type ScriptIntegrityHashMismatchContracts,
} from "./contracts.js";
import {
  createScriptIntegrityHashMismatchLucidActuator,
  type ScriptIntegrityHashMismatchWorkflowReferences,
} from "./lucid-actuator.js";
import { prepareScriptIntegrityHashMismatchArtifact } from "./replay.js";
import {
  IntegrityStep02DatumSchema,
  IntegrityStep03DatumSchema,
  IntegrityStep04DatumSchema,
  IntegrityStep05DatumSchema,
} from "./schemas.js";

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

export const SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC: CursorFamilySpec<"scriptIntegrityHashMismatch"> =
  Object.freeze<CursorFamilySpec<"scriptIntegrityHashMismatch">>({
    category: "scriptIntegrityHashMismatch",
    stepCount: 5,
    successors: { 1: [2], 2: [3], 3: [4], 4: [4, 5], 5: ["proof_token"] },
  });

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
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
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
  const hubOraclePolicyId = binding.contractEntries.hubOracleMint?.scriptHash;
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
  const actuator = createScriptIntegrityHashMismatchLucidActuator({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const artifacts = createCanonicalFamilyArtifactPort(({ evidence }) =>
    prepareScriptIntegrityHashMismatchArtifact(evidence),
  );
  let adapter = createCursorFamilyWorkflowAdapter({
    spec: SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC,
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    transactions: {
      portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
      category: "scriptIntegrityHashMismatch",
      prepare: artifacts.prepare,
      validatePreparedArtifact: artifacts.validatePreparedArtifact,
      capture: async ({ action, artifact }) => {
        const input = cursorFamilyActionInput({
          category: "scriptIntegrityHashMismatch",
          action,
        });
        const restored = artifacts.require(artifact);
        if (input.stage === "init")
          return actuator.capture({
            workflowAction: action,
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
            workflowAction: action,
            artifact: restored,
            action: {
              stage: "remove",
              nextRemovalOutRef: cursorStringField(input, "nextRemovalOutRef"),
              fraudProofOutRef: cursorStringField(input, "fraudProofOutRef"),
            },
          });
        const stages = [
          "step_01",
          "step_02",
          "step_03",
          "step_04",
          "step_05",
        ] as const;
        const stage = stages[Number(input.ordinal) - 1];
        if (stage === undefined)
          throw new Error("scriptIntegrityHashMismatch cursor ordinal changed");
        const threadOutRef = cursorStringField(input, "threadOutRef");
        return actuator.capture({
          workflowAction: action,
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
  adapter = withProofChunkPrerequisite({
    category: "scriptIntegrityHashMismatch",
    base: adapter,
    prerequisite: createAuthenticatedProofChunkPrerequisitePort({
      category: "scriptIntegrityHashMismatch",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      proofCborForAction: ({ action, artifact }) =>
        action.input.stage === "step_01"
          ? (artifacts.require(artifact).acceptedInclusion
              ?.txMembershipProofCbor ?? null)
          : null,
      transactionConfirmed: ({ headerHash, txHash }) =>
        l1.transactionConfirmed({ headerHash, txHash }),
    }),
  });
  return Object.freeze({
    binding,
    decisionDigest: config.decisionDigest,
    l1,
    actuator,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

export const executeManifestBoundScriptIntegrityHashMismatchWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundScriptIntegrityHashMismatchWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  return executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    replayer: SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  });
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
        return await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            executeManifestBoundScriptIntegrityHashMismatchWorkflow({
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
