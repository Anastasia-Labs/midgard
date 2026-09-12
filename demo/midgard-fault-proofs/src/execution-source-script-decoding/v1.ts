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
import { EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
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
  type BoundExecutionSourceScriptDecodingActuatorConfig,
  createExecutionSourceScriptDecodingActuator,
  type ExecutionSourceScriptDecodingWorkflowReferences,
} from "./actuator.js";
import { prepareExecutionSourceScriptDecodingArtifact } from "./authenticated-replay.js";
import {
  EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  type ExecutionSourceScriptDecodingContracts,
} from "./contracts.js";
import {
  ExecutionSourceStep02DatumSchema,
  ExecutionSourceStep03DatumSchema,
  ExecutionSourceStep04DatumSchema,
  ExecutionSourceStep05DatumSchema,
} from "./schemas.js";

export const EXECUTION_SOURCE_SCRIPT_DECODING_WORKFLOW =
  "midgard-execution-source-script-decoding-production-workflow-v1" as const;
export const EXECUTION_SOURCE_SCRIPT_DECODING_CONFIG_KEYS = Object.freeze([
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
export const EXECUTION_SOURCE_SCRIPT_DECODING_STEP_DATUM_SCHEMAS =
  Object.freeze([
    FraudProofComputationThreadStepDatum,
    ExecutionSourceStep02DatumSchema,
    ExecutionSourceStep03DatumSchema,
    ExecutionSourceStep04DatumSchema,
    ExecutionSourceStep05DatumSchema,
  ] as const);

export const EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC: CursorFamilySpec<"executionSourceScriptDecoding"> =
  Object.freeze<CursorFamilySpec<"executionSourceScriptDecoding">>({
    category: "executionSourceScriptDecoding",
    stepCount: 5,
    successors: { 1: [2], 2: [3], 3: [4], 4: [4, 5], 5: ["proof_token"] },
  });

export type ExecutionSourceScriptDecodingRemovalReferences = Readonly<{
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

export type ManifestBoundExecutionSourceScriptDecodingWorkflowConfig =
  Readonly<{
    manifest: unknown;
    blueprintJson: string;
    deploymentInfo: unknown;
    headerHash: string;
    lucid: LucidEvolution;
    signer: ResolvedProverSigner;
    source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
    decisionDigest: string;
    stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    referenceScripts: ExecutionSourceScriptDecodingWorkflowReferences &
      Readonly<{
        removal: ExecutionSourceScriptDecodingRemovalReferences;
      }>;
  }>;

export type ManifestBoundExecutionSourceScriptDecodingWorkflow = Readonly<{
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  binding: FraudProofWorkflowDeploymentBinding<never> &
    BoundExecutionSourceScriptDecodingActuatorConfig["binding"];
  actuator: ReturnType<typeof createExecutionSourceScriptDecodingActuator>;
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
export const createManifestBoundExecutionSourceScriptDecodingWorkflow = async (
  config: ManifestBoundExecutionSourceScriptDecodingWorkflowConfig,
): Promise<ManifestBoundExecutionSourceScriptDecodingWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...EXECUTION_SOURCE_SCRIPT_DECODING_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "executionSourceScriptDecoding production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error(
      "executionSourceScriptDecoding decision digest is malformed",
    );
  const rawBinding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "executionSourceScriptDecoding" as never,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: EXECUTION_SOURCE_SCRIPT_DECODING_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
    network: rawBinding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const binding =
    rawBinding as unknown as FraudProofWorkflowDeploymentBinding<never> &
      BoundExecutionSourceScriptDecodingActuatorConfig["binding"] & {
        resolvedContracts: {
          contracts: {
            computationThread: ExecutionSourceScriptDecodingContracts["computationThread"];
            fraudProof: ExecutionSourceScriptDecodingContracts["fraudProof"] & {
              spendingScriptHash: string;
            };
            executionSourceScriptDecoding?: {
              steps: ExecutionSourceScriptDecodingContracts["steps"];
            };
          };
        };
      };
  const chain =
    binding.resolvedContracts.contracts.executionSourceScriptDecoding;
  const hubOraclePolicyId =
    rawBinding.contractEntries.hubOracleMint?.scriptHash;
  const stateQueuePolicyId = rawBinding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 5 ||
    hubOraclePolicyId === undefined ||
    stateQueuePolicyId === undefined
  )
    throw new Error(
      "executionSourceScriptDecoding manifest omitted five-step chain or state-queue policy",
    );
  const bindReference = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding: rawBinding,
      contractName: name,
      utxo,
    });
  const stepNames = [
    "fraudProofExecutionSourceScriptDecoding",
    "fraudProofExecutionSourceScriptDecodingStep02",
    "fraudProofExecutionSourceScriptDecodingStep03",
    "fraudProofExecutionSourceScriptDecodingStep04",
    "fraudProofExecutionSourceScriptDecodingStep05",
  ] as const;
  const steps = stepNames.map((name, index) =>
    bindReference(name, config.referenceScripts.steps[index]!),
  ) as unknown as ExecutionSourceScriptDecodingWorkflowReferences["steps"];
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
  const contracts: ExecutionSourceScriptDecodingContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as ExecutionSourceScriptDecodingContracts["steps"],
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
  const actuator = createExecutionSourceScriptDecodingActuator({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const artifacts = createCanonicalFamilyArtifactPort(({ evidence }) =>
    prepareExecutionSourceScriptDecodingArtifact(evidence),
  );
  const adapter = createCursorFamilyWorkflowAdapter({
    spec: EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC,
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    transactions: {
      portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
      category: "executionSourceScriptDecoding",
      prepare: artifacts.prepare,
      validatePreparedArtifact: artifacts.validatePreparedArtifact,
      capture: async ({ action, artifact }) => {
        const input = cursorFamilyActionInput({
          category: "executionSourceScriptDecoding",
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
        const stages = [
          "step_01",
          "step_02",
          "step_03",
          "scan",
          "finalize",
        ] as const;
        const stage = stages[Number(input.ordinal) - 1];
        if (stage === undefined)
          throw new Error(
            "executionSourceScriptDecoding cursor ordinal changed",
          );
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
    source: config.source,
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

export const executeManifestBoundExecutionSourceScriptDecodingWorkflow =
  async ({
    workflow,
    sources,
    journal,
  }: {
    workflow: ManifestBoundExecutionSourceScriptDecodingWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
  }): Promise<FraudProofWorkflowRunResult> => {
    return executeManifestBoundFamilyRecovery({
      ...workflow,
      sources,
      journal,
      replayer: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    });
  };

export const runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflow =
  async (input: {
    workflow: ManifestBoundExecutionSourceScriptDecodingWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
      throw new Error(
        "executionSourceScriptDecoding runner rejects caller-authored evidence",
      );
    return await executeManifestBoundExecutionSourceScriptDecodingWorkflow(
      input,
    );
  };

export type LoadedExecutionSourceScriptDecodingWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundExecutionSourceScriptDecodingWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadExecutionSourceScriptDecodingWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedExecutionSourceScriptDecodingWorkflow>;

export const createExecutionSourceScriptDecodingWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadExecutionSourceScriptDecodingWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "executionSourceScriptDecoding")
        throw new Error(
          "executionSourceScriptDecoding runner category changed",
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
          category: "executionSourceScriptDecoding" as never,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: "executionSourceScriptDecoding" as never,
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
            "executionSourceScriptDecoding requires concrete public retained DA",
          );
        const workflow =
          await createManifestBoundExecutionSourceScriptDecodingWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "executionSourceScriptDecoding runtime binding changed invocation",
          );
        return (await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflow({
              workflow,
              sources: loaded.retainedDaSources,
              journal,
            }),
        })) as never;
      } finally {
        await loaded.close();
      }
    },
  });
