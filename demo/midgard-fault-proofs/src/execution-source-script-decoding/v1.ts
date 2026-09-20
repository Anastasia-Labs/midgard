import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  cursorFamilyActionInput,
  cursorStringField,
  STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
} from "../workflow/cursor-family-runtime.js";
import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import { type FraudProofWorkflowRunResult } from "../workflow/orchestrator.js";
import {
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

export const EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC: CursorFamilySpec<"executionSourceScriptDecoding"> &
  Readonly<{ stepCount: 5 }> = Object.freeze<
  CursorFamilySpec<"executionSourceScriptDecoding"> & Readonly<{ stepCount: 5 }>
>({
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

export type ManifestBoundExecutionSourceScriptDecodingWorkflow =
  ManifestBoundFamilyWorkflow<"executionSourceScriptDecoding", false, 5> &
    WorkflowExtension &
    Readonly<{
      decisionDigest: string;
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
    }>;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;
const STEP_CONTRACT_NAMES = [
  "fraudProofExecutionSourceScriptDecoding",
  "fraudProofExecutionSourceScriptDecodingStep02",
  "fraudProofExecutionSourceScriptDecodingStep03",
  "fraudProofExecutionSourceScriptDecodingStep04",
  "fraudProofExecutionSourceScriptDecodingStep05",
] as const;
type BoundContext = FamilyAssemblyContext<
  "executionSourceScriptDecoding",
  (typeof WITNESS_ROLES)[number],
  false,
  5
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createExecutionSourceScriptDecodingActuator>;
  lucid: LucidEvolution;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
const bindFamily = (context: BoundContext) => {
  const {
    binding,
    references: { steps, witnesses },
  } = context;
  const chain =
    binding.resolvedContracts.contracts.executionSourceScriptDecoding;
  const hubOraclePolicyId = binding.contractEntries.hubOracleMint?.scriptHash;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 5 ||
    hubOraclePolicyId === undefined ||
    stateQueuePolicyId === undefined
  )
    throw new Error(
      "executionSourceScriptDecoding manifest omitted five-step chain or state-queue policy",
    );
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
  const actuator = createExecutionSourceScriptDecodingActuator({
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  });
  const artifacts = createCanonicalFamilyArtifactPort(({ evidence }) =>
    prepareExecutionSourceScriptDecodingArtifact(evidence),
  );
  return { actuator, artifacts };
};
const bound = new WeakMap<BoundContext, ReturnType<typeof bindFamily>>();
const boundFor = (context: BoundContext) => {
  const existing = bound.get(context);
  if (existing !== undefined) return existing;
  const created = bindFamily(context);
  bound.set(context, created);
  return created;
};
const createTransactionPort = (
  context: BoundContext,
): CursorFamilyTransactionPort<"executionSourceScriptDecoding"> => {
  const { actuator, artifacts } = boundFor(context);
  return {
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
        throw new Error("executionSourceScriptDecoding cursor ordinal changed");
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
  };
};
export const EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION = defineFamily<
  "executionSourceScriptDecoding",
  (typeof WITNESS_ROLES)[number],
  false,
  5
>({
  category: "executionSourceScriptDecoding",
  stepDatumSchemas: EXECUTION_SOURCE_SCRIPT_DECODING_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  auxiliaryReferenceScripts: STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
  replayer: () => EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC,
    stepContractNames: STEP_CONTRACT_NAMES,
    transactionPort: createTransactionPort,
  },
  extend: (context): WorkflowExtension => ({
    actuator: boundFor(context).actuator,
    lucid: context.lucid,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  }),
});
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
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION,
    {
      ...assemblyConfig,
      auxiliaryReferenceScripts: config.referenceScripts.removal,
    },
  );
  return Object.freeze({
    ...(workflow as typeof workflow & WorkflowExtension),
    source: config.source,
    decisionDigest,
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
