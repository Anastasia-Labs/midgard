import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
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

export const SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC: CursorFamilySpec<"scriptIntegrityHashMismatch"> &
  Readonly<{ stepCount: 5 }> = Object.freeze<
  CursorFamilySpec<"scriptIntegrityHashMismatch"> & Readonly<{ stepCount: 5 }>
>({
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

export type ManifestBoundScriptIntegrityHashMismatchWorkflow =
  ManifestBoundFamilyWorkflow<"scriptIntegrityHashMismatch", false, 5> &
    WorkflowExtension &
    Readonly<{ decisionDigest: string }>;

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
  removal: STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
} as const);

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;
type BoundContext = FamilyAssemblyContext<
  "scriptIntegrityHashMismatch",
  (typeof WITNESS_ROLES)[number],
  false,
  5
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createScriptIntegrityHashMismatchLucidActuator>;
}>;
const bindFamily = (context: BoundContext) => {
  const {
    binding,
    references: { steps, witnesses },
  } = context;
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
  const actuator = createScriptIntegrityHashMismatchLucidActuator({
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  });
  const artifacts = createCanonicalFamilyArtifactPort(({ evidence }) =>
    prepareScriptIntegrityHashMismatchArtifact(evidence),
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
): CursorFamilyTransactionPort<"scriptIntegrityHashMismatch"> => {
  const { actuator, artifacts } = boundFor(context);
  return {
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
  };
};
export const SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_DEFINITION = defineFamily<
  "scriptIntegrityHashMismatch",
  (typeof WITNESS_ROLES)[number],
  false,
  5
>({
  category: "scriptIntegrityHashMismatch",
  stepDatumSchemas: SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  auxiliaryReferenceScripts: contractRoles.removal,
  replayer: () => SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: SCRIPT_INTEGRITY_HASH_MISMATCH_CURSOR_SPEC,
    stepContractNames: contractRoles.steps,
    transactionPort: createTransactionPort,
  },
  proofChunk: (context, { action, artifact }) =>
    action.input.stage === "step_01"
      ? (boundFor(context).artifacts.require(artifact).acceptedInclusion
          ?.txMembershipProofCbor ?? null)
      : null,
  extend: (context): WorkflowExtension => ({
    actuator: boundFor(context).actuator,
  }),
});
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
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_DEFINITION,
    {
      ...assemblyConfig,
      auxiliaryReferenceScripts: config.referenceScripts.removal,
    },
  );
  return Object.freeze({
    ...(workflow as typeof workflow & WorkflowExtension),
    decisionDigest,
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
