import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import { type LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayPredecessorEvidence,
} from "../workflow/complete-replay.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  type FamilyAssemblyContext,
  type FamilyDeploymentContext,
} from "../workflow/family-definition.js";
import type { FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { type ExecutionNativeScriptInvalidContracts } from "./contracts.js";
import { detectExecutionNativeScriptInvalidCanonicalViolations } from "./replay.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidStep02DatumSchema,
  ExecutionNativeScriptInvalidStep03DatumSchema,
  ExecutionNativeScriptInvalidStep04DatumSchema,
  ExecutionNativeScriptInvalidStep05DatumSchema,
  ExecutionNativeScriptInvalidStep06DatumSchema,
} from "./schemas.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_WORKFLOW =
  "midgard-execution-native-script-invalid-production-workflow-v1" as const;

export const EXECUTION_NATIVE_SCRIPT_INVALID_CONFIG_KEYS = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "l1Source",
  "stateQueueMutationLeaseCoordinator",
  "referenceScripts",
] as const);

/**
 * The classifier-admitted replay context carrying the authenticated
 * predecessor. Optional only so a reconciliation-only resume, which never
 * replays, still binds.
 */
export const EXECUTION_NATIVE_SCRIPT_INVALID_OPTIONAL_CONFIG_KEYS =
  Object.freeze(["replayContext"] as const);

export const EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS = Object.freeze(
  [
    FraudProofComputationThreadStepDatum,
    ExecutionNativeScriptInvalidStep02DatumSchema,
    ExecutionNativeScriptInvalidStep03DatumSchema,
    ExecutionNativeScriptInvalidStep04DatumSchema,
    ExecutionNativeScriptInvalidStep05DatumSchema,
    ExecutionNativeScriptInvalidStep06DatumSchema,
    ExecutionNativeScriptInvalidStep02DatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ] as const,
);

export type ExecutionNativeScriptInvalidWorkflowReferenceScripts = Readonly<{
  steps: readonly [
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
  ];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
  removal: Readonly<{
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
}>;

export type ManifestBoundExecutionNativeScriptInvalidWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  l1Source: FraudProofL1Source;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ExecutionNativeScriptInvalidWorkflowReferenceScripts;
}>;

export type ManifestBoundExecutionNativeScriptInvalidWorkflow = Readonly<{
  deployment: Deployment;
  binding: FraudProofWorkflowDeploymentBinding<"executionNativeScriptInvalid">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  l1Source: FraudProofL1Source;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  contracts: ExecutionNativeScriptInvalidContracts;
  references: ExecutionNativeScriptInvalidWorkflowReferenceScripts;
  l1: FraudProofFamilyL1ObservationPort<"executionNativeScriptInvalid">;
}>;

export const contractNames = Object.freeze([
  "fraudProofExecutionNativeScriptInvalid",
  "fraudProofExecutionNativeScriptInvalidStep02",
  "fraudProofExecutionNativeScriptInvalidStep03",
  "fraudProofExecutionNativeScriptInvalidStep04",
  "fraudProofExecutionNativeScriptInvalidStep05",
  "fraudProofExecutionNativeScriptInvalidStep06",
  "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
  "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
  "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
] as const);

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;

export const REMOVAL_CONTRACTS = {
  correctionLockSpend: "correctionLockSpend",
  stateQueueSpend: "stateQueueSpend",
  stateQueueMint: "stateQueueMint",
  stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
  activeOperatorsSpend: "activeOperatorsSpend",
  activeOperatorsMint: "activeOperatorsMint",
  retiredOperatorsSpend: "retiredOperatorsSpend",
  retiredOperatorsMint: "retiredOperatorsMint",
  schedulerSpend: "schedulerSpend",
} as const;

type Deployment = FamilyDeploymentContext<
  "executionNativeScriptInvalid",
  (typeof WITNESS_ROLES)[number],
  true,
  13
>;

export type RunContext = Readonly<{
  workflow: ManifestBoundExecutionNativeScriptInvalidWorkflow;
  sources: readonly RetainedDaPayloadSource[];
}>;

export type BoundContext = FamilyAssemblyContext<
  "executionNativeScriptInvalid",
  (typeof WITNESS_ROLES)[number],
  true,
  13,
  RunContext
>;

/** Rebuild the one actionable ID32 decision solely from L1 and retained DA. */
export const prepareManifestBoundExecutionNativeScriptInvalidReplay =
  async (input: {
    workflow: ManifestBoundExecutionNativeScriptInvalidWorkflow;
    sources: readonly import("../transition-trace/fetch.js").RetainedDaPayloadSource[];
  }) => {
    if (Object.keys(input).sort().join(",") !== "sources,workflow")
      throw new Error(
        "executionNativeScriptInvalid replay rejects caller-authored evidence",
      );
    const { workflow, sources } = input;
    const block = await fetchCanonicalBlockEvidence({
      observation: await observeFraudProofWorkflowHeader(workflow.l1, {
        headerHash: workflow.binding.definition.headerHash,
      }),
      sources,
    });
    const predecessor = completeCanonicalReplayPredecessorEvidence({
      evidence: block,
      context: workflow.replayContext,
    });
    const detections = detectExecutionNativeScriptInvalidCanonicalViolations({
      block,
      predecessor,
    });
    if (detections.length !== 1)
      throw new Error(
        `executionNativeScriptInvalid replay yielded ${detections.length.toString()} exact findings`,
      );
    return Object.freeze({ block, predecessor, detection: detections[0]! });
  };

export type PreparedExecutionNativeScriptInvalid = Awaited<
  ReturnType<typeof prepareManifestBoundExecutionNativeScriptInvalidReplay>
>;
