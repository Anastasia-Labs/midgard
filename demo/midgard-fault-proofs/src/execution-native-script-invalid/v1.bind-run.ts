import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { workflowActuationAuthorizingDecisionDigest } from "../workflow/actuation-permit.js";
import {
  encodeWorkflowArtifact,
  requireWorkflowArtifactMatches,
} from "../workflow/artifact-codec.js";
import {
  completeCanonicalReplayPredecessorEvidence,
  EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
} from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import { defineFamily } from "../workflow/family-definition.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import { executeManifestBoundFamilyRecovery } from "../workflow/manifest-bound-family-recovery.js";
import { type FraudProofWorkflowRunResult } from "../workflow/orchestrator.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES,
  EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES,
  type ExecutionNativeScriptInvalidContracts,
} from "./contracts.js";
import { detectExecutionNativeScriptInvalidCanonicalViolations } from "./replay.js";
import { captureExecutionNativeScriptInvalidAction } from "./v1.capture-execution-native-script-invalid-action.js";
import {
  type BoundContext,
  contractNames,
  EXECUTION_NATIVE_SCRIPT_INVALID_CONFIG_KEYS,
  EXECUTION_NATIVE_SCRIPT_INVALID_OPTIONAL_CONFIG_KEYS,
  EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS,
  type ExecutionNativeScriptInvalidWorkflowReferenceScripts,
  type ManifestBoundExecutionNativeScriptInvalidWorkflow,
  type ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
  type PreparedExecutionNativeScriptInvalid,
  REMOVAL_CONTRACTS,
  type RunContext,
  WITNESS_ROLES,
} from "./v1.prepare-manifest-bound-execution-native-script-invalid-replay.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "./workflow-spec.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION = defineFamily<
  "executionNativeScriptInvalid",
  (typeof WITNESS_ROLES)[number],
  true,
  13,
  RunContext
>({
  category: "executionNativeScriptInvalid",
  stepDatumSchemas: EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  auxiliaryReferenceScripts: REMOVAL_CONTRACTS,
  replayer: () => EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
    stepContractNames: contractNames,
    transactionPort: (context) => runFor(context).transactions,
  },
});

/** Strict manifest/reference construction; no proof inputs or callbacks. */
export const createManifestBoundExecutionNativeScriptInvalidWorkflow = async (
  config: ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
): Promise<ManifestBoundExecutionNativeScriptInvalidWorkflow> => {
  if (
    Object.keys(config)
      .filter(
        (key) =>
          !(
            EXECUTION_NATIVE_SCRIPT_INVALID_OPTIONAL_CONFIG_KEYS as readonly string[]
          ).includes(key),
      )
      .sort()
      .join("\0") !==
    [...EXECUTION_NATIVE_SCRIPT_INVALID_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "executionNativeScriptInvalid production config contains callback authority",
    );
  const deployment = await bindManifestBoundFamilyWorkflow(
    EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
    { ...config, auxiliaryReferenceScripts: config.referenceScripts.removal },
  );
  const {
    binding,
    references: { steps },
  } = deployment;
  const chain =
    binding.resolvedContracts.contracts.executionNativeScriptInvalid;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    chain.steps.length !== 13 ||
    stateQueuePolicyId === undefined ||
    certificate === null
  )
    throw new Error(
      "executionNativeScriptInvalid manifest omitted thirteen-step chain",
    );
  const contracts: ExecutionNativeScriptInvalidContracts = Object.freeze({
    steps: chain.steps.slice(0, 6).map((step, index) => ({
      ...step,
      blueprintTitle: EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })),
    acceptedPrelude: chain.steps.slice(6).map((step, index) => ({
      ...step,
      blueprintTitle:
        EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES[index]!,
      referenceOutRef: `${steps[index + 6]!.txHash}#${steps[index + 6]!.outputIndex.toString()}`,
    })),
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  return Object.freeze({
    deployment,
    binding,
    lucid: config.lucid,
    signer: config.signer,
    l1Source: config.l1Source,
    ...(config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext }),
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    contracts,
    references: {
      ...deployment.references,
      removal:
        deployment.auxiliaryReferences as ExecutionNativeScriptInvalidWorkflowReferenceScripts["removal"],
    },
    l1: deployment.l1,
  });
};

export type ExecutionNativeScriptInvalidRunResult = FraudProofWorkflowRunResult;

const bindRun = (context: BoundContext) => {
  const { workflow } = context.runtime;
  let fresh: PreparedExecutionNativeScriptInvalid | undefined;
  const prepareArtifact = async (
    block: import("../evidence/canonical-block-evidence.js").CanonicalBlockEvidence,
  ) => {
    const predecessor = completeCanonicalReplayPredecessorEvidence({
      evidence: block,
      context: workflow.replayContext,
    });
    const detections = detectExecutionNativeScriptInvalidCanonicalViolations({
      block,
      predecessor,
    });
    const detection = detections[0];
    if (detection === undefined)
      throw new Error(
        "executionNativeScriptInvalid replay has no selected artifact",
      );
    return { block, predecessor, detection };
  };
  // The canonical envelope records payload identity; the family artifact records
  // exact selected proof material and the admitted predecessor's identity.
  const material = (prepared: PreparedExecutionNativeScriptInvalid) => ({
    header: prepared.block.header,
    headerHash: prepared.block.headerHash,
    detection: prepared.detection,
    predecessor:
      prepared.predecessor === undefined
        ? null
        : {
            headerHash: prepared.predecessor.headerHash,
            payloadEnvelopeSha256: prepared.predecessor.payloadEnvelopeSha256,
            payloadSha256: prepared.predecessor.payloadSha256,
          },
  });
  const transactions: CursorFamilyTransactionPort<"executionNativeScriptInvalid"> =
    {
      portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
      category: "executionNativeScriptInvalid",
      prepare: async ({ evidence }) => {
        fresh = await prepareArtifact(evidence);
        return encodeWorkflowArtifact(material(fresh));
      },
      validatePreparedArtifact: async ({ evidence, artifact }) => {
        fresh = await prepareArtifact(evidence);
        requireWorkflowArtifactMatches(artifact, material(fresh));
      },
      capture: async ({ action, artifact }) => {
        if (fresh === undefined)
          throw new Error(
            "executionNativeScriptInvalid capture requires current authenticated material",
          );
        requireWorkflowArtifactMatches(artifact, material(fresh));
        return captureExecutionNativeScriptInvalidAction({
          workflow,
          prepared: fresh,
          action,
        });
      },
    };
  return { transactions };
};

const runs = new WeakMap<BoundContext, ReturnType<typeof bindRun>>();

const runFor = (context: BoundContext) => {
  const existing = runs.get(context);
  if (existing !== undefined) return existing;
  const created = bindRun(context);
  runs.set(context, created);
  return created;
};

export const runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow =
  async ({
    workflow,
    sources,
    journal,
  }: {
    workflow: ManifestBoundExecutionNativeScriptInvalidWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
  }): Promise<FraudProofWorkflowRunResult> => {
    const decisionDigest = workflowActuationAuthorizingDecisionDigest(journal);
    if (decisionDigest === undefined)
      throw new Error(
        "executionNativeScriptInvalid journal carries no admitted decision digest",
      );
    const assembled = assembleBoundManifestBoundFamilyWorkflow(
      EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
      workflow.deployment,
      { workflow, sources },
    );
    return executeManifestBoundFamilyRecovery({
      ...assembled,
      sources,
      journal,
      decisionDigest,
      ...(workflow.replayContext === undefined
        ? {}
        : { replayContext: workflow.replayContext }),
    });
  };
