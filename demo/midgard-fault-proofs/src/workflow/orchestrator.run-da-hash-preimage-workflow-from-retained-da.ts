import type { AuthenticatedStateQueueHeaderObservation } from "@al-ft/midgard-sdk/fraud-proof";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  fetchFraudProofEvidence,
  FRAUD_PROOF_EVIDENCE_ROUTE,
} from "../evidence/fraud-proof-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "./classification.classify-canonical-block-violations.js";
import type { CompleteCanonicalReplayContext } from "./complete-replay.js";
import { normalizeWorkflowJson, type WorkflowJsonObject } from "./identity.js";
import type {
  FraudProofWorkflowRegistry,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import { verifiedReleaseFinality } from "./orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runAdmittedFraudProofWorkflow } from "./orchestrator.run-admitted-fraud-proof-workflow.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import type { WorkflowRun } from "./workflow-run.js";

/** Canonical-block classified workflow entry retained for all ordinary families. */
export const runFraudProofWorkflow = async ({
  hubOraclePolicyId,
  evidence,
  detections,
  replayContext,
  registry,
  run,
  terminalVerifier,
  releaseFinalityAuthority,
  maxActions,
}: {
  readonly hubOraclePolicyId: string;
  readonly evidence: CanonicalBlockEvidence;
  readonly detections: readonly CanonicalViolationDetection[];
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly registry: FraudProofWorkflowRegistry;
  readonly run: WorkflowRun;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResult> => {
  const verified = await verifiedReleaseFinality({
    hubOraclePolicyId,
    authority: releaseFinalityAuthority,
  });
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections,
    minimumConfirmationDepth: 1,
  });
  if (classification.decision !== "fault_detected") {
    return { kind: classification.decision, classification };
  }
  return await runAdmittedFraudProofWorkflow({
    hubOraclePolicyId: verified.hubOraclePolicyId,
    category: classification.category,
    headerHash: evidence.headerHash,
    prepareFamilyArtifact: async (adapter) =>
      await adapter.prepare({
        evidence,
        classification,
        ...(replayContext === undefined ? {} : { replayContext }),
      }),
    registry,
    run,
    terminalVerifier,
    releaseFinality: verified.releaseFinality,
    ...(maxActions === undefined ? {} : { maxActions }),
  });
};

/**
 * Dedicated Q44 entry. It owns the public-DA fetch and typed raw-leaf route,
 * so no caller-authored classification or durable proof artifact can enter the
 * shared lifecycle. A canonical payload is not silently treated as Q44.
 */
export const runDaHashPreimageWorkflowFromRetainedDa = async ({
  hubOraclePolicyId,
  observation,
  sources,
  registry,
  run,
  terminalVerifier,
  releaseFinalityAuthority,
  retries,
  maxActions,
}: {
  readonly hubOraclePolicyId: string;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly registry: FraudProofWorkflowRegistry;
  readonly run: WorkflowRun;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly retries?: number;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResult> => {
  const scope = [...registry.keys()];
  if (scope.length !== 1 || scope[0] !== "daHashPreimage") {
    throw new Error(
      `dedicated Q44 workflow requires the exact daHashPreimage registry; found=${scope.join(",")}`,
    );
  }
  const verified = await verifiedReleaseFinality({
    hubOraclePolicyId,
    authority: releaseFinalityAuthority,
  });
  const routed = await fetchFraudProofEvidence({
    observation,
    sources,
    ...(retries === undefined ? {} : { retries }),
    minimumConfirmationDepth: 1,
  });
  if (
    routed.schemaVersion !== FRAUD_PROOF_EVIDENCE_ROUTE ||
    routed.kind !== "da_hash_preimage"
  ) {
    throw new Error(
      "dedicated Q44 workflow found no authenticated raw source-leaf defect",
    );
  }
  const familyArtifact = normalizeWorkflowJson({
    schemaVersion: "midgard-production-da-hash-preimage-artifact-v1",
    headerHash: routed.plan.headerHash,
    committedTransactionsRoot: routed.plan.committedTransactionsRoot,
    l2TransactionCount: routed.plan.l2TransactionCount,
    committedTxId: routed.plan.violation.committedTxId,
    entries: routed.evidence.entries,
  }) as WorkflowJsonObject;
  return await runAdmittedFraudProofWorkflow({
    hubOraclePolicyId: verified.hubOraclePolicyId,
    category: "daHashPreimage",
    headerHash: routed.evidence.headerHash,
    prepareFamilyArtifact: async () => familyArtifact,
    registry,
    run,
    terminalVerifier,
    releaseFinality: verified.releaseFinality,
    ...(maxActions === undefined ? {} : { maxActions }),
  });
};
