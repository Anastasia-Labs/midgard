import {
  type AuthenticatedStateQueueHeaderObservation,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  fetchFraudProofEvidence,
  FRAUD_PROOF_EVIDENCE_ROUTE,
} from "../evidence/fraud-proof-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  workflowActuationDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "./actuation-permit.js";
import {
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "./classification.js";
import { type CompleteCanonicalReplayContext } from "./complete-replay.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeFraudProofWorkflowIdentity,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "./journal.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRegistry,
  type FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import { type FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import {
  canonicalEvidenceBinding,
  createFraudProofWorkflowRegistry,
  type PersistedArtifactEnvelope,
  verifiedReleaseFinality,
  type WorkflowEvidenceBinding,
} from "./orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runAdmittedFraudProofWorkflow } from "./orchestrator.run-admitted-fraud-proof-workflow.js";
import {
  type FraudProofReleaseFinalityAuthority,
  validateVerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy.js";

/** Resume the existing adapter from its durable evidence only. This entrypoint
 * requires an opaque reconciliation permit and cannot prepare a new execution. */
export const resumeRecordedFraudProofWorkflow = async (input: {
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}): Promise<FraudProofWorkflowRunResult> => {
  if (!workflowJournalIsReconciliationOnly(input.journal))
    throw new Error(
      "recorded workflow recovery requires reconciliation-only authority",
    );
  const identity = normalizeFraudProofWorkflowIdentity({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: input.deploymentFingerprint,
    category: input.category,
    target: { kind: "state_queue_header", headerHash: input.headerHash },
    decisionDigest: workflowActuationDecisionDigest(input.journal),
  });
  const entries = await input.journal.load(
    computeFraudProofWorkflowId(identity),
  );
  validateFraudProofWorkflowJournal({
    workflowId: computeFraudProofWorkflowId(identity),
    entries,
    expectedIdentity: identity,
  });
  const prepared = entries[1];
  if (prepared?.event.kind !== "prepared")
    throw new Error("recorded workflow has no prepared evidence");
  const envelope = prepared.event.artifact as PersistedArtifactEnvelope;
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    await input.releaseFinalityAuthority.verifyForWorkflow({
      deploymentFingerprint: input.deploymentFingerprint,
    }),
  );
  return await runAdmittedFraudProofWorkflow({
    ...input,
    releaseFinality,
    evidenceBinding: envelope.evidenceBinding,
    registry: createFraudProofWorkflowRegistry({
      adapters: [input.adapter],
      launchScope: [input.category],
    }),
    prepareFamilyArtifact: async () => {
      throw new Error("recorded workflow cannot prepare a new artifact");
    },
  });
};

/** Canonical-block classified workflow entry retained for all ordinary families. */
export const runFraudProofWorkflow = async ({
  deploymentFingerprint,
  evidence,
  detections,
  replayContext,
  registry,
  journal,
  terminalVerifier,
  releaseFinalityAuthority,
  maxSubmissionAttempts,
  maxActions,
  now,
}: {
  readonly deploymentFingerprint: string;
  readonly evidence: CanonicalBlockEvidence;
  readonly detections: readonly CanonicalViolationDetection[];
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly registry: FraudProofWorkflowRegistry;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
  readonly now?: () => Date;
}): Promise<FraudProofWorkflowRunResult> => {
  const verified = await verifiedReleaseFinality({
    deploymentFingerprint,
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
    deploymentFingerprint: verified.deploymentFingerprint,
    category: classification.category,
    headerHash: evidence.headerHash,
    evidenceBinding: canonicalEvidenceBinding(evidence),
    prepareFamilyArtifact: async (adapter) =>
      await adapter.prepare({
        evidence,
        classification,
        ...(replayContext === undefined ? {} : { replayContext }),
      }),
    validateFamilyArtifact: async (adapter, artifact) => {
      await adapter.validatePreparedArtifact?.({
        evidence,
        classification,
        artifact,
        ...(replayContext === undefined ? {} : { replayContext }),
      });
    },
    registry,
    journal,
    terminalVerifier,
    releaseFinality: verified.releaseFinality,
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
    ...(now === undefined ? {} : { now }),
  });
};

/**
 * Dedicated Q44 entry. It owns the public-DA fetch and typed raw-leaf route,
 * so no caller-authored classification or durable proof artifact can enter the
 * shared lifecycle. A canonical payload is not silently treated as Q44.
 */
export const runDaHashPreimageWorkflowFromRetainedDa = async ({
  deploymentFingerprint,
  observation,
  sources,
  registry,
  journal,
  terminalVerifier,
  releaseFinalityAuthority,
  retries,
  maxSubmissionAttempts,
  maxActions,
  now,
}: {
  readonly deploymentFingerprint: string;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly registry: FraudProofWorkflowRegistry;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly retries?: number;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
  readonly now?: () => Date;
}): Promise<FraudProofWorkflowRunResult> => {
  const scope = [...registry.keys()];
  if (scope.length !== 1 || scope[0] !== "daHashPreimage") {
    throw new Error(
      `dedicated Q44 workflow requires the exact daHashPreimage registry; found=${scope.join(",")}`,
    );
  }
  const verified = await verifiedReleaseFinality({
    deploymentFingerprint,
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
  const familyArtifact = normalizeJournalJson({
    schemaVersion: "midgard-production-da-hash-preimage-artifact-v1",
    headerHash: routed.plan.headerHash,
    committedTransactionsRoot: routed.plan.committedTransactionsRoot,
    l2TransactionCount: routed.plan.l2TransactionCount,
    committedTxId: routed.plan.violation.committedTxId,
    entries: routed.evidence.entries,
  }) as JournalJsonObject;
  const evidenceBinding = normalizeJournalJson({
    route: "authenticated_source_leaf",
    headerHash: routed.evidence.headerHash,
    payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
    payloadSha256: routed.evidence.payloadSha256,
    committedTransactionsRoot: routed.evidence.committedTransactionsRoot,
    l2TransactionCount: routed.evidence.l2TransactionCount.toString(),
    committedTxId: routed.plan.violation.committedTxId,
    l1BlockHash: routed.evidence.l1ChainPoint.blockHash,
    l1Slot: routed.evidence.l1ChainPoint.slot.toString(),
  }) as WorkflowEvidenceBinding;
  return await runAdmittedFraudProofWorkflow({
    deploymentFingerprint: verified.deploymentFingerprint,
    category: "daHashPreimage",
    headerHash: routed.evidence.headerHash,
    evidenceBinding,
    prepareFamilyArtifact: async () => familyArtifact,
    registry,
    journal,
    terminalVerifier,
    releaseFinality: verified.releaseFinality,
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
    ...(now === undefined ? {} : { now }),
  });
};
