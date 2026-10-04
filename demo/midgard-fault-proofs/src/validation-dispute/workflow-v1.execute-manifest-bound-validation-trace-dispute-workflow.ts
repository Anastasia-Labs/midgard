import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  workflowActuationAuthorizingDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "../workflow/actuation-permit.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import {
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
} from "../workflow/journal.js";
import { verifiedReleaseFinality } from "../workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import {
  createFraudProofWorkflowRegistry,
  resumeRecordedFraudProofWorkflow,
} from "../workflow/orchestrator.js";
import { runAdmittedFraudProofWorkflow } from "../workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { VALIDATION_TRACE_DISPUTE_CATEGORY } from "./workflow-family.js";
import { type ManifestBoundValidationTraceDisputeWorkflow } from "./workflow-v1.create-manifest-bound-validation-trace-dispute-workflow.js";
import {
  preparedValidationTraceChallengeArtifact,
  VALIDATION_TRACE_COUNTERPARTY_WAIT,
} from "./workflow-v1.recovery-adapter.js";

/** Run the interactive grammar through the common funding and recovery lifecycle. */
export const executeManifestBoundValidationTraceDisputeWorkflow = async ({
  workflow,
  journal,
}: {
  workflow: ManifestBoundValidationTraceDisputeWorkflow;
  journal: FraudProofWorkflowJournalStore;
}) => {
  const { binding, adapter, terminalVerifier, releaseFinalityAuthority } =
    workflow;
  const category = VALIDATION_TRACE_DISPUTE_CATEGORY;
  const headerHash = binding.definition.headerHash;
  const authorizingDecision =
    workflowActuationAuthorizingDecisionDigest(journal);
  if (
    authorizingDecision !== undefined &&
    authorizingDecision !== workflow.decisionDigest
  )
    throw new Error(
      "validationTraceDispute journal changed its authorizing decision",
    );
  if (workflowJournalIsReconciliationOnly(journal)) {
    const recovered = await resumeRecordedFraudProofWorkflow({
      deploymentFingerprint: binding.deploymentFingerprint,
      category,
      headerHash,
      journal,
      adapter,
      terminalVerifier,
      releaseFinalityAuthority,
    });
    if (!("workflowId" in recovered))
      throw new Error(
        "validationTraceDispute recovery lost its admitted workflow",
      );
    return recovered;
  }
  const challenge = workflow.challenge;
  if (challenge === undefined || workflow.material === undefined)
    throw new Error(
      "validationTraceDispute execution requires the freshly admitted validation-trace challenge; construction without one serves startup readiness only",
    );
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash,
  });
  const artifact = preparedValidationTraceChallengeArtifact(challenge);
  const verified = await verifiedReleaseFinality({
    deploymentFingerprint: binding.deploymentFingerprint,
    authority: releaseFinalityAuthority,
  });
  const result = await runAdmittedFraudProofWorkflow({
    deploymentFingerprint: binding.deploymentFingerprint,
    category,
    headerHash,
    evidenceBinding: {
      route: "canonical_block",
      headerHash,
      payloadEnvelopeSha256: challenge.coordinate.payloadEnvelopeSha256,
      payloadSha256: challenge.coordinate.payloadSha256,
      l1BlockHash: observation.chainPoint.blockHash,
      l1Slot: observation.chainPoint.slot.toString(),
    },
    prepareFamilyArtifact: async () => artifact,
    validateFamilyArtifact: async (_adapter, recorded) => {
      if (journalJsonDigest(recorded) !== journalJsonDigest(artifact))
        throw new Error(
          "validationTraceDispute journal was prepared for a different admitted challenge",
        );
    },
    registry: createFraudProofWorkflowRegistry({
      adapters: [adapter],
      launchScope: [category],
    }),
    journal,
    terminalVerifier,
    releaseFinality: verified.releaseFinality,
  });
  if (!("workflowId" in result))
    throw new Error("validationTraceDispute runner lost its admitted workflow");
  if (
    result.kind === "pending" &&
    result.reason.startsWith(VALIDATION_TRACE_COUNTERPARTY_WAIT)
  )
    return {
      ...result,
      kind: "awaiting_counterparty" as const,
      responseDeadline: Number(
        result.reason.slice(VALIDATION_TRACE_COUNTERPARTY_WAIT.length),
      ),
    };
  return result;
};

/**
 * The admitted challenge is root-bound to the freshly authenticated canonical
 * block for this header: re-fetch the retained payload and require exact
 * payload identity before actuating. A challenge-free construction reaches
 * execution, which fail-closes with the precise requirement.
 */
export const assertValidationTraceDisputeChallengeCurrent = async ({
  workflow,
  sources,
}: {
  workflow: ManifestBoundValidationTraceDisputeWorkflow;
  sources: readonly RetainedDaPayloadSource[];
}): Promise<void> => {
  if (workflow.challenge === undefined) return;
  const block = await fetchCanonicalBlockEvidence({
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
  });
  if (
    workflow.challenge.coordinate.payloadEnvelopeSha256 !==
      block.payloadEnvelopeSha256 ||
    workflow.challenge.coordinate.payloadSha256 !== block.payloadSha256
  )
    throw new Error(
      "validationTraceDispute challenge diverged from the authenticated canonical block",
    );
};

/**
 * The launch route the generic runner drives: the uniform
 * `{workflow, sources, journal}` input every family takes, asserting the
 * challenge against the retained canonical block before the one move.
 */
export const runOrResumeManifestBoundValidationTraceDisputeWorkflow =
  async (input: {
    workflow: ManifestBoundValidationTraceDisputeWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
      throw new Error(
        "validationTraceDispute runner rejects caller-authored evidence",
      );
    if (!workflowJournalIsReconciliationOnly(input.journal))
      await assertValidationTraceDisputeChallengeCurrent(input);
    return await executeManifestBoundValidationTraceDisputeWorkflow({
      workflow: input.workflow,
      journal: input.journal,
    });
  };
