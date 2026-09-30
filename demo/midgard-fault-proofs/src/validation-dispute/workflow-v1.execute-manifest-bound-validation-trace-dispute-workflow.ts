import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { type ValidationTraceChallenge } from "../workflow/challenge-authority.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
} from "../workflow/journal.js";
import { submitCapturedTransaction } from "../workflow/transaction-boundary.js";
import {
  planValidationTraceDisputeMove,
  type ValidationTraceDisputeRetainedRouteInput,
} from "./workflow-engine.js";
import { VALIDATION_TRACE_DISPUTE_CATEGORY } from "./workflow-family.js";
import { type ManifestBoundValidationTraceDisputeWorkflow } from "./workflow-v1.create-manifest-bound-validation-trace-dispute-workflow.js";

const appendEvent = async (
  journal: FraudProofWorkflowJournalStore,
  workflowId: string,
  identity: FraudProofWorkflowIdentity,
  event: FraudProofWorkflowJournalEvent,
) => {
  const sequence = (await journal.load(workflowId)).length;
  await journal.append(
    {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence,
      recordedAt: new Date().toISOString(),
      event,
    },
    sequence,
  );
};

const preparedChallengeArtifact = (challenge: ValidationTraceChallenge) =>
  Object.freeze({
    schemaVersion: "midgard-validation-trace-dispute-prepared-v1" as const,
    challengeDigest: challenge.challengeDigest,
    claimCbor: challenge.claimCbor,
    challengerDescriptorCbor: challenge.challengerDescriptorCbor,
  });

/**
 * One chain-state-derived, locally evaluated, intent-journaled dispute move —
 * or the deliberate decision to wait on the operator's response clock.
 */
export const executeManifestBoundValidationTraceDisputeWorkflow = async ({
  workflow,
  journal,
}: {
  workflow: ManifestBoundValidationTraceDisputeWorkflow;
  journal: FraudProofWorkflowJournalStore;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const challenge = workflow.challenge;
  const material = workflow.material;
  if (challenge === undefined || material === undefined)
    throw new Error(
      "validationTraceDispute execution requires the freshly admitted validation-trace challenge; construction without one serves startup readiness only",
    );
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: VALIDATION_TRACE_DISPUTE_CATEGORY,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const preparedArtifact = preparedChallengeArtifact(challenge);
  const artifactDigest = journalJsonDigest(preparedArtifact);
  let entries = await journal.load(workflowId);
  if (entries.length === 0) {
    await appendEvent(journal, workflowId, identity, { kind: "started" });
    await appendEvent(journal, workflowId, identity, {
      kind: "prepared",
      artifact: preparedArtifact,
      artifactDigest,
    });
    entries = await journal.load(workflowId);
  }
  if (entries.length === 1) {
    await appendEvent(journal, workflowId, identity, {
      kind: "prepared",
      artifact: preparedArtifact,
      artifactDigest,
    });
    entries = await journal.load(workflowId);
  }
  const prepared = entries.find(({ event }) => event.kind === "prepared");
  if (
    prepared?.event.kind !== "prepared" ||
    prepared.event.artifactDigest !== artifactDigest
  )
    throw new Error(
      "validationTraceDispute journal was prepared for a different admitted challenge",
    );
  const pending = [...entries]
    .reverse()
    .find(({ event }) => event.kind === "submission_intent");
  const intent =
    pending?.event.kind === "submission_intent" ? pending.event : undefined;
  if (
    intent !== undefined &&
    !entries.some(
      ({ event }) =>
        event.kind === "confirmed" && event.actionId === intent.actionId,
    )
  ) {
    if (
      !(await workflow.l1.transactionConfirmed({
        headerHash,
        txHash: intent.txHash,
      }))
    )
      return { kind: "pending" as const, workflowId, txHash: intent.txHash };
    await appendEvent(journal, workflowId, identity, {
      kind: "reconciled",
      actionId: intent.actionId,
      txHash: intent.txHash,
      outcome: "confirmed",
    });
    await appendEvent(journal, workflowId, identity, {
      kind: "confirmed",
      actionId: intent.actionId,
      txHash: intent.txHash,
    });
  }
  const now = Date.now();
  const stage = await workflow.deriveStage(now);
  const retainedInput =
    intent !== undefined &&
    typeof intent.actionInput === "object" &&
    intent.actionInput !== null &&
    "durableRouteInput" in intent.actionInput
      ? ((
          intent.actionInput as {
            durableRouteInput?: ValidationTraceDisputeRetainedRouteInput;
          }
        ).durableRouteInput ?? undefined)
      : undefined;
  const move = planValidationTraceDisputeMove({
    stage,
    ...(retainedInput === undefined ? {} : { retained: retainedInput }),
  });
  if (move.kind === "completed")
    return { kind: "completed" as const, workflowId };
  if (move.kind === "await_counterparty")
    return {
      kind: "awaiting_counterparty" as const,
      workflowId,
      responseDeadline: move.responseDeadline,
    };
  const captured = await workflow.actuator.capture({
    action: move.action,
    material,
    ...(retainedInput === undefined ? {} : { retained: retainedInput }),
  });
  const actionId = `${move.action.stage}:${captured.transaction.txHash}`;
  await appendEvent(journal, workflowId, identity, {
    kind: "preflight_passed",
    actionId,
    txHash: captured.transaction.txHash,
    localEvaluator: "lucid-evolution-local-uplc-v1",
    referenceScripts: captured.transaction.referenceScripts,
  });
  await appendEvent(journal, workflowId, identity, {
    kind: "submission_intent",
    actionId,
    actionInput: {
      schemaVersion: "midgard-validation-trace-dispute-action-v1",
      stage: move.action.stage,
      challengeDigest: challenge.challengeDigest,
      ...(captured.durableRouteInput === undefined
        ? {}
        : { durableRouteInput: captured.durableRouteInput }),
    },
    ...(captured.mutationLease === undefined
      ? {}
      : {
          durableRecovery: {
            stateQueueMutationLease: {
              token: captured.mutationLease.token,
              source: captured.mutationLease.source,
            },
          },
        }),
    attempt: 1,
    txHash: captured.transaction.txHash,
  });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("validationTraceDispute provider substituted transaction");
  await appendEvent(journal, workflowId, identity, {
    kind: "submitted",
    actionId,
    attempt: 1,
    txHash: submitted,
  });
  return { kind: "pending" as const, workflowId, txHash: submitted };
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
    await assertValidationTraceDisputeChallengeCurrent(input);
    return await executeManifestBoundValidationTraceDisputeWorkflow({
      workflow: input.workflow,
      journal: input.journal,
    });
  };
