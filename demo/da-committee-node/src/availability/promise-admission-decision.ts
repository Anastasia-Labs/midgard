import {
  availabilityResponseAdmission,
  type AvailabilityResponseAdmissionDecision,
  type AvailabilityResponseObligation,
} from "@al-ft/midgard-core";

import type { CommitteePromiseAdmissionSnapshot } from "./promise-admission.js";
import type {
  CommitteePromiseRuntimePolicyAuthority,
  CommitteePromiseWorkload,
} from "./promise-runtime-policy.js";

export const committeePromisePolicyEnvelope = (
  authority: CommitteePromiseRuntimePolicyAuthority,
  workload: CommitteePromiseWorkload,
) =>
  authority.causal
    ? authority.envelope?.(workload)
    : authority.policy(workload);

/** The configured causal policy consumes both sets without converting its
 * probability model into fabricated deterministic per-step allowances. */
export const committeePromiseAdmissionDecision = (
  input: Readonly<{
    authority: CommitteePromiseRuntimePolicyAuthority;
    workload: CommitteePromiseWorkload;
    snapshot: CommitteePromiseAdmissionSnapshot;
    deploymentId: string;
    nowMs: number;
    obligations: readonly AvailabilityResponseObligation[];
    currentProgress: readonly AvailabilityResponseObligation[];
    candidate: AvailabilityResponseObligation;
  }>,
): AvailabilityResponseAdmissionDecision => {
  const { authority, snapshot } = input;
  if (!authority.causal)
    return availabilityResponseAdmission({
      deploymentId: input.deploymentId,
      boundary: snapshot.boundary,
      nowMs: input.nowMs,
      policy: authority.policy(input.workload),
      blocking: snapshot.blocking,
      evidenceComplete: snapshot.complete,
      obligations: input.obligations,
      candidate: input.candidate,
    });
  const envelope = authority.envelope?.(input.workload);
  const base = {
    deploymentId: input.deploymentId,
    boundary: snapshot.boundary,
    candidateHeaderHash: input.candidate.headerHash,
    candidateCommitmentDigest: input.candidate.commitmentDigest,
    policyId: envelope?.id,
    envelopeId: envelope?.envelopeId,
  };
  const incomplete = (
    reason: string,
  ): AvailabilityResponseAdmissionDecision => ({
    ...base,
    status: "incomplete_evidence",
    reason,
  });
  if (
    !envelope ||
    !snapshot.complete ||
    !snapshot.currentSchedulingCommitmentDigests ||
    !snapshot.boundary.schedulingEvidenceDigest ||
    !snapshot.boundary.actorStateDigest ||
    !snapshot.retainedAttempts ||
    !Number.isSafeInteger(snapshot.boundary.observedAtMs) ||
    snapshot.boundary.observedAtMs > input.nowMs
  )
    return incomplete("causal_source_certificate_unavailable");
  if (snapshot.blocking.kind === "unresolved")
    return {
      ...base,
      status: "unbounded_blocking",
      reason: snapshot.blocking.reason,
    };
  if (snapshot.blocking.remainingMs !== 0)
    return incomplete("causal_actor_blocking_outside_measured_domain");
  const full = new Map(
    input.obligations.map((job) => [job.commitmentDigest, job]),
  );
  full.set(input.candidate.commitmentDigest, input.candidate);
  const failures = new Map<string, number>();
  for (const job of full.values()) {
    const failed = snapshot.retainedAttempts.filter(
      (attempt) =>
        attempt.headerHash === job.headerHash && attempt.state === "expired",
    );
    if (
      failed.some(
        (attempt) => !["publish", "settle", "close"].includes(attempt.action),
      )
    )
      return incomplete("durable_response_failure_action_ambiguous");
    failures.set(job.commitmentDigest, failed.length);
  }
  const result = authority.causal.evaluate({
    fullProtectedObligations: [...full.values()],
    currentCommitmentDigests: snapshot.currentSchedulingCommitmentDigests,
    currentProgress: input.currentProgress,
    candidate: input.candidate,
    usedFailedAttempts: failures,
  });
  return {
    ...base,
    status:
      result.status === "sufficient"
        ? "admitted"
        : result.status === "insufficient"
          ? "insufficient_capacity"
          : "incomplete_evidence",
    reason: result.reason,
    obligations: result.protectedPromises,
    limitingCommitmentDigest: result.limitingCommitmentDigest,
  };
};
