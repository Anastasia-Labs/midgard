import type { AvailabilityResponseObligation } from "@al-ft/midgard-core";

import {
  type CommitteePromiseCausalModel,
  committeePromiseCausalProbability,
} from "./promise-causal-model.js";

type Result = Readonly<{
  status: "sufficient" | "insufficient" | "unknown";
  reason: string;
  protectedPromises: number;
  schedulingPromises: number;
  minimumSuccessProbability?: number;
  limitingCommitmentDigest?: string;
}>;
const natural = (value: number): boolean =>
  Number.isSafeInteger(value) && value >= 0;

/** Conditional timing arithmetic only. The runtime must separately verify the
 * complete scheduling certificate, durable failures, clock and full capital /
 * resource allocation. This result is never a signing or release authority. */
export const evaluateCommitteePromiseTiming = (
  input: Readonly<{
    model: CommitteePromiseCausalModel;
    target: Readonly<{ numerator: number; denominator: number }>;
    slotLengthMs: number;
    upperNetworkTimeMs: number;
    maximumCurrentSchedulingPromises: number;
    fullProtectedObligations: readonly AvailabilityResponseObligation[];
    currentCommitmentDigests: ReadonlySet<string>;
    currentProgress: readonly AvailabilityResponseObligation[];
    candidate: AvailabilityResponseObligation;
    usedFailedAttempts: ReadonlyMap<string, number>;
  }>,
): Result => {
  const full = new Map<string, AvailabilityResponseObligation>();
  const current = new Map<string, AvailabilityResponseObligation>();
  const result = (status: Result["status"], reason: string): Result => ({
    status,
    reason,
    protectedPromises: full.size,
    schedulingPromises: current.size,
  });
  if (
    !natural(input.target.numerator) ||
    !natural(input.target.denominator) ||
    input.target.numerator === 0 ||
    input.target.numerator > input.target.denominator ||
    !natural(input.slotLengthMs) ||
    input.slotLengthMs === 0 ||
    !natural(input.upperNetworkTimeMs) ||
    input.maximumCurrentSchedulingPromises !== 1 ||
    input.candidate.kind !== "potential"
  )
    return result("unknown", "conditional_timing_domain_unavailable");
  for (const job of input.fullProtectedObligations) {
    if (full.has(job.commitmentDigest))
      return result("unknown", "protected_commitment_duplicate");
    full.set(job.commitmentDigest, job);
  }
  const prior = full.get(input.candidate.commitmentDigest);
  if (prior && prior.headerHash !== input.candidate.headerHash)
    return result("unknown", "candidate_header_disagreement");
  if (!prior) full.set(input.candidate.commitmentDigest, input.candidate);
  for (const digest of input.currentCommitmentDigests) {
    const job = full.get(digest);
    if (!job) return result("unknown", "current_commitment_not_protected");
    current.set(digest, job);
  }
  current.set(input.candidate.commitmentDigest, input.candidate);
  const progressed = new Set<string>();
  for (const job of input.currentProgress) {
    // All authenticated active challenges consume timing capacity, including
    // commitments without this node's own persisted signature.
    const covered = full.get(job.commitmentDigest);
    if (
      !covered ||
      covered.headerHash !== job.headerHash ||
      job.kind === "potential" ||
      progressed.has(job.commitmentDigest)
    )
      return result("unknown", "current_progress_not_covered");
    progressed.add(job.commitmentDigest);
    current.set(job.commitmentDigest, job);
  }
  if (current.size > input.maximumCurrentSchedulingPromises)
    return result("insufficient", "current_scheduling_slots_exhausted");
  let minimum = 1,
    limitingCommitmentDigest: string | undefined;
  for (const job of current.values()) {
    const used = input.usedFailedAttempts.get(job.commitmentDigest);
    if (
      used === undefined ||
      !natural(used) ||
      ![
        job.remainingPublications,
        job.remainingSettlements,
        job.remainingCloses,
      ].every(natural) ||
      !["potential", "active", "completion"].includes(job.kind) ||
      !/^[0-9a-f]{56}$/u.test(job.headerHash) ||
      !/^[0-9a-f]{64}$/u.test(job.commitmentDigest)
    )
      return result(
        "unknown",
        "actual_prefix_or_durable_failure_evidence_unavailable",
      );
    const availableMs =
      job.kind === "potential"
        ? job.responseWindowMs
        : job.responseDeadlineMs === undefined
          ? undefined
          : job.responseDeadlineMs - input.upperNetworkTimeMs;
    if (availableMs === undefined || !Number.isSafeInteger(availableMs))
      return result("unknown", "actual_response_horizon_unavailable");
    if (availableMs <= 0)
      return result("insufficient", "actual_response_horizon_exhausted");
    try {
      const probability = committeePromiseCausalProbability({
        model: input.model,
        remainingActions:
          job.remainingPublications +
          job.remainingSettlements +
          job.remainingCloses,
        remainingHorizonSlots: Math.floor(availableMs / input.slotLengthMs),
        usedFailedAttempts: used,
      }).successProbability;
      if (probability < minimum) {
        minimum = probability;
        limitingCommitmentDigest = job.commitmentDigest;
      }
    } catch {
      return result(
        "unknown",
        "conditional_prefix_model_outside_supported_domain",
      );
    }
  }
  return {
    ...result(
      minimum >= input.target.numerator / input.target.denominator
        ? "sufficient"
        : "insufficient",
      "conditional_current_prefix_probability",
    ),
    minimumSuccessProbability: minimum,
    limitingCommitmentDigest,
  };
};
