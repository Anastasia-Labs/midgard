/** Verified scalars supplied by the canonical runtime adapter, never guessed durations. */
export type AvailabilityResponseAdmissionPolicy = Readonly<{
  id: string;
  publishStepMs: number;
  settleStepMs: number;
  closeStepMs: number;
  discoveryAndClockMarginMs: number;
  supportedRecoveryMs: number;
  /** The finite enforced software caps and conditional chain assumptions this policy uses. */
  envelopeId: string;
}>;

export type AvailabilityResponseAdmissionBoundary = Readonly<{
  pointId: string;
  rollbackGeneration: number;
  observedAtMs: number;
}>;

export type AvailabilityResponseObligation = Readonly<{
  headerHash: string;
  commitmentDigest: string;
  kind: "potential" | "active" | "completion";
  remainingPublications: number;
  remainingSettlements: number;
  remainingCloses: number;
  responseDeadlineMs?: number;
  responseWindowMs?: number;
}>;

export type AvailabilityResponseAdmissionInput = Readonly<{
  deploymentId: string;
  boundary: AvailabilityResponseAdmissionBoundary;
  nowMs: number;
  policy?: AvailabilityResponseAdmissionPolicy;
  blocking:
    | Readonly<{ kind: "bounded"; remainingMs: number }>
    | Readonly<{ kind: "unresolved"; reason: string }>;
  evidenceComplete: boolean;
  obligations: readonly AvailabilityResponseObligation[];
  candidate: AvailabilityResponseObligation;
}>;

export type AvailabilityResponseAdmissionDecision = Readonly<{
  status:
    | "admitted"
    | "insufficient_capacity"
    | "incomplete_evidence"
    | "unbounded_blocking";
  deploymentId: string;
  candidateHeaderHash?: string;
  candidateCommitmentDigest?: string;
  boundary?: AvailabilityResponseAdmissionBoundary;
  policyId?: string;
  envelopeId?: string;
  reason: string;
  requiredMs?: number;
  availableMs?: number;
  minimumSlackMs?: number;
  publications?: number;
  settlements?: number;
  closes?: number;
  obligations?: number;
  limitingCommitmentDigest?: string;
}>;

const natural = (value: number): boolean =>
  Number.isSafeInteger(value) && value >= 0;
const positive = (value: number): boolean => natural(value) && value > 0;
const identity = (value: string): boolean =>
  typeof value === "string" && value.length > 0;
/** Scalar validation only; the runtime owns enforcement and deployment binding. */
export const availabilityResponseAdmissionPolicyIsFinite = (
  policy: AvailabilityResponseAdmissionPolicy | undefined,
): policy is AvailabilityResponseAdmissionPolicy =>
  policy !== undefined &&
  identity(policy.id) &&
  identity(policy.envelopeId) &&
  positive(policy.publishStepMs) &&
  positive(policy.settleStepMs) &&
  positive(policy.closeStepMs) &&
  natural(policy.discoveryAndClockMarginMs) &&
  natural(policy.supportedRecoveryMs);
const safeBigInt = (value: bigint): number | undefined =>
  value >= BigInt(Number.MIN_SAFE_INTEGER) &&
  value <= BigInt(Number.MAX_SAFE_INTEGER)
    ? Number(value)
    : undefined;

/** Counts actual onchain publications, independently for each tranche. */
export const availabilityResponsePublicationCount = (
  remainingTrancheBytes: readonly number[],
  chunkByteLength: number,
): number => {
  if (
    !positive(chunkByteLength) ||
    remainingTrancheBytes.some((value) => !natural(value))
  )
    throw new Error(
      "Response publication count requires verified byte geometry",
    );
  const chunk = BigInt(chunkByteLength);
  const count = remainingTrancheBytes.reduce(
    (sum, bytes) => sum + (BigInt(bytes) + chunk - 1n) / chunk,
    0n,
  );
  const result = safeBigInt(count);
  if (result === undefined)
    throw new Error(
      "Response publication count exceeds safe integer arithmetic",
    );
  return result;
};

const sameDemand = (
  a: AvailabilityResponseObligation,
  b: AvailabilityResponseObligation,
): boolean =>
  a.headerHash === b.headerHash &&
  a.kind === b.kind &&
  a.remainingPublications === b.remainingPublications &&
  a.remainingSettlements === b.remainingSettlements &&
  a.remainingCloses === b.remainingCloses &&
  a.responseDeadlineMs === b.responseDeadlineMs &&
  a.responseWindowMs === b.responseWindowMs;

/**
 * Conservatively reserves the serial responder's total live demand under the
 * shortest allowance. Potential future Open times are unknown: an earliest
 * deadline prefix calculation alone cannot establish their future feasibility.
 * Equality is an operational refusal, not a claim about validator semantics.
 */
export const availabilityResponseAdmission = (
  input: AvailabilityResponseAdmissionInput,
): AvailabilityResponseAdmissionDecision => {
  const base = {
    deploymentId: input.deploymentId,
    candidateHeaderHash: input.candidate.headerHash,
    candidateCommitmentDigest: input.candidate.commitmentDigest,
    boundary: input.boundary,
    policyId: input.policy?.id,
    envelopeId: input.policy?.envelopeId,
  };
  const incomplete = (
    reason: string,
  ): AvailabilityResponseAdmissionDecision => ({
    ...base,
    status: "incomplete_evidence",
    reason,
  });
  if (!input.evidenceComplete)
    return incomplete("canonical_admission_evidence_incomplete");
  if (
    !/^[0-9a-f]{64}$/u.test(input.deploymentId) ||
    !identity(input.boundary.pointId) ||
    !natural(input.boundary.rollbackGeneration) ||
    !natural(input.boundary.observedAtMs) ||
    !natural(input.nowMs) ||
    input.boundary.observedAtMs > input.nowMs
  )
    return incomplete("invalid_admission_boundary");
  const policy = input.policy;
  if (!availabilityResponseAdmissionPolicyIsFinite(policy))
    return incomplete("finite_runtime_policy_unavailable");
  if (input.blocking.kind === "unresolved")
    return {
      ...base,
      status: "unbounded_blocking",
      reason: input.blocking.reason,
    };
  if (!natural(input.blocking.remainingMs))
    return incomplete("actor_blocking_allowance_unavailable");
  const jobs = new Map<string, AvailabilityResponseObligation>();
  for (const job of [...input.obligations, input.candidate]) {
    if (
      !/^[0-9a-f]{56}$/u.test(job.headerHash) ||
      !/^[0-9a-f]{64}$/u.test(job.commitmentDigest) ||
      !natural(job.remainingPublications) ||
      !natural(job.remainingSettlements) ||
      !natural(job.remainingCloses) ||
      !["potential", "active", "completion"].includes(job.kind) ||
      (job.kind === "potential" &&
        (job.responseWindowMs === undefined ||
          !positive(job.responseWindowMs))) ||
      (job.kind === "active" &&
        (job.responseDeadlineMs === undefined ||
          !natural(job.responseDeadlineMs))) ||
      (job.kind === "completion" && job.remainingPublications !== 0)
    )
      return incomplete("invalid_response_obligation");
    const prior = jobs.get(job.commitmentDigest);
    if (prior === undefined) {
      jobs.set(job.commitmentDigest, job);
      continue;
    }
    if (prior.headerHash !== job.headerHash)
      return incomplete("commitment_header_disagreement");
    // Authenticated active progress replaces the potential copy of exactly the
    // same signed promise. Duplicate active observations must agree completely.
    if (prior.kind === "potential" && job.kind !== "potential")
      jobs.set(job.commitmentDigest, job);
    else if (prior.kind !== "potential" && job.kind === "potential") continue;
    else if (!sameDemand(prior, job))
      return incomplete("commitment_progress_disagreement");
  }
  if (input.candidate.kind !== "potential")
    return incomplete("new_promise_requires_potential_obligation");
  const margin = BigInt(policy.discoveryAndClockMarginMs);
  const allowanceOf = (
    job: AvailabilityResponseObligation,
  ): bigint | undefined =>
    job.kind === "completion"
      ? undefined
      : (job.kind === "active"
          ? BigInt(job.responseDeadlineMs!) - BigInt(input.nowMs)
          : BigInt(job.responseWindowMs!)) - margin;
  const ordered = [...jobs.values()].sort((a, b) => {
    const aa = allowanceOf(a),
      bb = allowanceOf(b);
    if (aa === undefined && bb !== undefined) return 1;
    if (aa !== undefined && bb === undefined) return -1;
    if (aa !== undefined && bb !== undefined && aa !== bb)
      return aa < bb ? -1 : 1;
    return a.commitmentDigest < b.commitmentDigest
      ? -1
      : a.commitmentDigest > b.commitmentDigest
        ? 1
        : 0;
  });
  let required =
    BigInt(input.blocking.remainingMs) + BigInt(policy.supportedRecoveryMs);
  let publications = 0n,
    settlements = 0n,
    closes = 0n;
  let available: bigint | undefined,
    limitingCommitmentDigest: string | undefined;
  for (const job of ordered) {
    publications += BigInt(job.remainingPublications);
    settlements += BigInt(job.remainingSettlements);
    closes += BigInt(job.remainingCloses);
    required +=
      BigInt(job.remainingPublications) * BigInt(policy.publishStepMs) +
      BigInt(job.remainingSettlements) * BigInt(policy.settleStepMs) +
      BigInt(job.remainingCloses) * BigInt(policy.closeStepMs);
    const allowance = allowanceOf(job);
    if (
      allowance !== undefined &&
      (available === undefined || allowance < available)
    ) {
      available = allowance;
      limitingCommitmentDigest = job.commitmentDigest;
    }
  }
  if (available === undefined)
    return incomplete("response_allowance_unavailable");
  const totals = [
    required,
    available,
    available - required,
    publications,
    settlements,
    closes,
  ].map(safeBigInt);
  if (totals.some((value) => value === undefined))
    return incomplete("response_admission_arithmetic_overflow");
  return {
    ...base,
    status: required < available ? "admitted" : "insufficient_capacity",
    reason:
      required < available
        ? "conditional_serial_response_capacity"
        : "serial_response_demand_exceeds_allowance",
    requiredMs: totals[0],
    availableMs: totals[1],
    minimumSlackMs: totals[2],
    publications: totals[3],
    settlements: totals[4],
    closes: totals[5],
    obligations: jobs.size,
    limitingCommitmentDigest,
  };
};
