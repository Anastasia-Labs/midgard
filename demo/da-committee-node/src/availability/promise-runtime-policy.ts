import { createHash } from "node:crypto";

import type { AvailabilityResponseAdmissionPolicy } from "@al-ft/midgard-core";
import { canonicalJson } from "@al-ft/midgard-core/canonical-json";

import type { CommitteePromiseCausalAuthority } from "./promise-causal-policy.js";

/** Success allowances are adopted assumptions, distinct from refusal caps. */
export type CommitteePromiseActionAllowance = Readonly<{
  successfulSoftwareMs: number;
  chainProgressAndObservationMs: number;
  pollAndObservationMs: number;
  allowedFailedAttempts: number;
  failedAttemptAndRecoveryMs: number;
}>;
export type CommitteePromiseRuntimeBinding = Readonly<{
  deploymentFingerprint: string;
  contractManifestId: string;
  actorId: string;
  protocolDigest: string;
  runtimeBuildDigest: string;
  resourceProfileDigest: string;
}>;
export type CommitteePromiseWorkload = Readonly<{
  retainedPayloadBytes: number;
  outstandingPromises: number;
  tranches: number;
  publications: number;
  walletInputs: number;
  journalEntries: number;
  challengeRecords: number;
  storeRecords: number;
  storeEncodedBytes: number;
}>;
export type CommitteePromiseStageEnforcement = Readonly<{
  stage: string;
  implementationDigest: string;
  refusalCapMs: number;
  /** The installed adapter reports this; artifact declarations do not suffice. */
  mode: "abort_and_fence" | "late_result_fence";
}>;
export type CommitteePromiseRuntimePolicyArtifact = Readonly<{
  schemaVersion: 1;
  policyId: string;
  binding: CommitteePromiseRuntimeBinding;
  enforcement: readonly CommitteePromiseStageEnforcement[];
  workloadCaps: CommitteePromiseWorkload;
  assumptions: Readonly<{
    publish: CommitteePromiseActionAllowance;
    settle: CommitteePromiseActionAllowance;
    close: CommitteePromiseActionAllowance;
    discoveryAndClockMarginMs: number;
    aggregateOutageAndRecoveryMs: number;
    /** Additional failed attempts across the entire promise, charged once. */
    aggregateAllowedFailedAttempts: number;
    /** Identifies the explicitly adopted bounded contention/outage model. */
    faultModelDigest: string;
  }>;
  calibrationEvidenceDigest: string;
  /** Freshness is absolute; an expired or breached policy never renews itself. */
  validUntilMs: number;
}>;

const canonical = (value: unknown): string =>
  canonicalJson(value, "committee promise runtime policy");
export const committeePromiseRuntimePolicyDigest = (
  artifact: CommitteePromiseRuntimePolicyArtifact,
): string => createHash("sha256").update(canonical(artifact)).digest("hex");
const hash = (value: string): boolean => /^[0-9a-f]{64}$/u.test(value);
const natural = (value: number): boolean =>
  Number.isSafeInteger(value) && value >= 0;
const positive = (value: number): boolean => natural(value) && value > 0;
const workloadKeys = [
  "retainedPayloadBytes",
  "outstandingPromises",
  "tranches",
  "publications",
  "walletInputs",
  "journalEntries",
  "challengeRecords",
  "storeRecords",
  "storeEncodedBytes",
] as const;
const actionTime = (value: CommitteePromiseActionAllowance): number => {
  if (
    !positive(value.successfulSoftwareMs) ||
    !positive(value.chainProgressAndObservationMs) ||
    !natural(value.pollAndObservationMs) ||
    !natural(value.allowedFailedAttempts) ||
    !natural(value.failedAttemptAndRecoveryMs) ||
    (value.allowedFailedAttempts > 0 && value.failedAttemptAndRecoveryMs === 0)
  )
    throw new Error("Successful action and retry assumptions are incomplete");
  const sum =
    BigInt(value.successfulSoftwareMs) +
    BigInt(value.chainProgressAndObservationMs) +
    BigInt(value.pollAndObservationMs) +
    BigInt(value.allowedFailedAttempts) *
      BigInt(value.failedAttemptAndRecoveryMs);
  if (sum > BigInt(Number.MAX_SAFE_INTEGER))
    throw new Error("Action allowance overflows");
  return Number(sum);
};
export type CommitteePromiseRuntimePolicyStatus =
  | Readonly<{ status: "unavailable"; reason: string }>
  | Readonly<{ status: "conditional"; policyId: string; envelopeId: string }>;
export type CommitteePromiseRuntimePolicyAuthority = Readonly<{
  binding?: CommitteePromiseRuntimeBinding;
  causal?: CommitteePromiseCausalAuthority;
  envelope?: (
    workload: CommitteePromiseWorkload,
  ) => Readonly<{ id: string; envelopeId: string }> | undefined;
  status: () => CommitteePromiseRuntimePolicyStatus;
  /** Full restorable demand, including each explicitly adopted failed attempt. */
  futureIntentRows?: (workload: CommitteePromiseWorkload) => number | undefined;
  policy: (
    workload: CommitteePromiseWorkload,
  ) => AvailabilityResponseAdmissionPolicy | undefined;
  /** Sticky for this adopted capability: healthy polling cannot clear a breach. */
  breach: (reason: string) => void;
}>;

/** Runtime-only adoption: trusted configuration pins the complete artifact. */
export const verifyCommitteePromiseRuntimePolicy = (
  input: Readonly<{
    artifact: CommitteePromiseRuntimePolicyArtifact;
    trustedPolicyDigest: string;
    liveBinding: CommitteePromiseRuntimeBinding;
    installedEnforcement: readonly CommitteePromiseStageEnforcement[];
    /** Evidence must actually be loaded/verified by the runtime owner. */
    verifiedCalibrationEvidenceDigest: string;
    adoptedFaultModelDigest: string;
    now?: () => number;
    monotonicMs?: () => number;
    maxWallClockDriftMs?: number;
  }>,
): CommitteePromiseRuntimePolicyAuthority => {
  const now = input.now ?? Date.now;
  const monotonic = input.monotonicMs ?? (() => performance.now());
  const initialWall = now();
  const initialMonotonic = monotonic();
  let unavailable: string | undefined;
  let scalar: AvailabilityResponseAdmissionPolicy | undefined;
  let caps: CommitteePromiseWorkload | undefined;
  let attempts:
    | Readonly<{
        publish: number;
        settle: number;
        close: number;
        aggregate: number;
      }>
    | undefined;
  let validUntilMs = 0;
  let binding: CommitteePromiseRuntimeBinding | undefined;
  try {
    const artifact = input.artifact;
    const envelopeId = committeePromiseRuntimePolicyDigest(artifact);
    if (
      !hash(input.trustedPolicyDigest) ||
      envelopeId !== input.trustedPolicyDigest
    )
      throw new Error("trusted_policy_digest_mismatch");
    if (
      artifact.schemaVersion !== 1 ||
      !artifact.policyId ||
      canonical(artifact.binding) !== canonical(input.liveBinding) ||
      !Object.values(artifact.binding).every(
        (value) => typeof value === "string" && value.length > 0,
      ) ||
      !hash(artifact.binding.deploymentFingerprint) ||
      !hash(artifact.binding.contractManifestId) ||
      !/^[0-9a-f]{56}$/u.test(artifact.binding.actorId) ||
      !hash(artifact.binding.protocolDigest) ||
      !hash(artifact.binding.runtimeBuildDigest) ||
      !hash(artifact.binding.resourceProfileDigest)
    )
      throw new Error("runtime_policy_binding_mismatch");
    if (
      !hash(artifact.calibrationEvidenceDigest) ||
      artifact.calibrationEvidenceDigest !==
        input.verifiedCalibrationEvidenceDigest ||
      !hash(artifact.assumptions.faultModelDigest) ||
      artifact.assumptions.faultModelDigest !== input.adoptedFaultModelDigest
    )
      throw new Error("runtime_policy_evidence_unavailable");
    const stages = new Set(artifact.enforcement.map((item) => item.stage));
    if (
      stages.size !== artifact.enforcement.length ||
      !["observer", "read", "unsigned_preparation", "poll", "drain"].every(
        (stage) => stages.has(stage),
      ) ||
      artifact.enforcement.some(
        (item) =>
          !hash(item.implementationDigest) ||
          !positive(item.refusalCapMs) ||
          !["abort_and_fence", "late_result_fence"].includes(item.mode),
      ) ||
      canonical(artifact.enforcement) !== canonical(input.installedEnforcement)
    )
      throw new Error("runtime_policy_enforcement_unavailable");
    if (
      Object.keys(artifact.workloadCaps).length !== workloadKeys.length ||
      !workloadKeys.every((key) => positive(artifact.workloadCaps[key])) ||
      !positive(artifact.validUntilMs) ||
      !natural(artifact.assumptions.discoveryAndClockMarginMs) ||
      !natural(artifact.assumptions.aggregateOutageAndRecoveryMs) ||
      !natural(artifact.assumptions.aggregateAllowedFailedAttempts) ||
      (artifact.assumptions.aggregateAllowedFailedAttempts > 0 &&
        artifact.assumptions.aggregateOutageAndRecoveryMs === 0)
    )
      throw new Error("runtime_policy_domain_incomplete");
    binding = Object.freeze({ ...artifact.binding });
    validUntilMs = artifact.validUntilMs;
    caps = { ...artifact.workloadCaps };
    attempts = Object.freeze({
      publish: artifact.assumptions.publish.allowedFailedAttempts + 1,
      settle: artifact.assumptions.settle.allowedFailedAttempts + 1,
      close: artifact.assumptions.close.allowedFailedAttempts + 1,
      aggregate: artifact.assumptions.aggregateAllowedFailedAttempts,
    });
    scalar = Object.freeze({
      id: artifact.policyId,
      envelopeId,
      publishStepMs: actionTime(artifact.assumptions.publish),
      settleStepMs: actionTime(artifact.assumptions.settle),
      closeStepMs: actionTime(artifact.assumptions.close),
      discoveryAndClockMarginMs: artifact.assumptions.discoveryAndClockMarginMs,
      supportedRecoveryMs: artifact.assumptions.aggregateOutageAndRecoveryMs,
    });
  } catch (error) {
    unavailable =
      error instanceof Error ? error.message : "runtime_policy_unavailable";
  }
  const status = (): CommitteePromiseRuntimePolicyStatus => {
    const currentTime = now();
    const currentMonotonic = monotonic();
    if (
      !unavailable &&
      (!natural(currentTime) ||
        currentTime >= validUntilMs ||
        !Number.isFinite(initialMonotonic) ||
        !Number.isFinite(currentMonotonic) ||
        currentMonotonic < initialMonotonic ||
        currentMonotonic - initialMonotonic >= validUntilMs - initialWall)
    )
      unavailable = "runtime_policy_expired";
    if (
      !unavailable &&
      input.maxWallClockDriftMs !== undefined &&
      (!natural(input.maxWallClockDriftMs) ||
        Math.abs(
          currentTime - initialWall - (currentMonotonic - initialMonotonic),
        ) > input.maxWallClockDriftMs)
    )
      unavailable = "runtime_policy_clock_drift";
    return unavailable || scalar === undefined
      ? {
          status: "unavailable",
          reason: unavailable ?? "runtime_policy_unavailable",
        }
      : {
          status: "conditional",
          policyId: scalar.id,
          envelopeId: scalar.envelopeId,
        };
  };
  return Object.freeze({
    binding,
    status,
    futureIntentRows: (workload: CommitteePromiseWorkload) => {
      if (
        status().status !== "conditional" ||
        attempts === undefined ||
        !natural(workload.publications) ||
        !natural(workload.tranches) ||
        !natural(workload.outstandingPromises)
      )
        return undefined;
      const rows =
        BigInt(workload.publications) * BigInt(attempts.publish) +
        BigInt(workload.tranches) * BigInt(attempts.settle) +
        BigInt(workload.outstandingPromises) *
          (BigInt(attempts.close) + BigInt(attempts.aggregate));
      return rows <= BigInt(Number.MAX_SAFE_INTEGER) ? Number(rows) : undefined;
    },
    policy: (workload: CommitteePromiseWorkload) => {
      if (status().status !== "conditional" || caps === undefined)
        return undefined;
      if (
        !Object.entries(caps).every(
          ([key, maximum]) =>
            natural(workload[key as keyof CommitteePromiseWorkload]) &&
            workload[key as keyof CommitteePromiseWorkload] <= maximum,
        )
      )
        return undefined;
      return scalar;
    },
    breach: (reason: string) => {
      unavailable = `runtime_policy_breached:${reason}`;
    },
  });
};
