import type { AvailabilityResponseAdmissionPolicy } from "@al-ft/midgard-core";

import {
  type CommitteePromiseRuntimeBinding,
  type CommitteePromiseRuntimePolicyArtifact,
  committeePromiseRuntimePolicyDigest,
  verifyCommitteePromiseRuntimePolicy,
} from "../../src/availability/promise-runtime-policy.js";

/** Explicit fixture assumptions/evidence; never loaded by production factories. */
export const testPromiseRuntimePolicy = (
  policy: AvailabilityResponseAdmissionPolicy,
  binding?: Partial<CommitteePromiseRuntimeBinding>,
) => {
  const digest = "ab".repeat(32);
  const allowance = (total: number) => ({
    successfulSoftwareMs: total - 1,
    chainProgressAndObservationMs: 1,
    pollAndObservationMs: 0,
    allowedFailedAttempts: 0,
    failedAttemptAndRecoveryMs: 0,
  });
  const artifact: CommitteePromiseRuntimePolicyArtifact = {
    schemaVersion: 1,
    policyId: policy.id,
    binding: {
      deploymentFingerprint: digest,
      contractManifestId: digest,
      actorId: "cd".repeat(28),
      protocolDigest: digest,
      runtimeBuildDigest: digest,
      resourceProfileDigest: digest,
      ...binding,
    },
    enforcement: [
      "observer",
      "read",
      "unsigned_preparation",
      "poll",
      "drain",
    ].map((stage) => ({
      stage,
      implementationDigest: digest,
      refusalCapMs: 1000,
      mode: "late_result_fence" as const,
    })),
    workloadCaps: {
      retainedPayloadBytes: 10000000,
      outstandingPromises: 100,
      tranches: 100,
      publications: 10000,
      walletInputs: 100,
      journalEntries: 100,
      challengeRecords: 100,
      storeRecords: 10000,
      storeEncodedBytes: 10000000,
    },
    assumptions: {
      publish: allowance(policy.publishStepMs),
      settle: allowance(policy.settleStepMs),
      close: allowance(policy.closeStepMs),
      discoveryAndClockMarginMs: policy.discoveryAndClockMarginMs,
      aggregateOutageAndRecoveryMs: policy.supportedRecoveryMs,
      aggregateAllowedFailedAttempts: 0,
      faultModelDigest: digest,
    },
    calibrationEvidenceDigest: digest,
    validUntilMs: 1000000,
  };
  return {
    artifact,
    authority: verifyCommitteePromiseRuntimePolicy({
      artifact,
      trustedPolicyDigest: committeePromiseRuntimePolicyDigest(artifact),
      liveBinding: artifact.binding,
      installedEnforcement: artifact.enforcement,
      verifiedCalibrationEvidenceDigest: digest,
      adoptedFaultModelDigest: digest,
      now: () => 1000,
    }),
  };
};
