import {
  type CommitteePromiseCausalArtifact,
  committeePromiseCausalPolicyDigest,
  verifyCommitteePromiseCausalPolicy,
} from "../../src/availability/promise-causal-policy.js";
import type { CommitteePromiseRuntimeBinding } from "../../src/availability/promise-runtime-policy.js";

/** Explicit owner-shaped fixture; the configured factory never loads test evidence. */
export const testPromiseCausalPolicy = (
  binding?: Partial<CommitteePromiseRuntimeBinding>,
) => {
  const hash = "ab".repeat(32);
  let now = 1000,
    mono = 0,
    generation = 0;
  const artifact: CommitteePromiseCausalArtifact = {
    schemaVersion: 2,
    policyId: "private-controlled-test",
    binding: {
      deploymentFingerprint: hash,
      contractManifestId: hash,
      actorId: "cd".repeat(28),
      protocolDigest: hash,
      runtimeBuildDigest: hash,
      resourceProfileDigest: hash,
      ...binding,
    },
    sourceBinding: {
      sourceDigest: hash,
      genesisDigest: hash,
      rollbackGeneration: 0,
    },
    enforcement: Object.entries({
      cursor: 5000,
      poll: 15000,
      scheduling_lag: 2000,
      source: 10000,
      build_sign_persist: 2000,
      submit: 3000,
    }).map(([stage, refusalCapMs]) => ({
      stage,
      refusalCapMs,
      implementationDigest: hash,
      mode: "late_result_fence",
    })),
    workloadCaps: {
      retainedPayloadBytes: 72 * 28040,
      outstandingPromises: 72,
      tranches: 72,
      publications: 144,
      walletInputs: 32,
      journalEntries: 1024,
      challengeRecords: 1,
      storeRecords: 512,
      storeEncodedBytes: 8388608,
    },
    causal: {
      activeSlotProbability: { numerator: 1, denominator: 20 },
      target: { numerator: 999, denominator: 1000 },
      slotLengthMs: 1000,
      initialReadyMs: 45000,
      includedToNextReadyMs: 37000,
      expiryToReplacementReadyMs: 52000,
      minimumEligibleFutureSlots: 50,
      aggregateAllowedFailedAttempts: 5,
      maximumCurrentSchedulingPromises: 1,
    },
    faultModelDigest: hash,
    calibrationEvidenceDigest: hash,
    validUntilMs: 1000000,
  };
  const input = {
    artifact,
    trustedPolicyDigest: committeePromiseCausalPolicyDigest(artifact),
    liveBinding: artifact.binding,
    liveSourceBinding: artifact.sourceBinding,
    installedEnforcement: artifact.enforcement,
    verifiedCalibrationEvidenceDigest: hash,
    adoptedFaultModelDigest: hash,
    upperNetworkTimeMs: () => now,
    assertEpochCurrent: () => {
      if (generation !== artifact.sourceBinding.rollbackGeneration)
        throw new Error("rollback_generation_changed");
    },
    nowMs: () => now,
    monotonicMs: () => mono,
    maximumWallMonotonicDriftMs: 2000,
  };
  const workload = {
    retainedPayloadBytes: 28040,
    outstandingPromises: 1,
    tranches: 1,
    publications: 2,
    walletInputs: 1,
    journalEntries: 0,
    challengeRecords: 0,
    storeRecords: 0,
    storeEncodedBytes: 0,
  };
  return {
    artifact,
    input,
    workload,
    authority: verifyCommitteePromiseCausalPolicy(input),
    setClock: (wall: number, monotonic: number) => {
      now = wall;
      mono = monotonic;
    },
    rollback: () => {
      generation++;
    },
  };
};
