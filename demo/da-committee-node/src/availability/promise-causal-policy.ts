import { createHash } from "node:crypto";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";

import { evaluateCommitteePromiseTiming } from "./promise-causal-timing.js";
import type {
  CommitteePromiseRuntimeBinding,
  CommitteePromiseRuntimePolicyAuthority,
  CommitteePromiseStageEnforcement,
  CommitteePromiseWorkload,
} from "./promise-runtime-policy.js";

export type CommitteePromiseCausalArtifact = Readonly<{
  schemaVersion: 2;
  policyId: string;
  binding: CommitteePromiseRuntimeBinding;
  sourceBinding: Readonly<{
    sourceDigest: string;
    genesisDigest: string;
    rollbackGeneration: number;
  }>;
  enforcement: readonly CommitteePromiseStageEnforcement[];
  workloadCaps: CommitteePromiseWorkload;
  causal: Readonly<{
    activeSlotProbability: Readonly<{ numerator: number; denominator: number }>;
    target: Readonly<{ numerator: number; denominator: number }>;
    slotLengthMs: number;
    initialReadyMs: number;
    includedToNextReadyMs: number;
    expiryToReplacementReadyMs: number;
    minimumEligibleFutureSlots: number;
    aggregateAllowedFailedAttempts: number;
    maximumCurrentSchedulingPromises: number;
  }>;
  /** Conditional controlled-chain assumptions, not a deterministic ledger SLA. */
  faultModelDigest: string;
  calibrationEvidenceDigest: string;
  validUntilMs: number;
}>;
export type CommitteePromiseCausalAuthority = Readonly<{
  minimumEligibleFutureSlots: number;
  aggregateAllowedFailedAttempts: number;
  upperNetworkTimeMs: () => number;
  evaluate: (
    input: Pick<
      Parameters<typeof evaluateCommitteePromiseTiming>[0],
      | "fullProtectedObligations"
      | "currentCommitmentDigests"
      | "currentProgress"
      | "candidate"
      | "usedFailedAttempts"
    >,
  ) => ReturnType<typeof evaluateCommitteePromiseTiming>;
}>;
const canonical = (value: unknown) =>
  canonicalJson(value, "committee causal runtime policy");
export const committeePromiseCausalPolicyDigest = (
  artifact: CommitteePromiseCausalArtifact,
): string => createHash("sha256").update(canonical(artifact)).digest("hex");
const natural = (value: number) => Number.isSafeInteger(value) && value >= 0;
const hash = (value: string) => /^[0-9a-f]{64}$/u.test(value);

/** Only explicitly loaded owner evidence can adopt this controlled profile.
 * Deadline caps fence refusal; finite success and chain progress remain the
 * named measured/adopted assumptions. No production defaults are inferred. */
export const verifyCommitteePromiseCausalPolicy = (
  input: Readonly<{
    artifact: CommitteePromiseCausalArtifact;
    trustedPolicyDigest: string;
    liveBinding: CommitteePromiseRuntimeBinding;
    liveSourceBinding: CommitteePromiseCausalArtifact["sourceBinding"];
    installedEnforcement: readonly CommitteePromiseStageEnforcement[];
    verifiedCalibrationEvidenceDigest: string;
    adoptedFaultModelDigest: string;
    /** This derives from fresh genesis + wall/monotonic clock authority. */
    upperNetworkTimeMs: () => number;
    /** Synchronous epoch check; fresh source fences update the observed epoch. */
    assertEpochCurrent: () => void;
    nowMs?: () => number;
    monotonicMs?: () => number;
    maximumWallMonotonicDriftMs: number;
  }>,
): CommitteePromiseRuntimePolicyAuthority => {
  const now = input.nowMs ?? Date.now;
  const mono = input.monotonicMs ?? (() => performance.now());
  const initialWall = now(),
    initialMono = mono();
  let fault: string | undefined;
  let adopted: CommitteePromiseCausalArtifact | undefined;
  let envelopeId = "";
  try {
    const artifact = input.artifact;
    envelopeId = committeePromiseCausalPolicyDigest(artifact);
    if (
      !hash(input.trustedPolicyDigest) ||
      envelopeId !== input.trustedPolicyDigest
    )
      throw new Error("trusted_causal_policy_digest_mismatch");
    if (
      artifact.schemaVersion !== 2 ||
      !artifact.policyId ||
      canonical(artifact.binding) !== canonical(input.liveBinding) ||
      !hash(artifact.binding.deploymentFingerprint) ||
      !hash(artifact.binding.contractManifestId) ||
      !/^[0-9a-f]{56}$/u.test(artifact.binding.actorId) ||
      !hash(artifact.binding.protocolDigest) ||
      !hash(artifact.binding.runtimeBuildDigest) ||
      !hash(artifact.binding.resourceProfileDigest) ||
      canonical(artifact.sourceBinding) !==
        canonical(input.liveSourceBinding) ||
      !hash(artifact.sourceBinding.sourceDigest) ||
      !hash(artifact.sourceBinding.genesisDigest) ||
      !natural(artifact.sourceBinding.rollbackGeneration)
    )
      throw new Error("causal_runtime_binding_mismatch");
    if (
      !hash(artifact.calibrationEvidenceDigest) ||
      artifact.calibrationEvidenceDigest !==
        input.verifiedCalibrationEvidenceDigest ||
      !hash(artifact.faultModelDigest) ||
      artifact.faultModelDigest !== input.adoptedFaultModelDigest
    )
      throw new Error("causal_policy_evidence_unavailable");
    const expectedStages = new Map([
      ["cursor", 5000],
      ["poll", 15000],
      ["scheduling_lag", 2000],
      ["source", 10000],
      ["build_sign_persist", 2000],
      ["submit", 3000],
    ]);
    if (
      artifact.enforcement.length !== expectedStages.size ||
      canonical(artifact.enforcement) !==
        canonical(input.installedEnforcement) ||
      artifact.enforcement.some(
        (item) =>
          expectedStages.get(item.stage) !== item.refusalCapMs ||
          !hash(item.implementationDigest) ||
          !["abort_and_fence", "late_result_fence"].includes(item.mode),
      ) ||
      new Set(artifact.enforcement.map((item) => item.stage)).size !==
        expectedStages.size
    )
      throw new Error("causal_stage_enforcement_unavailable");
    const c = artifact.causal;
    // This first supported calibration is deliberately a finite private profile.
    if (
      canonical(c) !==
      canonical({
        activeSlotProbability: { numerator: 1, denominator: 20 },
        target: { numerator: 999, denominator: 1000 },
        slotLengthMs: 1000,
        initialReadyMs: 45000,
        includedToNextReadyMs: 37000,
        expiryToReplacementReadyMs: 52000,
        minimumEligibleFutureSlots: 50,
        aggregateAllowedFailedAttempts: 5,
        maximumCurrentSchedulingPromises: 1,
      })
    )
      throw new Error("causal_profile_outside_calibrated_domain");
    const ceilings: CommitteePromiseWorkload = {
      retainedPayloadBytes: 72 * 28040,
      outstandingPromises: 72,
      tranches: 72,
      publications: 144,
      walletInputs: 32,
      journalEntries: 1024,
      challengeRecords: 1,
      storeRecords: 512,
      storeEncodedBytes: 8 * 1024 * 1024,
    };
    if (
      canonical(artifact.workloadCaps) !== canonical(ceilings) ||
      !natural(artifact.validUntilMs) ||
      artifact.validUntilMs <= initialWall ||
      !natural(initialWall) ||
      !Number.isFinite(initialMono) ||
      !natural(input.maximumWallMonotonicDriftMs)
    )
      throw new Error("causal_resource_or_clock_domain_unavailable");
    adopted = structuredClone(artifact);
  } catch (error) {
    fault =
      error instanceof Error ? error.message : "causal_policy_unavailable";
  }
  const status: CommitteePromiseRuntimePolicyAuthority["status"] = () => {
    try {
      const wall = now(),
        currentMono = mono();
      if (
        !natural(wall) ||
        !Number.isFinite(currentMono) ||
        currentMono < initialMono ||
        Math.abs(wall - initialWall - (currentMono - initialMono)) >
          input.maximumWallMonotonicDriftMs
      )
        throw new Error("causal_policy_clock_drift");
      if (
        !adopted ||
        Math.max(wall, initialWall + currentMono - initialMono) >=
          adopted.validUntilMs
      )
        throw new Error("causal_policy_expired");
      input.assertEpochCurrent();
    } catch (error) {
      fault ??=
        error instanceof Error
          ? error.message
          : "causal_policy_epoch_unavailable";
    }
    return fault || !adopted
      ? { status: "unavailable", reason: fault ?? "causal_policy_unavailable" }
      : { status: "conditional", policyId: adopted.policyId, envelopeId };
  };
  const covers = (workload: CommitteePromiseWorkload) =>
    status().status === "conditional" &&
    adopted !== undefined &&
    Object.keys(adopted.workloadCaps).every((key) => {
      const k = key as keyof CommitteePromiseWorkload;
      return natural(workload[k]) && workload[k] <= adopted!.workloadCaps[k];
    });
  const causal: CommitteePromiseCausalAuthority | undefined = adopted
    ? {
        minimumEligibleFutureSlots: adopted.causal.minimumEligibleFutureSlots,
        aggregateAllowedFailedAttempts:
          adopted.causal.aggregateAllowedFailedAttempts,
        upperNetworkTimeMs: input.upperNetworkTimeMs,
        evaluate: (args) => {
          if (status().status !== "conditional" || !adopted)
            throw new Error("Causal capability is unavailable");
          const c = adopted.causal;
          return evaluateCommitteePromiseTiming({
            ...args,
            model: {
              activeSlotProbability:
                c.activeSlotProbability.numerator /
                c.activeSlotProbability.denominator,
              eligibleFutureSlots: c.minimumEligibleFutureSlots,
              initialReadySlots: c.initialReadyMs / c.slotLengthMs,
              includedToNextReadySlots:
                c.includedToNextReadyMs / c.slotLengthMs,
              expiryToReplacementReadySlots:
                c.expiryToReplacementReadyMs / c.slotLengthMs,
              aggregateAllowedFailedAttempts: c.aggregateAllowedFailedAttempts,
            },
            target: c.target,
            slotLengthMs: c.slotLengthMs,
            upperNetworkTimeMs: input.upperNetworkTimeMs(),
            maximumCurrentSchedulingPromises:
              c.maximumCurrentSchedulingPromises,
          });
        },
      }
    : undefined;
  return Object.freeze({
    binding: adopted?.binding,
    causal,
    status,
    policy: () => undefined,
    envelope: (workload) =>
      covers(workload) ? { id: adopted!.policyId, envelopeId } : undefined,
    futureIntentRows: (workload) => {
      if (!covers(workload)) return undefined;
      const rows =
        BigInt(workload.publications) +
        BigInt(workload.tranches) +
        BigInt(workload.outstandingPromises) *
          BigInt(1 + adopted!.causal.aggregateAllowedFailedAttempts);
      return rows <= BigInt(Number.MAX_SAFE_INTEGER) ? Number(rows) : undefined;
    },
    breach: (reason) => {
      fault ??= reason;
    },
  });
};
