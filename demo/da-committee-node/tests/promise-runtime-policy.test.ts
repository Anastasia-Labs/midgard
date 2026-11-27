import { describe, expect, it } from "vitest";

import {
  committeePromiseRuntimePolicyDigest,
  verifyCommitteePromiseRuntimePolicy,
} from "../src/availability/promise-runtime-policy.js";
import { testPromiseRuntimePolicy } from "./helpers/promise-runtime-policy.js";

const fixture = () => {
  const { artifact } = testPromiseRuntimePolicy({
    id: "test-only",
    envelopeId: "not-authority",
    publishStepMs: 100,
    settleStepMs: 200,
    closeStepMs: 300,
    discoveryAndClockMarginMs: 10,
    supportedRecoveryMs: 20,
  });
  let now = 1000;
  const input = {
    artifact,
    trustedPolicyDigest: committeePromiseRuntimePolicyDigest(artifact),
    liveBinding: artifact.binding,
    installedEnforcement: artifact.enforcement,
    verifiedCalibrationEvidenceDigest: artifact.calibrationEvidenceDigest,
    adoptedFaultModelDigest: artifact.assumptions.faultModelDigest,
    now: () => now,
  };
  const workload = {
    retainedPayloadBytes: 10,
    outstandingPromises: 1,
    tranches: 1,
    publications: 1,
    walletInputs: 1,
    journalEntries: 0,
    challengeRecords: 0,
    storeRecords: 0,
    storeEncodedBytes: 0,
  };
  return {
    input,
    workload,
    setNow: (value: number) => {
      now = value;
    },
  };
};
describe("conditional committee runtime policy authority", () => {
  it("requires caps for every retained input domain and refuses total history outside that domain", () => {
    const f = fixture();
    const authority = verifyCommitteePromiseRuntimePolicy(f.input);
    expect(
      authority.policy({ ...f.workload, storeRecords: 10001 }),
    ).toBeUndefined();
    expect(
      authority.policy({ ...f.workload, storeEncodedBytes: 10000001 }),
    ).toBeUndefined();
    expect(
      authority.policy({ ...f.workload, journalEntries: 101 }),
    ).toBeUndefined();
    const incomplete = structuredClone(f.input.artifact);
    Reflect.deleteProperty(incomplete.workloadCaps, "storeRecords");
    const unavailable = verifyCommitteePromiseRuntimePolicy({
      ...f.input,
      artifact: incomplete,
      trustedPolicyDigest: committeePromiseRuntimePolicyDigest(incomplete),
    });
    expect(unavailable.status()).toEqual({
      status: "unavailable",
      reason: "runtime_policy_domain_incomplete",
    });
  });
  it("reserves aggregate retry intents once per full-restorable promise", () => {
    const f = fixture();
    const artifact = {
      ...f.input.artifact,
      assumptions: {
        ...f.input.artifact.assumptions,
        aggregateAllowedFailedAttempts: 5,
      },
    };
    const authority = verifyCommitteePromiseRuntimePolicy({
      ...f.input,
      artifact,
      trustedPolicyDigest: committeePromiseRuntimePolicyDigest(artifact),
    });
    expect(
      authority.futureIntentRows?.({ ...f.workload, publications: 2 }),
    ).toBe(9);
    expect(
      authority.futureIntentRows?.({ ...f.workload, publications: 1 }),
    ).toBe(8);
    expect(
      authority.futureIntentRows?.({
        ...f.workload,
        outstandingPromises: 2,
        tranches: 2,
        publications: 4,
      }),
    ).toBe(18);
    const incomplete = {
      ...artifact,
      assumptions: {
        ...artifact.assumptions,
        aggregateOutageAndRecoveryMs: 0,
      },
    };
    expect(
      verifyCommitteePromiseRuntimePolicy({
        ...f.input,
        artifact: incomplete,
        trustedPolicyDigest: committeePromiseRuntimePolicyDigest(incomplete),
      }).status(),
    ).toMatchObject({ status: "unavailable" });
  });
  it("derives disjoint action costs from a digest-pinned artifact and installed bindings", () => {
    const f = fixture();
    const artifact = {
      ...f.input.artifact,
      assumptions: {
        ...f.input.artifact.assumptions,
        publish: {
          successfulSoftwareMs: 10,
          chainProgressAndObservationMs: 20,
          pollAndObservationMs: 30,
          allowedFailedAttempts: 2,
          failedAttemptAndRecoveryMs: 40,
        },
      },
    };
    const authority = verifyCommitteePromiseRuntimePolicy({
      ...f.input,
      artifact,
      trustedPolicyDigest: committeePromiseRuntimePolicyDigest(artifact),
    });
    expect(authority.policy(f.workload)).toMatchObject({
      publishStepMs: 140,
      settleStepMs: 200,
      closeStepMs: 300,
      supportedRecoveryMs: 20,
      discoveryAndClockMarginMs: 10,
      envelopeId: committeePromiseRuntimePolicyDigest(artifact),
    });
  });
  it.each([
    "digest",
    "deployment",
    "runtime",
    "evidence",
    "enforcement",
    "fault",
  ])("refuses a %s authority mismatch", (kind) => {
    const f = fixture();
    const bad =
      kind === "digest"
        ? { trustedPolicyDigest: "00".repeat(32) }
        : kind === "deployment"
          ? {
              liveBinding: {
                ...f.input.liveBinding,
                deploymentFingerprint: "different",
              },
            }
          : kind === "runtime"
            ? {
                liveBinding: {
                  ...f.input.liveBinding,
                  runtimeBuildDigest: "00".repeat(32),
                },
              }
            : kind === "evidence"
              ? { verifiedCalibrationEvidenceDigest: "00".repeat(32) }
              : kind === "fault"
                ? { adoptedFaultModelDigest: "00".repeat(32) }
                : { installedEnforcement: [] };
    const authority = verifyCommitteePromiseRuntimePolicy({
      ...f.input,
      ...bad,
    });
    expect(authority.status().status).toBe("unavailable");
    expect(authority.policy(f.workload)).toBeUndefined();
  });
  it("refuses uncalibrated workloads while permitting a later in-domain check", () => {
    const f = fixture();
    const authority = verifyCommitteePromiseRuntimePolicy(f.input);
    expect(
      authority.policy({ ...f.workload, outstandingPromises: 101 }),
    ).toBeUndefined();
    expect(
      authority.policy({ ...f.workload, walletInputs: Number.NaN }),
    ).toBeUndefined();
    expect(authority.policy(f.workload)).toBeDefined();
  });
  it("makes expiry and a detected breach sticky across healthy polls", () => {
    const f = fixture();
    const authority = verifyCommitteePromiseRuntimePolicy(f.input);
    authority.breach("software_allowance_exceeded");
    expect(authority.status()).toEqual({
      status: "unavailable",
      reason: "runtime_policy_breached:software_allowance_exceeded",
    });
    expect(authority.policy(f.workload)).toBeUndefined();
    const expiring = verifyCommitteePromiseRuntimePolicy(f.input);
    f.setNow(f.input.artifact.validUntilMs);
    expect(expiring.policy(f.workload)).toBeUndefined();
    f.setNow(1000);
    expect(expiring.policy(f.workload)).toBeUndefined();
  });
  it("does not adopt missing successful-stage assumptions or unsafe arithmetic", () => {
    const f = fixture();
    for (const successfulSoftwareMs of [0, Number.MAX_SAFE_INTEGER]) {
      const artifact = {
        ...f.input.artifact,
        assumptions: {
          ...f.input.artifact.assumptions,
          publish: {
            ...f.input.artifact.assumptions.publish,
            successfulSoftwareMs,
          },
        },
      };
      expect(
        verifyCommitteePromiseRuntimePolicy({
          ...f.input,
          artifact,
          trustedPolicyDigest: committeePromiseRuntimePolicyDigest(artifact),
        }).status().status,
      ).toBe("unavailable");
    }
  });
});
