import { describe, expect, it } from "vitest";

import {
  availabilityResponseAdmission,
  type AvailabilityResponseAdmissionInput,
  type AvailabilityResponseObligation,
  availabilityResponsePublicationCount,
} from "../src/availability-response-admission.js";
const job = (
  byte: string,
  publications = 4,
  window = 1000,
): AvailabilityResponseObligation => ({
  headerHash: byte.repeat(56),
  commitmentDigest: byte.repeat(64),
  kind: "potential",
  remainingPublications: publications,
  remainingSettlements: 1,
  remainingCloses: 1,
  responseWindowMs: window,
});
const input = (candidate = job("1")): AvailabilityResponseAdmissionInput => ({
  deploymentId: "aa".repeat(32),
  boundary: {
    pointId: "100:canonical",
    rollbackGeneration: 2,
    observedAtMs: 100,
  },
  nowMs: 100,
  policy: {
    id: "test-enforced-caps",
    envelopeId: "conditional-test-envelope",
    publishStepMs: 100,
    settleStepMs: 100,
    closeStepMs: 100,
    discoveryAndClockMarginMs: 0,
    supportedRecoveryMs: 0,
  },
  blocking: { kind: "bounded", remainingMs: 0 },
  evidenceComplete: true,
  obligations: [],
  candidate,
});
describe("availability response promise admission", () => {
  it("refuses a second individually feasible promise when cumulative work cannot fit", () => {
    const a = input();
    expect(availabilityResponseAdmission(a).status).toBe("admitted");
    expect(availabilityResponseAdmission(input(job("2"))).status).toBe(
      "admitted",
    );
    expect(
      availabilityResponseAdmission({
        ...input(job("2")),
        obligations: [a.candidate],
      }),
    ).toMatchObject({
      status: "insufficient_capacity",
      requiredMs: 1200,
      availableMs: 1000,
    });
  });
  it("refuses equality and admits one millisecond of remaining margin", () => {
    expect(availabilityResponseAdmission(input(job("1", 4, 600))).status).toBe(
      "insufficient_capacity",
    );
    expect(
      availabilityResponseAdmission(input(job("1", 4, 601))).minimumSlackMs,
    ).toBe(1);
  });
  it("counts per-tranche ceiling and authentic full geometry without transport-stream arithmetic", () => {
    expect(
      availabilityResponsePublicationCount(Array(16).fill(4194304), 14020),
    ).toBe(4800);
    expect(availabilityResponsePublicationCount([14021, 14021], 14020)).toBe(4);
    const full = input(job("1", 4800, 172800000));
    expect(
      availabilityResponseAdmission({
        ...full,
        candidate: { ...full.candidate, remainingSettlements: 16 },
        policy: {
          ...full.policy!,
          publishStepMs: 40000,
          settleStepMs: 40000,
          closeStepMs: 40000,
        },
      }),
    ).toMatchObject({
      status: "insufficient_capacity",
      requiredMs: 192680000,
      availableMs: 172800000,
    });
  });
  it("deduplicates an exact promise and uses authenticated progress for the active copy", () => {
    const potential = job("1");
    const active = {
      ...potential,
      kind: "active" as const,
      remainingPublications: 1,
      responseDeadlineMs: 1100,
      responseWindowMs: undefined,
    };
    expect(
      availabilityResponseAdmission({
        ...input(job("2", 1)),
        obligations: [potential, potential, active],
      }),
    ).toMatchObject({
      status: "admitted",
      obligations: 2,
      publications: 2,
      requiredMs: 600,
    });
  });
  it("counts completion interference after publication expiry without using that old deadline", () => {
    const complete = {
      ...job("1"),
      kind: "completion" as const,
      remainingPublications: 0,
      responseDeadlineMs: 0,
      responseWindowMs: undefined,
    };
    expect(
      availabilityResponseAdmission({
        ...input(job("2", 1)),
        obligations: [complete],
      }),
    ).toMatchObject({ status: "admitted", requiredMs: 500, availableMs: 1000 });
  });
  it("does not treat unknown evidence or runtime policy as zero demand", () => {
    expect(
      availabilityResponseAdmission({ ...input(), policy: undefined }).status,
    ).toBe("incomplete_evidence");
    expect(
      availabilityResponseAdmission({ ...input(), evidenceComplete: false })
        .status,
    ).toBe("incomplete_evidence");
    expect(
      availabilityResponseAdmission({
        ...input(),
        policy: { ...input().policy!, publishStepMs: Infinity },
      }).status,
    ).toBe("incomplete_evidence");
  });
  it("refuses unresolved actor blocking rather than dropping its old liability", () => {
    expect(
      availabilityResponseAdmission({
        ...input(),
        blocking: { kind: "unresolved", reason: "ambiguous_signed_intent" },
      }),
    ).toMatchObject({
      status: "unbounded_blocking",
      reason: "ambiguous_signed_intent",
    });
  });
  it("refuses conflicting duplicate progress and unsafe arithmetic", () => {
    const a = job("1");
    expect(
      availabilityResponseAdmission({
        ...input(job("2")),
        obligations: [a, { ...a, remainingPublications: 3 }],
      }).status,
    ).toBe("incomplete_evidence");
    expect(
      availabilityResponseAdmission({
        ...input(),
        policy: { ...input().policy!, publishStepMs: Number.MAX_SAFE_INTEGER },
      }).reason,
    ).toBe("response_admission_arithmetic_overflow");
  });
  it("binds the shortest active deadline and includes blocking, recovery and clock margin", () => {
    const a = {
      ...job("1", 1),
      kind: "active" as const,
      responseDeadlineMs: 701,
      responseWindowMs: undefined,
    };
    expect(
      availabilityResponseAdmission({
        ...input(job("2", 1)),
        obligations: [a],
        blocking: { kind: "bounded", remainingMs: 10 },
        policy: {
          ...input().policy!,
          supportedRecoveryMs: 10,
          discoveryAndClockMarginMs: 1,
        },
      }),
    ).toMatchObject({
      status: "insufficient_capacity",
      requiredMs: 620,
      availableMs: 600,
      limitingCommitmentDigest: a.commitmentDigest,
    });
  });
});
