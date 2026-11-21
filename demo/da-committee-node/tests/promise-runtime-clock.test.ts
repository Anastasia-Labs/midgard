import { describe, expect, it } from "vitest";

import { committeePromiseNetworkClock } from "../src/availability/promise-network-clock.js";
import {
  committeePromiseRuntimePolicyDigest,
  verifyCommitteePromiseRuntimePolicy,
} from "../src/availability/promise-runtime-policy.js";
import { testPromiseRuntimePolicy } from "./helpers/promise-runtime-policy.js";

const fixture = () => {
  let wall = 1000;
  let mono = 0;
  const { artifact: base } = testPromiseRuntimePolicy({
    id: "clock-fixture",
    envelopeId: "test-only",
    publishStepMs: 100,
    settleStepMs: 100,
    closeStepMs: 100,
    discoveryAndClockMarginMs: 0,
    supportedRecoveryMs: 0,
  });
  const artifact = { ...base, validUntilMs: 1100 };
  const authority = verifyCommitteePromiseRuntimePolicy({
    artifact,
    trustedPolicyDigest: committeePromiseRuntimePolicyDigest(artifact),
    liveBinding: artifact.binding,
    installedEnforcement: artifact.enforcement,
    verifiedCalibrationEvidenceDigest: artifact.calibrationEvidenceDigest,
    adoptedFaultModelDigest: artifact.assumptions.faultModelDigest,
    now: () => wall,
    monotonicMs: () => mono,
    maxWallClockDriftMs: 20,
  });
  return {
    authority,
    set: (nextWall: number, nextMono: number) => {
      wall = nextWall;
      mono = nextMono;
    },
  };
};
describe("adopted capability clock fences", () => {
  it("expires monotonically when the wall clock moves backward", () => {
    const f = fixture();
    expect(f.authority.status().status).toBe("conditional");
    f.set(900, 100);
    expect(f.authority.status()).toEqual({
      status: "unavailable",
      reason: "runtime_policy_expired",
    });
    f.set(1001, 101);
    expect(f.authority.status().status).toBe("unavailable");
  });
  it("latches unsupported wall/monotonic drift before expiry", () => {
    const f = fixture();
    f.set(990, 20);
    expect(f.authority.status()).toEqual({
      status: "unavailable",
      reason: "runtime_policy_clock_drift",
    });
    f.set(1021, 21);
    expect(f.authority.status().status).toBe("unavailable");
  });
});

describe("genesis-bound upper network time", () => {
  const fixture = (skewMs = 0) => {
    let wall = 470_000,
      mono = 0;
    const clock = committeePromiseNetworkClock({
      slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1000 },
      initialProducerMemberSkewMs: skewMs,
      maxWallMonotonicDriftMs: 20,
      nowMs: () => wall,
      monotonicMs: () => mono,
    });
    return {
      clock,
      set: (w: number, m: number) => {
        wall = w;
        mono = m;
      },
    };
  };
  it("uses current upper time despite a healthy older selected tip", () => {
    // The selected tip at420 can be valid through empty slots. It cannot make
    // the current470/exclusive480 attempt have fifty eligible future slots.
    const f = fixture();
    expect(f.clock.upperReadySlot(0)).toBe(470);
    expect(f.clock.eligibleFutureSlots(480, 0)).toBe(9);
    expect(f.clock.eligibleFutureSlots(521, 0)).toBe(50);
    expect(f.clock.eligibleFutureSlots(520, 0)).toBe(49);
  });
  it("includes initial skew and remaining submit propagation in the upper bound", () => {
    const f = fixture(2000);
    expect(f.clock.upperReadySlot(3000)).toBe(475);
    expect(f.clock.eligibleFutureSlots(480, 3000)).toBe(4);
    expect(f.clock.remainingHorizonSlots(720_000, 3000)).toBe(245);
  });
  it("never extends TTL eligibility after a backward wall step", () => {
    const f = fixture();
    f.set(470_990, 1000);
    expect(f.clock.upperReadySlot(0)).toBe(471);
    expect(f.clock.eligibleFutureSlots(521, 0)).toBe(49);
    f.set(469_000, 1001);
    expect(() => f.clock.upperReadySlot(0)).toThrow("drift");
    f.set(471_002, 1002);
    expect(() => f.clock.upperReadySlot(0)).toThrow("drift");
  });
});
