import { describe, expect, it } from "vitest";

import { MIDGARD_CONSENSUS_LIMITS } from "../src/consensus-profile.js";
import { DA_TRANSPORT_LIMITS } from "../src/da-transport.js";
import {
  DEPLOYMENT_PROFILES,
  SELECTED_DEPLOYMENT_PROFILE,
} from "../src/deployment-profile.js";
import {
  assertDaChallengeWindowWithinMaturity,
  assertRetentionDaysCoverWindow,
  assertRetentionWindowCoversDeployment,
  assertWorstCaseProofTimeWithinBound,
  MIDGARD_MIN_RETENTION_DAYS,
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
  retentionDaysCoverWindow,
  retentionDeadlineForBlock,
} from "../src/retention-window.js";

export const BLOCK_END = Date.UTC(2026, 0, 1, 0, 0, 0);

// Expected values are worked out here from the compiled deployment profile's
// raw timing, not read back from the module under test, so a broken
// derivation cannot verify itself.
export const MATURITY_MS = SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms;

const PROOF_BOUND_MS = MATURITY_MS / 2;

export const HORIZON_MS = MATURITY_MS + PROOF_BOUND_MS;

const DEPLOYED_RETENTION_MS = 15 * RETENTION_MS_PER_DAY;

const MARGIN_MS = DEPLOYED_RETENTION_MS - HORIZON_MS;

const MIN_RETENTION_DAYS = Math.ceil(HORIZON_MS / RETENTION_MS_PER_DAY);

const manifestWith = (retentionDays: unknown): unknown => ({
  da: { transportProfile: { retentionDays } },
});

describe("MIDGARD_RETENTION_WINDOW_V1 derived arithmetic (F04)", () => {
  it("derives every constant from the frozen profiles, never from a literal", () => {
    expect(MIDGARD_RETENTION_WINDOW.maturityMs).toBe(MATURITY_MS);
    expect(MIDGARD_RETENTION_WINDOW.maturityMs).toBe(
      MIDGARD_CONSENSUS_LIMITS.blockMaturityMs,
    );
    expect(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs).toBe(
      PROOF_BOUND_MS,
    );
    expect(MIDGARD_RETENTION_WINDOW.requiredRetentionMs).toBe(HORIZON_MS);
    expect(MIDGARD_RETENTION_WINDOW.retentionDays).toBe(
      DA_TRANSPORT_LIMITS.minimumRetentionDays,
    );
    expect(MIDGARD_RETENTION_WINDOW.retentionDays).toBe(15);
    expect(MIDGARD_RETENTION_WINDOW.deployedRetentionMs).toBe(1_296_000_000);
    expect(MIDGARD_RETENTION_WINDOW.marginMs).toBe(MARGIN_MS);
    expect(MIDGARD_RETENTION_WINDOW.deployedRetentionMs).toBeGreaterThanOrEqual(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    );
    expect(MIDGARD_MIN_RETENTION_DAYS).toBe(MIN_RETENTION_DAYS);
  });

  it("keeps every profile's horizon inside the 15-day retention", () => {
    // The module-load assertion only sees the compiled profile; CI compiles
    // preprod-testing, so the other profiles are checked here. Mainnet is the
    // fixed decision 0002 vector: 907_200_000 ms horizon, 388_800_000 ms margin.
    for (const profile of Object.values(DEPLOYMENT_PROFILES)) {
      const maturityMs = profile.timing.block_maturity_ms;
      expect(maturityMs + maturityMs / 2).toBeLessThanOrEqual(
        DEPLOYED_RETENTION_MS,
      );
    }
    const mainnetMaturityMs =
      DEPLOYMENT_PROFILES.mainnet.timing.block_maturity_ms;
    expect(mainnetMaturityMs + mainnetMaturityMs / 2).toBe(907_200_000);
    expect(
      DEPLOYED_RETENTION_MS - (mainnetMaturityMs + mainnetMaturityMs / 2),
    ).toBe(388_800_000);
  });

  it("records but never enforces against the measured dispute schedule", () => {
    expect(MIDGARD_RETENTION_WINDOW.measuredValidationDisputeScheduleMs).toBe(
      MIDGARD_CONSENSUS_LIMITS.minValidationDisputeMaturityMs,
    );
    // The measured schedule differs from the half-maturity bound under every
    // profile (far below it on mainnet, above it on the testing profiles), so
    // the horizon below can only equal maturity plus the bound.
    expect(
      MIDGARD_RETENTION_WINDOW.measuredValidationDisputeScheduleMs,
    ).not.toBe(MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs);
    expect(MIDGARD_RETENTION_WINDOW.requiredRetentionMs).toBe(
      MIDGARD_RETENTION_WINDOW.maturityMs +
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
    );
  });
});

describe("DA challenge window within block maturity", () => {
  it("holds for every deployment profile", () => {
    // Worked from the raw profile timing, not from the module under test.
    for (const profile of Object.values(DEPLOYMENT_PROFILES)) {
      expect(profile.timing.da_challenge_window_ms).toBeLessThanOrEqual(
        profile.timing.block_maturity_ms,
      );
      expect(() =>
        assertDaChallengeWindowWithinMaturity(profile.name, profile.timing),
      ).not.toThrow();
    }
  });

  it("accepts a window equal to maturity and rejects one millisecond more", () => {
    expect(() =>
      assertDaChallengeWindowWithinMaturity("boundary", {
        block_maturity_ms: 900_000,
        da_challenge_window_ms: 900_000,
      }),
    ).not.toThrow();
    expect(() =>
      assertDaChallengeWindowWithinMaturity("boundary", {
        block_maturity_ms: 900_000,
        da_challenge_window_ms: 900_001,
      }),
    ).toThrow(/must not exceed block_maturity_ms=900000/u);
  });

  it("rejects malformed or non-positive timing", () => {
    for (const [maturity, window] of [
      [0, 0],
      [900_000, 0],
      [-1, -2],
      [900_000.5, 1],
      [Number.NaN, 1],
      ["900000", 1],
      [900_000, undefined],
      [Number.MAX_SAFE_INTEGER + 1, 1],
    ] as const) {
      expect(() =>
        assertDaChallengeWindowWithinMaturity("malformed", {
          block_maturity_ms: maturity,
          da_challenge_window_ms: window,
        }),
      ).toThrow(/must be positive safe integers/u);
    }
  });
});

describe("worst-case proof-time bound", () => {
  it("accepts exactly the bound and rejects one millisecond past it", () => {
    expect(assertWorstCaseProofTimeWithinBound(PROOF_BOUND_MS)).toBe(
      PROOF_BOUND_MS,
    );
    expect(() =>
      assertWorstCaseProofTimeWithinBound(PROOF_BOUND_MS + 1),
    ).toThrow(/exceeds the canonical V1 worst-case proof-time bound/u);
  });

  it("rejects malformed observations", () => {
    for (const bad of [Number.NaN, -1, 1.5, "302400000", null, 2 ** 53]) {
      expect(() => assertWorstCaseProofTimeWithinBound(bad)).toThrow();
    }
  });
});

describe("retention-days floor and deployment binding", () => {
  // Under a testing profile the horizon is under one day, so MIN_RETENTION_DAYS
  // is 1 and these cases only check "days >= 1": they cannot tell the horizon
  // from maturity alone or any other sub-day floor. Only a long-maturity
  // compile (mainnet: 11 days pass, 10 fail) exercises the comparison itself.
  it("accepts the derived minimum days and rejects one day fewer", () => {
    expect(assertRetentionDaysCoverWindow(15)).toBe(15);
    expect(retentionDaysCoverWindow(15)).toBe(true);
    expect(retentionDaysCoverWindow(14)).toBe(true);
    expect(retentionDaysCoverWindow(MIN_RETENTION_DAYS)).toBe(true);
    expect(retentionDaysCoverWindow(MIN_RETENTION_DAYS - 1)).toBe(false);
    expect(() =>
      assertRetentionDaysCoverWindow(MIN_RETENTION_DAYS - 1),
    ).toThrow(`must be at least ${String(MIN_RETENTION_DAYS)} days`);
  });

  it("binds the window to deployment identity via da.transportProfile", () => {
    expect(assertRetentionWindowCoversDeployment(manifestWith(15))).toBe(15);
    expect(() =>
      assertRetentionWindowCoversDeployment(
        manifestWith(MIN_RETENTION_DAYS - 1),
      ),
    ).toThrow(/da\.transportProfile\.retentionDays must be at least/u);
  });

  it("rejects malformed manifest retention values and shapes", () => {
    for (const bad of [Number.NaN, -1, 1.5, "15", null, 2 ** 53]) {
      expect(() =>
        assertRetentionWindowCoversDeployment(manifestWith(bad)),
      ).toThrow(/must be a non-negative safe integer number of days/u);
    }
    expect(() => assertRetentionWindowCoversDeployment(null)).toThrow(
      /Deployment manifest must be an object/u,
    );
    expect(() => assertRetentionWindowCoversDeployment({})).toThrow(
      /Deployment manifest da must be an object/u,
    );
    expect(() => assertRetentionWindowCoversDeployment({ da: {} })).toThrow(
      /da\.transportProfile must be an object/u,
    );
  });

  it("keeps the >=15-day manifest floor in verifyDeploymentManifestV1Identity", () => {
    // Mutation guard: the manifest surface must reject 14 even though 14 days
    // still covers the derived 11-day horizon.
    expect(retentionDaysCoverWindow(14)).toBe(true);
    expect(DA_TRANSPORT_LIMITS.minimumRetentionDays).toBe(15);
  });
});

describe("retentionDeadlineForBlockV1", () => {
  it("keys the deadline on block end time, not local insert time", () => {
    const deadline = retentionDeadlineForBlock({
      blockEndTimeMs: BLOCK_END,
      retentionDays: 15,
    });
    expect(deadline.challengeableUntilMs).toBe(BLOCK_END + HORIZON_MS);
    expect(deadline.retainUntilMs).toBe(BLOCK_END + DEPLOYED_RETENTION_MS);
    expect(deadline.deployedRetentionMs).toBe(DEPLOYED_RETENTION_MS);
    expect(deadline.remainingMs(BLOCK_END)).toBe(HORIZON_MS);
    expect(deadline.remainingMs(deadline.challengeableUntilMs)).toBe(0);
    expect(deadline.remainingMs(deadline.challengeableUntilMs + 1)).toBe(-1);
  });

  it("accepts retentionDays=0 without collapsing challengeability", () => {
    const deadline = retentionDeadlineForBlock({
      blockEndTimeMs: BLOCK_END,
      retentionDays: 0,
    });
    expect(deadline.retainUntilMs).toBe(BLOCK_END);
    expect(deadline.challengeableUntilMs).toBe(BLOCK_END + HORIZON_MS);
  });

  it("rejects malformed block end times", () => {
    for (const bad of [Number.NaN, -1, 1.5, "0", null]) {
      expect(() => retentionDeadlineForBlock({ blockEndTimeMs: bad })).toThrow(
        /blockEndTimeMs must be a non-negative safe integer/u,
      );
    }
  });
});
