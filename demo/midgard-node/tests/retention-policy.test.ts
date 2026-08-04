import {
  MIDGARD_RETENTION_WINDOW_V1,
  RETENTION_MS_PER_DAY_V1,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  assertRetentionDaysMatchesDeploymentV1,
  computeChallengeableCutoff,
  computeRetentionCutoff,
  MIN_DA_PAYLOAD_RETENTION_DAYS,
  shouldPruneRetention,
  validateRetentionDays,
} from "@/database/retention-policy.js";

describe("retention policy", () => {
  it("disables pruning when retentionDays is 0", () => {
    expect(shouldPruneRetention(0)).toBe(false);
  });

  it("derives the minimum retention floor from the canonical V1 window", () => {
    expect(MIN_DA_PAYLOAD_RETENTION_DAYS).toBe(15);
    expect(MIN_DA_PAYLOAD_RETENTION_DAYS).toBe(
      MIDGARD_RETENTION_WINDOW_V1.retentionDays,
    );
    // Mutation guard: lowering the floor below the derived deployment window
    // must fail this test.
    expect(
      MIN_DA_PAYLOAD_RETENTION_DAYS * RETENTION_MS_PER_DAY_V1,
    ).toBeGreaterThanOrEqual(MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs);
    expect(MIN_DA_PAYLOAD_RETENTION_DAYS * RETENTION_MS_PER_DAY_V1).toBe(
      1_296_000_000,
    );
  });

  it("requires positive retention to cover DA payload availability", () => {
    expect(validateRetentionDays(MIN_DA_PAYLOAD_RETENTION_DAYS)).toBe(
      MIN_DA_PAYLOAD_RETENTION_DAYS,
    );
    expect(() => validateRetentionDays(1)).toThrow(
      "RETENTION_DAYS must be 0 or at least 15 days",
    );
    expect(() => validateRetentionDays(-5)).toThrow(
      "RETENTION_DAYS must be a non-negative safe integer",
    );
  });

  it("accepts 15 days at the boundary and rejects 14", () => {
    expect(validateRetentionDays(15)).toBe(15);
    expect(() => validateRetentionDays(14)).toThrow(
      "RETENTION_DAYS must be 0 or at least 15 days",
    );
  });

  it("rejects malformed retention day values", () => {
    for (const bad of [
      Number.NaN,
      -1,
      1.5,
      "15" as unknown as number,
      null as unknown as number,
      2 ** 53,
    ]) {
      expect(() => validateRetentionDays(bad)).toThrow(
        "RETENTION_DAYS must be a non-negative safe integer",
      );
    }
  });

  it("enables pruning when retentionDays covers DA payload availability", () => {
    expect(shouldPruneRetention(MIN_DA_PAYLOAD_RETENTION_DAYS)).toBe(true);
    expect(shouldPruneRetention(30)).toBe(true);
  });

  it("computes cutoff date by subtracting whole days", () => {
    const now = new Date("2026-02-24T00:00:00.000Z");
    const cutoff = computeRetentionCutoff(now, MIN_DA_PAYLOAD_RETENTION_DAYS);
    expect(cutoff.toISOString()).toBe("2026-02-09T00:00:00.000Z");
  });

  it("computes the challengeability cutoff from maturity plus the proof bound", () => {
    const now = new Date("2026-02-24T00:00:00.000Z");
    const cutoff = computeChallengeableCutoff(now);
    expect(now.getTime() - cutoff.getTime()).toBe(907_200_000);
    expect(now.getTime() - cutoff.getTime()).toBe(
      MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs,
    );
    // Never the measured 11h dispute schedule.
    expect(now.getTime() - cutoff.getTime()).not.toBe(
      MIDGARD_RETENTION_WINDOW_V1.measuredValidationDisputeScheduleMs,
    );
  });
});

describe("assertRetentionDaysMatchesDeploymentV1", () => {
  it("accepts an env window at or above the manifest window", () => {
    expect(assertRetentionDaysMatchesDeploymentV1(15, 15)).toBe(15);
    expect(assertRetentionDaysMatchesDeploymentV1(30, 15)).toBe(30);
  });

  it("throws when the env window is shorter than the manifest window", () => {
    expect(() => assertRetentionDaysMatchesDeploymentV1(15, 16)).toThrow(
      /shorter than the deployment manifest/u,
    );
  });

  it("accepts retentionDays=0 because pruning is disabled entirely", () => {
    expect(assertRetentionDaysMatchesDeploymentV1(0, 15)).toBe(0);
  });

  it("defaults the manifest window to the derived deployment value", () => {
    expect(assertRetentionDaysMatchesDeploymentV1(15)).toBe(15);
    expect(() => assertRetentionDaysMatchesDeploymentV1(14)).toThrow(
      "RETENTION_DAYS must be 0 or at least 15 days",
    );
  });

  it("rejects a malformed manifest window", () => {
    for (const bad of [
      Number.NaN,
      -1,
      1.5,
      "15" as unknown as number,
      null as unknown as number,
    ]) {
      expect(() => assertRetentionDaysMatchesDeploymentV1(15, bad)).toThrow(
        /Deployment manifest da\.transportProfile\.retentionDays/u,
      );
    }
  });
});
