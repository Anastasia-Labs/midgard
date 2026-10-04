import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import {
  computeChallengeableCutoff,
  computeHousekeepingCutoff,
  computeRetentionCutoff,
  MIN_DA_PAYLOAD_RETENTION_DAYS,
  resolveHousekeepingRetentionDays,
  shouldPruneRetention,
  validateRetentionDays,
} from "../src/database/retention-policy.js";

describe("retention policy", () => {
  it("disables pruning when retentionDays is 0", () => {
    expect(shouldPruneRetention(0)).toBe(false);
  });

  it("derives the minimum retention floor from the canonical V1 window", () => {
    expect(MIN_DA_PAYLOAD_RETENTION_DAYS).toBe(15);
    expect(MIN_DA_PAYLOAD_RETENTION_DAYS).toBe(
      MIDGARD_RETENTION_WINDOW.retentionDays,
    );
    // Mutation guard: lowering the floor below the derived deployment window
    // must fail this test.
    expect(
      MIN_DA_PAYLOAD_RETENTION_DAYS * RETENTION_MS_PER_DAY,
    ).toBeGreaterThanOrEqual(MIDGARD_RETENTION_WINDOW.requiredRetentionMs);
    expect(MIN_DA_PAYLOAD_RETENTION_DAYS * RETENTION_MS_PER_DAY).toBe(
      1_296_000_000,
    );
  });

  it("checks only the shape of RETENTION_DAYS: the deployment's window decides the floor", () => {
    expect(validateRetentionDays(0)).toBe(0);
    expect(validateRetentionDays(1)).toBe(1);
    expect(validateRetentionDays(15)).toBe(15);
    expect(() => validateRetentionDays(-5)).toThrow(
      "RETENTION_DAYS must be a non-negative safe integer",
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

  it("enables wall-clock table pruning only for a non-zero retentionDays", () => {
    expect(shouldPruneRetention(0)).toBe(false);
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
    const maturityMs = SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms;
    expect(now.getTime() - cutoff.getTime()).toBe(maturityMs + maturityMs / 2);
    expect(now.getTime() - cutoff.getTime()).toBe(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    );
    // Never the measured dispute schedule.
    expect(now.getTime() - cutoff.getTime()).not.toBe(
      MIDGARD_RETENTION_WINDOW.measuredValidationDisputeScheduleMs,
    );
  });
});

describe("resolveHousekeepingRetentionDays (B5)", () => {
  const resolve = (
    configured: number | undefined,
    manifestRetentionDays: number | undefined,
  ) => resolveHousekeepingRetentionDays({ configured, manifestRetentionDays });

  it("uses the verified manifest's window when RETENTION_DAYS is unset, never a compiled constant", () => {
    expect(resolve(undefined, 15)).toBe(15);
    // A manifest declaring a window other than the compiled one wins.
    expect(resolve(undefined, 21)).toBe(21);
    expect(21).not.toBe(MIN_DA_PAYLOAD_RETENTION_DAYS);
  });

  it("honours an explicit window at or above the manifest's", () => {
    expect(resolve(21, 21)).toBe(21);
    expect(resolve(30, 21)).toBe(30);
  });

  it("refuses an explicit window shorter than the manifest's, with the fix in the message", () => {
    expect(() => resolve(20, 21)).toThrow(
      /RETENTION_DAYS=20 is shorter than the verified deployment manifest da\.transportProfile\.retentionDays=21/u,
    );
    // The compiled window would admit 15; the manifest's 21 refuses it.
    expect(() => resolve(15, 21)).toThrow(/shorter than the verified/u);
  });

  it("keeps everything for an explicit 0, which retains longer than any window", () => {
    expect(resolve(0, 21)).toBe(0);
    expect(resolve(0, undefined)).toBe(0);
  });

  it("prunes nothing without a manifest unless RETENTION_DAYS covers the compiled profile's window", () => {
    expect(resolve(undefined, undefined)).toBe(0);
    expect(resolve(MIN_DA_PAYLOAD_RETENTION_DAYS, undefined)).toBe(
      MIN_DA_PAYLOAD_RETENTION_DAYS,
    );
    expect(() => resolve(MIN_DA_PAYLOAD_RETENTION_DAYS - 1, undefined)).toThrow(
      /shorter than the derived deployment's retention window/u,
    );
  });

  it("rejects a malformed manifest window or RETENTION_DAYS", () => {
    for (const bad of [
      0,
      Number.NaN,
      -1,
      1.5,
      "15" as unknown as number,
      null as unknown as number,
    ]) {
      expect(() => resolve(15, bad)).toThrow(
        /Deployment manifest da\.transportProfile\.retentionDays/u,
      );
    }
    expect(() => resolve(-1, 15)).toThrow(
      "RETENTION_DAYS must be a non-negative safe integer",
    );
  });
});

describe("computeHousekeepingCutoff", () => {
  it("never reaches inside the DA challenge horizon", () => {
    const now = new Date("2026-02-24T00:00:00.000Z");
    const challengeable = computeChallengeableCutoff(now);
    for (const days of [1, 15, 30]) {
      const cutoff = computeHousekeepingCutoff(now, days);
      expect(cutoff.getTime()).toBeLessThanOrEqual(challengeable.getTime());
      expect(cutoff.getTime()).toBeLessThanOrEqual(
        computeRetentionCutoff(now, days).getTime(),
      );
    }
  });
});
