import { describe, expect, it } from "vitest";

import {
  computeRetentionCutoff,
  MIN_DA_PAYLOAD_RETENTION_DAYS,
  shouldPruneRetention,
  validateRetentionDays,
} from "@/database/retention-policy.js";

describe("retention policy", () => {
  it("disables pruning when retentionDays is 0", () => {
    expect(shouldPruneRetention(0)).toBe(false);
  });

  it("requires positive retention to cover DA payload availability", () => {
    expect(validateRetentionDays(MIN_DA_PAYLOAD_RETENTION_DAYS)).toBe(
      MIN_DA_PAYLOAD_RETENTION_DAYS,
    );
    expect(() => validateRetentionDays(1)).toThrow(
      "RETENTION_DAYS must be 0 or at least 8 days",
    );
    expect(() => validateRetentionDays(-5)).toThrow(
      "RETENTION_DAYS must be a non-negative safe integer",
    );
  });

  it("enables pruning when retentionDays covers DA payload availability", () => {
    expect(shouldPruneRetention(MIN_DA_PAYLOAD_RETENTION_DAYS)).toBe(true);
    expect(shouldPruneRetention(30)).toBe(true);
  });

  it("computes cutoff date by subtracting whole days", () => {
    const now = new Date("2026-02-24T00:00:00.000Z");
    const cutoff = computeRetentionCutoff(now, MIN_DA_PAYLOAD_RETENTION_DAYS);
    expect(cutoff.toISOString()).toBe("2026-02-16T00:00:00.000Z");
  });
});
