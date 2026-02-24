import { describe, expect, it } from "vitest";
import { computeRetentionCutoff, shouldPruneRetention } from "@/database/retention-policy.js";

describe("retention policy", () => {
  it("disables pruning when retentionDays <= 0", () => {
    expect(shouldPruneRetention(0)).toBe(false);
    expect(shouldPruneRetention(-5)).toBe(false);
  });

  it("enables pruning when retentionDays > 0", () => {
    expect(shouldPruneRetention(1)).toBe(true);
    expect(shouldPruneRetention(30)).toBe(true);
  });

  it("computes cutoff date by subtracting whole days", () => {
    const now = new Date("2026-02-24T00:00:00.000Z");
    const cutoff = computeRetentionCutoff(now, 7);
    expect(cutoff.toISOString()).toBe("2026-02-17T00:00:00.000Z");
  });
});
