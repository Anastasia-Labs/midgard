import { describe, expect, it } from "vitest";

import {
  l1ProviderReadiness,
  pendingFinalizationAgeDetail,
  READINESS_L1_PROVIDER_UNHEALTHY_AFTER_MS,
  readinessDatabaseError,
  readinessL1ProviderUnhealthyAfterMs,
} from "../src/commands/listen-router.get-readiness-handler.inputs.js";

const NOW = 10_000_000;
const BOUND = READINESS_L1_PROVIDER_UNHEALTHY_AFTER_MS;

describe("readiness degradation bounds", () => {
  it("reports nothing for a healthy provider", () => {
    expect(
      l1ProviderReadiness({
        healthy: true,
        lastSuccessAtMs: 0,
        lastExactSuccessAtMs: 0,
        nowMs: NOW,
      }),
    ).toEqual({});
  });

  it("keeps a failure within the bound of both successes a detail", () => {
    expect(
      l1ProviderReadiness({
        healthy: false,
        lastSuccessAtMs: NOW - 1_000,
        lastExactSuccessAtMs: NOW - BOUND,
        nowMs: NOW,
      }),
    ).toEqual({
      detail: `provider_query_degraded:l1-provider:${BOUND.toString()}`,
    });
  });

  it("makes a failure unready once either success is past the bound, or absent", () => {
    for (const [lastSuccessAtMs, lastExactSuccessAtMs] of [
      [NOW - BOUND - 1, NOW - 1_000],
      [NOW - 1_000, NOW - BOUND - 1],
      [0, NOW - 1_000],
      [NOW - 1_000, 0],
    ] as const)
      expect(
        l1ProviderReadiness({
          healthy: false,
          lastSuccessAtMs,
          lastExactSuccessAtMs,
          nowMs: NOW,
        }),
      ).toEqual({ reason: "provider_query_unhealthy:l1-provider" });
  });

  it("never applies a provider-failure bound shorter than the ledger-tip staleness bound", () => {
    expect(readinessL1ProviderUnhealthyAfterMs(undefined)).toBe(BOUND);
    expect(readinessL1ProviderUnhealthyAfterMs(200_000)).toBe(BOUND);
    expect(readinessL1ProviderUnhealthyAfterMs(Number.NaN)).toBe(BOUND);
    const longTip = BOUND + 60_000;
    expect(readinessL1ProviderUnhealthyAfterMs(longTip)).toBe(longTip);
    // A failure past the flat bound but inside a longer ledger-tip bound
    // stays a detail; past that bound it is a reason.
    const failing = {
      healthy: false,
      lastExactSuccessAtMs: NOW - 1_000,
      nowMs: NOW,
      unhealthyAfterMs: readinessL1ProviderUnhealthyAfterMs(longTip),
    };
    expect(
      l1ProviderReadiness({ ...failing, lastSuccessAtMs: NOW - BOUND - 1 }),
    ).toEqual({
      detail: `provider_query_degraded:l1-provider:${(BOUND + 1).toString()}`,
    });
    expect(
      l1ProviderReadiness({ ...failing, lastSuccessAtMs: NOW - longTip - 1 }),
    ).toEqual({ reason: "provider_query_unhealthy:l1-provider" });
  });

  it("names a journal only past the bound", () => {
    expect(pendingFinalizationAgeDetail(null)).toBeUndefined();
    expect(pendingFinalizationAgeDetail(15 * 60_000)).toBeUndefined();
    expect(pendingFinalizationAgeDetail(15 * 60_000 + 1)).toBe(
      "pending_finalization_age:900001:900000",
    );
    expect(pendingFinalizationAgeDetail(11, 10)).toBe(
      "pending_finalization_age:11:10",
    );
  });

  it("reports only the first line of a database failure, bounded", () => {
    expect(readinessDatabaseError(new Error("down\nstack"))).toBe("down");
    expect(readinessDatabaseError({ message: "x".repeat(300) })).toHaveLength(
      200,
    );
    expect(readinessDatabaseError("plain")).toBe("plain");
  });
});
