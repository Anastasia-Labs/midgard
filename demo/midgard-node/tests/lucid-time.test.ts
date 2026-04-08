import { describe, expect, it } from "vitest";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import {
  slotToUnixTimeForLucid,
  slotToUnixTimeForLucidOrEmulatorFallback,
} from "@/lucid-time.js";

const mkLucid = (config: {
  readonly network: "Custom";
  readonly provider: { readonly time?: number; readonly slot?: number };
}): LucidEvolution =>
  ({
    config: () => config,
  }) as LucidEvolution;

describe("lucid-time", () => {
  it("derives slot unix time from custom provider anchors when available", () => {
    const lucid = mkLucid({
      network: "Custom",
      provider: { time: 10_000, slot: 10 },
    });

    expect(slotToUnixTimeForLucid(lucid, 15)).toBe(15_000);
  });

  it("returns undefined for strict custom slot resolution without anchors", () => {
    const lucid = mkLucid({
      network: "Custom",
      provider: {},
    });

    expect(slotToUnixTimeForLucid(lucid, 7)).toBeUndefined();
  });

  it("falls back to slot-based emulator time when anchors are missing", () => {
    const lucid = mkLucid({
      network: "Custom",
      provider: {},
    });

    expect(slotToUnixTimeForLucidOrEmulatorFallback(lucid, 7)).toBe(7_000);
  });
});
