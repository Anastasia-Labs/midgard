import {
  type LucidEvolution,
  SLOT_CONFIG_NETWORK,
  unixTimeToSlot,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  configureCustomSlotConfigFromShelleyGenesis,
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
    const previous = SLOT_CONFIG_NETWORK.Custom;
    SLOT_CONFIG_NETWORK.Custom = {
      zeroTime: 0,
      zeroSlot: 0,
      slotLength: 0,
    };
    try {
      const lucid = mkLucid({
        network: "Custom",
        provider: {},
      });

      expect(slotToUnixTimeForLucid(lucid, 7)).toBeUndefined();
    } finally {
      SLOT_CONFIG_NETWORK.Custom = previous;
    }
  });

  it("falls back to slot-based emulator time when anchors are missing", () => {
    const previous = SLOT_CONFIG_NETWORK.Custom;
    SLOT_CONFIG_NETWORK.Custom = {
      zeroTime: 0,
      zeroSlot: 0,
      slotLength: 0,
    };
    try {
      const lucid = mkLucid({
        network: "Custom",
        provider: {},
      });

      expect(slotToUnixTimeForLucidOrEmulatorFallback(lucid, 7)).toBe(7_000);
    } finally {
      SLOT_CONFIG_NETWORK.Custom = previous;
    }
  });

  it("uses the installed Ogmios Custom mapping for Kupmios providers", () => {
    const previous = SLOT_CONFIG_NETWORK.Custom;
    try {
      configureCustomSlotConfigFromShelleyGenesis(
        { startTimeMs: 58_000, slotLengthMs: 1_000 },
        {
          currentSlot: 42,
          observedAtMs: 100_042,
          slotLengthMs: 1_000,
        },
      );
      const lucid = mkLucid({
        network: "Custom",
        provider: {},
      });

      expect(slotToUnixTimeForLucid(lucid, 47)).toBe(105_000);
      expect(slotToUnixTimeForLucidOrEmulatorFallback(lucid, 47)).toBe(105_000);
    } finally {
      SLOT_CONFIG_NETWORK.Custom = previous;
    }
  });

  it("uses the exact genesis epoch despite a 542ms observation skew", () => {
    const previous = SLOT_CONFIG_NETWORK.Custom;
    try {
      configureCustomSlotConfigFromShelleyGenesis(
        { startTimeMs: 1_784_004_979_000, slotLengthMs: 1_000 },
        {
          currentSlot: 4_494,
          observedAtMs: 1_784_009_473_542,
          slotLengthMs: 1_000,
        },
      );
      const enclosingSlot = (unixTime: number): number =>
        unixTimeToSlot("Custom", unixTime);
      expect(SLOT_CONFIG_NETWORK.Custom).toEqual({
        zeroTime: 1_784_004_979_000,
        zeroSlot: 0,
        slotLength: 1_000,
      });
      expect(enclosingSlot(1_784_009_473_000)).toBe(4_494);
      expect(enclosingSlot(1_784_009_473_999)).toBe(4_494);
      expect(enclosingSlot(1_784_009_474_000)).toBe(4_495);
      const endSlotBoundary = slotToUnixTimeForLucid(
        mkLucid({ network: "Custom", provider: {} }),
        4_914,
      );
      if (endSlotBoundary === undefined) {
        throw new Error("Expected an authoritative Custom slot mapping");
      }
      expect(endSlotBoundary - 1).toBe(1_784_009_892_999);
      expect(1_784_009_893_541 - (endSlotBoundary - 1)).toBe(542);
    } finally {
      SLOT_CONFIG_NETWORK.Custom = previous;
    }
  });

  it("rejects invalid, disagreeing, or conflicting Custom slot inputs", () => {
    const previous = SLOT_CONFIG_NETWORK.Custom;
    try {
      expect(() =>
        configureCustomSlotConfigFromShelleyGenesis(
          { startTimeMs: 58_000, slotLengthMs: 1_000 },
          {
            currentSlot: -1,
            observedAtMs: 100_000,
            slotLengthMs: 1_000,
          },
        ),
      ).toThrow(/currentSlot/);
      expect(() =>
        configureCustomSlotConfigFromShelleyGenesis(
          { startTimeMs: 58_000, slotLengthMs: 2_000 },
          {
            currentSlot: 42,
            observedAtMs: 100_042,
            slotLengthMs: 1_000,
          },
        ),
      ).toThrow(/slot length disagreement/);
      expect(() =>
        configureCustomSlotConfigFromShelleyGenesis(
          { startTimeMs: 58_000, slotLengthMs: 1_000 },
          {
            currentSlot: 45,
            observedAtMs: 100_042,
            slotLengthMs: 1_000,
          },
        ),
      ).toThrow(/clock disagreement/);
      configureCustomSlotConfigFromShelleyGenesis(
        { startTimeMs: 58_000, slotLengthMs: 1_000 },
        {
          currentSlot: 42,
          observedAtMs: 100_042,
          slotLengthMs: 1_000,
        },
      );
      expect(() =>
        configureCustomSlotConfigFromShelleyGenesis(
          { startTimeMs: 57_000, slotLengthMs: 1_000 },
          {
            currentSlot: 43,
            observedAtMs: 100_042,
            slotLengthMs: 1_000,
          },
        ),
      ).toThrow(/conflicting/);
    } finally {
      SLOT_CONFIG_NETWORK.Custom = previous;
    }
  });

  it("does not modify known-network slot mappings", () => {
    const previousCustom = SLOT_CONFIG_NETWORK.Custom;
    const previousPreprod = SLOT_CONFIG_NETWORK.Preprod;
    try {
      configureCustomSlotConfigFromShelleyGenesis(
        { startTimeMs: 58_000, slotLengthMs: 1_000 },
        {
          currentSlot: 42,
          observedAtMs: 100_042,
          slotLengthMs: 1_000,
        },
      );
      expect(SLOT_CONFIG_NETWORK.Preprod).toEqual(previousPreprod);
    } finally {
      SLOT_CONFIG_NETWORK.Custom = previousCustom;
    }
  });

  it("does not let provider observation anchors override the genesis epoch", () => {
    const previous = SLOT_CONFIG_NETWORK.Custom;
    try {
      configureCustomSlotConfigFromShelleyGenesis(
        { startTimeMs: 58_000, slotLengthMs: 1_000 },
        {
          currentSlot: 42,
          observedAtMs: 100_542,
          slotLengthMs: 1_000,
        },
      );
      const lucid = mkLucid({
        network: "Custom",
        provider: { slot: 42, time: 100_542 },
      });

      expect(slotToUnixTimeForLucid(lucid, 42)).toBe(100_000);
    } finally {
      SLOT_CONFIG_NETWORK.Custom = previous;
    }
  });
});
