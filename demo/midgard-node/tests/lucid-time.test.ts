import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  canonicalSlotConfigForLucid,
  customSlotConfigFromShelleyGenesis,
  slotToUnixTimeForLucid,
  slotToUnixTimeForLucidOrEmulatorFallback,
  unixTimeToSlotForConfig,
} from "../src/lucid-time.js";

describe("lucid-time", () => {
  it("reproduces Lucid's enclosing-slot conversion from plain worker data", async () => {
    const account = generateEmulatorAccount({ lovelace: 50_000_000n });
    const emulator = new Emulator([account]);
    const lucid = await Lucid(emulator, "Custom");
    const slotConfig = canonicalSlotConfigForLucid(lucid);
    const unixTimeMs = emulator.now() + 12_345;

    expect(unixTimeToSlotForConfig(unixTimeMs, slotConfig)).toBe(
      lucid.unixTimeToSlot(unixTimeMs),
    );
    expect(slotConfig).toEqual(lucid.config().slotConfig);
  });

  it("fails closed on invalid or pre-genesis worker slot inputs", () => {
    expect(() =>
      unixTimeToSlotForConfig(999, {
        zeroTime: 1_000,
        zeroSlot: 0,
        slotLength: 1_000,
      }),
    ).toThrow(/invalid slot/u);
    expect(() =>
      unixTimeToSlotForConfig(1_000, {
        zeroTime: 1_000,
        zeroSlot: 0,
        slotLength: 0,
      }),
    ).toThrow(/slotLength/u);
  });

  it("derives an exact per-instance Custom mapping from Shelley genesis", () => {
    expect(
      customSlotConfigFromShelleyGenesis(
        { startTimeMs: 1_784_004_979_000, slotLengthMs: 1_000 },
        {
          currentSlot: 4_494,
          observedAtMs: 1_784_009_473_542,
          slotLengthMs: 1_000,
        },
      ),
    ).toEqual({
      zeroTime: 1_784_004_979_000,
      zeroSlot: 0,
      slotLength: 1_000,
    });
  });

  it("keeps distinct Custom slot mappings isolated between Lucid clients", async () => {
    const account = generateEmulatorAccount({ lovelace: 50_000_000n });
    const emulator = new Emulator([account]);
    const first = await Lucid(emulator, "Custom", {
      slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1_000 },
    });
    const second = await Lucid(emulator, "Custom", {
      slotConfig: { zeroTime: 90_000, zeroSlot: 0, slotLength: 1_000 },
    });

    expect(slotToUnixTimeForLucid(first, 15)).toBe(15_000);
    expect(slotToUnixTimeForLucid(second, 15)).toBe(105_000);
    expect(slotToUnixTimeForLucid(first, 15)).toBe(15_000);
  });

  it("delegates strict conversion to the active Lucid instance", () => {
    const lucid = {
      slotToUnixTime: (slot: number) => slot * 2_000 + 10,
    } as unknown as LucidEvolution;

    expect(slotToUnixTimeForLucid(lucid, 7)).toBe(14_010);
  });

  it("returns undefined on unavailable strict conversion and keeps emulator fallback", () => {
    const lucid = {
      slotToUnixTime: () => {
        throw new Error("slot mapping unavailable");
      },
    } as unknown as LucidEvolution;

    expect(slotToUnixTimeForLucid(lucid, 7)).toBeUndefined();
    expect(slotToUnixTimeForLucidOrEmulatorFallback(lucid, 7)).toBe(7_000);
  });

  it("rejects invalid or disagreeing Custom slot evidence", () => {
    expect(() =>
      customSlotConfigFromShelleyGenesis(
        { startTimeMs: 58_000, slotLengthMs: 1_000 },
        {
          currentSlot: -1,
          observedAtMs: 100_000,
          slotLengthMs: 1_000,
        },
      ),
    ).toThrow(/currentSlot/);
    expect(() =>
      customSlotConfigFromShelleyGenesis(
        { startTimeMs: 58_000, slotLengthMs: 2_000 },
        {
          currentSlot: 42,
          observedAtMs: 100_042,
          slotLengthMs: 1_000,
        },
      ),
    ).toThrow(/slot length disagreement/);
    expect(() =>
      customSlotConfigFromShelleyGenesis(
        { startTimeMs: 58_000, slotLengthMs: 1_000 },
        {
          currentSlot: 45,
          observedAtMs: 100_042,
          slotLengthMs: 1_000,
        },
      ),
    ).toThrow(/clock disagreement/);
    expect(() =>
      customSlotConfigFromShelleyGenesis(
        { startTimeMs: 101_000, slotLengthMs: 1_000 },
        {
          currentSlot: 0,
          observedAtMs: 100_000,
          slotLengthMs: 1_000,
        },
      ),
    ).toThrow(/precedes/);
  });
});
