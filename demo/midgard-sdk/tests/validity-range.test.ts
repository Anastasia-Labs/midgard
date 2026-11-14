import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  slotAlignedLowerBoundAtOrAfter,
  slotAlignedUpperBoundAtOrBefore,
  type SlotClock,
} from "../src/validity-range.js";

// The same floor conversion Lucid applies (unixTimeToEnclosingSlot).
const slotClock = (
  zeroTime: number,
  slotLength: number,
  zeroSlot = 0,
): SlotClock => ({
  unixTimeToSlot: (unixTime) =>
    Math.floor((unixTime - zeroTime) / slotLength) + zeroSlot,
  slotToUnixTime: (slot) => zeroTime + (slot - zeroSlot) * slotLength,
});

// The inclusive lower bound a validator sees for `validFrom(ms)`.
const presentedLowerBound = (clock: SlotClock, validFrom: bigint): bigint =>
  BigInt(clock.slotToUnixTime(clock.unixTimeToSlot(Number(validFrom))));

describe("slotAlignedLowerBoundAtOrAfter", () => {
  const preprodLike = slotClock(1_655_769_600_000, 1000, 86_400);

  it("leaves a slot-boundary time unchanged", () => {
    expect(
      slotAlignedLowerBoundAtOrAfter(preprodLike, 1_790_408_317_000n),
    ).toBe(1_790_408_317_000n);
  });

  it("rounds a deadline ending in 999 ms up to the next slot boundary", () => {
    const deadline = 1_790_408_316_999n;
    const aligned = slotAlignedLowerBoundAtOrAfter(preprodLike, deadline);
    expect(aligned).toBe(1_790_408_317_000n);
    // Floored as-is the deadline is presented 999 ms early; aligned it is
    // presented exactly and satisfies `lower >= deadline`.
    expect(presentedLowerBound(preprodLike, deadline)).toBe(1_790_408_316_000n);
    expect(presentedLowerBound(preprodLike, aligned)).toBe(aligned);
    expect(presentedLowerBound(preprodLike, aligned) >= deadline).toBe(true);
  });

  it("rounds one millisecond past a boundary up to the following boundary", () => {
    expect(
      slotAlignedLowerBoundAtOrAfter(preprodLike, 1_790_408_317_001n),
    ).toBe(1_790_408_318_000n);
  });

  it("follows the network slot length and origin rather than a 1 s grid", () => {
    const twentySecond = slotClock(1_000_500, 20_000);
    expect(slotAlignedLowerBoundAtOrAfter(twentySecond, 1_020_500n)).toBe(
      1_020_500n,
    );
    expect(slotAlignedLowerBoundAtOrAfter(twentySecond, 1_020_501n)).toBe(
      1_040_500n,
    );
    expect(slotAlignedLowerBoundAtOrAfter(twentySecond, 1_040_499n)).toBe(
      1_040_500n,
    );
    const halfSecond = slotClock(7, 500);
    expect(slotAlignedLowerBoundAtOrAfter(halfSecond, 1_999n)).toBe(2_007n);
    expect(slotAlignedLowerBoundAtOrAfter(halfSecond, 2_007n)).toBe(2_007n);
    expect(slotAlignedLowerBoundAtOrAfter(halfSecond, 2_008n)).toBe(2_507n);
  });

  it("uses the emulator's slot grid when given a Lucid instance", async () => {
    const account = generateEmulatorAccount({ lovelace: 10_000_000n });
    const emulator = new Emulator([account]);
    const lucid = await Lucid(emulator, "Preprod");
    const boundary = BigInt(lucid.slotToUnixTime(42));
    expect(slotAlignedLowerBoundAtOrAfter(lucid, boundary)).toBe(boundary);
    expect(slotAlignedLowerBoundAtOrAfter(lucid, boundary - 1n)).toBe(boundary);
    expect(slotAlignedLowerBoundAtOrAfter(lucid, boundary + 1n)).toBe(
      BigInt(lucid.slotToUnixTime(43)),
    );
  });

  it("rejects a time outside the safe integer range", () => {
    expect(() =>
      slotAlignedLowerBoundAtOrAfter(
        preprodLike,
        BigInt(Number.MAX_SAFE_INTEGER) + 1n,
      ),
    ).toThrow(/unsafe validity lower bound/u);
  });
});

describe("slotAlignedUpperBoundAtOrBefore", () => {
  // The inclusive upper bound a validator sees for `validTo(ms)`.
  const presentedUpperBound = (clock: SlotClock, validTo: bigint): bigint =>
    BigInt(clock.slotToUnixTime(clock.unixTimeToSlot(Number(validTo)))) - 1n;

  it("floors to the slot boundary so that validTo - 1 is the presented bound", () => {
    const preprodLike = slotClock(1_655_769_600_000, 1000, 86_400);
    const midSlot = 1_790_408_317_437n;
    expect(presentedUpperBound(preprodLike, midSlot)).not.toBe(midSlot - 1n);
    const aligned = slotAlignedUpperBoundAtOrBefore(preprodLike, midSlot);
    expect(aligned).toBe(1_790_408_317_000n);
    expect(presentedUpperBound(preprodLike, aligned)).toBe(aligned - 1n);
    expect(slotAlignedUpperBoundAtOrBefore(preprodLike, aligned)).toBe(aligned);
  });

  it("follows the network slot length and origin", () => {
    const twentySecond = slotClock(1_000_500, 20_000);
    expect(slotAlignedUpperBoundAtOrBefore(twentySecond, 1_020_500n)).toBe(
      1_020_500n,
    );
    expect(slotAlignedUpperBoundAtOrBefore(twentySecond, 1_040_499n)).toBe(
      1_020_500n,
    );
  });

  it("rejects a time outside the safe integer range", () => {
    expect(() =>
      slotAlignedUpperBoundAtOrBefore(
        slotClock(0, 1000),
        BigInt(Number.MAX_SAFE_INTEGER) + 1n,
      ),
    ).toThrow(/unsafe validity upper bound/);
  });
});
