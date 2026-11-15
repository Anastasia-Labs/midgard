import { afterEach, describe, expect, it, vi } from "vitest";

import { AvailabilityResponderAwaitingScanError } from "../../src/availability/awaiting-scan-error.js";
import {
  committeeL1Clock,
  committeeL1NowMs,
  committeeL1Time,
} from "../../src/l1/follower/l1-clock.js";

const ZERO_TIME = 1_700_000_000_000;
const slotConfig = { zeroTime: ZERO_TIME, zeroSlot: 0, slotLength: 1_000 };
const slotStart = (slot: number) => ZERO_TIME + slot * 1_000;

const scene = () => {
  let monotonic = 50_000;
  const clock = committeeL1Clock({
    slotConfig,
    monotonicNowMs: () => monotonic,
  });
  return {
    clock,
    advance: (ms: number) => {
      monotonic += ms;
    },
  };
};

afterEach(() => vi.useRealTimers());

describe("committee L1 clock", () => {
  it("has no time until a tip is observed, and the responder then waits", () => {
    const { clock } = scene();
    expect(clock.tipSlot()).toBeNull();
    expect(clock.slotNow()).toBeNull();
    expect(clock.nowMs()).toBeNull();
    expect(() => committeeL1NowMs(clock)).toThrow(
      AvailabilityResponderAwaitingScanError,
    );
    expect(() => committeeL1Time(clock)).toThrow(
      AvailabilityResponderAwaitingScanError,
    );
  });

  it("is the observed tip plus the whole slots elapsed on the monotonic clock", () => {
    const { clock, advance } = scene();
    clock.observeTipSlot(200);
    expect(clock.slotNow()).toBe(200);
    expect(clock.nowMs()).toBe(slotStart(200));
    advance(2_999);
    expect(clock.slotNow()).toBe(202);
    expect(committeeL1NowMs(clock)).toBe(slotStart(202));
    advance(1);
    expect(committeeL1Time(clock)).toEqual({
      tipSlot: 200,
      nowMs: slotStart(203),
    });
  });

  it("never decreases for a stale or rolled-back tip, and re-anchors on a newer one", () => {
    const { clock, advance } = scene();
    clock.observeTipSlot(100);
    advance(10_000);
    expect(clock.slotNow()).toBe(110);
    clock.observeTipSlot(130);
    expect(clock.slotNow()).toBe(130);
    clock.observeTipSlot(90);
    expect(clock.tipSlot()).toBe(90);
    expect(clock.slotNow()).toBe(130);
    expect(clock.nowMs()).toBe(slotStart(130));
    advance(5_000);
    expect(clock.slotNow()).toBe(135);
  });

  it("ignores the wall clock entirely", () => {
    vi.useFakeTimers();
    const { clock, advance } = scene();
    vi.setSystemTime(slotStart(500));
    clock.observeTipSlot(300);
    advance(4_000);
    const before = clock.nowMs();
    for (const skewMs of [-120_000, 120_000, -3_600_000]) {
      vi.setSystemTime(slotStart(500) + skewMs);
      expect(clock.nowMs()).toBe(before);
    }
    expect(before).toBe(slotStart(304));
  });

  it("maps slots on the follower's slot configuration", () => {
    const clock = committeeL1Clock({
      slotConfig: { zeroTime: 1_000, zeroSlot: 10, slotLength: 200 },
      monotonicNowMs: () => 0,
    });
    clock.observeTipSlot(15);
    expect(clock.nowMs()).toBe(1_000 + 5 * 200);
  });
});
