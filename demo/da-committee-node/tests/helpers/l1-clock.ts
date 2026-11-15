import type { SlotConfig } from "@lucid-evolution/lucid";

import {
  type AvailabilityResponderBuildTime,
  availabilityResponderL1BuildTime,
} from "../../src/availability/factory.discover-availability-responder-challenges.js";
import {
  type CommitteeL1Clock,
  committeeL1Clock,
} from "../../src/l1/follower/l1-clock.js";

/** One-second slots from the POSIX epoch: the slot is the second. */
export const TEST_SLOT_CONFIG: SlotConfig = {
  zeroTime: 0,
  zeroSlot: 0,
  slotLength: 1_000,
};

/**
 * A committee L1 clock for a test. With `tipSlot` it has observed that tip
 * already, as a responder step's first boundary read would have.
 */
export const testL1Clock = (
  input: Readonly<{ slotConfig?: SlotConfig; tipSlot?: number }> = {},
): CommitteeL1Clock => {
  const clock = committeeL1Clock({
    slotConfig: input.slotConfig ?? TEST_SLOT_CONFIG,
  });
  if (input.tipSlot !== undefined) clock.observeTipSlot(input.tipSlot);
  return clock;
};

/** The plain responder's build time on a test L1 clock (`testL1Clock`). */
export const testBuildTime = (
  input: Parameters<typeof testL1Clock>[0] = {},
): (() => AvailabilityResponderBuildTime) => {
  const clock = testL1Clock(input);
  return () => availabilityResponderL1BuildTime(clock);
};

/**
 * A committee L1 clock on a controlled monotonic clock that observed
 * `tipSlot` `elapsedMs` ago: its "now" is the tip plus the whole slots since.
 */
export const l1ClockAt = (
  input: Readonly<{
    slotConfig: SlotConfig;
    tipSlot: number;
    elapsedMs: number;
  }>,
): Readonly<{ clock: CommitteeL1Clock; advance: (ms: number) => void }> => {
  let monotonic = 0;
  const clock = committeeL1Clock({
    slotConfig: input.slotConfig,
    monotonicNowMs: () => monotonic,
  });
  clock.observeTipSlot(input.tipSlot);
  monotonic += input.elapsedMs;
  return {
    clock,
    advance: (ms) => {
      monotonic += ms;
    },
  };
};
