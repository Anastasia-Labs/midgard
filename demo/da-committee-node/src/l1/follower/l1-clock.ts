/**
 * The committee's L1 clock (plan §3.6): the only "now" for an L1 validity or
 * deadline decision the availability responder takes. It is the newest
 * follower tip slot plus the slots elapsed since that tip was observed,
 * never decreasing (`createSlotClock`, `@al-ft/midgard-l1-follower/heads`),
 * with the elapsed time read on a monotonic clock. A committee wall clock
 * that runs fast, slow or jumps cannot move it, so it can never call a
 * response due, expired or past its deadline.
 *
 * The tip comes from the follower boundary the responder already reads: each
 * boundary read observes its point's slot. Until one has, the clock has no
 * "now" and no L1 decision may be taken: the responder waits for the
 * follower (`AvailabilityResponderAwaitingScanError`).
 */
import {
  createSlotClock,
  type MonotonicClock,
} from "@al-ft/midgard-l1-follower/heads";
import type { SlotConfig } from "@lucid-evolution/lucid";

import { AvailabilityResponderAwaitingScanError } from "../../availability/awaiting-scan-error.js";

export type CommitteeL1Clock = Readonly<{
  /** Records the follower's tip slot, as a boundary read saw it. */
  observeTipSlot(slot: number): void;
  /** The latest observed tip slot, or null before the first observation. */
  tipSlot(): number | null;
  /** The newest tip slot plus the whole slots elapsed since it was observed. */
  slotNow(): number | null;
  /** The POSIX time (ms) at the start of `slotNow`, on the follower's slots. */
  nowMs(): number | null;
}>;

/** The L1 time one decision is taken at: the observed tip, and "now". */
export type CommitteeL1Time = Readonly<{ tipSlot: number; nowMs: number }>;

export const committeeL1Clock = (
  input: Readonly<{
    /** The follower's slot configuration (its Lucid client's). */
    slotConfig: SlotConfig;
    /** Defaults to `performance.now()`. */
    monotonicNowMs?: MonotonicClock;
  }>,
): CommitteeL1Clock => {
  const { zeroTime, zeroSlot, slotLength } = input.slotConfig;
  if (!Number.isSafeInteger(zeroTime) || !Number.isSafeInteger(zeroSlot))
    throw new Error("The committee L1 clock needs a whole slot configuration");
  const clock = createSlotClock({
    slotLengthMs: slotLength,
    ...(input.monotonicNowMs === undefined
      ? {}
      : { monotonicNowMs: input.monotonicNowMs }),
  });
  return {
    observeTipSlot: (slot) => clock.observeTipSlot(slot),
    tipSlot: () => clock.tipSlot(),
    slotNow: () => clock.slotNow(),
    nowMs: () => {
      const slot = clock.slotNow();
      return slot === null ? null : zeroTime + (slot - zeroSlot) * slotLength;
    },
  };
};

const noTipYet = () =>
  new AvailabilityResponderAwaitingScanError(
    "no follower tip has been observed yet, so the committee has no L1 time",
  );

/** The clock's "now" in ms; the responder waits while no tip was observed. */
export const committeeL1NowMs = (clock: CommitteeL1Clock): number => {
  const nowMs = clock.nowMs();
  if (nowMs === null) throw noTipYet();
  return nowMs;
};

/** The observed tip and "now"; the responder waits while there is none. */
export const committeeL1Time = (clock: CommitteeL1Clock): CommitteeL1Time => {
  const nowMs = committeeL1NowMs(clock);
  const tipSlot = clock.tipSlot();
  if (tipSlot === null) throw noTipYet();
  return { tipSlot, nowMs };
};
