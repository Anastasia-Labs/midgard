/**
 * Test L1 tips for `l1SlotNow`: registers a tip source on a Lucid client (a
 * stub or a live-like one) whose covered tip slot a test sets directly, with a
 * monotonic clock the test owns. A wall clock the test fakes never reaches it.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { registerL1TipSource } from "../../src/l1-heads.js";

export type TestL1Tip = {
  /** Sets the ledger tip slot the next read returns. */
  setTipSlot(slot: number): void;
  /** Moves the monotonic clock forward. */
  advanceMs(ms: number): void;
  /** How many reads the source has served. */
  reads(): number;
};

export const registerTestL1Tip = (
  api: LucidEvolution,
  tipSlot: number,
): TestL1Tip => {
  let slot = tipSlot;
  let nowMs = 0;
  let reads = 0;
  registerL1TipSource(
    [api],
    () =>
      Effect.sync(() => {
        reads += 1;
        return slot;
      }),
    { slotLengthMs: 1_000, monotonicNowMs: () => nowMs },
  );
  return {
    setTipSlot: (next) => {
      slot = next;
      // Past the refresh window, so the next `l1SlotNow` reads it.
      nowMs += 1_000;
    },
    advanceMs: (ms) => {
      nowMs += ms;
    },
    reads: () => reads,
  };
};

/** A wall clock this many ms fast, for the "10 minutes fast" checks. */
export const TEN_MINUTES_MS = 10 * 60_000;
