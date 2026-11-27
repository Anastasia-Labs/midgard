import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { journeyFamilyDeadline } from "./journey-family-deadline.js";
import { MAX_TIMER_MS } from "./journey-timing.journey-timing-for-plan.js";
import {
  journeyExecutionTiming,
  journeyTimingForCategory,
} from "./journey-timing.js";

beforeEach(() => {
  vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "performance"] });
});
afterEach(() => {
  vi.useRealTimers();
});

/**
 * A family's limit as `journey.test.ts` builds it on a devnet, at Preprod's
 * cadence and at the fast devnet's: the family's execution timing from the
 * live cadence and a captured tip, and its healthy replay's utilization.
 */
describe.each([0.05, 0.25])(
  "a family's limit at active slot coefficient %s",
  (activeSlotsCoeff) => {
    const timing = journeyTimingForCategory("executionNativeScriptInvalid", {
      slotLengthSeconds: 1,
      activeSlotsCoeff,
      confirmationDepth: 30,
      actionDepth: 1,
    });
    const execution = journeyExecutionTiming(timing, {
      blockNo: 1_700,
      slot: 34_000,
      blockHash: "11".repeat(32),
    });
    const { replayUtilization } = execution.healthyReplay;
    const limitMs = execution.journeyTimeoutMs;
    /** The flat count the correction was once budgeted by. */
    const oldCorrectionBudgetMs = 32 * timing.transactionAllowanceMs;
    const replayOf = (pausedMs: number) =>
      (pausedMs * replayUtilization) / (1 - replayUtilization);

    it("never cuts off a correction longer than the old count-based budget", async () => {
      const deadline = journeyFamilyDeadline({ limitMs, replayUtilization });
      try {
        const beforeMs = timing.allowances.watcherStartupMs;
        await vi.advanceTimersByTimeAsync(beforeMs);
        // Longer than the old budget, and than the family's whole limit.
        const correctionMs = oldCorrectionBudgetMs + limitMs;
        await deadline.excluding(async () => {
          await vi.advanceTimersByTimeAsync(correctionMs);
          expect(deadline.signal.aborted).toBe(false);
        });
        // The rest of the family keeps its whole budget, plus the replay
        // of the blocks that arrived during the correction.
        const restMs = limitMs - beforeMs + replayOf(correctionMs);
        await vi.advanceTimersByTimeAsync(Math.floor(restMs) - 1);
        expect(deadline.signal.aborted).toBe(false);
        await vi.advanceTimersByTimeAsync(2);
        expect(deadline.signal.aborted).toBe(true);
        expect(deadline.signal.reason).toEqual(
          new Error(`The family's ${limitMs.toString()} ms limit passed`),
        );
      } finally {
        deadline.dispose();
      }
    });

    it("still ends a family whose other stages overrun its limit", async () => {
      const deadline = journeyFamilyDeadline({ limitMs, replayUtilization });
      try {
        await vi.advanceTimersByTimeAsync(limitMs - 1);
        expect(deadline.signal.aborted).toBe(false);
        await vi.advanceTimersByTimeAsync(1);
        expect(deadline.signal.aborted).toBe(true);
      } finally {
        deadline.dispose();
      }
    });

    it("keeps every timer inside Node's range when a pause extends the limit past it", async () => {
      const deadline = journeyFamilyDeadline({ limitMs, replayUtilization });
      try {
        // A pause long enough that the remaining limit exceeds one timer.
        const pausedMs = Math.ceil(
          (2 * MAX_TIMER_MS * (1 - replayUtilization)) / replayUtilization,
        );
        await deadline.excluding(async () => {
          await vi.advanceTimersByTimeAsync(pausedMs);
        });
        const restMs = limitMs + replayOf(pausedMs);
        expect(restMs).toBeGreaterThan(2 * MAX_TIMER_MS);
        // Node would run a longer timer after one millisecond; the limit
        // waits in whole timers of at most MAX_TIMER_MS instead.
        const before = performance.now();
        await vi.advanceTimersToNextTimerAsync();
        expect(performance.now() - before).toBe(MAX_TIMER_MS);
        expect(deadline.signal.aborted).toBe(false);
        await vi.advanceTimersByTimeAsync(
          Math.floor(restMs) - MAX_TIMER_MS - 1,
        );
        expect(deadline.signal.aborted).toBe(false);
        await vi.advanceTimersByTimeAsync(2);
        expect(deadline.signal.aborted).toBe(true);
      } finally {
        deadline.dispose();
      }
    });
  },
);
