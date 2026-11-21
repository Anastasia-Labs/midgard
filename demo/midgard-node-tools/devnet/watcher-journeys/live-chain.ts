import { setTimeout as pause } from "node:timers/promises";

import type { LucidEvolution } from "@lucid-evolution/lucid";
import type { PublishedWorkflowChain } from "midgard-node/tests/helpers/published-workflow-deployment";

import { awaitLedgerTipSlot } from "./ledger-tip.js";

/**
 * How long the tip may stay short of a bound once the wall clock has passed
 * it, before the wait counts the tip as stalled.
 */
export const LEDGER_TIP_STALL_ALLOWANCE_MS = 15 * 60_000;

/** Shared timing for fresh and resumed live journeys. Polling never gates on a block. */
export const createLiveWorkflowChain = ({
  lucid,
  slotLength,
  readTipSlot,
  sleep = pause,
}: {
  lucid: Pick<
    LucidEvolution,
    "currentSlot" | "slotToUnixTime" | "unixTimeToSlot"
  >;
  slotLength: number;
  readTipSlot: () => Promise<number>;
  sleep?: (milliseconds: number) => Promise<unknown>;
}): PublishedWorkflowChain => ({
  now: () => lucid.slotToUnixTime(lucid.currentSlot()),
  delaySlots: async (slots) => {
    if (slots > 0) await sleep(slots * slotLength);
  },
  awaitLedgerTime: async (targetUnixTimeMs) => {
    const enclosingSlot = lucid.unixTimeToSlot(targetUnixTimeMs);
    // Strict bounds such as activation + 1ms must reach the following slot.
    const targetSlot =
      lucid.slotToUnixTime(enclosingSlot) < targetUnixTimeMs
        ? enclosingSlot + 1
        : enclosingSlot;
    // A bound still ahead of the wall clock (the end of a shift, say) is
    // waited for in full; the stall allowance runs from when it is due.
    await awaitLedgerTipSlot({
      targetSlot,
      readTipSlot,
      timeoutMs:
        Math.max(0, targetUnixTimeMs - Date.now()) +
        LEDGER_TIP_STALL_ALLOWANCE_MS,
      pollMs: slotLength,
      sleep,
    });
  },
});
