import { afterEach, describe, expect, it, vi } from "vitest";

import { createLiveWorkflowChain } from "./live-chain.js";

const fixture = (tips: number[], slotLength = 1000) => {
  const readTipSlot = vi.fn(async () => {
    const tip = tips.shift();
    if (tip === undefined) throw new Error("Unexpected ledger query");
    return tip;
  });
  const sleep = vi.fn(async (_milliseconds: number) => {});
  const chain = createLiveWorkflowChain({
    lucid: {
      currentSlot: () => 200,
      slotToUnixTime: (slot) => 10_000 + slot * slotLength,
      unixTimeToSlot: (time) => Math.floor((time - 10_000) / slotLength),
    },
    slotLength,
    readTipSlot,
    sleep,
  });
  return { chain, sleep, readTipSlot };
};

describe("live journey polling and protocol timing", () => {
  it("polls delayed receipts without querying a stalled ledger tip", async () => {
    const { chain, sleep, readTipSlot } = fixture([]);
    const receipt = vi
      .fn()
      .mockResolvedValueOnce(false)
      .mockResolvedValueOnce(false)
      .mockResolvedValue(true);
    while (!(await receipt())) await chain.delaySlots(1);
    expect(receipt).toHaveBeenCalledTimes(3);
    expect(sleep.mock.calls).toEqual([[1000], [1000]]);
    expect(readTipSlot).not.toHaveBeenCalled();
  });

  it("returns immediately for a satisfied bound even when the tip trails the clock", async () => {
    const { chain, sleep, readTipSlot } = fixture([150]);
    expect(chain.now()).toBe(210_000);
    await chain.awaitLedgerTime(140_000);
    expect(readTipSlot).toHaveBeenCalledTimes(1);
    expect(sleep).not.toHaveBeenCalled();
  });

  it("still waits for the canonical bound when the wall clock has passed it", async () => {
    const { chain, sleep, readTipSlot } = fixture([100, 129, 130]);
    await chain.awaitLedgerTime(140_000);
    expect(readTipSlot).toHaveBeenCalledTimes(3);
    expect(sleep.mock.calls).toEqual([[1000], [1000]]);
  });

  it("waits for a future bound independently of the current wall-clock slot", async () => {
    const { chain, sleep } = fixture([200, 201]);
    await chain.awaitLedgerTime(211_000);
    expect(sleep.mock.calls).toEqual([[1000]]);
  });

  it.each([1000, 2000])(
    "rounds a strict +1ms bound up using %i ms slots",
    async (slotLength) => {
      const { chain, sleep, readTipSlot } = fixture([130, 131], slotLength);
      await chain.awaitLedgerTime(10_000 + 130 * slotLength + 1);
      expect(readTipSlot).toHaveBeenCalledTimes(2);
      expect(sleep.mock.calls).toEqual([[slotLength]]);
    },
  );

  describe("a bound further ahead than a stalled tip's allowance", () => {
    afterEach(() => {
      vi.useRealTimers();
    });
    /**
     * A Preprod-shaped chain: the wall clock is the system clock, each sleep
     * passes that much of it, and the tip moves in 20-slot block jumps
     * behind it (or not at all once `stallAt` is reached).
     */
    const preprod = (stallAt = Number.POSITIVE_INFINITY) => {
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(210_000);
      const slotOf = (time: number) => Math.floor((time - 10_000) / 1000);
      return createLiveWorkflowChain({
        lucid: {
          currentSlot: () => slotOf(Date.now()),
          slotToUnixTime: (slot) => 10_000 + slot * 1000,
          unixTimeToSlot: slotOf,
        },
        slotLength: 1000,
        readTipSlot: async () => {
          const slot = slotOf(Date.now());
          return Math.min(slot - (slot % 20), stallAt);
        },
        sleep: async (milliseconds) => {
          vi.setSystemTime(Date.now() + milliseconds);
        },
      });
    };

    it("waits out a whole operator shift for a tip moving in block jumps", async () => {
      const chain = preprod();
      const shiftEnd = chain.now() + 1_800_000;
      await chain.awaitLedgerTime(shiftEnd);
      expect(chain.now()).toBeGreaterThanOrEqual(shiftEnd);
    });

    it("still refuses a tip that stalls before a far bound", async () => {
      const chain = preprod(1000);
      await expect(
        chain.awaitLedgerTime(chain.now() + 1_800_000),
      ).rejects.toThrow(/Ledger tip stalled at slot 1000/u);
    });
  });

  it("a zero polling delay does not wait for the clock or ledger", async () => {
    const { chain, sleep, readTipSlot } = fixture([]);
    await chain.delaySlots(0);
    expect(sleep).not.toHaveBeenCalled();
    expect(readTipSlot).not.toHaveBeenCalled();
  });
});
