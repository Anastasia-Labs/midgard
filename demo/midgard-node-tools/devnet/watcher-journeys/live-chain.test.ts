import { describe, expect, it, vi } from "vitest";

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

  it("a zero polling delay does not wait for the clock or ledger", async () => {
    const { chain, sleep, readTipSlot } = fixture([]);
    await chain.delaySlots(0);
    expect(sleep).not.toHaveBeenCalled();
    expect(readTipSlot).not.toHaveBeenCalled();
  });
});
