import { describe, expect, it } from "vitest";

import { awaitLedgerTipSlot } from "./ledger-tip.js";

describe("ledger tip wait", () => {
  it("returns once the tip reaches the target slot", async () => {
    const tips = [100, 100, 118, 131];
    const slept: number[] = [];
    await expect(
      awaitLedgerTipSlot({
        targetSlot: 130,
        readTipSlot: async () => tips.shift() ?? 131,
        timeoutMs: 60_000,
        pollMs: 250,
        now: () => 0,
        sleep: async (ms) => {
          slept.push(ms);
        },
      }),
    ).resolves.toBe(131);
    expect(slept).toEqual([250, 250, 250]);
  });

  it("does not wait when the tip is already past the target", async () => {
    let reads = 0;
    await expect(
      awaitLedgerTipSlot({
        targetSlot: 130,
        readTipSlot: async () => {
          reads += 1;
          return 140;
        },
        timeoutMs: 1,
        now: () => 0,
        sleep: async () => {
          throw new Error("must not sleep");
        },
      }),
    ).resolves.toBe(140);
    expect(reads).toBe(1);
  });

  it("fails once the tip stalls past the deadline", async () => {
    let clock = 0;
    await expect(
      awaitLedgerTipSlot({
        targetSlot: 130,
        readTipSlot: async () => 100,
        timeoutMs: 5_000,
        pollMs: 1_000,
        now: () => clock,
        sleep: async (ms) => {
          clock += ms;
        },
      }),
    ).rejects.toThrow("stalled at slot 100 before reaching slot 130");
  });
});
