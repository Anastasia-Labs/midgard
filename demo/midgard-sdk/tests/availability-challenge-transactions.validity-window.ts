import { Emulator, Lucid } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS,
  daAvailabilityValidityWindow,
} from "../src/availability-challenge-transactions.js";

/** A Lucid client on a fresh emulator, `slots` slots past its first. */
const scene = async (slots: number) => {
  const emulator = new Emulator([]);
  // The client's slot config starts where the emulator is when it connects.
  const lucid = await Lucid(emulator, "Custom");
  emulator.awaitSlot(slots);
  return { emulator, lucid };
};

describe("availability validity window", () => {
  it("is ready with a range inside the limit when the tip is at the clock", async () => {
    const { emulator, lucid } = await scene(300);
    const window = daAvailabilityValidityWindow({
      lucid,
      tip: { currentSlot: emulator.slot, ledgerTipSlot: emulator.slot },
      nowMs: emulator.now(),
    });
    expect(window.status).toBe("ready");
    if (window.status !== "ready") return;
    // The ledger judges the lower bound against the slot after its tip.
    expect(
      lucid.unixTimeToSlot(Number(window.validFrom)) + 2,
    ).toBeLessThanOrEqual(emulator.slot);
    expect(window.validTo).toBeGreaterThan(BigInt(emulator.now()));
    expect(window.validTo - window.validFrom).toBeLessThanOrEqual(
      DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS,
    );
  });

  it("is not due while the ledger tip trails the clock past the range limit", async () => {
    const { emulator, lucid } = await scene(300);
    const tipSlot = emulator.slot - 70;
    const window = daAvailabilityValidityWindow({
      lucid,
      tip: { currentSlot: emulator.slot, ledgerTipSlot: tipSlot },
      nowMs: emulator.now(),
    });
    expect(window).toMatchObject({ status: "not_due", tipSlot });
    if (window.status !== "not_due") return;
    expect(window.dueSlot).toBe(
      lucid.unixTimeToSlot(Number(window.validFrom)) + 2,
    );
    expect(window.dueSlot).toBeGreaterThan(tipSlot);
  });

  it("refuses an upper limit at or before the L1 clock: the window closed", async () => {
    const { emulator, lucid } = await scene(300);
    for (const upperLimitMs of [
      BigInt(emulator.now()),
      BigInt(emulator.now() - 30_000),
    ])
      expect(
        daAvailabilityValidityWindow({
          lucid,
          tip: { currentSlot: emulator.slot, ledgerTipSlot: emulator.slot },
          nowMs: emulator.now(),
          limits: { upperLimitMs },
        }),
      ).toMatchObject({
        status: "refused",
        reason: "window_closed",
        detail: expect.stringContaining(
          "the validity window closed before the L1 clock",
        ),
      });
  });

  it("refuses a lower limit at or past the upper bound: no valid range", async () => {
    const { emulator, lucid } = await scene(300);
    const nowMs = emulator.now();
    for (const limits of [
      // The lower limit is past the upper limit.
      {
        lowerLimitMs: BigInt(nowMs + 30_000),
        upperLimitMs: BigInt(nowMs + 20_000),
      },
      // The lower limit is past the lookahead the upper bound takes.
      { lowerLimitMs: BigInt(nowMs + 120_000) },
    ])
      expect(
        daAvailabilityValidityWindow({
          lucid,
          tip: { currentSlot: emulator.slot, ledgerTipSlot: emulator.slot },
          nowMs,
          limits,
        }),
      ).toMatchObject({
        status: "refused",
        reason: "no_interval",
        detail: expect.stringContaining(
          "the validity window leaves no interval",
        ),
      });
  });
});
