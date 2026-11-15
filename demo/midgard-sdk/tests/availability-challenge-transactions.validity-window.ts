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

  it("is unchanged by anchoring while the old window is admissible at the tip", async () => {
    for (const lagSlots of [0, 30, 58]) {
      const { emulator, lucid } = await scene(300);
      const nowMs = emulator.now();
      // The lookahead past the L1 clock, and the lower bound the longest
      // range allows from it.
      expect(
        daAvailabilityValidityWindow({
          lucid,
          tip: {
            currentSlot: emulator.slot,
            ledgerTipSlot: emulator.slot - lagSlots,
          },
          nowMs,
        }),
      ).toEqual({
        status: "ready",
        validFrom: BigInt(nowMs + 60_000 - 120_000),
        validTo: BigInt(nowMs + 60_000),
      });
    }
  });

  /** The window with the ledger tip `lagSlots` slots behind the L1 clock. */
  const lagging = async (
    lagSlots: number,
    limitsOf?: (
      at: Readonly<{ nowMs: number; tipMs: number }>,
    ) => Parameters<typeof daAvailabilityValidityWindow>[0]["limits"],
  ) => {
    const { emulator, lucid } = await scene(300);
    const tipSlot = emulator.slot - lagSlots;
    const nowMs = emulator.now();
    const limits = limitsOf?.({ nowMs, tipMs: lucid.slotToUnixTime(tipSlot) });
    const window = daAvailabilityValidityWindow({
      lucid,
      tip: { currentSlot: emulator.slot, ledgerTipSlot: tipSlot },
      nowMs,
      ...(limits === undefined ? {} : { limits }),
    });
    return { lucid, tipSlot, nowMs, nowSlot: emulator.slot, limits, window };
  };

  /** Admissible at the tip, includable after the L1 clock's slot, in range. */
  const expectAdmissible = ({
    lucid,
    tipSlot,
    nowMs,
    nowSlot,
    window,
  }: Awaited<ReturnType<typeof lagging>>) => {
    expect(window.status).toBe("ready");
    if (window.status !== "ready") return;
    expect(
      lucid.unixTimeToSlot(Number(window.validFrom)) + 2,
    ).toBeLessThanOrEqual(tipSlot);
    expect(window.validTo).toBeGreaterThan(BigInt(nowMs));
    expect(lucid.unixTimeToSlot(Number(window.validTo))).toBeGreaterThanOrEqual(
      nowSlot + 2,
    );
    expect(window.validTo - window.validFrom).toBeLessThanOrEqual(
      DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS,
    );
  };

  it("is ready, anchored on the tip, while the tip trails the clock just past the backdate", async () => {
    for (const lagSlots of [59, 61]) {
      const scene = await lagging(lagSlots);
      expectAdmissible(scene);
      // The lower bound the submit margin before the tip, and the longest
      // range from it.
      expect(scene.window).toMatchObject({
        validFrom: BigInt(scene.lucid.slotToUnixTime(scene.tipSlot - 2)),
        validTo: BigInt(scene.lucid.slotToUnixTime(scene.tipSlot - 2 + 120)),
      });
    }
  });

  it("is ready at the longest lag the range leaves the submit margin past the clock for", async () => {
    const scene = await lagging(116);
    expectAdmissible(scene);
    expect(scene.window).toMatchObject({
      validTo: BigInt(scene.lucid.slotToUnixTime(scene.nowSlot + 2)),
    });
  });

  it("is not due once the tip trails the clock past the anchored range", async () => {
    const { lucid, tipSlot, window } = await lagging(117);
    expect(window).toMatchObject({ status: "not_due", tipSlot });
    if (window.status !== "not_due") return;
    expect(window.dueSlot).toBe(
      lucid.unixTimeToSlot(Number(window.validFrom)) + 2,
    );
    expect(window.dueSlot).toBeGreaterThan(tipSlot);
  });

  it("anchors a deadline-bounded window on the tip: a deadline past the anchored range is reached now", async () => {
    // The deadline is 40 s past the L1 clock, the tip 90 s behind it: the
    // range ending at the deadline starts past the tip.
    const anchored = await lagging(90, ({ nowMs }) => ({
      upperLimitMs: BigInt(nowMs + 40_000 + 1),
    }));
    expectAdmissible(anchored);
    expect(anchored.window).toMatchObject({
      validTo: BigInt(anchored.lucid.slotToUnixTime(anchored.tipSlot + 118)),
    });
    if (anchored.window.status === "ready")
      expect(anchored.window.validTo).toBeLessThanOrEqual(
        anchored.limits!.upperLimitMs!,
      );
    // A deadline inside the range from the tip caps the upper bound first.
    const capped = await lagging(90, ({ nowMs }) => ({
      upperLimitMs: BigInt(nowMs + 20_000 + 1),
    }));
    expectAdmissible(capped);
    expect(capped.window).toMatchObject({
      validTo: BigInt(capped.nowMs + 20_000),
    });
    // Past the covered lag, the same deadline waits for the next block.
    const beyond = await lagging(117, ({ nowMs }) => ({
      upperLimitMs: BigInt(nowMs + 40_000 + 1),
    }));
    expect(beyond.window.status).toBe("not_due");
  });

  it("never starts a lower-limited window before its limit, however far the tip trails", async () => {
    for (const lagSlots of [0, 61, 90, 116, 117])
      for (const offsetMs of [-30_000, -2_000, -2_000 + 1, 0]) {
        const { limits, window } = await lagging(lagSlots, ({ tipMs }) => ({
          lowerLimitMs: BigInt(tipMs + offsetMs),
        }));
        // Ready while the limit is the submit margin behind the tip and the
        // anchored range reaches past the clock; not due otherwise.
        expect(window.status).toBe(
          lagSlots <= 116 && offsetMs <= -2_000 ? "ready" : "not_due",
        );
        if (window.status === "refused") continue;
        expect(window.validFrom).toBeGreaterThanOrEqual(limits!.lowerLimitMs!);
      }
  });

  it("waits rather than refuses a lower limit past the anchored range's end", async () => {
    // The anchored interval ends 118 s past the tip, before the limit; the
    // lookahead's still has room after it, once the tip reaches it.
    const { tipSlot, window } = await lagging(90, ({ tipMs }) => ({
      lowerLimitMs: BigInt(tipMs + 119_000),
    }));
    expect(window).toMatchObject({ status: "not_due", tipSlot });
  });

  it("anchors only while the interval runs the caller's minimum room past the clock", async () => {
    /** The window `lagSlots` behind the clock, on one scene, by its room
     * (the window's default without one). */
    const roomed = async (lagSlots: number) => {
      const { emulator, lucid } = await scene(300);
      const input = {
        lucid,
        tip: {
          currentSlot: emulator.slot,
          ledgerTipSlot: emulator.slot - lagSlots,
        },
        nowMs: emulator.now(),
      };
      return (minimumRoomSlots?: number) =>
        daAvailabilityValidityWindow(
          minimumRoomSlots === undefined
            ? input
            : { ...input, minimumRoomSlots },
        );
    };
    // The anchored end is 118 slots past the tip: 56 slots of room is left
    // up to a lag of 62.
    expect((await roomed(62))(56).status).toBe("ready");
    expect((await roomed(63))(56).status).toBe("not_due");
    // A room under the submit margin is the submit margin.
    expect((await roomed(116))(1).status).toBe("ready");
    expect((await roomed(117))(1).status).toBe("not_due");
    // The room gates only the anchored interval: the lookahead's is unchanged.
    const at30 = await roomed(30);
    expect(at30(100)).toEqual(at30());
    const at62 = await roomed(62);
    expect(() => at62(1.5)).toThrow("Invalid validity window minimum room");
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
