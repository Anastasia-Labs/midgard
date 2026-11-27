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

  it("is unchanged by anchoring while the lookahead's window is admissible at the tip", async () => {
    for (const [lagSlots, validFromMs] of [
      // The lower bound backdated 60 s from the tip.
      [0, -60_000],
      [30, -90_000],
      [120, -180_000],
      // The lower bound the longest range allows from the lookahead.
      [150, -180_000],
      [178, -180_000],
    ] as const) {
      const { emulator, lucid } = await scene(300);
      const nowMs = emulator.now();
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
        validFrom: BigInt(nowMs + validFromMs),
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

  it("is ready, anchored on the tip, once the lookahead's window starts past the tip", async () => {
    for (const lagSlots of [179, 200]) {
      const scene = await lagging(lagSlots);
      expectAdmissible(scene);
      // The lower bound the submit margin before the tip, and the longest
      // range from it.
      expect(scene.window).toMatchObject({
        validFrom: BigInt(scene.lucid.slotToUnixTime(scene.tipSlot - 2)),
        validTo: BigInt(scene.lucid.slotToUnixTime(scene.tipSlot - 2 + 240)),
      });
    }
  });

  it("is ready at the longest lag the range leaves the submit margin past the clock for", async () => {
    const scene = await lagging(236);
    expectAdmissible(scene);
    expect(scene.window).toMatchObject({
      validTo: BigInt(scene.lucid.slotToUnixTime(scene.nowSlot + 2)),
    });
  });

  it("is not due once the tip trails the clock past the anchored range", async () => {
    const { lucid, tipSlot, window } = await lagging(237);
    expect(window).toMatchObject({ status: "not_due", tipSlot });
    if (window.status !== "not_due") return;
    expect(window.dueSlot).toBe(
      lucid.unixTimeToSlot(Number(window.validFrom)) + 2,
    );
    expect(window.dueSlot).toBeGreaterThan(tipSlot);
  });

  it("keeps every ready window within 240 s, from the lookahead to 178 slots behind and anchored to 236", async () => {
    expect(DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS).toBe(240_000n);
    const { emulator, lucid } = await scene(300);
    const nowMs = emulator.now();
    const lookaheadEnd = BigInt(nowMs + 60_000);
    for (let lagSlots = 0; lagSlots <= 240; lagSlots++) {
      const tipSlot = emulator.slot - lagSlots;
      const window = daAvailabilityValidityWindow({
        lucid,
        tip: { currentSlot: emulator.slot, ledgerTipSlot: tipSlot },
        nowMs,
      });
      expect(window.status, `lag ${lagSlots.toString()}`).toBe(
        lagSlots <= 236 ? "ready" : "not_due",
      );
      if (window.status !== "ready") continue;
      expect(window.validTo - window.validFrom).toBeLessThanOrEqual(240_000n);
      expect(
        lucid.unixTimeToSlot(Number(window.validFrom)) + 2,
      ).toBeLessThanOrEqual(tipSlot);
      // The lookahead's interval through 178 slots, anchored past it.
      expect(
        window.validTo === lookaheadEnd,
        `lag ${lagSlots.toString()}`,
      ).toBe(lagSlots <= 178);
    }
  });

  it("anchors a deadline-bounded window on the tip: a deadline past the anchored range is reached now", async () => {
    // The deadline is 40 s past the L1 clock, the tip 210 s behind it: the
    // range ending at the deadline starts past the tip.
    const anchored = await lagging(210, ({ nowMs }) => ({
      upperLimitMs: BigInt(nowMs + 40_000 + 1),
    }));
    expectAdmissible(anchored);
    expect(anchored.window).toMatchObject({
      validTo: BigInt(anchored.lucid.slotToUnixTime(anchored.tipSlot + 238)),
    });
    if (anchored.window.status === "ready")
      expect(anchored.window.validTo).toBeLessThanOrEqual(
        anchored.limits!.upperLimitMs!,
      );
    // A deadline inside the range from the tip caps the upper bound first.
    const capped = await lagging(210, ({ nowMs }) => ({
      upperLimitMs: BigInt(nowMs + 20_000 + 1),
    }));
    expectAdmissible(capped);
    expect(capped.window).toMatchObject({
      validTo: BigInt(capped.nowMs + 20_000),
    });
    // Past the covered lag, the same deadline waits for the next block.
    const beyond = await lagging(237, ({ nowMs }) => ({
      upperLimitMs: BigInt(nowMs + 40_000 + 1),
    }));
    expect(beyond.window.status).toBe("not_due");
  });

  it("never starts a lower-limited window before its limit, however far the tip trails", async () => {
    for (const lagSlots of [0, 61, 179, 236, 237])
      for (const offsetMs of [-30_000, -2_000, -2_000 + 1, 0]) {
        const { limits, window } = await lagging(lagSlots, ({ tipMs }) => ({
          lowerLimitMs: BigInt(tipMs + offsetMs),
        }));
        // Ready while the limit is the submit margin behind the tip and the
        // anchored range reaches past the clock; not due otherwise.
        expect(window.status).toBe(
          lagSlots <= 236 && offsetMs <= -2_000 ? "ready" : "not_due",
        );
        if (window.status === "refused") continue;
        expect(window.validFrom).toBeGreaterThanOrEqual(limits!.lowerLimitMs!);
      }
  });

  it("waits rather than refuses a lower limit past the anchored range's end", async () => {
    // The anchored interval ends 238 s past the tip, before the limit; the
    // lookahead's still has room after it, once the tip reaches it.
    const { tipSlot, window } = await lagging(210, ({ tipMs }) => ({
      lowerLimitMs: BigInt(tipMs + 239_000),
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
    // The anchored end is 238 slots past the tip: 58 slots of room is left
    // up to a lag of 180.
    expect((await roomed(180))(58).status).toBe("ready");
    expect((await roomed(181))(58).status).toBe("not_due");
    // A room under the submit margin is the submit margin.
    expect((await roomed(236))(1).status).toBe("ready");
    expect((await roomed(237))(1).status).toBe("not_due");
    // The room gates only the anchored interval: the lookahead's is unchanged.
    const at150 = await roomed(150);
    expect(at150(100)).toEqual(at150());
    const at180 = await roomed(180);
    expect(() => at180(1.5)).toThrow("Invalid validity window minimum room");
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
