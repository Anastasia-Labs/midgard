import { describe, expect, it } from "vitest";

import {
  planSubmitTiming,
  planSubmitTimingAfterInlineWait,
} from "@/transactions/submit-timing.js";

const snapshot = (currentSlot: number) => ({
  source: "test" as const,
  currentSlot,
  observedAtMs: 1_779_150_000_000,
  slotLengthMs: 1_000,
});

describe("submit timing planner", () => {
  it("classifies ready, wait, not_due, expired, and narrow windows", () => {
    expect(
      planSubmitTiming({
        callerLabel: "test",
        invalidBeforeSlot: 10,
        invalidHereafterSlot: 20,
        slotSnapshot: snapshot(12),
      }),
    ).toMatchObject({ status: "ready", targetSlot: 12, waitMs: 0 });

    expect(
      planSubmitTiming({
        callerLabel: "test",
        invalidBeforeSlot: 10,
        invalidHereafterSlot: 20,
        slotSnapshot: snapshot(9),
        maxInlineWaitMs: 3_000,
      }),
    ).toMatchObject({ status: "wait", targetSlot: 12, waitMs: 3_000 });

    expect(
      planSubmitTiming({
        callerLabel: "test",
        invalidBeforeSlot: 10,
        invalidHereafterSlot: 20,
        slotSnapshot: snapshot(8),
        maxInlineWaitMs: 1_000,
      }),
    ).toMatchObject({ status: "not_due", targetSlot: 12, waitMs: 4_000 });

    expect(
      planSubmitTiming({
        callerLabel: "test",
        invalidBeforeSlot: 10,
        invalidHereafterSlot: 20,
        slotSnapshot: snapshot(20),
      }),
    ).toMatchObject({ status: "expired" });

    expect(
      planSubmitTiming({
        callerLabel: "test",
        invalidBeforeSlot: 10,
        invalidHereafterSlot: 12,
        slotSnapshot: snapshot(9),
      }),
    ).toMatchObject({ status: "window_too_narrow" });
  });

  it("marks a short inline wait as stalled when refreshed local slot evidence is still behind", () => {
    const initial = planSubmitTiming({
      callerLabel: "test",
      invalidBeforeSlot: 10,
      invalidHereafterSlot: 20,
      slotSnapshot: snapshot(9),
      maxInlineWaitMs: 3_000,
    });
    if (initial.status !== "wait") {
      throw new Error("expected wait plan");
    }

    expect(
      planSubmitTimingAfterInlineWait(initial, snapshot(10)),
    ).toMatchObject({
      status: "slot_source_stalled",
      targetSlot: 12,
    });
  });

  it("fails closed when bounded timing has no local slot source", () => {
    expect(
      planSubmitTiming({
        callerLabel: "test",
        invalidBeforeSlot: 10,
        invalidHereafterSlot: 20,
        slotSnapshotError: new Error("ogmios unavailable"),
      }),
    ).toMatchObject({ status: "slot_source_unavailable" });
  });
});
