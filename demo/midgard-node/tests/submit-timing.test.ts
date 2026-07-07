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

  it("defers positive waits when the caller disallows inline sleeps", () => {
    expect(
      planSubmitTiming({
        callerLabel: "test",
        invalidBeforeSlot: 10,
        invalidHereafterSlot: 20,
        slotSnapshot: snapshot(11),
        maxInlineWaitMs: 60_000,
        inlineWaitPolicy: "defer_positive_wait",
        dependencyKey: "dep-1s",
        invalidationKey: "inv-1s",
      }),
    ).toMatchObject({
      status: "not_due",
      targetSlot: 12,
      currentSlot: 11,
      dueSlot: 12,
      waitMs: 1_000,
      dependencyKey: "dep-1s",
      invalidationKey: "inv-1s",
    });

    expect(
      planSubmitTiming({
        callerLabel: "test",
        invalidBeforeSlot: 100,
        invalidHereafterSlot: 150,
        slotSnapshot: snapshot(84),
        maxInlineWaitMs: 60_000,
        inlineWaitPolicy: "defer_positive_wait",
      }),
    ).toMatchObject({
      status: "not_due",
      targetSlot: 102,
      currentSlot: 84,
      waitMs: 18_000,
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
