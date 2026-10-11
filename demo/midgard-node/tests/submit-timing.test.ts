import { describe, expect, it } from "vitest";

import {
  planSubmitTiming,
  planSubmitTimingAfterInlineWait,
} from "../src/transactions/submit-timing.js";
import { slotAwareDueWorkFromSubmitTiming } from "../src/transactions/submit-timing-due-work.js";

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

  // Devnet r4: the merge was valid from slot 64360; the wall-clock submit slot
  // was 64368 while the ledger's latest block was at 64340, and the mempool
  // refused it at ledger slot 64341.
  const trailingTip = (ledgerTipSlot: number, currentSlot = 64_368) => ({
    source: "l1_node_tip" as const,
    currentSlot,
    ledgerTipSlot,
    observedAtMs: 1_779_150_000_000,
    slotLengthMs: 1_000,
  });

  it("judges a validity lower bound at the ledger tip, not the wall-clock slot ahead of it", () => {
    const deferred = planSubmitTiming({
      callerLabel: "merge",
      invalidBeforeSlot: 64_360,
      slotSnapshot: trailingTip(64_340),
      inlineWaitPolicy: "defer_positive_wait",
    });
    expect(deferred).toMatchObject({
      status: "not_due",
      targetSlot: 64_362,
      dueSlot: 64_362,
      currentSlot: 64_340,
      observedSlot: 64_340,
      deltaSlots: 22,
      waitMs: 22_000,
    });
    if (deferred.status !== "not_due") throw new Error("expected not_due");
    expect(
      slotAwareDueWorkFromSubmitTiming({
        kind: "merge_submit_validity",
        key: "merge:test",
        callerLabel: "merge",
        reason: "test",
        plan: { ...deferred, dependencyKey: "d", invalidationKey: "i" },
      }),
    ).toMatchObject({ observedSlot: 64_340, dueSlot: 64_362 });

    expect(
      planSubmitTiming({
        callerLabel: "merge",
        invalidBeforeSlot: 64_360,
        slotSnapshot: trailingTip(64_362, 64_390),
        inlineWaitPolicy: "defer_positive_wait",
      }),
    ).toMatchObject({
      status: "ready",
      currentSlot: 64_362,
      targetSlot: 64_362,
    });
  });

  it("keeps an inline wait stalled while the ledger tip trails, even once wall time passes the target", () => {
    const initial = planSubmitTiming({
      callerLabel: "test",
      invalidBeforeSlot: 64_360,
      slotSnapshot: trailingTip(64_359, 64_359),
      maxInlineWaitMs: 3_000,
    });
    if (initial.status !== "wait") throw new Error("expected wait plan");
    expect(
      planSubmitTimingAfterInlineWait(initial, trailingTip(64_359, 64_368)),
    ).toMatchObject({ status: "slot_source_stalled", currentSlot: 64_359 });
  });

  it("still judges expiry at the wall-clock slot, which is never behind the ledger tip", () => {
    expect(
      planSubmitTiming({
        callerLabel: "test",
        invalidBeforeSlot: 64_300,
        invalidHereafterSlot: 64_365,
        slotSnapshot: trailingTip(64_340),
      }),
    ).toMatchObject({ status: "expired", currentSlot: 64_368 });
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
