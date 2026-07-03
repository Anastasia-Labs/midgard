import { describe, expect, it } from "vitest";

import {
  createSlotAwareDueWorkRegistry,
  type SlotAwareDueWork,
} from "@/fibers/slot-aware-due-work.js";
import {
  DEFERRABLE_SUBMIT_TIMING_OWNERS,
  slotAwareDueWorkFromSubmitTiming,
  type SubmitTimingNotDuePlanWithDueWorkEvidence,
} from "@/transactions/submit-timing-due-work.js";

const entry: SlotAwareDueWork = {
  kind: "commit_scheduler_refresh",
  key: "block_commitment",
  callerLabel: "scheduler-refresh",
  reason: "scheduler_transition_not_reached",
  observedSlot: 10,
  dueSlot: 20,
  dueAtMs: 1_779_150_010_000,
  waitMs: 10_000,
  slotSource: "test",
  dependencyKey: "dep-a",
  invalidationKey: "inv-a",
};

describe("slot-aware due-work registry", () => {
  it("exposes only owner-backed due-work kinds", () => {
    expect(Object.keys(DEFERRABLE_SUBMIT_TIMING_OWNERS).sort()).toEqual([
      "commit_scheduler_refresh",
      "merge_submit_validity",
    ]);
  });

  it("skips before due slot and wakes at due slot", () => {
    const registry = createSlotAwareDueWorkRegistry();
    registry.register(entry);

    expect(
      registry.check({
        kind: entry.kind,
        key: entry.key,
        currentSlot: 19,
        dependencyKey: entry.dependencyKey,
        invalidationKey: entry.invalidationKey,
      }),
    ).toMatchObject({ status: "skip" });

    expect(
      registry.check({
        kind: entry.kind,
        key: entry.key,
        currentSlot: 20,
        dependencyKey: entry.dependencyKey,
        invalidationKey: entry.invalidationKey,
      }),
    ).toMatchObject({ status: "due" });
    expect(registry.peek(entry.kind, entry.key)).toBeUndefined();
  });

  it("skips and wakes every remaining owner-backed kind", () => {
    const registry = createSlotAwareDueWorkRegistry();
    const kinds = Object.keys(
      DEFERRABLE_SUBMIT_TIMING_OWNERS,
    ) as (keyof typeof DEFERRABLE_SUBMIT_TIMING_OWNERS)[];

    for (const kind of kinds) {
      const keyedEntry: SlotAwareDueWork = {
        ...entry,
        kind,
        key: `${kind}:key`,
      };
      registry.register(keyedEntry);

      expect(
        registry.check({
          kind,
          key: keyedEntry.key,
          currentSlot: keyedEntry.dueSlot - 1,
          dependencyKey: keyedEntry.dependencyKey,
          invalidationKey: keyedEntry.invalidationKey,
        }),
      ).toMatchObject({ status: "skip" });

      expect(
        registry.check({
          kind,
          key: keyedEntry.key,
          currentSlot: keyedEntry.dueSlot,
          dependencyKey: keyedEntry.dependencyKey,
          invalidationKey: keyedEntry.invalidationKey,
        }),
      ).toMatchObject({ status: "due" });
    }
  });

  it("invalidates when dependency evidence changes", () => {
    const registry = createSlotAwareDueWorkRegistry();
    registry.register(entry);

    expect(
      registry.check({
        kind: entry.kind,
        key: entry.key,
        currentSlot: 12,
        dependencyKey: "dep-b",
        invalidationKey: entry.invalidationKey,
      }),
    ).toMatchObject({
      status: "invalidated",
      reason: "dependency_key_changed",
    });
    expect(registry.peek(entry.kind, entry.key)).toBeUndefined();
  });

  it("converts not-due submit timing to due work without inventing evidence", () => {
    const plan = {
      status: "not_due",
      callerLabel: "merge",
      targetSlot: 20,
      dueSlot: 20,
      currentSlot: 10,
      observedSlot: 10,
      observedAtMs: 1_000,
      deltaSlots: 10,
      waitMs: 10_000,
      slotLengthMs: 1_000,
      slotSource: "local_ogmios_tip",
      invalidBeforeSlot: 18,
      reason: "wait_ms=10000,max_inline_wait_ms=5000",
      dependencyKey: "merge:header:18",
      invalidationKey: "merge:header:18",
    } satisfies SubmitTimingNotDuePlanWithDueWorkEvidence;

    expect(
      slotAwareDueWorkFromSubmitTiming({
        kind: "merge_submit_validity",
        key: "merge:header:18",
        callerLabel: "merge",
        reason: "merge_submit_validity_not_reached",
        plan,
        nowMs: 1_000,
      }),
    ).toStrictEqual({
      kind: "merge_submit_validity",
      key: "merge:header:18",
      callerLabel: "merge",
      reason: "merge_submit_validity_not_reached",
      observedSlot: plan.currentSlot,
      dueSlot: plan.targetSlot,
      dueAtMs: 11_000,
      waitMs: plan.waitMs,
      slotSource: plan.slotSource,
      dependencyKey: plan.dependencyKey,
      invalidationKey: plan.invalidationKey,
    });
  });
});
