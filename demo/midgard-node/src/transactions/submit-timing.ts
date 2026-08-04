import {
  SUBMIT_SLOT_LENGTH_MS,
  SUBMIT_SLOT_VALIDITY_BUFFER,
  type SubmitSlotSnapshot,
} from "@/local-ledger-slot.js";

export type InlineWaitPolicy = "allow_inline_wait" | "defer_positive_wait";

export type SubmitTimingInput = {
  readonly callerLabel: string;
  readonly invalidBeforeSlot?: number;
  readonly invalidHereafterSlot?: number;
  readonly slotSnapshot?: SubmitSlotSnapshot;
  readonly slotSnapshotError?: unknown;
  readonly submitSlotBuffer?: number;
  readonly maxInlineWaitMs?: number;
  readonly inlineWaitPolicy?: InlineWaitPolicy;
  readonly dependencyKey?: string;
  readonly invalidationKey?: string;
};

export type SubmitTimingReadyPlan = {
  readonly status: "ready";
  readonly callerLabel: string;
  readonly targetSlot?: number;
  readonly currentSlot?: number;
  readonly observedSlot?: number;
  readonly observedAtMs?: number;
  readonly waitMs: 0;
  readonly slotSource?: string;
  readonly dependencyKey?: string;
  readonly invalidationKey?: string;
};

export type SubmitTimingWaitPlan = {
  readonly status: "wait";
  readonly callerLabel: string;
  readonly targetSlot: number;
  readonly dueSlot: number;
  readonly currentSlot: number;
  readonly observedSlot: number;
  readonly observedAtMs: number;
  readonly deltaSlots: number;
  readonly waitMs: number;
  readonly slotLengthMs: number;
  readonly slotSource: string;
  readonly invalidBeforeSlot: number;
  readonly invalidHereafterSlot?: number;
  readonly dependencyKey?: string;
  readonly invalidationKey?: string;
};

export type SubmitTimingNotDuePlan = Omit<SubmitTimingWaitPlan, "status"> & {
  readonly status: "not_due";
  readonly reason: string;
};

export type SubmitTimingExpiredPlan = {
  readonly status: "expired";
  readonly callerLabel: string;
  readonly currentSlot: number;
  readonly observedSlot: number;
  readonly observedAtMs: number;
  readonly invalidBeforeSlot?: number;
  readonly invalidHereafterSlot: number;
  readonly slotSource: string;
  readonly dependencyKey?: string;
  readonly invalidationKey?: string;
};

export type SubmitTimingWindowTooNarrowPlan = {
  readonly status: "window_too_narrow";
  readonly callerLabel: string;
  readonly currentSlot: number;
  readonly observedSlot: number;
  readonly observedAtMs: number;
  readonly targetSlot: number;
  readonly invalidBeforeSlot: number;
  readonly invalidHereafterSlot: number;
  readonly slotSource: string;
  readonly dependencyKey?: string;
  readonly invalidationKey?: string;
};

export type SubmitTimingSlotSourceUnavailablePlan = {
  readonly status: "slot_source_unavailable";
  readonly callerLabel: string;
  readonly invalidBeforeSlot?: number;
  readonly invalidHereafterSlot?: number;
  readonly cause: unknown;
  readonly dependencyKey?: string;
  readonly invalidationKey?: string;
};

export type SubmitTimingSlotSourceStalledPlan = Omit<
  SubmitTimingWaitPlan,
  "status"
> & {
  readonly status: "slot_source_stalled";
  readonly reason: string;
};

export type SubmitTimingPlan =
  | SubmitTimingReadyPlan
  | SubmitTimingWaitPlan
  | SubmitTimingNotDuePlan
  | SubmitTimingExpiredPlan
  | SubmitTimingWindowTooNarrowPlan
  | SubmitTimingSlotSourceUnavailablePlan
  | SubmitTimingSlotSourceStalledPlan;

const safeNonNegativeInteger = (
  value: number | undefined,
  fallback: number,
): number =>
  value === undefined || !Number.isSafeInteger(value) || value < 0
    ? fallback
    : value;

export const planSubmitTiming = (
  input: SubmitTimingInput,
): SubmitTimingPlan => {
  const base = {
    callerLabel: input.callerLabel,
    ...(input.dependencyKey === undefined
      ? {}
      : { dependencyKey: input.dependencyKey }),
    ...(input.invalidationKey === undefined
      ? {}
      : { invalidationKey: input.invalidationKey }),
  };
  const hasBound =
    input.invalidBeforeSlot !== undefined ||
    input.invalidHereafterSlot !== undefined;
  if (!hasBound) {
    return { ...base, status: "ready", waitMs: 0 };
  }
  const snapshot = input.slotSnapshot;
  if (snapshot === undefined) {
    return {
      ...base,
      status: "slot_source_unavailable",
      ...(input.invalidBeforeSlot === undefined
        ? {}
        : { invalidBeforeSlot: input.invalidBeforeSlot }),
      ...(input.invalidHereafterSlot === undefined
        ? {}
        : { invalidHereafterSlot: input.invalidHereafterSlot }),
      cause:
        input.slotSnapshotError ?? new Error("missing submit slot snapshot"),
    };
  }

  const currentSlot = snapshot.currentSlot;
  const slotSource = snapshot.source;
  const observedSlot = snapshot.currentSlot;
  const observedAtMs = snapshot.observedAtMs;
  if (
    input.invalidHereafterSlot !== undefined &&
    currentSlot >= input.invalidHereafterSlot
  ) {
    return {
      ...base,
      status: "expired",
      currentSlot,
      observedSlot,
      observedAtMs,
      ...(input.invalidBeforeSlot === undefined
        ? {}
        : { invalidBeforeSlot: input.invalidBeforeSlot }),
      invalidHereafterSlot: input.invalidHereafterSlot,
      slotSource,
    };
  }
  if (input.invalidBeforeSlot === undefined) {
    return {
      ...base,
      status: "ready",
      currentSlot,
      observedSlot,
      observedAtMs,
      waitMs: 0,
      slotSource,
    };
  }

  const submitSlotBuffer = safeNonNegativeInteger(
    input.submitSlotBuffer,
    SUBMIT_SLOT_VALIDITY_BUFFER,
  );
  const targetSlot = input.invalidBeforeSlot + submitSlotBuffer;
  if (
    input.invalidHereafterSlot !== undefined &&
    targetSlot >= input.invalidHereafterSlot
  ) {
    return {
      ...base,
      status: "window_too_narrow",
      currentSlot,
      observedSlot,
      observedAtMs,
      targetSlot,
      invalidBeforeSlot: input.invalidBeforeSlot,
      invalidHereafterSlot: input.invalidHereafterSlot,
      slotSource,
    };
  }
  if (currentSlot >= targetSlot) {
    return {
      ...base,
      status: "ready",
      targetSlot,
      currentSlot,
      observedSlot,
      observedAtMs,
      waitMs: 0,
      slotSource,
    };
  }

  const slotLengthMs = Math.max(
    1,
    Math.floor(snapshot.slotLengthMs || SUBMIT_SLOT_LENGTH_MS),
  );
  const deltaSlots = targetSlot - currentSlot;
  const waitMs = deltaSlots * slotLengthMs;
  const waitBase = {
    ...base,
    targetSlot,
    dueSlot: targetSlot,
    currentSlot,
    observedSlot,
    observedAtMs,
    deltaSlots,
    waitMs,
    slotLengthMs,
    slotSource,
    invalidBeforeSlot: input.invalidBeforeSlot,
    ...(input.invalidHereafterSlot === undefined
      ? {}
      : { invalidHereafterSlot: input.invalidHereafterSlot }),
  };
  const maxInlineWaitMs = safeNonNegativeInteger(
    input.maxInlineWaitMs,
    Number.MAX_SAFE_INTEGER,
  );
  if (input.inlineWaitPolicy === "defer_positive_wait") {
    return {
      ...waitBase,
      status: "not_due",
      reason: `inline_wait_policy=defer_positive_wait,wait_ms=${waitMs.toString()}`,
    };
  }
  return waitMs <= maxInlineWaitMs
    ? { ...waitBase, status: "wait" }
    : {
        ...waitBase,
        status: "not_due",
        reason: `wait_ms=${waitMs.toString()},max_inline_wait_ms=${maxInlineWaitMs.toString()}`,
      };
};

export const planSubmitTimingAfterInlineWait = (
  original: SubmitTimingWaitPlan,
  refreshedSnapshot: SubmitSlotSnapshot | undefined,
  refreshedError?: unknown,
):
  | SubmitTimingReadyPlan
  | SubmitTimingSlotSourceStalledPlan
  | SubmitTimingPlan => {
  const refreshed = planSubmitTiming({
    callerLabel: original.callerLabel,
    invalidBeforeSlot: original.invalidBeforeSlot,
    invalidHereafterSlot: original.invalidHereafterSlot,
    slotSnapshot: refreshedSnapshot,
    slotSnapshotError: refreshedError,
    submitSlotBuffer: original.targetSlot - original.invalidBeforeSlot,
    maxInlineWaitMs: 0,
    dependencyKey: original.dependencyKey,
    invalidationKey: original.invalidationKey,
  });
  if (refreshed.status === "ready") {
    return refreshed;
  }
  if (refreshed.status === "wait" || refreshed.status === "not_due") {
    return {
      ...refreshed,
      status: "slot_source_stalled",
      reason: `slot source did not reach target after inline wait: observed_slot=${refreshed.currentSlot.toString()},target_slot=${refreshed.targetSlot.toString()}`,
    };
  }
  return refreshed;
};
