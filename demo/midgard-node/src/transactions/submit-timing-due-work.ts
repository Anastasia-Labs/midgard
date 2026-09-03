import type {
  SlotAwareDueWork,
  SlotAwareDueWorkKind,
} from "../fibers/slot-aware-due-work.js";
import type { SubmitTimingNotDuePlan } from "./submit-timing.js";

export const DEFERRABLE_SUBMIT_TIMING_OWNERS = {
  commit_scheduler_refresh: true,
  merge_submit_validity: true,
} as const satisfies Record<SlotAwareDueWorkKind, true>;

export type DeferrableSubmitTimingOwner =
  keyof typeof DEFERRABLE_SUBMIT_TIMING_OWNERS;

export type SubmitTimingNotDuePlanWithDueWorkEvidence =
  SubmitTimingNotDuePlan & {
    readonly dependencyKey: string;
    readonly invalidationKey: string;
  };

export type DueWorkFromSubmitTimingInput = {
  readonly kind: DeferrableSubmitTimingOwner;
  readonly key: string;
  readonly callerLabel: string;
  readonly reason: string;
  readonly plan: SubmitTimingNotDuePlanWithDueWorkEvidence;
  readonly nowMs?: number;
};

export const slotAwareDueWorkFromSubmitTiming = ({
  kind,
  key,
  callerLabel,
  reason,
  plan,
}: DueWorkFromSubmitTimingInput): SlotAwareDueWork => ({
  kind,
  key,
  callerLabel,
  reason,
  observedSlot: plan.currentSlot,
  dueSlot: plan.targetSlot,
  dueAtMs: plan.observedAtMs + plan.waitMs,
  waitMs: plan.waitMs,
  slotSource: plan.slotSource,
  dependencyKey: plan.dependencyKey,
  invalidationKey: plan.invalidationKey,
});
