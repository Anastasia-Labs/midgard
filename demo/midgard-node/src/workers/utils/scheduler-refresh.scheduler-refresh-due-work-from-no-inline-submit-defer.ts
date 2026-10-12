import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { SlotAwareDueWork } from "../../fibers/slot-aware-due-work.js";
import { slotToUnixTimeForLucid } from "../../lucid-time.js";
import {
  slotAwareDueWorkFromSubmitTiming,
  type SubmitTimingNotDuePlanWithDueWorkEvidence,
} from "../../transactions/submit-timing-due-work.js";
import { type NoInlineSubmitDefer } from "../../transactions/utils.js";

export type NodeUtxoWithDatum = {
  readonly utxo: UTxO;
  readonly datum: SDK.LinkedListNodeView;
};

export type RealStateQueueWitnessContext = {
  readonly operatorKeyHash: string;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly correctionLockRefInput: SDK.CorrectionLockUTxO;
  readonly activeOperatorInput: UTxO & { datum: string };
  readonly activeOperatorsSpendingScript: Script;
  readonly activeOperatorsSpendingScriptRef?: UTxO;
  readonly stateQueueSpendingScriptRef?: UTxO;
  readonly stateQueueMintingScriptRef?: UTxO;
  readonly stateQueueCommitYieldScriptRef: UTxO;
  /** The operator wallet view's UTxOs (§8.5), read after scheduler alignment. */
  readonly operatorWalletInputs: readonly UTxO[];
};

export type CommitTimingDueWork = {
  readonly type: "CommitTimingDueWork";
  readonly dueWork: SlotAwareDueWork;
};

export const schedulerRefreshDependencyKey = ({
  schedulerOutRef,
  currentOperator,
  currentStartTime,
  targetOperator,
  selectionKind,
}: {
  readonly schedulerOutRef: string;
  readonly currentOperator: string;
  readonly currentStartTime: bigint;
  readonly targetOperator: string;
  readonly selectionKind: SchedulerRefreshWitnessSelection["kind"];
}): string =>
  [
    `scheduler=${schedulerOutRef}`,
    `current_operator=${currentOperator}`,
    `current_start=${currentStartTime.toString()}`,
    `target_operator=${targetOperator}`,
    `selection=${selectionKind}`,
  ].join(",");

export const schedulerRefreshDueWorkFromSubmitTiming = (input: {
  readonly plan: SubmitTimingNotDuePlanWithDueWorkEvidence;
  readonly reason?: string;
}): CommitTimingDueWork => ({
  type: "CommitTimingDueWork",
  dueWork: slotAwareDueWorkFromSubmitTiming({
    kind: "commit_scheduler_refresh",
    key: "block_commitment",
    callerLabel: "scheduler-refresh",
    reason: input.reason ?? "scheduler_transition_not_reached",
    plan: input.plan,
  }),
});

export type SchedulerRefreshWitnessSelection =
  | {
      readonly kind: "Advance";
      readonly activeNode: NodeUtxoWithDatum;
    }
  | {
      readonly kind: "AppointFirst";
      readonly activeNode: NodeUtxoWithDatum;
      readonly registeredWitnessNode: NodeUtxoWithDatum;
    }
  | {
      readonly kind: "Rewind";
      readonly activeNode: NodeUtxoWithDatum;
      readonly activeRootNode: NodeUtxoWithDatum;
      readonly registeredWitnessNode: NodeUtxoWithDatum;
    };

export type SchedulerAlignmentResult =
  | { readonly schedulerRefInput: UTxO }
  | CommitTimingDueWork;

export type ActiveSchedulerState = {
  readonly operator: string;
  readonly startTime: bigint;
};

export type SchedulerRefreshStartTimeMode =
  | "validity-lower-bound"
  | "previous-shift-end";

export const SCHEDULER_REFRESH_POLL_INTERVAL = "2 seconds";

export const SCHEDULER_REFRESH_MAX_POLLS = 30;

export const SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL = 96;

export const SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS = 5 * 60_000;

export const SCHEDULER_SUBMISSION_CONFIRMATION_POLL_INTERVAL_MS = 5_000;

export const SCHEDULER_SHIFT_DURATION_MS = SDK.SHIFT_DURATION_MS;

// The selected profile's range, which on-chain env.max_validity_range_length
// mirrors for scheduler spends.
export const SCHEDULER_TRANSITION_VALIDITY_WINDOW_MS =
  SDK.MAX_VALIDITY_RANGE_LENGTH_MS;

export const SCHEDULER_REFRESH_VALID_FROM_BACKDATE_MS = 30_000;

export const SCHEDULER_FIRST_APPOINTMENT_MIN_VALIDITY_GAP_MS = 30n * 1000n;

export const SCHEDULER_MAX_PRE_SUBMIT_WAIT_MS = 120_000;

const PREVIOUS_SHIFT_END_SCHEDULER_SPENDING_SCRIPT_HASHES = new Set([
  // Pre-2026-07-07 deployed preprod scheduler validators advance one shift at a
  // time and require output start_time == previous shift end.
  "adc0cb0642e888ec8f003ae71b5412bde1b31789c951597932f46712",
]);

export const schedulerRefreshStartTimeModeForSpendingScriptHash = (
  schedulerSpendingScriptHash: string,
): SchedulerRefreshStartTimeMode =>
  PREVIOUS_SHIFT_END_SCHEDULER_SPENDING_SCRIPT_HASHES.has(
    schedulerSpendingScriptHash.toLowerCase(),
  )
    ? "previous-shift-end"
    : "validity-lower-bound";

export const schedulerRefreshDueWorkFromNoInlineSubmitDefer = ({
  defer,
  localSubmitSlot,
  nowMs,
}: {
  readonly defer: NoInlineSubmitDefer;
  readonly localSubmitSlot?: SubmitSlotSnapshot;
  readonly nowMs: number;
}): CommitTimingDueWork => {
  const providerSlotWait = defer.kind === "provider_slot_wait";
  const observedSlot =
    providerSlotWait && localSubmitSlot !== undefined
      ? localSubmitSlot.currentSlot
      : defer.currentSlot;
  const slotLengthMs = Math.max(
    1,
    Math.floor(localSubmitSlot?.slotLengthMs ?? 1_000),
  );
  const dueSlot =
    providerSlotWait && localSubmitSlot !== undefined
      ? localSubmitSlot.currentSlot +
        Math.max(1, Math.ceil(defer.waitMs / slotLengthMs))
      : defer.dueSlot;
  return {
    type: "CommitTimingDueWork",
    dueWork: {
      kind: "commit_scheduler_refresh",
      key: "block_commitment",
      callerLabel: defer.callerLabel,
      reason: `scheduler_refresh_${defer.kind}_not_reached`,
      observedSlot,
      dueSlot,
      dueAtMs: nowMs + defer.waitMs,
      waitMs: defer.waitMs,
      slotSource:
        providerSlotWait && localSubmitSlot !== undefined
          ? localSubmitSlot.source
          : defer.slotSource,
      dependencyKey: defer.dependencyKey,
      invalidationKey: defer.invalidationKey,
    },
  };
};

export const schedulerRefreshRequiredOutsideMutationWorkerDueWork = ({
  submitTimingSnapshot,
  invalidBeforeSlot,
  invalidHereafterSlot,
  schedulerDependencyKey,
}: {
  readonly submitTimingSnapshot: SubmitSlotSnapshot;
  readonly invalidBeforeSlot: number;
  readonly invalidHereafterSlot: number;
  readonly schedulerDependencyKey: string;
}): CommitTimingDueWork =>
  schedulerRefreshDueWorkFromSubmitTiming({
    reason: "scheduler_refresh_required_outside_mutation_worker",
    plan: {
      status: "not_due",
      callerLabel: "scheduler-refresh",
      targetSlot: submitTimingSnapshot.currentSlot + 1,
      dueSlot: submitTimingSnapshot.currentSlot + 1,
      currentSlot: submitTimingSnapshot.currentSlot,
      observedSlot: submitTimingSnapshot.currentSlot,
      observedAtMs: submitTimingSnapshot.observedAtMs,
      deltaSlots: 1,
      waitMs: Math.max(1, submitTimingSnapshot.slotLengthMs),
      slotLengthMs: Math.max(1, submitTimingSnapshot.slotLengthMs),
      slotSource: submitTimingSnapshot.source,
      invalidBeforeSlot,
      ...(Number.isSafeInteger(invalidHereafterSlot)
        ? { invalidHereafterSlot }
        : {}),
      reason: "scheduler_refresh_required_outside_mutation_worker",
      dependencyKey: schedulerDependencyKey,
      invalidationKey: schedulerDependencyKey,
    },
  });

export type SchedulerSlotSnapshot = {
  readonly currentSlot: number;
  readonly currentSlotStartMs: number;
  readonly observedAtMs: number;
};

export const captureSchedulerSlotSnapshot = (
  lucid: LucidEvolution,
  observedAtMs: number = Date.now(),
  submitSlot?: SubmitSlotSnapshot,
): SchedulerSlotSnapshot => {
  if (submitSlot !== undefined) {
    return schedulerSlotSnapshotFromSubmitSlot(lucid, submitSlot);
  }
  const currentSlot = lucid.currentSlot();
  return {
    currentSlot,
    currentSlotStartMs:
      slotToUnixTimeForLucid(lucid, currentSlot) ?? observedAtMs,
    observedAtMs,
  };
};

export const schedulerSlotSnapshotFromSubmitSlot = (
  lucid: LucidEvolution,
  submitSlot: SubmitSlotSnapshot,
): SchedulerSlotSnapshot => ({
  currentSlot: submitSlot.currentSlot,
  currentSlotStartMs:
    slotToUnixTimeForLucid(lucid, submitSlot.currentSlot) ??
    submitSlot.observedAtMs,
  observedAtMs: submitSlot.observedAtMs,
});

export const nodeKeyBytes = (key: SDK.NodeKey): string | undefined =>
  key === "Empty" ? undefined : key.Key.key;

export const linkKeyBytes = (
  datum: SDK.LinkedListNodeView,
): string | undefined =>
  datum.next === "Empty" ? undefined : datum.next.Key.key;

export const activeSchedulerState = (
  datum: SDK.SchedulerDatum,
): ActiveSchedulerState | undefined =>
  datum === "NoActiveOperators"
    ? undefined
    : {
        operator: datum.ActiveOperator.operator,
        startTime: BigInt(datum.ActiveOperator.start_time),
      };

export const activeSchedulerDatum = (
  operator: string,
  startTime: bigint,
): SDK.SchedulerDatum => ({
  ActiveOperator: {
    operator,
    start_time: startTime,
  },
});

/**
 * The latest inclusive header end a commit can carry in `active`'s shift. The
 * shift is half-open, so the header's exclusive validTo, one past its end,
 * lands at most on the shift end that `schedulerStateCoversCommitTarget`
 * accepts. See `CurrentOperatorSchedulerWindow`.
 */
export const latestSchedulerShiftHeaderEndTime = (
  active: ActiveSchedulerState,
): bigint => active.startTime + SCHEDULER_SHIFT_DURATION_MS - 1n;

/**
 * `targetStartTime` is a commit's exclusive validTo (the witness build passes
 * the resolved validTo), so it may equal the shift end, one past
 * `latestSchedulerShiftHeaderEndTime`.
 */
export const schedulerStateCoversCommitTarget = ({
  currentSchedulerState,
  operatorKeyHash,
  targetStartTime,
}: {
  readonly currentSchedulerState: ActiveSchedulerState | undefined;
  readonly operatorKeyHash: string;
  readonly targetStartTime: bigint;
}): boolean => {
  if (currentSchedulerState?.operator !== operatorKeyHash) {
    return false;
  }
  const currentShiftEndTime =
    currentSchedulerState.startTime + SCHEDULER_SHIFT_DURATION_MS;
  return (
    currentSchedulerState.startTime <= targetStartTime &&
    targetStartTime <= currentShiftEndTime
  );
};
