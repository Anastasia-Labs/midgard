import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import { Option } from "effect";

import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "../../database/utils/tx.js";
import { planSubmitTiming } from "../../transactions/submit-timing.js";
import { slotAwareDueWorkFromSubmitTiming } from "../../transactions/submit-timing-due-work.js";
import {
  type CommitSchedulerDiscoveryStage,
  commitSchedulerEvidenceKey,
  type CommitSchedulerState,
  type CommitSchedulerStateQueueEvidence,
  type CommitTxCandidateSelection,
  type CurrentOperatorSchedulerWindow,
  EARLIEST_COMMIT_SCHEDULER_PLANNER_VERSION,
  type EarliestCommitSchedulerPlan,
} from "./commit-block-planner.commit-scheduler-evidence-key.js";

export const planEarliestCommitSchedulerDueWork = ({
  callerLabel,
  discoveryStage,
  schedulerOutRef,
  schedulerState,
  currentOperatorKeyHash,
  submitSlotSnapshot,
  submitSlotSnapshotError,
  stateQueueEvidence,
  localFinalizationPending,
  maxInlineWaitMs,
  plannerVersion = EARLIEST_COMMIT_SCHEDULER_PLANNER_VERSION,
}: {
  readonly callerLabel: string;
  readonly discoveryStage: CommitSchedulerDiscoveryStage;
  readonly schedulerOutRef: string;
  readonly schedulerState: CommitSchedulerState;
  readonly currentOperatorKeyHash: string;
  readonly submitSlotSnapshot?: SubmitSlotSnapshot;
  readonly submitSlotSnapshotError?: unknown;
  readonly stateQueueEvidence: CommitSchedulerStateQueueEvidence;
  readonly localFinalizationPending: boolean;
  readonly maxInlineWaitMs: number;
  readonly plannerVersion?: string;
}): EarliestCommitSchedulerPlan => {
  const dependencyKey = commitSchedulerEvidenceKey({
    schedulerOutRef,
    schedulerState,
    currentOperatorKeyHash,
    stateQueueEvidence,
    localFinalizationPending,
    plannerVersion,
  });
  const proceed = (reason: string): EarliestCommitSchedulerPlan => ({
    status: "proceed",
    reason,
    dependencyKey,
    invalidationKey: dependencyKey,
  });

  if (localFinalizationPending) {
    return proceed("local_finalization_pending");
  }
  if (
    stateQueueEvidence.tailCommitBaseOutRef.trim() === "" ||
    !Number.isSafeInteger(stateQueueEvidence.tailBlockEndTimeMs)
  ) {
    return {
      status: "ambiguous",
      reason: "state_queue_evidence_unsafe",
    };
  }
  if (schedulerState.status === "no_active_operators") {
    return {
      status: "ambiguous",
      reason: "scheduler_has_no_active_operator",
    };
  }
  if (!Number.isSafeInteger(schedulerState.startTimeMs)) {
    return {
      status: "ambiguous",
      reason: "scheduler_active_start_time_unsafe",
    };
  }
  if (schedulerState.operatorKeyHash === currentOperatorKeyHash) {
    return proceed("current_operator_already_active");
  }
  if (
    schedulerState.transitionInvalidBeforeSlot === undefined ||
    !Number.isSafeInteger(schedulerState.transitionInvalidBeforeSlot)
  ) {
    return {
      status: "ambiguous",
      reason: "scheduler_transition_slot_unavailable",
    };
  }

  const timingPlan = planSubmitTiming({
    callerLabel,
    invalidBeforeSlot: schedulerState.transitionInvalidBeforeSlot,
    invalidHereafterSlot: schedulerState.transitionInvalidHereafterSlot,
    slotSnapshot: submitSlotSnapshot,
    slotSnapshotError: submitSlotSnapshotError,
    maxInlineWaitMs,
    inlineWaitPolicy: "defer_positive_wait",
    dependencyKey,
    invalidationKey: dependencyKey,
  });

  if (timingPlan.status === "not_due") {
    return {
      status: "register_due_work",
      reason: "scheduler_transition_not_reached",
      discoveryStage,
      dueWork: slotAwareDueWorkFromSubmitTiming({
        kind: "commit_scheduler_refresh",
        key: "block_commitment",
        callerLabel,
        reason: "scheduler_transition_not_reached",
        plan: {
          ...timingPlan,
          dependencyKey,
          invalidationKey: dependencyKey,
        },
      }),
    };
  }

  if (timingPlan.status === "ready" || timingPlan.status === "wait") {
    return proceed(`scheduler_submit_timing_${timingPlan.status}`);
  }

  return {
    status: "ambiguous",
    reason: `scheduler_submit_timing_${timingPlan.status}`,
  };
};

export type SchedulerAwareCommitSelectionPlan = {
  readonly candidateSelection: CommitTxCandidateSelection;
  readonly userEventOnlyEndTime: Date;
  readonly currentSchedulerWindow?: CurrentOperatorSchedulerWindow;
  readonly status:
    | "no_current_scheduler_window"
    | "current_scheduler_budget_too_low"
    | "current_scheduler_window_not_ahead"
    | "current_scheduler_end_time_floor_exceeds_window"
    | "using_current_scheduler_window";
  readonly prunedTxCount: number;
  readonly originalTxCount: number;
  readonly blockEndTimeCapMs?: number;
  readonly reason: string;
};

export const establishEndTimeFromTxRequests = (
  candidateTxs: readonly EntryWithTimeStamp[],
): Option.Option<Date> =>
  candidateTxs.length > 0
    ? Option.some(candidateTxs[candidateTxs.length - 1][TxColumns.TIMESTAMPTZ])
    : Option.none();
