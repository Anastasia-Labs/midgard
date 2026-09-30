import { Option } from "effect";

import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "../../database/utils/tx.js";
import type { SubmitSlotSnapshot } from "../../local-ledger-slot.js";
import { planSubmitTiming } from "../../transactions/submit-timing.js";
import { slotAwareDueWorkFromSubmitTiming } from "../../transactions/submit-timing-due-work.js";
import {
  type CommitBatchBudgetLimits,
  type CommitBatchPlan,
  type CommitBatchStopReason,
  type CommitSchedulerDiscoveryStage,
  commitSchedulerEvidenceKey,
  type CommitSchedulerState,
  type CommitSchedulerStateQueueEvidence,
  type CommitTxCandidateSelection,
  type CommitTxSourceTable,
  type CurrentOperatorSchedulerWindow,
  DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  EARLIEST_COMMIT_SCHEDULER_PLANNER_VERSION,
  type EarliestCommitSchedulerPlan,
  type PlannedCommitBatchSelection,
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

export const selectCommitTxCandidates = ({
  mempoolTxs,
  processedMempoolTxs,
}: {
  readonly mempoolTxs: readonly EntryWithTimeStamp[];
  readonly processedMempoolTxs: readonly EntryWithTimeStamp[];
}): CommitTxCandidateSelection => {
  const candidateTxs =
    processedMempoolTxs.length > 0 ? processedMempoolTxs : mempoolTxs;
  const sourceTable =
    processedMempoolTxs.length > 0
      ? "processed_mempool"
      : mempoolTxs.length > 0
        ? "mempool"
        : "none";

  return {
    candidateTxs,
    candidateTxHashes: candidateTxs.map((entry) =>
      Buffer.from(entry[TxColumns.TX_ID]),
    ),
    candidateTxsSize: candidateTxs.reduce(
      (total, entry) => total + entry[TxColumns.TX].length,
      0,
    ),
    sourceTable,
  };
};

export const buildCommitTxCandidateSelection = (
  candidateTxs: readonly EntryWithTimeStamp[],
  sourceTable: CommitTxSourceTable,
): CommitTxCandidateSelection => ({
  candidateTxs,
  candidateTxHashes: candidateTxs.map((entry) =>
    Buffer.from(entry[TxColumns.TX_ID]),
  ),
  candidateTxsSize: candidateTxs.reduce(
    (total, entry) => total + entry[TxColumns.TX].length,
    0,
  ),
  sourceTable: candidateTxs.length > 0 ? sourceTable : "none",
});

const estimateCommitBatchPlan = (
  txs: readonly EntryWithTimeStamp[],
  limits: CommitBatchBudgetLimits,
  stopReason: CommitBatchStopReason,
): CommitBatchPlan => {
  const selectedTxBytes = txs.reduce(
    (total, entry) => total + entry[TxColumns.TX].length,
    0,
  );
  const selectedTxCount = txs.length;
  return {
    selectedTxCount,
    selectedTxBytes,
    selectedLedgerOpCount: selectedTxCount * limits.estimatedLedgerOpsPerTx,
    selectedTransitionStepCount:
      selectedTxCount * limits.estimatedTransitionStepsPerTx,
    estimatedDaPayloadBytes:
      selectedTxBytes + selectedTxCount * limits.estimatedDaOverheadBytesPerTx,
    estimatedCommitTxBytes:
      limits.estimatedCommitTxOverheadBytes + selectedTxCount * 32,
    estimatedCommitBuildMs:
      selectedTxCount * limits.estimatedCommitBuildMsPerTx,
    stopReason,
  };
};

const firstExceededBudget = (
  plan: CommitBatchPlan,
  limits: CommitBatchBudgetLimits,
): CommitBatchStopReason | null => {
  if (plan.selectedTxCount > limits.maxL2TxCount) {
    return "tx_count_budget";
  }
  if (plan.selectedTxBytes > limits.maxCanonicalTxBytes) {
    return "tx_bytes_budget";
  }
  if (plan.selectedLedgerOpCount > limits.maxLedgerOpCount) {
    return "ledger_ops_budget";
  }
  if (plan.selectedTransitionStepCount > limits.maxTransitionStepCount) {
    return "transition_steps_budget";
  }
  if (plan.estimatedDaPayloadBytes > limits.maxDaPayloadBytes) {
    return "da_payload_budget";
  }
  if (plan.estimatedCommitTxBytes > limits.maxCommitTxBytes) {
    return "commit_tx_budget";
  }
  if (plan.estimatedCommitBuildMs > limits.maxEstimatedCommitBuildMs) {
    return "latency_budget";
  }
  return null;
};

export const planCommitBatchBudgets = ({
  candidateSelection,
  limits = DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
}: {
  readonly candidateSelection: CommitTxCandidateSelection;
  readonly limits?: CommitBatchBudgetLimits;
}): PlannedCommitBatchSelection => {
  const selected: EntryWithTimeStamp[] = [];
  let stopReason: CommitBatchStopReason = "mempool_exhausted";
  let selectedTxBytes = 0;

  for (const candidate of candidateSelection.candidateTxs) {
    const selectedTxCount = selected.length + 1;
    const nextSelectedTxBytes =
      selectedTxBytes + candidate[TxColumns.TX].length;
    const nextPlan: CommitBatchPlan = {
      selectedTxCount,
      selectedTxBytes: nextSelectedTxBytes,
      selectedLedgerOpCount: selectedTxCount * limits.estimatedLedgerOpsPerTx,
      selectedTransitionStepCount:
        selectedTxCount * limits.estimatedTransitionStepsPerTx,
      estimatedDaPayloadBytes:
        nextSelectedTxBytes +
        selectedTxCount * limits.estimatedDaOverheadBytesPerTx,
      estimatedCommitTxBytes:
        limits.estimatedCommitTxOverheadBytes + selectedTxCount * 32,
      estimatedCommitBuildMs:
        selectedTxCount * limits.estimatedCommitBuildMsPerTx,
      stopReason: "mempool_exhausted",
    };
    const exceeded = firstExceededBudget(nextPlan, limits);
    if (exceeded !== null) {
      stopReason = exceeded;
      break;
    }
    selected.push(candidate);
    selectedTxBytes = nextSelectedTxBytes;
  }

  // Empty is the only safe result when the first candidate does not fit. The
  // former "always include one" fallback silently exceeded consensus bounds.
  const candidateTxs = selected;
  const finalPlan = estimateCommitBatchPlan(candidateTxs, limits, stopReason);
  return {
    candidateSelection: buildCommitTxCandidateSelection(
      candidateTxs,
      candidateSelection.sourceTable,
    ),
    plan: finalPlan,
    originalTxCount: candidateSelection.candidateTxs.length,
    prunedTxCount: Math.max(
      0,
      candidateSelection.candidateTxs.length - candidateTxs.length,
    ),
  };
};
