import { Option } from "effect";

import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "@/database/utils/tx.js";
import type { SubmitSlotSnapshot } from "@/local-ledger-slot.js";
import { planSubmitTiming } from "@/transactions/submit-timing.js";
import { slotAwareDueWorkFromSubmitTiming } from "@/transactions/submit-timing-due-work.js";
import type { CommitEndTimeFit } from "@/workers/utils/commit-end-time.js";

export type SuccessfulCommitBatch = {
  readonly txsToInsertImmutable: readonly EntryWithTimeStamp[];
  readonly blockTxHashes: readonly Buffer[];
  readonly clearMempoolTxHashes: readonly Buffer[];
};

export type CommitTxSourceTable = "mempool" | "processed_mempool" | "none";

export type CommitTxCandidateSelection = {
  readonly candidateTxs: readonly EntryWithTimeStamp[];
  readonly candidateTxHashes: readonly Buffer[];
  readonly candidateTxsSize: number;
  readonly sourceTable: CommitTxSourceTable;
};

export type CommitBatchStopReason =
  | "tx_count_budget"
  | "tx_bytes_budget"
  | "ledger_ops_budget"
  | "transition_steps_budget"
  | "da_payload_budget"
  | "commit_tx_budget"
  | "latency_budget"
  | "mempool_exhausted";

export type CommitBatchBudgetLimits = {
  readonly maxL2TxCount: number;
  readonly maxCanonicalTxBytes: number;
  readonly maxLedgerOpCount: number;
  readonly maxTransitionStepCount: number;
  readonly maxDaPayloadBytes: number;
  readonly maxCommitTxBytes: number;
  readonly maxEstimatedCommitBuildMs: number;
  readonly estimatedLedgerOpsPerTx: number;
  readonly estimatedTransitionStepsPerTx: number;
  readonly estimatedDaOverheadBytesPerTx: number;
  readonly estimatedCommitTxOverheadBytes: number;
  readonly estimatedCommitBuildMsPerTx: number;
};

export type CommitBatchPlan = {
  readonly selectedTxCount: number;
  readonly selectedTxBytes: number;
  readonly selectedLedgerOpCount: number;
  readonly selectedTransitionStepCount: number;
  readonly estimatedDaPayloadBytes: number;
  readonly estimatedCommitTxBytes: number;
  readonly estimatedCommitBuildMs: number;
  readonly stopReason: CommitBatchStopReason;
};

export type PlannedCommitBatchSelection = {
  readonly candidateSelection: CommitTxCandidateSelection;
  readonly plan: CommitBatchPlan;
  readonly originalTxCount: number;
  readonly prunedTxCount: number;
};

export const DEFAULT_COMMIT_BATCH_BUDGET_LIMITS: CommitBatchBudgetLimits = {
  maxL2TxCount: 10_000,
  maxCanonicalTxBytes: 32 * 1024 * 1024,
  maxLedgerOpCount: 40_000,
  maxTransitionStepCount: 40_000,
  maxDaPayloadBytes: 64 * 1024 * 1024,
  maxCommitTxBytes: 128 * 1024,
  maxEstimatedCommitBuildMs: 30_000,
  estimatedLedgerOpsPerTx: 2,
  estimatedTransitionStepsPerTx: 1,
  estimatedDaOverheadBytesPerTx: 128,
  estimatedCommitTxOverheadBytes: 512,
  estimatedCommitBuildMsPerTx: 1,
};

export type CurrentOperatorSchedulerWindow = {
  readonly schedulerOutRef: string;
  readonly operatorKeyHash: string;
  readonly startTimeMs: number;
  readonly endTimeMs: number;
};

export type CommitSchedulerDiscoveryStage =
  | "pre_lease"
  | "worker_pre_ingestion"
  | "scheduler_refresh_deep";

export type CommitSchedulerState =
  | {
      readonly status: "active";
      readonly operatorKeyHash: string;
      readonly startTimeMs: number;
      readonly transitionInvalidBeforeSlot?: number;
      readonly transitionInvalidHereafterSlot?: number;
    }
  | {
      readonly status: "no_active_operators";
    };

export type CommitSchedulerStateQueueEvidence = {
  readonly tailCommitBaseOutRef: string;
  readonly tailBlockEndTimeMs: number;
  readonly stateQueueHasUnmergedTail: boolean;
  readonly rootOutRef?: string;
};

export type EarliestCommitSchedulerPlan =
  | {
      readonly status: "proceed";
      readonly reason: string;
      readonly dependencyKey: string;
      readonly invalidationKey: string;
    }
  | {
      readonly status: "register_due_work";
      readonly dueWork: ReturnType<typeof slotAwareDueWorkFromSubmitTiming>;
      readonly reason: "scheduler_transition_not_reached";
      readonly discoveryStage: CommitSchedulerDiscoveryStage;
    }
  | {
      readonly status: "ambiguous";
      readonly reason: string;
    };

export const EARLIEST_COMMIT_SCHEDULER_PLANNER_VERSION =
  "earliest_commit_scheduler_v1";

const safeNumberEvidence = (label: string, value: number): string =>
  `${label}=${Number.isSafeInteger(value) ? value.toString() : "unsafe"}`;

const commitSchedulerEvidenceKey = ({
  schedulerOutRef,
  schedulerState,
  currentOperatorKeyHash,
  stateQueueEvidence,
  localFinalizationPending,
  plannerVersion,
}: {
  readonly schedulerOutRef: string;
  readonly schedulerState: CommitSchedulerState;
  readonly currentOperatorKeyHash: string;
  readonly stateQueueEvidence: CommitSchedulerStateQueueEvidence;
  readonly localFinalizationPending: boolean;
  readonly plannerVersion: string;
}): string =>
  [
    `planner=${plannerVersion}`,
    `scheduler=${schedulerOutRef}`,
    schedulerState.status === "active"
      ? `scheduler_state=active,scheduler_operator=${schedulerState.operatorKeyHash},${safeNumberEvidence("scheduler_start_ms", schedulerState.startTimeMs)},transition_invalid_before_slot=${schedulerState.transitionInvalidBeforeSlot?.toString() ?? "missing"},transition_invalid_hereafter_slot=${schedulerState.transitionInvalidHereafterSlot?.toString() ?? "missing"},selection=active_other_shift_boundary`
      : "scheduler_state=no_active_operators",
    `current_operator=${currentOperatorKeyHash}`,
    `state_queue_tail_base=${stateQueueEvidence.tailCommitBaseOutRef}`,
    safeNumberEvidence(
      "state_queue_tail_end_ms",
      stateQueueEvidence.tailBlockEndTimeMs,
    ),
    `state_queue_unmerged_tail=${stateQueueEvidence.stateQueueHasUnmergedTail.toString()}`,
    ...(stateQueueEvidence.rootOutRef === undefined
      ? []
      : [`state_queue_root=${stateQueueEvidence.rootOutRef}`]),
    `local_finalization_pending=${localFinalizationPending.toString()}`,
  ].join(",");

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
    ? Option.some(candidateTxs[0][TxColumns.TIMESTAMPTZ])
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

const buildCommitTxCandidateSelection = (
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

  for (const candidate of candidateSelection.candidateTxs) {
    const next = [...selected, candidate];
    const nextPlan = estimateCommitBatchPlan(next, limits, "mempool_exhausted");
    const exceeded = firstExceededBudget(nextPlan, limits);
    if (exceeded !== null) {
      stopReason = exceeded;
      break;
    }
    selected.push(candidate);
  }

  const candidateTxs =
    candidateSelection.candidateTxs.length > 0 && selected.length === 0
      ? [candidateSelection.candidateTxs[0]!]
      : selected;
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

export const planSchedulerAwareCommitSelection = ({
  candidateSelection,
  userEventOnlyEndTime,
  currentSchedulerWindow,
  currentBlockStartTimeMs,
  nowMs,
  minimumCurrentWindowBudgetMs,
  productionMinimumFutureBufferMs,
  currentWindowCommitEndTimeFit,
}: {
  readonly candidateSelection: CommitTxCandidateSelection;
  readonly userEventOnlyEndTime: Date;
  readonly currentSchedulerWindow?: CurrentOperatorSchedulerWindow;
  readonly currentBlockStartTimeMs: number;
  readonly nowMs: number;
  readonly minimumCurrentWindowBudgetMs: number;
  readonly productionMinimumFutureBufferMs?: number;
  readonly currentWindowCommitEndTimeFit?: CommitEndTimeFit;
}): SchedulerAwareCommitSelectionPlan => {
  if (currentSchedulerWindow === undefined) {
    return {
      candidateSelection,
      userEventOnlyEndTime,
      status: "no_current_scheduler_window",
      prunedTxCount: 0,
      originalTxCount: candidateSelection.candidateTxs.length,
      reason: "current operator is not active in the scheduler window",
    };
  }

  const remainingCurrentWindowMs = currentSchedulerWindow.endTimeMs - nowMs;
  if (remainingCurrentWindowMs < minimumCurrentWindowBudgetMs) {
    return {
      candidateSelection,
      userEventOnlyEndTime,
      currentSchedulerWindow,
      status: "current_scheduler_budget_too_low",
      prunedTxCount: 0,
      originalTxCount: candidateSelection.candidateTxs.length,
      reason: `remaining_current_window_ms=${remainingCurrentWindowMs.toString()},minimum_current_window_budget_ms=${minimumCurrentWindowBudgetMs.toString()}`,
    };
  }

  if (currentSchedulerWindow.endTimeMs <= currentBlockStartTimeMs) {
    return {
      candidateSelection,
      userEventOnlyEndTime,
      currentSchedulerWindow,
      status: "current_scheduler_window_not_ahead",
      prunedTxCount: 0,
      originalTxCount: candidateSelection.candidateTxs.length,
      reason: `current_scheduler_end_ms=${currentSchedulerWindow.endTimeMs.toString()},current_block_start_ms=${currentBlockStartTimeMs.toString()}`,
    };
  }

  const commitEndTimeFitUsesSchedulerCap =
    currentWindowCommitEndTimeFit?.maximumEndTimeMs ===
    currentSchedulerWindow.endTimeMs;
  const commitEndTimeFitExceedsSchedulerWindow =
    currentWindowCommitEndTimeFit === undefined ||
    currentWindowCommitEndTimeFit.status === "exceeds_cap" ||
    currentWindowCommitEndTimeFit.resolvedEndTime >
      currentSchedulerWindow.endTimeMs;
  if (
    !commitEndTimeFitUsesSchedulerCap ||
    commitEndTimeFitExceedsSchedulerWindow
  ) {
    const resolvedEndTimeMs =
      currentWindowCommitEndTimeFit?.resolvedEndTime ?? "missing";
    const fitReason =
      currentWindowCommitEndTimeFit === undefined
        ? "commit_end_time_fit=missing"
        : currentWindowCommitEndTimeFit.status === "exceeds_cap"
          ? currentWindowCommitEndTimeFit.reason
          : !commitEndTimeFitUsesSchedulerCap
            ? `commit_end_time_fit_cap_mismatch=${String(currentWindowCommitEndTimeFit.maximumEndTimeMs)}`
            : "commit_end_time_fit_exceeds_scheduler_window";
    return {
      candidateSelection,
      userEventOnlyEndTime,
      currentSchedulerWindow,
      status: "current_scheduler_end_time_floor_exceeds_window",
      prunedTxCount: 0,
      originalTxCount: candidateSelection.candidateTxs.length,
      reason: `resolved_end_time_ms=${resolvedEndTimeMs.toString()},current_scheduler_end_ms=${currentSchedulerWindow.endTimeMs.toString()},minimum_future_buffer_ms=${(productionMinimumFutureBufferMs ?? 0).toString()},remaining_current_window_ms=${remainingCurrentWindowMs.toString()},${fitReason}`,
    };
  }

  const cappedTxs = candidateSelection.candidateTxs.filter(
    (entry) =>
      entry[TxColumns.TIMESTAMPTZ].getTime() <=
      currentSchedulerWindow.endTimeMs,
  );
  const cappedUserEventOnlyEndTime =
    userEventOnlyEndTime.getTime() > currentSchedulerWindow.endTimeMs
      ? new Date(currentSchedulerWindow.endTimeMs)
      : userEventOnlyEndTime;
  const prunedTxCount =
    candidateSelection.candidateTxs.length - cappedTxs.length;
  return {
    candidateSelection: buildCommitTxCandidateSelection(
      cappedTxs,
      candidateSelection.sourceTable,
    ),
    userEventOnlyEndTime: cappedUserEventOnlyEndTime,
    currentSchedulerWindow,
    status: "using_current_scheduler_window",
    prunedTxCount,
    originalTxCount: candidateSelection.candidateTxs.length,
    blockEndTimeCapMs: currentSchedulerWindow.endTimeMs,
    reason: `scheduler_out_ref=${currentSchedulerWindow.schedulerOutRef},current_scheduler_end_ms=${currentSchedulerWindow.endTimeMs.toString()},resolved_end_time_ms=${currentWindowCommitEndTimeFit.resolvedEndTime.toString()},pruned_tx_count=${prunedTxCount.toString()}`,
  };
};

export const buildSuccessfulCommitBatches = (
  mempoolTxs: readonly EntryWithTimeStamp[],
  mempoolTxHashes: readonly Buffer[],
  processedMempoolTxs: readonly EntryWithTimeStamp[],
  batchSize: number,
): readonly SuccessfulCommitBatch[] => {
  const allTxs: readonly EntryWithTimeStamp[] = [
    ...mempoolTxs,
    ...processedMempoolTxs,
  ];
  const allBlockHashes: readonly Buffer[] = [
    ...mempoolTxHashes,
    ...processedMempoolTxs.map((tx) => tx[TxColumns.TX_ID]),
  ];

  if (allTxs.length === 0) {
    return [];
  }

  const batches: SuccessfulCommitBatch[] = [];
  const step = Math.max(1, batchSize);

  for (let start = 0; start < allTxs.length; start += step) {
    const end = Math.min(start + step, allTxs.length);
    const clearStart = Math.min(start, mempoolTxHashes.length);
    const clearEnd = Math.min(end, mempoolTxHashes.length);

    batches.push({
      txsToInsertImmutable: allTxs.slice(start, end),
      blockTxHashes: allBlockHashes.slice(start, end),
      clearMempoolTxHashes: mempoolTxHashes.slice(clearStart, clearEnd),
    });
  }

  return batches;
};

export type CommitRootSelectionInput = {
  readonly hasTxRequests: boolean;
  readonly computedUtxoRoot: string;
  readonly computedTxRoot: string;
  readonly emptyRoot: string;
};

export const selectCommitRoots = ({
  computedUtxoRoot,
  computedTxRoot,
  emptyRoot,
}: CommitRootSelectionInput): {
  readonly utxoRoot: string;
  readonly txRoot: string;
} => {
  if (computedUtxoRoot.length > 0 && computedTxRoot.length > 0) {
    return {
      utxoRoot: computedUtxoRoot,
      txRoot: computedTxRoot,
    };
  }
  return {
    utxoRoot: emptyRoot,
    txRoot: emptyRoot,
  };
};

export const shouldAttemptLocalFinalizationRecovery = (input: {
  readonly localFinalizationPending: boolean;
  readonly hasAvailableConfirmedBlock: boolean;
}): boolean =>
  input.localFinalizationPending && input.hasAvailableConfirmedBlock;

export const shouldDeferCommitSubmission = (input: {
  readonly localFinalizationPending: boolean;
  readonly hasAvailableConfirmedBlock: boolean;
}): boolean =>
  input.localFinalizationPending && !input.hasAvailableConfirmedBlock;

export const shouldSkipIdleCommitBehindUnmergedTail = (input: {
  readonly localFinalizationPending: boolean;
  readonly stateQueueHasUnmergedTail: boolean;
  readonly mempoolTxCount: number;
  readonly processedTxCount: number;
  readonly pendingUserEventCount: number;
}): boolean =>
  !input.localFinalizationPending &&
  input.stateQueueHasUnmergedTail &&
  input.mempoolTxCount === 0 &&
  input.processedTxCount === 0 &&
  input.pendingUserEventCount === 0;

export const rootsMatchConfirmedHeader = (input: {
  readonly computedUtxoRoot: string;
  readonly computedTxRoot: string;
  readonly confirmedUtxoRoot: string;
  readonly confirmedTxRoot: string;
}): boolean =>
  input.computedUtxoRoot === input.confirmedUtxoRoot &&
  input.computedTxRoot === input.confirmedTxRoot;
