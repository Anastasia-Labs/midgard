import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "../../database/utils/tx.js";
import {
  HISTORY_COMMIT_LANDING_MARGIN_MS,
  HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
} from "../../services/history-commit-window.js";
import {
  type CommitTxCandidateSelection,
  type CurrentOperatorSchedulerWindow,
  type SuccessfulCommitBatch,
} from "./commit-block-planner.commit-scheduler-evidence-key.js";
import { type SchedulerAwareCommitSelectionPlan } from "./commit-block-planner.plan-earliest-commit-scheduler-due-work.js";
import { buildCommitTxCandidateSelection } from "./commit-block-planner.select-commit-tx-candidates.js";
import {
  COMMIT_MIN_PRE_WITNESS_BUDGET_MS,
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  type CommitEndTimeFit,
} from "./commit-end-time.js";

/** The budgets `planSchedulerAwareCommitSelection` applies. A source-owned
 * commit caps its end, which is its inclusive TTL, to the current shift only
 * while the shift still leaves the history landing margin. */
export const schedulerAwareCommitWindowBudgets = (
  sourceOwned: boolean,
): {
  readonly minimumCurrentWindowBudgetMs: number;
  readonly productionMinimumFutureBufferMs: number;
} =>
  sourceOwned
    ? {
        minimumCurrentWindowBudgetMs: HISTORY_COMMIT_LANDING_MARGIN_MS,
        productionMinimumFutureBufferMs:
          HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
      }
    : {
        minimumCurrentWindowBudgetMs: COMMIT_MIN_PRE_WITNESS_BUDGET_MS,
        productionMinimumFutureBufferMs: COMMIT_MINIMUM_FUTURE_BUFFER_MS,
      };

export const planSchedulerAwareCommitSelection = ({
  candidateSelection,
  userEventOnlyEndTime,
  currentSchedulerWindow,
  currentBlockStartTimeMs,
  nowMs,
  minimumCurrentWindowBudgetMs,
  productionMinimumFutureBufferMs: minimumFutureBufferMs,
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
    currentWindowCommitEndTimeFit.resolvedEndTime - 1 >
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
            : "commit_inclusive_end_time_fit_exceeds_scheduler_window";
    return {
      candidateSelection,
      userEventOnlyEndTime,
      currentSchedulerWindow,
      status: "current_scheduler_end_time_floor_exceeds_window",
      prunedTxCount: 0,
      originalTxCount: candidateSelection.candidateTxs.length,
      reason: `resolved_valid_to_ms=${resolvedEndTimeMs.toString()},resolved_inclusive_end_time_ms=${typeof resolvedEndTimeMs === "number" ? (resolvedEndTimeMs - 1).toString() : "missing"},current_scheduler_end_ms=${currentSchedulerWindow.endTimeMs.toString()},minimum_future_buffer_ms=${(minimumFutureBufferMs ?? 0).toString()},remaining_current_window_ms=${remainingCurrentWindowMs.toString()},${fitReason}`,
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
    reason: `scheduler_out_ref=${currentSchedulerWindow.schedulerOutRef},current_scheduler_end_ms=${currentSchedulerWindow.endTimeMs.toString()},resolved_valid_to_ms=${currentWindowCommitEndTimeFit.resolvedEndTime.toString()},resolved_inclusive_end_time_ms=${(currentWindowCommitEndTimeFit.resolvedEndTime - 1).toString()},pruned_tx_count=${prunedTxCount.toString()}`,
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
