import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";

import { EntryWithTimeStamp } from "../../database/utils/tx.js";
import { slotAwareDueWorkFromSubmitTiming } from "../../transactions/submit-timing-due-work.js";

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
  maxL2TxCount: MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount,
  maxCanonicalTxBytes:
    MIDGARD_CONSENSUS_LIMITS.maxCanonicalTransactionBytesPerBlock,
  maxLedgerOpCount: MIDGARD_CONSENSUS_LIMITS.maxLedgerOperationCount,
  maxTransitionStepCount: MIDGARD_CONSENSUS_LIMITS.maxTransitionStepCount,
  maxDaPayloadBytes: MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
  maxCommitTxBytes: 128 * 1024,
  maxEstimatedCommitBuildMs: 30_000,
  estimatedLedgerOpsPerTx: 2,
  estimatedTransitionStepsPerTx: 1,
  estimatedDaOverheadBytesPerTx: 128,
  estimatedCommitTxOverheadBytes: 512,
  estimatedCommitBuildMsPerTx: 1,
};

export const MIN_COMMIT_BUILD_MS_PER_TX = 0.05;

export const MAX_COMMIT_BUILD_MS_PER_TX = 50;

export const clampCommitBuildMsPerTx = (value: number): number =>
  Math.min(
    MAX_COMMIT_BUILD_MS_PER_TX,
    Math.max(MIN_COMMIT_BUILD_MS_PER_TX, value),
  );

export const updateCommitBuildEwma = ({
  previousMsPerTx,
  measuredBuildMs,
  processedTxCount,
  alpha,
}: {
  readonly previousMsPerTx: number;
  readonly measuredBuildMs: number;
  readonly processedTxCount: number;
  readonly alpha: number;
}): number => {
  if (
    !Number.isFinite(alpha) ||
    alpha <= 0 ||
    alpha > 1 ||
    !Number.isFinite(measuredBuildMs) ||
    measuredBuildMs < 0 ||
    !Number.isSafeInteger(processedTxCount) ||
    processedTxCount <= 0
  ) {
    return clampCommitBuildMsPerTx(previousMsPerTx);
  }
  const sample = clampCommitBuildMsPerTx(measuredBuildMs / processedTxCount);
  return clampCommitBuildMsPerTx(
    alpha * sample + (1 - alpha) * previousMsPerTx,
  );
};

export const calibratedCommitBuildMsPerTx = ({
  msPerTxEwma,
  safetyFactor,
}: {
  readonly msPerTxEwma: number;
  readonly safetyFactor: number;
}): number =>
  clampCommitBuildMsPerTx(
    msPerTxEwma *
      (Number.isFinite(safetyFactor) && safetyFactor > 0 ? safetyFactor : 1),
  );

/**
 * The local operator's current scheduler shift. The shift is half-open,
 * `[start_time, start_time + shift_duration)`: the next shift's refresh may
 * take effect at `start_time + shift_duration` (scheduler.ak requires its
 * inclusive lower bound at or after that instant), so a commit whose exclusive
 * validity end passes it is outside this shift.
 *
 * Both bounds are INCLUSIVE header end times. `endTimeMs` is the latest header
 * end a commit can carry in this shift, `start_time + shift_duration - 1`: a
 * header end is its transaction's inclusive upper bound, so its exclusive
 * validTo is `endTimeMs + 1`, exactly the shift end that
 * `schedulerStateCoversCommitTarget` still accepts. Every consumer treats it
 * as an inclusive cap (`maximumEndTimeMs`, `blockEndTimeCapMs`, the L2
 * transaction timestamp filter).
 */
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

export const commitSchedulerEvidenceKey = ({
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
