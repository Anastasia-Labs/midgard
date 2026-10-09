import { Effect, Ref } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  MempoolDB,
  WithdrawalsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { publishMempoolLedgerDelta } from "../services/globals.js";
import {
  forgetLoggedState,
  logOnStateChange,
} from "../services/globals.liveness-reasons.js";
import { HISTORY_COMMIT_LANDING_MARGIN_MS } from "../services/history-commit-window.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import {
  landedStateQueueSnapshot,
  type StateQueueSnapshot,
} from "../services/landed-state-queue.js";
import {
  type SerializedStateQueueUTxO,
  WorkerOutput,
} from "../workers/utils/commit-block-header.js";
import type {
  CommitSchedulerStateQueueEvidence,
  EarliestCommitSchedulerPlan,
} from "../workers/utils/commit-block-planner.js";
import { COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../workers/utils/commit-end-time.js";
import {
  fetchRealStateQueueWitnessContext,
  resolveEarliestCommitSchedulerDueWorkPlan,
} from "../workers/utils/scheduler-refresh.js";
import {
  BLOCK_COMMITMENT_DUE_WORK_KEY,
  BLOCK_COMMITMENT_DUE_WORK_KIND,
  publishFullMempoolLedgerReload,
} from "./block-commitment.promote-or-recover-native-mpf.js";
import { clearCommitWorkerFailure } from "./block-commitment.worker-readiness.js";
import { emitQueueStateMetrics } from "./queue-metrics.js";
import {
  checkSlotAwareDueWork,
  clearSlotAwareDueWork,
  type SlotAwareDueWork,
} from "./slot-aware-due-work.js";

export const publishCommitMempoolLedgerMutation = (
  globals: Globals,
  workerOutput: WorkerOutput,
  deltaLogMax: number,
): Effect.Effect<void> => {
  // Only outputs that mutate the mempool ledger require work here.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (workerOutput.type) {
    case "SuccessfulSubmissionOutput":
    case "SuccessfulLocalFinalizationRecoveryOutput":
      return workerOutput.mempoolLedgerDeletedOutRefHexes.length === 0
        ? Effect.void
        : publishMempoolLedgerDelta(
            globals,
            {
              full: false,
              upserts: [],
              deletes: workerOutput.mempoolLedgerDeletedOutRefHexes,
            },
            deltaLogMax,
          ).pipe(Effect.asVoid);
    // A failed attempt may have committed a commit-stage rejection's
    // mempool_ledger revert without its notice reaching the parent.
    case "SubmittedAwaitingLocalFinalizationOutput":
    case "FailureOutput":
      return publishFullMempoolLedgerReload(globals, deltaLogMax);
    default:
      return Effect.void;
  }
};

const stateQueueEvidenceFromSnapshot = (
  snapshot: StateQueueSnapshot,
): CommitSchedulerStateQueueEvidence => ({
  tailCommitBaseOutRef: snapshot.tailCommitBase.outRef,
  tailBlockEndTimeMs: snapshot.tailCommitBase.blockEndTimeMs,
  stateQueueHasUnmergedTail:
    snapshot.root.outRef !== snapshot.tailCommitBase.outRef,
});

const localFinalizationPendingCommitSchedulerPlan =
  (): EarliestCommitSchedulerPlan => ({
    status: "proceed",
    reason: "local_finalization_pending",
    dependencyKey:
      "planner=earliest_commit_scheduler_v1,local_finalization_pending=true",
    invalidationKey:
      "planner=earliest_commit_scheduler_v1,local_finalization_pending=true",
  });

export const planPreLeaseCommitSchedulerDueWork = Effect.gen(function* () {
  const globals = yield* Globals;
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const LOCAL_FINALIZATION_PENDING = yield* globals.LOCAL_FINALIZATION_PENDING;
  if (LOCAL_FINALIZATION_PENDING) {
    return localFinalizationPendingCommitSchedulerPlan();
  }
  const snapshot = yield* landedStateQueueSnapshot(
    contracts.stateQueue,
    "commit_preflight",
  );
  yield* lucid.switchToOperatorsMainWallet;
  return yield* resolveEarliestCommitSchedulerDueWorkPlan({
    lucid: lucid.api,
    contracts,
    submitSlotSnapshot: lucid.submitSlotSnapshot,
    stateQueueEvidence: stateQueueEvidenceFromSnapshot(snapshot),
    localFinalizationPending: LOCAL_FINALIZATION_PENDING,
    callerLabel: "commit-scheduler-preflight",
    discoveryStage: "pre_lease",
  });
});

export const clearRegisteredCommitDueWork = (): void => {
  clearSlotAwareDueWork(
    BLOCK_COMMITMENT_DUE_WORK_KIND,
    BLOCK_COMMITMENT_DUE_WORK_KEY,
  );
};

const pendingUserEventCountUpTo = (
  effectiveEndTime: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const [depositEntries, forcedTransactionEntries, withdrawalEntries] =
      yield* Effect.all(
        [
          DepositsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
          ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          ),
          WithdrawalsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
        ],
        { concurrency: "unbounded" },
      );
    return (
      depositEntries.length +
      forcedTransactionEntries.length +
      withdrawalEntries.length
    );
  });

export const shouldDeferCommitWorkerForLocalFinalization = ({
  localFinalizationPending,
  availableLocalFinalizationBlock,
}: {
  readonly localFinalizationPending: boolean;
  readonly availableLocalFinalizationBlock: SerializedStateQueueUTxO | "";
}): boolean =>
  localFinalizationPending && availableLocalFinalizationBlock === "";

export const shouldAttemptCommitPipeline = ({
  localFinalizationPending,
  availableLocalFinalizationBlock,
  mempoolTxCount,
  processedUnsubmittedTxCount,
  pendingUserEventCount,
}: {
  readonly localFinalizationPending: boolean;
  readonly availableLocalFinalizationBlock: SerializedStateQueueUTxO | "";
  readonly mempoolTxCount: bigint;
  readonly processedUnsubmittedTxCount: number;
  readonly pendingUserEventCount: number;
}): boolean =>
  !shouldDeferCommitWorkerForLocalFinalization({
    localFinalizationPending,
    availableLocalFinalizationBlock,
  }) &&
  (localFinalizationPending ||
    mempoolTxCount > 0n ||
    processedUnsubmittedTxCount > 0 ||
    pendingUserEventCount > 0);

const COMMIT_PIPELINE_SKIP_LOG_KEY = "block_commitment_idle_skip";

/**
 * True when the tick has no commitment work, so it skips before the L1
 * control plane. The skip is logged once per state change (then at debug);
 * `COMMIT_PIPELINE_IDLE` records whether the tick found no work and
 * `COMMIT_PIPELINE_BACKLOG` what it counted. Skipping never exposes the
 * operator to a strike: a strike must cite an undelivered deposit,
 * withdrawal or tx order, and each one due by the tick counts as work here.
 */
export const shouldSkipIdleCommitPipelineBeforeSchedulerAlignment = Effect.gen(
  function* () {
    const globals = yield* Globals;
    const localFinalizationPending = yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    );
    const availableLocalFinalizationBlock = yield* Ref.get(
      globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
    );
    if (
      shouldDeferCommitWorkerForLocalFinalization({
        localFinalizationPending,
        availableLocalFinalizationBlock,
      })
    ) {
      yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, false);
      yield* logOnStateChange(
        globals,
        COMMIT_PIPELINE_SKIP_LOG_KEY,
        "local_finalization_deferred",
        "🔹 Local finalization is pending without a confirmed recovery block; skipping the commit worker until confirmation advances recovery.",
      );
      yield* emitQueueStateMetrics;
      return true;
    }
    const processedUnsubmittedTxCount = yield* Ref.get(
      globals.PROCESSED_UNSUBMITTED_TXS_COUNT,
    );
    const mempoolTxCount = yield* MempoolDB.retrieveTxCount;
    const pendingUserEventCount = yield* pendingUserEventCountUpTo(
      new Date(Date.now() + COMMIT_MINIMUM_FUTURE_BUFFER_MS),
    );
    yield* Ref.set(globals.COMMIT_PIPELINE_BACKLOG, {
      mempoolTxCount: Number(mempoolTxCount),
      pendingUserEventCount,
    });
    if (
      shouldAttemptCommitPipeline({
        localFinalizationPending,
        availableLocalFinalizationBlock,
        mempoolTxCount,
        processedUnsubmittedTxCount,
        pendingUserEventCount,
      })
    ) {
      yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, false);
      yield* forgetLoggedState(globals, COMMIT_PIPELINE_SKIP_LOG_KEY);
      return false;
    }
    yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, true);
    yield* clearCommitWorkerFailure(globals);
    yield* logOnStateChange(
      globals,
      COMMIT_PIPELINE_SKIP_LOG_KEY,
      "no_pending_work",
      "🔹 No pending tx/user-event work for block commitment; skipping pre-lease scheduler alignment.",
    );
    yield* emitQueueStateMetrics;
    return true;
  },
);

export const isDetailedSchedulerAlignmentDueWork = (
  entry: SlotAwareDueWork,
): boolean =>
  entry.kind === BLOCK_COMMITMENT_DUE_WORK_KIND &&
  entry.key === BLOCK_COMMITMENT_DUE_WORK_KEY &&
  entry.dependencyKey.startsWith("scheduler=");

/**
 * The commit end the scheduler must cover before the mutation worker runs.
 * Every runtime commit is source-owned and the worker caps its end to the
 * current shift, so the shift must still leave the history landing margin.
 * A later target would demand a refresh the scheduler admits only after the
 * shift ends, and an AppointFirst aimed further out would start the first
 * shift after ends the worker may build, which then fail until it begins.
 */
export const preLeaseCommitSchedulerTargetMs = (nowMs: number): number =>
  nowMs + HISTORY_COMMIT_LANDING_MARGIN_MS;

const resolveFreshDetailedSchedulerDueWork = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const alignedEndTime = preLeaseCommitSchedulerTargetMs(Date.now());
  const alignment = yield* fetchRealStateQueueWitnessContext(
    lucid.api,
    contracts,
    alignedEndTime,
    lucid.referenceScriptsAddress,
    lucid.submitSlotSnapshot,
    false,
  );
  if ("dueWork" in alignment) {
    return alignment.dueWork;
  }
  return undefined;
});

export const shouldSkipForDetailedSchedulerDueWork: Effect.Effect<
  boolean,
  never,
  Globals | Lucid | MidgardContracts | IntentJournal
> = Effect.gen(function* () {
  const freshDueWork = yield* Effect.either(
    resolveFreshDetailedSchedulerDueWork,
  );
  if (freshDueWork._tag === "Left") {
    clearRegisteredCommitDueWork();
    yield* Effect.logWarning(
      `🔹 Clearing detailed scheduler due work before re-plan because fresh scheduler alignment evidence failed: ${String(freshDueWork.left)}`,
    );
    return false;
  }
  if (freshDueWork.right === undefined) {
    clearRegisteredCommitDueWork();
    yield* Effect.logInfo(
      "🔹 Clearing detailed scheduler due work before re-plan because scheduler alignment is no longer required.",
    );
    return false;
  }
  const decision = checkSlotAwareDueWork({
    kind: BLOCK_COMMITMENT_DUE_WORK_KIND,
    key: BLOCK_COMMITMENT_DUE_WORK_KEY,
    currentSlot: freshDueWork.right.observedSlot,
    dependencyKey: freshDueWork.right.dependencyKey,
    invalidationKey: freshDueWork.right.invalidationKey,
  });
  switch (decision.status) {
    case "skip":
      yield* Effect.logInfo(
        `🔹 Skipping block commitment trigger because registered detailed scheduler due work is not due (kind=${decision.entry.kind},key=${decision.entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${decision.entry.dueSlot.toString()},wait_ms=${decision.entry.waitMs.toString()},dependency_key=${freshDueWork.right.dependencyKey}).`,
      );
      return true;
    case "due":
      yield* Effect.logInfo(
        `🔹 Waking detailed scheduler due work (kind=${decision.entry.kind},key=${decision.entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${decision.entry.dueSlot.toString()}).`,
      );
      return false;
    case "invalidated":
      yield* Effect.logInfo(
        `🔹 Clearing detailed scheduler due work before re-plan (${decision.reason}).`,
      );
      return false;
    case "missing":
      return false;
  }
}).pipe(
  Effect.catchAll((error) =>
    Effect.gen(function* () {
      clearRegisteredCommitDueWork();
      yield* Effect.logWarning(
        `🔹 Clearing detailed scheduler due work before re-plan after unexpected fresh-evidence failure: ${String(error)}`,
      );
      return false;
    }),
  ),
);
