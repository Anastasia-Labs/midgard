import * as SDK from "@al-ft/midgard-sdk";
import { Duration, Effect, Metric, Ref, Schedule } from "effect";
import { Worker } from "worker_threads";

import {
  DepositsDB,
  ForcedTransactionsDB,
  MempoolDB,
  StateQueueMutationLeasesDB,
  WithdrawalsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  type CommitPipelinePhase,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  fetchStateQueueSnapshotProgram,
  refreshStateQueueGlobalsFromSnapshot,
  type StateQueueSnapshot,
} from "@/services/state-queue-topology.js";
import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import type {
  CommitSchedulerStateQueueEvidence,
  EarliestCommitSchedulerPlan,
} from "@/workers/utils/commit-block-planner.js";
import { COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS } from "@/workers/utils/commit-end-time.js";
import { WorkerError } from "@/workers/utils/common.js";
import {
  fetchRealStateQueueWitnessContext,
  resolveEarliestCommitSchedulerDueWorkPlan,
} from "@/workers/utils/scheduler-refresh.js";

import { emitQueueStateMetrics } from "./queue-metrics.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";
import {
  checkSlotAwareDueWork,
  clearSlotAwareDueWork,
  peekSlotAwareDueWork,
  registerSlotAwareDueWork,
  type SlotAwareDueWork,
} from "./slot-aware-due-work.js";

/**
 * Background block-commitment loop that packages processed L2 transactions into
 * L1 commitment transactions.
 *
 * The fiber relies on a worker thread for the heavy lifting, then translates
 * the worker's outcome into updates of the node's shared global state and
 * operational metrics.
 */
const commitBlockNumTxGauge = Metric.gauge("commit_block_num_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the commit block",
  bigint: true,
});

const totalTxSizeGauge = Metric.gauge("total_tx_size", {
  description:
    "A gauge for tracking the total size of transactions in the commit block",
});

const commitBlockCounter = Metric.counter("commit_block_count", {
  description: "A counter for tracking the number of committed blocks",
  bigint: true,
  incremental: true,
});

const commitBlockTxCounter = Metric.counter("commit_block_tx_count", {
  description:
    "A counter for tracking the number of transactions in the commit block",
  bigint: true,
  incremental: true,
});

const commitBlockTxSizeGauge = Metric.gauge("commit_block_tx_size", {
  description: "A gauge for tracking the size of the commit block transaction",
});

const commitWorkerDurationTimer = Metric.timer(
  "commit_worker_duration",
  "Duration of one block commitment worker attempt in milliseconds",
);

const BLOCK_COMMITMENT_DUE_WORK_KIND = "commit_scheduler_refresh" as const;
const BLOCK_COMMITMENT_DUE_WORK_KEY = "block_commitment";

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

const planPreLeaseCommitSchedulerDueWork = Effect.gen(function* () {
  const globals = yield* Globals;
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const LOCAL_FINALIZATION_PENDING = yield* globals.LOCAL_FINALIZATION_PENDING;
  if (LOCAL_FINALIZATION_PENDING) {
    return localFinalizationPendingCommitSchedulerPlan();
  }
  const snapshot = yield* fetchStateQueueSnapshotProgram(
    lucid.api,
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

const clearRegisteredCommitDueWork = (): void => {
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

export const shouldAttemptCommitPipeline = ({
  localFinalizationPending,
  mempoolTxCount,
  processedUnsubmittedTxCount,
  pendingUserEventCount,
}: {
  readonly localFinalizationPending: boolean;
  readonly mempoolTxCount: bigint;
  readonly processedUnsubmittedTxCount: number;
  readonly pendingUserEventCount: number;
}): boolean =>
  localFinalizationPending ||
  mempoolTxCount > 0n ||
  processedUnsubmittedTxCount > 0 ||
  pendingUserEventCount > 0;

const shouldSkipIdleCommitPipelineBeforeSchedulerAlignment =
  Effect.gen(function* () {
    const globals = yield* Globals;
    const localFinalizationPending = yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    );
    const processedUnsubmittedTxCount = yield* Ref.get(
      globals.PROCESSED_UNSUBMITTED_TXS_COUNT,
    );
    const mempoolTxCount = yield* MempoolDB.retrieveTxCount;
    const pendingUserEventCount = yield* pendingUserEventCountUpTo(
      new Date(Date.now() + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS),
    );
    if (
      shouldAttemptCommitPipeline({
        localFinalizationPending,
        mempoolTxCount,
        processedUnsubmittedTxCount,
        pendingUserEventCount,
      })
    ) {
      return false;
    }
    yield* Effect.logInfo(
      "🔹 No pending tx/user-event work for block commitment; skipping pre-lease scheduler alignment.",
    );
    yield* emitQueueStateMetrics;
    return true;
  });

const isDetailedSchedulerAlignmentDueWork = (
  entry: SlotAwareDueWork,
): boolean =>
  entry.kind === BLOCK_COMMITMENT_DUE_WORK_KIND &&
  entry.key === BLOCK_COMMITMENT_DUE_WORK_KEY &&
  entry.dependencyKey.startsWith("scheduler=");

const resolveFreshDetailedSchedulerDueWork = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const alignedEndTime =
    Date.now() + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS;
  const alignment = yield* fetchRealStateQueueWitnessContext(
    lucid.api,
    contracts,
    alignedEndTime,
    undefined,
    lucid.referenceScriptsAddress,
    lucid.submitSlotSnapshot,
    false,
  );
  if ("dueWork" in alignment) {
    return alignment.dueWork;
  }
  return undefined;
});

const shouldSkipForDetailedSchedulerDueWork: Effect.Effect<
  boolean,
  never,
  Globals | Lucid | MidgardContracts
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

const shouldSkipForRegisteredCommitDueWork = Effect.gen(function* () {
  const entry = peekSlotAwareDueWork(
    BLOCK_COMMITMENT_DUE_WORK_KIND,
    BLOCK_COMMITMENT_DUE_WORK_KEY,
  );
  if (entry === undefined) {
    return false;
  }
  if (isDetailedSchedulerAlignmentDueWork(entry)) {
    return yield* shouldSkipForDetailedSchedulerDueWork;
  }
  const freshPlan = yield* Effect.either(planPreLeaseCommitSchedulerDueWork);
  if (freshPlan._tag === "Left") {
    clearRegisteredCommitDueWork();
    yield* Effect.logWarning(
      `🔹 Clearing block commitment due work before re-plan because fresh pre-lease evidence failed: ${String(freshPlan.left)}`,
    );
    return false;
  }
  if (freshPlan.right.status !== "register_due_work") {
    clearRegisteredCommitDueWork();
    yield* Effect.logInfo(
      `🔹 Clearing block commitment due work before re-plan because fresh pre-lease evidence no longer proves scheduler due-work (status=${freshPlan.right.status},reason=${freshPlan.right.reason}).`,
    );
    return false;
  }
  if (
    freshPlan.right.dueWork.key !== entry.key ||
    freshPlan.right.dueWork.dueSlot !== entry.dueSlot
  ) {
    clearRegisteredCommitDueWork();
    yield* Effect.logInfo(
      `🔹 Clearing block commitment due work before re-plan because fresh pre-lease due-work changed (old_due_slot=${entry.dueSlot.toString()},fresh_due_slot=${freshPlan.right.dueWork.dueSlot.toString()}).`,
    );
    return false;
  }
  const decision = checkSlotAwareDueWork({
    kind: BLOCK_COMMITMENT_DUE_WORK_KIND,
    key: BLOCK_COMMITMENT_DUE_WORK_KEY,
    currentSlot: freshPlan.right.dueWork.observedSlot,
    dependencyKey: freshPlan.right.dueWork.dependencyKey,
    invalidationKey: freshPlan.right.dueWork.invalidationKey,
  });
  switch (decision.status) {
    case "skip":
      yield* Effect.logInfo(
        `🔹 Skipping block commitment trigger because registered due work is not due discovery_stage=pre_lease (kind=${decision.entry.kind},key=${decision.entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${decision.entry.dueSlot.toString()},wait_ms=${decision.entry.waitMs.toString()},dependency_key=${freshPlan.right.dueWork.dependencyKey}).`,
      );
      return true;
    case "due":
      yield* Effect.logInfo(
        `🔹 Waking block commitment due work discovery_stage=pre_lease (kind=${decision.entry.kind},key=${decision.entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${decision.entry.dueSlot.toString()}).`,
      );
      return false;
    case "invalidated":
      yield* Effect.logInfo(
        `🔹 Clearing block commitment due work before re-plan discovery_stage=pre_lease (${decision.reason}).`,
      );
      return false;
    case "missing":
      return false;
  }
});

const registerPreLeaseCommitSchedulerDueWork = (
  plan: Extract<EarliestCommitSchedulerPlan, { status: "register_due_work" }>,
) =>
  Effect.sync(() => registerSlotAwareDueWork(plan.dueWork)).pipe(
    Effect.andThen((entry) =>
      Effect.logInfo(
        `🔹 Registered slot-aware due work before block commitment lease discovery_stage=${plan.discoveryStage} (kind=${entry.kind},key=${entry.key},current_slot=${entry.observedSlot.toString()},due_slot=${entry.dueSlot.toString()},due_at_ms=${entry.dueAtMs.toString()},wait_ms=${entry.waitMs.toString()},slot_source=${entry.slotSource},dependency_key=${entry.dependencyKey}).`,
      ),
    ),
  );

const registerPreLeaseCommitSchedulerDueWorkIfProven = Effect.gen(function* () {
  const plan = yield* Effect.either(planPreLeaseCommitSchedulerDueWork);
  if (plan._tag === "Left") {
    yield* Effect.logWarning(
      `🔹 Earliest block commitment scheduler preflight failed; continuing to full planner: ${String(plan.left)}`,
    );
    return false;
  }
  if (plan.right.status === "register_due_work") {
    yield* registerPreLeaseCommitSchedulerDueWork(plan.right);
    return true;
  }
  if (plan.right.status === "ambiguous") {
    yield* Effect.logInfo(
      `🔹 Earliest block commitment scheduler preflight ambiguous; continuing to full planner (reason=${plan.right.reason}).`,
    );
  }
  return false;
});

export const shouldRunPreLeaseSchedulerAlignment = (
  plan: EarliestCommitSchedulerPlan,
): boolean =>
  (plan.status === "proceed" &&
    (plan.reason === "scheduler_submit_timing_ready" ||
      plan.reason === "current_operator_already_active")) ||
  (plan.status === "ambiguous" &&
    plan.reason === "scheduler_has_no_active_operator");

const alignCommitSchedulerBeforeMutationWorker = Effect.gen(function* () {
  const plan = yield* Effect.either(planPreLeaseCommitSchedulerDueWork);
  if (plan._tag === "Left") {
    yield* Effect.logWarning(
      `🔹 Pre-lease scheduler alignment probe failed; continuing to mutation worker guard where scheduler refresh is disabled: ${String(plan.left)}`,
    );
    return false;
  }
  if (plan.right.status === "register_due_work") {
    yield* registerPreLeaseCommitSchedulerDueWork(plan.right);
    return true;
  }
  if (!shouldRunPreLeaseSchedulerAlignment(plan.right)) {
    return false;
  }
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const alignedEndTime =
    Date.now() + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS;
  const alignment = yield* Effect.either(
    fetchRealStateQueueWitnessContext(
      lucid.api,
      contracts,
      alignedEndTime,
      undefined,
      lucid.referenceScriptsAddress,
      lucid.submitSlotSnapshot,
      true,
    ),
  );
  if (alignment._tag === "Left") {
    yield* Effect.logWarning(
      `🔹 Pre-lease scheduler alignment failed; continuing to mutation worker guard where scheduler refresh is disabled: ${String(alignment.left)}`,
    );
    return false;
  }
  if ("dueWork" in alignment.right) {
    registerSlotAwareDueWork(alignment.right.dueWork);
    yield* Effect.logInfo(
      `🔹 Registered slot-aware due work from pre-lease scheduler alignment (kind=${alignment.right.dueWork.kind},key=${alignment.right.dueWork.key},current_slot=${alignment.right.dueWork.observedSlot.toString()},due_slot=${alignment.right.dueWork.dueSlot.toString()},wait_ms=${alignment.right.dueWork.waitMs.toString()},dependency_key=${alignment.right.dueWork.dependencyKey}).`,
    );
    return true;
  }
  yield* Effect.logInfo(
    `🔹 Scheduler alignment completed before block commitment mutation worker (reason=${plan.right.reason}).`,
  );
  return false;
});

type CommitPhaseAcquireResult =
  | {
      readonly acquired: true;
    }
  | {
      readonly acquired: false;
      readonly activePhase: CommitPipelinePhase;
    };

const acquireCommitPipelinePhase = (
  globals: Globals,
  targetPhase: Exclude<CommitPipelinePhase, "idle">,
): Effect.Effect<CommitPhaseAcquireResult> =>
  Ref.modify(
    globals.COMMIT_PIPELINE_PHASE,
    (activePhase): [CommitPhaseAcquireResult, CommitPipelinePhase] =>
      activePhase === "idle"
        ? [{ acquired: true }, targetPhase]
        : [{ acquired: false, activePhase }, activePhase],
  );

export const tryAcquireCommitSchedulerAlignmentPhase = (
  globals: Globals,
): Effect.Effect<CommitPhaseAcquireResult> =>
  acquireCommitPipelinePhase(globals, "scheduler_alignment");

export const releaseCommitSchedulerAlignmentPhase = (
  globals: Globals,
): Effect.Effect<void> => Ref.set(globals.COMMIT_PIPELINE_PHASE, "idle");

export const tryAcquireCommitMutationWorkerPhase = (
  globals: Globals,
): Effect.Effect<CommitPhaseAcquireResult> =>
  Effect.gen(function* () {
    const acquired = yield* acquireCommitPipelinePhase(
      globals,
      "mutation_worker",
    );
    if (acquired.acquired) {
      yield* Ref.set(globals.COMMIT_WORKER_ACTIVE, true);
    }
    return acquired;
  });

export const releaseCommitMutationWorkerPhase = (
  globals: Globals,
): Effect.Effect<void> =>
  Effect.all(
    [
      Ref.set(globals.COMMIT_WORKER_ACTIVE, false),
      Ref.set(globals.COMMIT_PIPELINE_PHASE, "idle"),
    ],
    { discard: true },
  );

const alignCommitSchedulerBeforeMutationWorkerIfIdle = Effect.gen(function* () {
  const globals = yield* Globals;
  const acquired = yield* tryAcquireCommitSchedulerAlignmentPhase(globals);
  if (!acquired.acquired) {
    yield* Effect.logInfo(
      `🔹 Skipping block commitment trigger because commit pipeline phase is already active (phase=${acquired.activePhase}).`,
    );
    return true;
  }
  return yield* alignCommitSchedulerBeforeMutationWorker.pipe(
    Effect.ensuring(releaseCommitSchedulerAlignmentPhase(globals)),
  );
});

/**
 * Launches one commitment worker, applies its result to global node state, and
 * updates the block-commitment metrics.
 */
export const buildAndSubmitCommitmentBlockAction = (
  stateQueueLeaseToken?: string,
) =>
  Effect.gen(function* () {
    const workerStartedAt = Date.now();
    const globals = yield* Globals;
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const AVAILABLE_LOCAL_FINALIZATION_BLOCK =
      yield* globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK;
    const LOCAL_FINALIZATION_PENDING =
      yield* globals.LOCAL_FINALIZATION_PENDING;
    let AVAILABLE_CONFIRMED_BLOCK = yield* Ref.get(
      globals.AVAILABLE_CONFIRMED_BLOCK,
    );
    let CURRENT_BLOCK_START_TIME_MS = yield* Ref.get(
      globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
    );
    let BASE_SNAPSHOT_ID: string | undefined;
    let STATE_QUEUE_HAS_UNMERGED_TAIL = false;
    if (!LOCAL_FINALIZATION_PENDING) {
      const snapshot = yield* fetchStateQueueSnapshotProgram(
        lucid.api,
        contracts.stateQueue,
        "commit_preflight",
      );
      yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
      AVAILABLE_CONFIRMED_BLOCK = snapshot.tailCommitBase.utxo;
      CURRENT_BLOCK_START_TIME_MS = snapshot.tailCommitBase.blockEndTimeMs;
      BASE_SNAPSHOT_ID = snapshot.snapshotId;
      STATE_QUEUE_HAS_UNMERGED_TAIL =
        snapshot.root.outRef !== snapshot.tailCommitBase.outRef;
      yield* Effect.logInfo(
        `🔹 Using live state-queue tail commit base ${snapshot.tailCommitBase.outRef} from snapshot ${snapshot.snapshotId}.`,
      );
    }
    const PROCESSED_UNSUBMITTED_TXS_COUNT =
      yield* globals.PROCESSED_UNSUBMITTED_TXS_COUNT;
    const PROCESSED_UNSUBMITTED_TXS_SIZE =
      yield* globals.PROCESSED_UNSUBMITTED_TXS_SIZE;

    const worker = Effect.async<WorkerOutput, WorkerError, never>((resume) => {
      Effect.runSync(Effect.logInfo(`👷 Starting block commitment worker...`));
      const worker = new Worker(
        resolveWorkerEntry(import.meta.url, "commit-block-header.js"),
        {
          workerData: {
            data: {
              availableConfirmedBlock: AVAILABLE_CONFIRMED_BLOCK,
              availableLocalFinalizationBlock:
                AVAILABLE_LOCAL_FINALIZATION_BLOCK,
              currentBlockStartTimeMs: CURRENT_BLOCK_START_TIME_MS,
              localFinalizationPending: LOCAL_FINALIZATION_PENDING,
              mempoolTxsCountSoFar: PROCESSED_UNSUBMITTED_TXS_COUNT,
              sizeOfProcessedTxsSoFar: PROCESSED_UNSUBMITTED_TXS_SIZE,
              stateQueueLeaseToken,
              baseSnapshotId: BASE_SNAPSHOT_ID,
              stateQueueHasUnmergedTail: STATE_QUEUE_HAS_UNMERGED_TAIL,
            },
          } as WorkerInput, // TODO: Consider other approaches to avoid type assertion here.
        },
      );
      worker.on("message", (output: WorkerOutput) => {
        if (output.type === "FailureOutput") {
          resume(
            Effect.fail(
              new WorkerError({
                worker: "commit-block-header",
                message: `Commitment worker failed`,
                cause: output.error,
              }),
            ),
          );
        } else {
          resume(Effect.succeed(output));
        }
        worker.terminate();
      });
      worker.on("error", (e: Error) => {
        resume(
          Effect.fail(
            new WorkerError({
              worker: "commit-block-header",
              message: `Error in commitment worker: ${e}`,
              cause: e,
            }),
          ),
        );
        worker.terminate();
      });
      worker.on("exit", (code: number) => {
        if (code !== 0) {
          resume(
            Effect.fail(
              new WorkerError({
                worker: "commit-block-header",
                message: `Commitment worker exited with code: ${code}`,
                cause: `exit code ${code}`,
              }),
            ),
          );
        }
      });
      return Effect.sync(() => {
        worker.terminate();
      });
    });

    const workerOutput: WorkerOutput = yield* worker;
    yield* commitWorkerDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - workerStartedAt)),
    );

    switch (workerOutput.type) {
      case "SuccessfulSubmissionOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          workerOutput.submittedTxHash,
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
          Date.now(),
        );
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          workerOutput.blockEndTimeMs,
        );
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, 0);
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_SIZE, 0);

        yield* commitBlockTxSizeGauge(Effect.succeed(workerOutput.txSize));
        yield* commitBlockNumTxGauge(
          Effect.succeed(BigInt(workerOutput.mempoolTxsCount)),
        );
        yield* Metric.increment(commitBlockCounter);
        yield* Metric.incrementBy(
          commitBlockTxCounter,
          BigInt(workerOutput.mempoolTxsCount),
        );
        yield* totalTxSizeGauge(Effect.succeed(workerOutput.sizeOfBlocksTxs));
        yield* Effect.logInfo("🔹 ☑️  Block submission completed.");
        break;
      }
      case "SubmittedAwaitingLocalFinalizationOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          workerOutput.submittedTxHash,
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
          Date.now(),
        );
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          workerOutput.blockEndTimeMs,
        );
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
        yield* Effect.logWarning(
          `🔹 Block submitted but local finalization is pending recovery: ${workerOutput.error}`,
        );
        break;
      }
      case "SubmittedAwaitingConfirmationOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          workerOutput.submittedTxHash,
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
          Date.now(),
        );
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          workerOutput.blockEndTimeMs,
        );
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
        yield* Effect.logInfo(
          "🔹 Block submitted; local finalization is intentionally deferred until L1 confirmation.",
        );
        break;
      }
      case "SuccessfulLocalFinalizationRecoveryOutput": {
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, 0);
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_SIZE, 0);
        yield* Effect.logInfo(
          "🔹 ☑️  Local finalization recovery completed for confirmed block.",
        );
        break;
      }
      case "SkippedSubmissionOutput": {
        yield* Ref.update(
          globals.PROCESSED_UNSUBMITTED_TXS_COUNT,
          (n) => n + workerOutput.mempoolTxsCount,
        );
        yield* Ref.update(
          globals.PROCESSED_UNSUBMITTED_TXS_SIZE,
          (n) => n + workerOutput.sizeOfProcessedTxs,
        );
        break;
      }
      case "NothingToCommitOutput": {
        break;
      }
      case "RegisteredDueWorkOutput": {
        registerSlotAwareDueWork(workerOutput.dueWork);
        yield* Effect.logInfo(
          `🔹 Registered slot-aware due work from commitment worker (kind=${workerOutput.dueWork.kind},key=${workerOutput.dueWork.key},due_slot=${workerOutput.dueWork.dueSlot.toString()},wait_ms=${workerOutput.dueWork.waitMs.toString()}).`,
        );
        break;
      }
      case "FailureOutput": {
        break;
      }
    }

    yield* emitQueueStateMetrics;
  });

/**
 * Single scheduled commitment tick with a guard that prevents overlapping
 * commitment workers.
 */
export const blockCommitmentAction: Effect.Effect<
  void,
  | WorkerError
  | SDK.LucidError
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError
  | DatabaseError,
  Globals | Lucid | MidgardContracts | Database | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const nodeConfig = yield* NodeConfig;
  yield* Ref.set(globals.HEARTBEAT_BLOCK_COMMITMENT, Date.now());
  const RESET_IN_PROGRESS = yield* Ref.get(globals.RESET_IN_PROGRESS);
  if (!RESET_IN_PROGRESS) {
    if (yield* shouldSkipIdleCommitPipelineBeforeSchedulerAlignment) {
      return;
    }
    if (yield* shouldSkipForRegisteredCommitDueWork) {
      return;
    }
    if (yield* registerPreLeaseCommitSchedulerDueWorkIfProven) {
      return;
    }
    if (yield* alignCommitSchedulerBeforeMutationWorkerIfIdle) {
      return;
    }
    const acquired = yield* tryAcquireCommitMutationWorkerPhase(globals);
    if (!acquired.acquired) {
      yield* Effect.logInfo(
        `🔹 Skipping block commitment trigger because commit pipeline phase is already active (phase=${acquired.activePhase}).`,
      );
      return;
    }

    yield* Effect.logInfo("🔹 New block commitment process started.");
    const leaseResult = yield* StateQueueMutationLeasesDB.tryWithLease(
      "block_commitment",
      (leaseToken) =>
        buildAndSubmitCommitmentBlockAction(leaseToken).pipe(
          Effect.withSpan("buildAndSubmitCommitmentBlockAction"),
        ),
      {
        ttlMs: nodeConfig.STATE_QUEUE_MUTATION_LEASE_TTL_MS,
        renewIntervalMs:
          nodeConfig.STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS,
      },
    ).pipe(Effect.ensuring(releaseCommitMutationWorkerPhase(globals)));
    if (leaseResult._tag === "Busy") {
      yield* Effect.logInfo(
        `🔹 Skipping block commitment trigger because the state-queue mutation lease is busy (${StateQueueMutationLeasesDB.describeActiveLease(
          leaseResult.activeLease,
        )}).`,
      );
    }
  }
});

/**
 * Fiber wrapper that repeats block-commitment work on the provided schedule.
 */
export const blockCommitmentFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Globals | Lucid | MidgardContracts | Database | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔵 Block commitment fiber started.");
    const action = blockCommitmentAction.pipe(
      Effect.withSpan("block-commitment-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
