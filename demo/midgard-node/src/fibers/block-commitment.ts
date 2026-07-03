import * as SDK from "@al-ft/midgard-sdk";
import { Duration, Effect, Metric, Ref, Schedule } from "effect";
import { Worker } from "worker_threads";

import { StateQueueMutationLeasesDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
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
import type {
  CommitSchedulerStateQueueEvidence,
  EarliestCommitSchedulerPlan,
} from "@/workers/utils/commit-block-planner.js";
import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import { WorkerError } from "@/workers/utils/common.js";
import { resolveEarliestCommitSchedulerDueWorkPlan } from "@/workers/utils/scheduler-refresh.js";

import { emitQueueStateMetrics } from "./queue-metrics.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";
import {
  checkSlotAwareDueWork,
  clearSlotAwareDueWork,
  peekSlotAwareDueWork,
  registerSlotAwareDueWork,
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

const shouldSkipForRegisteredCommitDueWork = Effect.gen(function* () {
  const entry = peekSlotAwareDueWork(
    BLOCK_COMMITMENT_DUE_WORK_KIND,
    BLOCK_COMMITMENT_DUE_WORK_KEY,
  );
  if (entry === undefined) {
    return false;
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
    if (yield* shouldSkipForRegisteredCommitDueWork) {
      return;
    }
    if (yield* registerPreLeaseCommitSchedulerDueWorkIfProven) {
      return;
    }
    const acquired = yield* Ref.modify(
      globals.COMMIT_WORKER_ACTIVE,
      (active) => (active ? [false, true] : [true, true]),
    );
    if (!acquired) {
      yield* Effect.logInfo(
        "🔹 Skipping block commitment trigger because a worker is already active.",
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
    ).pipe(Effect.ensuring(Ref.set(globals.COMMIT_WORKER_ACTIVE, false)));
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
