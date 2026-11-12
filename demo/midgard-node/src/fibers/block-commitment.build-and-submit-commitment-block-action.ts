import { Duration, Effect, Metric, Ref, Runtime } from "effect";

import { MpfEngineStateDB } from "../database/index.js";
import { canonicalSlotConfigForLucid } from "../lucid-time.js";
import {
  FollowerWrite,
  runAtFollowerView,
} from "../services/follower-write-gate.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { IntentJournal } from "../services/intent-journal.js";
import {
  landedStateQueueSnapshot,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/landed-state-queue.js";
import {
  WorkerInput,
  WorkerOutput,
} from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";
import { abandonUnsignedActiveJournal } from "./block-commitment.abandon-unsigned-journal.js";
import { extendCommitmentHoldForBacklog } from "./block-commitment.commit-hold-budget.js";
import { promoteCommitWorkerNativeResult } from "./block-commitment.native-result.js";
import {
  commitBlockCounter,
  commitBlockNumTxGauge,
  commitBlockTxCounter,
  commitBlockTxSizeGauge,
  commitWorkerDurationTimer,
  type CommitWorkerMessage,
  publishFullMempoolLedgerReload,
  recoverNativeMpfFromActiveJournalAfterWorkerFailure,
  takeCommitWorkerOutput,
  totalTxSizeGauge,
} from "./block-commitment.promote-or-recover-native-mpf.js";
import { runCommitWorkerInThread } from "./block-commitment.run-commit-worker-in-thread.js";
import { publishCommitMempoolLedgerMutation } from "./block-commitment.should-skip-for-detailed-scheduler-due-work.js";
import { COMMIT_WINDOW_PENDING } from "./block-commitment.worker-readiness.js";
import { classifyCommitWorkerOutputAgainstLeaseJournal } from "./commit-worker-failure-classification.js";
import { nativeMpfWorkerInput } from "./native-mpf-worker-input.js";
import { emitQueueStateMetrics } from "./queue-metrics.js";
import { registerSlotAwareDueWork } from "./slot-aware-due-work.js";

/** Run a commitment worker and publish its node state and metrics. */
export const buildAndSubmitCommitmentBlockAction = (
  stateQueueLeaseToken?: string,
) => {
  return Effect.gen(function* () {
    const history = yield* FollowerWrite;
    const workerStartedAt = Date.now();
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    // The worker below prepares and signs under this lease, so an unsigned
    // journal still active now was left by an attempt that stopped first.
    yield* abandonUnsignedActiveJournal;
    const snapshot = yield* landedStateQueueSnapshot(
      contracts.stateQueue,
      "commit_preflight",
    );
    yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
    yield* Effect.logInfo(
      `🔹 Using live state-queue tail commit base ${snapshot.tailCommitBase.outRef} from snapshot ${snapshot.snapshotId}.`,
    );
    const PROCESSED_UNSUBMITTED_TXS_COUNT =
      yield* globals.PROCESSED_UNSUBMITTED_TXS_COUNT;
    const PROCESSED_UNSUBMITTED_TXS_SIZE =
      yield* globals.PROCESSED_UNSUBMITTED_TXS_SIZE;
    const nativeMpfOwner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
    if (nativeMpfOwner === undefined) {
      return yield* Effect.fail(
        new WorkerError({
          worker: "commit-block-header",
          message: "Architecture G native owner is not initialized",
          cause: "NATIVE_MPF_OWNER is absent",
        }),
      );
    }
    const nativeMpfInput = yield* nativeMpfWorkerInput(
      nativeMpfOwner,
      "commit-block-header",
      nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256,
    );
    const ledgerStoreLeaseOwner =
      MpfEngineStateDB.nodeProcessCommitLeaseOwner();
    const intentJournal = yield* IntentJournal;
    const databaseRuntime = yield* Effect.runtime<Database>();
    const releaseTerminatedWorkerLedgerLease = () =>
      Runtime.runPromise(databaseRuntime)(
        MpfEngineStateDB.releaseLedgerStoreLease(ledgerStoreLeaseOwner).pipe(
          Effect.catchAll((cause) =>
            Effect.logError(
              "Failed to release the terminated commitment worker ledger MPF lease; its bounded TTL remains the fallback.",
              cause,
            ),
          ),
        ),
      );

    // Size this hold from the batch it serves before the worker starts, so a
    // large batch is not cut off by a budget sized for a small one.
    yield* extendCommitmentHoldForBacklog(
      globals,
      nodeConfig.COMMIT_MAX_L2_TX_COUNT,
    );
    const worker = runCommitWorkerInThread({
      workerOptions: {
        workerData: {
          nativeMpf: nativeMpfInput,
          history,
          data: {
            availableConfirmedBlock: snapshot.tailCommitBase.utxo,
            currentBlockStartTimeMs: snapshot.tailCommitBase.blockEndTimeMs,
            forcedValidationSlotConfig: canonicalSlotConfigForLucid(lucid.api),
            ledgerStoreLeaseOwner,
            mempoolTxsCountSoFar: PROCESSED_UNSUBMITTED_TXS_COUNT,
            sizeOfProcessedTxsSoFar: PROCESSED_UNSUBMITTED_TXS_SIZE,
            stateQueueLeaseToken,
            baseSnapshotId: snapshot.snapshotId,
            stateQueueHasUnmergedTail:
              snapshot.root.outRef !== snapshot.tailCommitBase.outRef,
          },
        } as WorkerInput, // TODO: Consider other approaches to avoid type assertion here.
        transferList: [nativeMpfInput.port],
      },
      takeOutput: (message: CommitWorkerMessage): WorkerOutput | undefined =>
        takeCommitWorkerOutput(
          globals,
          message,
          nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
          intentJournal.adopt,
        ),
      releaseLedgerLease: releaseTerminatedWorkerLedgerLease,
    });

    const workerOutput: WorkerOutput = yield* worker.pipe(
      Effect.flatMap((output) =>
        classifyCommitWorkerOutputAgainstLeaseJournal(
          globals,
          output,
          stateQueueLeaseToken,
        ),
      ),
      // A failed attempt may already have committed a commit-stage rejection
      // whose mempool_ledger revert the cache has not seen.
      Effect.tapError(() =>
        publishFullMempoolLedgerReload(
          globals,
          nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
        ),
      ),
      Effect.catchAll((workerError) =>
        recoverNativeMpfFromActiveJournalAfterWorkerFailure(
          nativeMpfOwner,
        ).pipe(
          Effect.tap((recovered) =>
            recovered
              ? Effect.logWarning(
                  "Architecture G recovered the submitted native generation from the durable journal after the commit worker failed before returning its promotion handle.",
                )
              : Effect.void,
          ),
          Effect.matchEffect({
            onFailure: (recoveryError) =>
              Effect.fail(
                new WorkerError({
                  worker: "commit-block-header",
                  message:
                    "Commit worker failed and live Architecture G journal recovery also failed",
                  cause: { workerError, recoveryError },
                }),
              ),
            onSuccess: () => Effect.fail(workerError),
          }),
        ),
      ),
    );
    yield* promoteCommitWorkerNativeResult(nativeMpfOwner, workerOutput);
    yield* commitWorkerDurationTimer(
      Effect.succeed(Duration.millis(Date.now() - workerStartedAt)),
    );
    yield* publishCommitMempoolLedgerMutation(
      globals,
      workerOutput,
      nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
    );

    switch (workerOutput.type) {
      case "SuccessfulSubmissionOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
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
      case "SubmittedAwaitingConfirmationOutput": {
        yield* Ref.update(globals.BLOCKS_IN_QUEUE, (n) => n + 1);
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
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
        // The processed transactions the block took are no longer
        // unsubmitted; landed-block processing applies the block.
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, 0);
        yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_SIZE, 0);
        yield* Effect.logInfo(
          "🔹 Block submitted; landed-block processing applies it once it lands.",
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
      case "AwaitingCommitBaseOutput": {
        yield* Effect.logWarning(
          `🔹 Commit deferred awaiting its base header_hash=${workerOutput.baseHeaderHash}: ${workerOutput.detail}`,
        );
        break;
      }
      case "AwaitingNextCommitWindowOutput": {
        yield* Effect.logWarning(
          `🔹 Commit deferred awaiting the next window (${COMMIT_WINDOW_PENDING}) header_hash=${workerOutput.heldHeaderHash}: ${workerOutput.detail}`,
        );
        break;
      }
      case "FailureOutput": {
        break;
      }
    }
    yield* emitQueueStateMetrics;
    return workerOutput;
  }).pipe(runAtFollowerView);
};
