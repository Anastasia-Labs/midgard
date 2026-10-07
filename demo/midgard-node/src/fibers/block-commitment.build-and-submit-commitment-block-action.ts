import { Duration, Effect, Metric, Option, Ref, Runtime } from "effect";

import {
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { canonicalSlotConfigForLucid } from "../lucid-time.js";
import {
  HistoryProducer,
  runHistoryProducer,
} from "../services/event-history-producer.js";
import type { ForeignBaseVerificationScope } from "../services/foreign-base-verification.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { recoverNativeMpfForLocalFinalization } from "../services/native-mpf-local-finalization.js";
import {
  fetchStateQueueSnapshotProgram,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/state-queue-topology.js";
import {
  WorkerInput,
  WorkerOutput,
} from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";
import { extendCommitmentHoldForBacklog } from "./block-commitment.commit-hold-budget.js";
import { promoteCommitWorkerNativeResult } from "./block-commitment.native-result.js";
import {
  applyCommitForeignVerification,
  beginCommitForeignVerification,
  notifyForeignNativeAdoptionRequested,
  prepareForeignBaseForCommitment,
} from "./block-commitment.prepare-foreign-base.js";
import {
  commitBlockCounter,
  commitBlockNumTxGauge,
  commitBlockTxCounter,
  commitBlockTxSizeGauge,
  commitWorkerDurationTimer,
  type CommitWorkerMessage,
  publishFullMempoolLedgerReload,
  recoverNativeMpfFromActiveJournalAfterWorkerFailure,
  resolveAuthoritativeLocalFinalizationPreflight,
  takeCommitWorkerOutput,
  totalTxSizeGauge,
} from "./block-commitment.promote-or-recover-native-mpf.js";
import { runCommitWorkerInThread } from "./block-commitment.run-commit-worker-in-thread.js";
import { publishCommitMempoolLedgerMutation } from "./block-commitment.should-skip-for-detailed-scheduler-due-work.js";
import { classifyCommitWorkerOutputForMutationLease } from "./commit-worker-failure-classification.js";
import { nativeMpfWorkerInput } from "./native-mpf-worker-input.js";
import { emitQueueStateMetrics } from "./queue-metrics.js";
import { registerSlotAwareDueWork } from "./slot-aware-due-work.js";

/** Run a commitment worker and publish its node state and metrics. */
export const buildAndSubmitCommitmentBlockAction = (
  stateQueueLeaseToken?: string,
) => {
  let adoptionRequested = false;
  return Effect.gen(function* () {
    const history = yield* HistoryProducer;
    const workerStartedAt = Date.now();
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    let AVAILABLE_LOCAL_FINALIZATION_BLOCK =
      yield* globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK;
    let LOCAL_FINALIZATION_PENDING = yield* globals.LOCAL_FINALIZATION_PENDING;
    let AVAILABLE_CONFIRMED_BLOCK = yield* Ref.get(
      globals.AVAILABLE_CONFIRMED_BLOCK,
    );
    let CURRENT_BLOCK_START_TIME_MS = yield* Ref.get(
      globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
    );
    let foreignVerificationScope: ForeignBaseVerificationScope | undefined;
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
      foreignVerificationScope = yield* beginCommitForeignVerification(
        globals,
        {
          ...history.token,
          baseHeaderHash: snapshot.tailCommitBase.headerHash,
        },
      );
      STATE_QUEUE_HAS_UNMERGED_TAIL =
        snapshot.root.outRef !== snapshot.tailCommitBase.outRef;
      const activePending = yield* PendingBlockFinalizationsDB.retrieveActive();
      const activeJournalHeaderHash = Option.match(activePending, {
        onNone: () => null,
        onSome: (row) =>
          row[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex"),
      });
      const activeJournalSubmittedTxHash = Option.match(activePending, {
        onNone: () => null,
        onSome: (row) =>
          row[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]?.toString(
            "hex",
          ) ?? null,
      });
      const activeJournalStatus = Option.match(activePending, {
        onNone: () => null,
        onSome: (row) => row[PendingBlockFinalizationsDB.Columns.STATUS],
      });
      const finalizationPreflight =
        resolveAuthoritativeLocalFinalizationPreflight({
          localFinalizationPending: LOCAL_FINALIZATION_PENDING,
          availableLocalFinalizationBlock: AVAILABLE_LOCAL_FINALIZATION_BLOCK,
          activeJournalHeaderHash,
          activeJournalSubmittedTxHash,
          activeJournalStatus,
          tailHeaderHash: snapshot.tailCommitBase.headerHash,
          tailBlock: snapshot.tailCommitBase.utxo,
        });
      LOCAL_FINALIZATION_PENDING =
        finalizationPreflight.localFinalizationPending;
      AVAILABLE_LOCAL_FINALIZATION_BLOCK =
        finalizationPreflight.availableLocalFinalizationBlock;
      if (finalizationPreflight.localFinalizationPending) {
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
        yield* Ref.set(
          globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
          AVAILABLE_LOCAL_FINALIZATION_BLOCK,
        );
        yield* Effect.logWarning(
          finalizationPreflight.recoveredRacedJournal
            ? `🔹 Recovered raced local-finalization preflight from authoritative journal and confirmed state-queue tail (header=${activeJournalHeaderHash ?? "unknown"}).`
            : `🔹 Deferred commitment from authoritative submitted journal that is not yet the confirmed state-queue tail (header=${activeJournalHeaderHash ?? "unknown"}).`,
        );
      }
      yield* Effect.logInfo(
        `🔹 Using live state-queue tail commit base ${snapshot.tailCommitBase.outRef} from snapshot ${snapshot.snapshotId}.`,
      );
    }
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
    if (
      LOCAL_FINALIZATION_PENDING &&
      AVAILABLE_LOCAL_FINALIZATION_BLOCK !== ""
    ) {
      yield* recoverNativeMpfForLocalFinalization(
        nativeMpfOwner,
        AVAILABLE_LOCAL_FINALIZATION_BLOCK,
      );
    }
    const foreignBase = yield* prepareForeignBaseForCommitment({
      localFinalizationPending: LOCAL_FINALIZATION_PENDING,
      availableConfirmedBlock: AVAILABLE_CONFIRMED_BLOCK,
      owner: nativeMpfOwner,
      globals,
      scope: foreignVerificationScope,
    });
    if (foreignBase !== undefined) {
      adoptionRequested = foreignBase.adoptionRequested;
      return foreignBase.output;
    }
    const nativeMpfInput = yield* nativeMpfWorkerInput(
      nativeMpfOwner,
      "commit-block-header",
      nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256,
    );
    const ledgerStoreLeaseOwner =
      MpfEngineStateDB.nodeProcessCommitLeaseOwner();
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
            availableConfirmedBlock: AVAILABLE_CONFIRMED_BLOCK,
            availableLocalFinalizationBlock: AVAILABLE_LOCAL_FINALIZATION_BLOCK,
            currentBlockStartTimeMs: CURRENT_BLOCK_START_TIME_MS,
            forcedValidationSlotConfig: canonicalSlotConfigForLucid(lucid.api),
            localFinalizationPending: LOCAL_FINALIZATION_PENDING,
            ledgerStoreLeaseOwner,
            mempoolTxsCountSoFar: PROCESSED_UNSUBMITTED_TXS_COUNT,
            sizeOfProcessedTxsSoFar: PROCESSED_UNSUBMITTED_TXS_SIZE,
            stateQueueLeaseToken,
            baseSnapshotId: BASE_SNAPSHOT_ID,
            stateQueueHasUnmergedTail: STATE_QUEUE_HAS_UNMERGED_TAIL,
          },
        } as WorkerInput, // TODO: Consider other approaches to avoid type assertion here.
        transferList: [nativeMpfInput.port],
      },
      takeOutput: (message: CommitWorkerMessage): WorkerOutput | undefined =>
        takeCommitWorkerOutput(
          globals,
          message,
          nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
        ),
      releaseLedgerLease: releaseTerminatedWorkerLedgerLease,
    });

    const workerOutput: WorkerOutput = yield* worker.pipe(
      Effect.flatMap((output) =>
        classifyCommitWorkerOutputForMutationLease({
          output,
          stateQueueLeaseToken,
          retrieveJournalEvidence: (token) =>
            PendingBlockFinalizationsDB.retrieveByStateQueueLeaseToken(
              token,
            ).pipe(
              Effect.map((rows) =>
                rows.map((row) => ({
                  headerHash:
                    row[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
                  submittedTxHash:
                    row[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH],
                  status: row[PendingBlockFinalizationsDB.Columns.STATUS],
                })),
              ),
            ),
        }),
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

    yield* applyCommitForeignVerification(
      globals,
      foreignVerificationScope,
      workerOutput,
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
      case "AwaitingForeignDaOutput": {
        yield* Effect.logWarning(
          `🔹 Commit deferred awaiting verified foreign DA header_hash=${workerOutput.foreignHeaderHash} reason=${workerOutput.reason}`,
        );
        break;
      }
      case "FailureOutput": {
        break;
      }
    }
    yield* emitQueueStateMetrics;
    return workerOutput;
  }).pipe(
    runHistoryProducer,
    Effect.tap(() => notifyForeignNativeAdoptionRequested(adoptionRequested)),
  );
};
