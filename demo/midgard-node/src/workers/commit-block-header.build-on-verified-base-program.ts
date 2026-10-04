import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  CommitBuildCalibrationDB,
  MempoolDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../database/utils/tx.js";
import { reachPipelinedCommitCrashCheckpoint } from "../e2e/pipelined-commit-crash-checkpoint.js";
import { fetchAndInsertDepositUTxOsForCommitBarrier } from "../fibers/fetch-and-insert-deposit-utxos.js";
import { fetchAndInsertTxOrderUTxOsForCommitBarrier } from "../fibers/fetch-and-insert-tx-order-utxos.js";
import { fetchAndInsertWithdrawalUTxOsForCommitBarrier } from "../fibers/fetch-and-insert-withdrawal-utxos.js";
import { minimumBarrierWatermarkMs } from "../fibers/speculative-commit-state.js";
import { unixTimeToSlotForConfig } from "../lucid-time.js";
import {
  configureCommitMpfRuntime,
  MidgardMpf,
  type NativeMpfBuildContext,
  processMpfs,
} from "../mpf/index.js";
import { assertHistoryProducer } from "../services/event-history-producer.js";
import {
  HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  historyEligibilityHorizon,
} from "../services/history-commit-window.js";
import {
  ContractDeploymentIdentity,
  Database,
  Lucid,
  MidgardContracts,
  NativeMpfWorkerPortClient,
  NodeConfig,
} from "../services/index.js";
import { outRefLabel } from "../tx-context.js";
import {
  alignCommitMpfsToBase,
  type AwaitSpeculativeCommitInstruction,
  MEMPOOL_LEDGER_REVERTED_NOTICE,
  type NotifyCommitWorkerParent,
  shouldPreserveCommitMpfRoots,
  shouldShortCircuitIdleCommitAttempt,
  workerPreIngestionDueWorkOutputFromPlan,
} from "./commit-block-header.commit-explicit-block-header-program.js";
import {
  type CommitLucidFactory,
  CommitWorkerInvariantError,
  defaultCommitLucidFactory,
  pendingUserEventCountsUpTo,
  pendingUserEventCountUpTo,
  shouldHydrateCommitBaseEntries,
} from "./commit-block-header.pending-user-event-counts-up-to.js";
import { resolveCommitBaseLedgerEntries } from "./commit-block-header.resolve-commit-base-ledger-entries.js";
import { revalidateAndPersistSpeculativeCandidateSources } from "./commit-block-header.revalidate-and-persist-speculative-candidate-sources.js";
import {
  resolveDepositsRoot,
  resolveForcedTransactionsRoot,
  resolveWithdrawalsRoot,
} from "./commit-block-header/event-roots.js";
import {
  getLatestBlockDatumEndTime,
  resolveCommitAppendFenceEndTimeCapLocal,
} from "./commit-block-header/state-queue.js";
import {
  deferProcessedCommitPayloadUntilConfirmation,
  recoverLocalFinalizationAgainstConfirmedBlock,
  submitDepositOnlyCommit,
  submitTxBackedCommit,
} from "./commit-block-header/submission.js";
import { reconcileOverdueAwaitingEventsAgainstRetainedForeignTips } from "./t2-foreign-event-reconciliation.js";
import {
  deserializeStateQueueUTxO,
  type SpeculativeCandidateInvalidatedOutput,
  type SpeculativeCandidateReadyOutput,
  WorkerInput,
  WorkerOutput,
} from "./utils/commit-block-header.js";
import {
  calibratedCommitBuildMsPerTx,
  type CommitSchedulerStateQueueEvidence,
  type CurrentOperatorSchedulerWindow,
  DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  establishEndTimeFromTxRequests,
  planCommitBatchBudgets,
  planSchedulerAwareCommitSelection,
  schedulerAwareCommitWindowBudgets,
  selectCommitTxCandidates,
  shouldDeferCommitSubmission,
  shouldSkipIdleCommitBehindUnmergedTail,
  updateCommitBuildEwma,
} from "./utils/commit-block-planner.js";
import {
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  resolveCommitEndTimeFit,
  resolveHistoryCommitEndTime,
  resolveLatestFeasibleCommitEndTime,
} from "./utils/commit-end-time.js";
import {
  resolveCurrentOperatorSchedulerWindow,
  resolveEarliestCommitSchedulerDueWorkPlan,
} from "./utils/scheduler-refresh.js";

export const buildOnVerifiedCommitBaseProgram = (
  workerInput: WorkerInput,
  transactionsMpf: MidgardMpf,
  awaitSpeculativeInstruction?: AwaitSpeculativeCommitInstruction,
  notifyParent?: NotifyCommitWorkerParent,
  activeMpfLeaseOwner?: string,
  localFinalizationTransactionsMpf?: MidgardMpf,
  postWaitMpfContext?: () => {
    readonly transactionsMpf: MidgardMpf;
    readonly localFinalizationTransactionsMpf?: MidgardMpf;
  },
  nativeMpfClient?: NativeMpfWorkerPortClient,
  nativeMpfState?: {
    context?: NativeMpfBuildContext;
    preserve: boolean;
  },
  acquireCommitLucid: CommitLucidFactory = defaultCommitLucidFactory,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
> =>
  Effect.gen(function* () {
    let acquiredCommitLucid: Lucid | undefined;
    const acquireCommitLucidOnce = Effect.suspend(() =>
      acquiredCommitLucid === undefined
        ? acquireCommitLucid().pipe(
            Effect.tap((lucid) =>
              Effect.sync(() => {
                acquiredCommitLucid = lucid;
              }),
            ),
          )
        : Effect.succeed(acquiredCommitLucid),
    );
    yield* assertHistoryProducer(workerInput.history);
    const workerStartedAtMs = Date.now();
    let baseHydrationPasses = 0;
    let mpfProcessingPasses = 0;
    let speculativeReadyPassCounts:
      | {
          readonly candidateId: string;
          readonly baseHydrationPasses: number;
          readonly mpfProcessingPasses: number;
        }
      | undefined;
    const attachSpeculativeExecutionEvidence = (
      output: WorkerOutput,
    ): WorkerOutput =>
      speculativeReadyPassCounts !== undefined &&
      output.type === "SubmittedAwaitingConfirmationOutput"
        ? {
            ...output,
            speculativeExecution: {
              candidateId: speculativeReadyPassCounts.candidateId,
              baseHydrationPassesBeforeReady:
                speculativeReadyPassCounts.baseHydrationPasses,
              mpfProcessingPassesBeforeReady:
                speculativeReadyPassCounts.mpfProcessingPasses,
              baseHydrationPassesAfterReady:
                baseHydrationPasses -
                speculativeReadyPassCounts.baseHydrationPasses,
              mpfProcessingPassesAfterReady:
                mpfProcessingPasses -
                speculativeReadyPassCounts.mpfProcessingPasses,
            },
          }
        : output;
    const nodeConfig = yield* NodeConfig;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    if (deploymentIdentity.deploymentMarker === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "V1 commit production requires a verified final deployment marker",
        }),
      );
    }
    const deploymentMarker = deploymentIdentity.deploymentMarker;
    yield* configureCommitMpfRuntime(nodeConfig);
    if (nativeMpfClient === undefined || workerInput.nativeMpf === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Architecture G commit worker is missing its main-owner port/root",
        }),
      );
    }
    yield* MpfEngineStateDB.assertLedgerAuditHealthy;
    yield* Effect.logInfo(
      `pipeline_trace phase=commit_worker_started at_ms=${workerStartedAtMs.toString()}`,
    );
    const excludedMempoolTxIds = new Set(
      workerInput.data.speculativeBuild?.excludedMempoolTxIds ?? [],
    );
    const retrievedMempoolTxs: EntryWithTimeStamp[] = [];
    let mempoolCursor: MempoolDB.MempoolCursor | undefined;
    while (retrievedMempoolTxs.length < nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE) {
      const page = yield* MempoolDB.retrievePage({
        after: mempoolCursor,
        limit: nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE,
        upTo: new Date(workerStartedAtMs),
      });
      retrievedMempoolTxs.push(
        ...page.entries.filter(
          (entry) =>
            !excludedMempoolTxIds.has(entry[TxColumns.TX_ID].toString("hex")),
        ),
      );
      if (page.nextCursor === null) break;
      mempoolCursor = page.nextCursor;
    }
    if (retrievedMempoolTxs.length > nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE) {
      retrievedMempoolTxs.length = nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE;
    }
    const currentBlockStartTime = new Date(
      workerInput.data.currentBlockStartTimeMs,
    );
    const retrievedProcessedPendingTxs = yield* ProcessedMempoolDB.retrieve;
    const excludeSubmittedBasePayload = (
      entry: (typeof retrievedMempoolTxs)[number],
    ): boolean =>
      !excludedMempoolTxIds.has(entry[TxColumns.TX_ID].toString("hex"));
    const mempoolTxs = retrievedMempoolTxs;
    const processedPendingTxs = retrievedProcessedPendingTxs.filter(
      excludeSubmittedBasePayload,
    );
    const rawCandidateSelection = selectCommitTxCandidates({
      mempoolTxs,
      processedMempoolTxs: processedPendingTxs,
    });
    const availableConfirmedBlock = workerInput.data.availableConfirmedBlock;
    const speculativeBuild = workerInput.data.speculativeBuild;
    const availableLocalFinalizationBlock =
      workerInput.data.availableLocalFinalizationBlock;
    const hasAvailableConfirmedBlock = availableConfirmedBlock !== "";
    const hasAvailableLocalFinalizationBlock =
      availableLocalFinalizationBlock !== "";
    const canBuildOnConfirmedBlock =
      (hasAvailableConfirmedBlock || speculativeBuild !== undefined) &&
      !workerInput.data.localFinalizationPending;
    let latestBlockForSchedulerPlanning: SDK.StateQueueUTxO | undefined;
    let latestEndTimeMsForSchedulerPlanning: number | undefined;
    if (canBuildOnConfirmedBlock) {
      if (speculativeBuild === undefined) {
        if (availableConfirmedBlock === "") {
          return yield* Effect.fail(
            new CommitWorkerInvariantError({
              message:
                "Confirmed commit build is missing its serialized state-queue base",
            }),
          );
        }
        latestBlockForSchedulerPlanning = yield* deserializeStateQueueUTxO(
          availableConfirmedBlock,
        );
        latestEndTimeMsForSchedulerPlanning = Number(
          (yield* getLatestBlockDatumEndTime(
            latestBlockForSchedulerPlanning.datum,
          )).getTime(),
        );
      } else {
        latestEndTimeMsForSchedulerPlanning =
          speculativeBuild.base.blockEndTimeMs;
      }
      const stateQueueEvidence: CommitSchedulerStateQueueEvidence = {
        tailCommitBaseOutRef:
          speculativeBuild === undefined
            ? outRefLabel(latestBlockForSchedulerPlanning!.utxo)
            : `${speculativeBuild.base.submittedTxHash}#0`,
        tailBlockEndTimeMs: latestEndTimeMsForSchedulerPlanning,
        stateQueueHasUnmergedTail:
          workerInput.data.stateQueueHasUnmergedTail ?? false,
      };
      if (speculativeBuild === undefined) {
        const contracts = yield* MidgardContracts;
        const lucid = yield* acquireCommitLucidOnce;
        yield* lucid.switchToOperatorsMainWallet;
        const preIngestionPlan = yield* Effect.either(
          resolveEarliestCommitSchedulerDueWorkPlan({
            lucid: lucid.api,
            contracts,
            submitSlotSnapshot: lucid.submitSlotSnapshot,
            stateQueueEvidence,
            localFinalizationPending: workerInput.data.localFinalizationPending,
            callerLabel: "commit-scheduler-worker-pre-ingestion",
            discoveryStage: "worker_pre_ingestion",
          }),
        );
        if (preIngestionPlan._tag === "Right") {
          const output = workerPreIngestionDueWorkOutputFromPlan(
            preIngestionPlan.right,
          );
          if (output !== undefined) {
            yield* Effect.logInfo(
              `🔹 Registered slot-aware due work before commit ingestion barriers discovery_stage=worker_pre_ingestion (kind=${output.dueWork.kind},key=${output.dueWork.key},current_slot=${output.dueWork.observedSlot.toString()},due_slot=${output.dueWork.dueSlot.toString()},due_at_ms=${output.dueWork.dueAtMs.toString()},wait_ms=${output.dueWork.waitMs.toString()},slot_source=${output.dueWork.slotSource},dependency_key=${output.dueWork.dependencyKey}).`,
            );
            return output;
          }
        } else {
          yield* Effect.logWarning(
            `🔹 Worker pre-ingestion scheduler due-work preflight failed; continuing to full planner: ${String(preIngestionPlan.left)}`,
          );
        }
      }
    }
    const historyEndTime =
      workerInput.history === undefined
        ? undefined
        : new Date(historyEligibilityHorizon(workerInput.history.coverage));
    const depositIngestionBarrierTime =
      workerInput.history !== undefined
        ? historyEndTime!
        : speculativeBuild === undefined
          ? yield* acquireCommitLucidOnce.pipe(
              Effect.flatMap((lucid) =>
                fetchAndInsertDepositUTxOsForCommitBarrier(new Date()).pipe(
                  Effect.provideService(Lucid, lucid),
                ),
              ),
            )
          : new Date(speculativeBuild.watermarks.depositMs);
    const withdrawalIngestionBarrierTime =
      workerInput.history !== undefined
        ? historyEndTime!
        : speculativeBuild === undefined
          ? yield* acquireCommitLucidOnce.pipe(
              Effect.flatMap((lucid) =>
                fetchAndInsertWithdrawalUTxOsForCommitBarrier(
                  depositIngestionBarrierTime,
                ).pipe(Effect.provideService(Lucid, lucid)),
              ),
            )
          : new Date(speculativeBuild.watermarks.withdrawalMs);
    const txOrderIngestionBarrierTime =
      speculativeBuild === undefined
        ? yield* acquireCommitLucidOnce.pipe(
            Effect.flatMap((lucid) =>
              fetchAndInsertTxOrderUTxOsForCommitBarrier(
                withdrawalIngestionBarrierTime,
              ).pipe(Effect.provideService(Lucid, lucid)),
            ),
          )
        : new Date(speculativeBuild.watermarks.txOrderMs);
    const userEventOnlyEndTime = [
      depositIngestionBarrierTime,
      withdrawalIngestionBarrierTime,
      txOrderIngestionBarrierTime,
    ].reduce((earliest, candidate) =>
      candidate.getTime() < earliest.getTime() ? candidate : earliest,
    );

    let currentSchedulerWindow: CurrentOperatorSchedulerWindow | undefined;
    let currentWindowCommitEndTimeFit:
      | ReturnType<typeof resolveCommitEndTimeFit>
      | undefined;
    const schedulerPlanningNowMs = Date.now();
    if (canBuildOnConfirmedBlock && speculativeBuild === undefined) {
      const contracts = yield* MidgardContracts;
      const lucid = yield* acquireCommitLucidOnce;
      yield* lucid.switchToOperatorsMainWallet;
      currentSchedulerWindow = yield* resolveCurrentOperatorSchedulerWindow(
        lucid.api,
        contracts,
      );
      if (currentSchedulerWindow !== undefined) {
        const latestEndTimeMs = Number(latestEndTimeMsForSchedulerPlanning);
        const txBackedCandidateEndTime = establishEndTimeFromTxRequests(
          rawCandidateSelection.candidateTxs,
        );
        const candidateEndTimeMs = Option.isSome(txBackedCandidateEndTime)
          ? txBackedCandidateEndTime.value.getTime()
          : userEventOnlyEndTime.getTime();
        // A source-owned commit's history horizon caps its end; it is not a
        // floor. The current window fits whenever some end inside it clears
        // the monotonic and current-time floors. Selected L2 transactions are
        // timestamped no later than the worker start, below the current-time
        // floor.
        currentWindowCommitEndTimeFit =
          workerInput.history === undefined
            ? resolveCommitEndTimeFit({
                lucid: lucid.api,
                latestEndTime: latestEndTimeMs,
                candidateEndTime: candidateEndTimeMs,
                nowMs: schedulerPlanningNowMs,
                minimumFutureBufferMs: COMMIT_MINIMUM_FUTURE_BUFFER_MS,
                maximumEndTimeMs: currentSchedulerWindow.endTimeMs,
              })
            : resolveLatestFeasibleCommitEndTime({
                lucid: lucid.api,
                latestEndTime: latestEndTimeMs,
                nowMs: schedulerPlanningNowMs,
                minimumFutureBufferMs: HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
                maximumEndTimeMs: currentSchedulerWindow.endTimeMs,
              });
      }
    }
    const schedulerAwareCommitSelection = planSchedulerAwareCommitSelection({
      candidateSelection: rawCandidateSelection,
      userEventOnlyEndTime,
      currentSchedulerWindow,
      currentBlockStartTimeMs: currentBlockStartTime.getTime(),
      nowMs: schedulerPlanningNowMs,
      ...schedulerAwareCommitWindowBudgets(workerInput.history !== undefined),
      currentWindowCommitEndTimeFit,
    });
    if (
      schedulerAwareCommitSelection.status === "using_current_scheduler_window"
    ) {
      yield* Effect.logInfo(
        `🔹 Scheduler-aware commit planner using current scheduler window (${schedulerAwareCommitSelection.reason},user_event_end_time=${schedulerAwareCommitSelection.userEventOnlyEndTime.toISOString()}).`,
      );
    } else if (
      schedulerAwareCommitSelection.status ===
        "current_scheduler_budget_too_low" ||
      schedulerAwareCommitSelection.status ===
        "current_scheduler_end_time_floor_exceeds_window"
    ) {
      yield* Effect.logInfo(
        `🔹 Scheduler-aware commit planner will not cap to current scheduler window (${schedulerAwareCommitSelection.reason}).`,
      );
    }
    const calibration =
      nodeConfig.COMMIT_BUILD_COST_MODEL === "ewma"
        ? yield* CommitBuildCalibrationDB.retrieve
        : undefined;
    const estimatedCommitBuildMsPerTx =
      calibration === undefined
        ? DEFAULT_COMMIT_BATCH_BUDGET_LIMITS.estimatedCommitBuildMsPerTx
        : calibratedCommitBuildMsPerTx({
            msPerTxEwma: calibration.msPerTxEwma,
            safetyFactor: nodeConfig.COMMIT_BUILD_EWMA_SAFETY_FACTOR,
          });
    const budgetedCommitSelection = planCommitBatchBudgets({
      candidateSelection: schedulerAwareCommitSelection.candidateSelection,
      limits: {
        ...DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
        maxL2TxCount: nodeConfig.COMMIT_MAX_L2_TX_COUNT,
        maxLedgerOpCount: nodeConfig.COMMIT_MAX_LEDGER_OP_COUNT,
        maxTransitionStepCount: nodeConfig.COMMIT_MAX_TRANSITION_STEP_COUNT,
        estimatedCommitBuildMsPerTx,
      },
    });
    const batchSelectedAtMs = Date.now();
    if (
      budgetedCommitSelection.plan.selectedTxCount > 0 ||
      budgetedCommitSelection.prunedTxCount > 0
    ) {
      yield* Effect.logInfo(
        `🔹 Commit batch planner selected tx_count=${budgetedCommitSelection.plan.selectedTxCount.toString()}, tx_bytes=${budgetedCommitSelection.plan.selectedTxBytes.toString()}, estimated_da_payload_bytes=${budgetedCommitSelection.plan.estimatedDaPayloadBytes.toString()}, estimated_commit_build_ms=${budgetedCommitSelection.plan.estimatedCommitBuildMs.toString()}, stop_reason=${budgetedCommitSelection.plan.stopReason}, pruned_tx_count=${budgetedCommitSelection.prunedTxCount.toString()}.`,
      );
    }
    const candidateSelection = budgetedCommitSelection.candidateSelection;
    yield* Effect.logInfo(
      `pipeline_trace phase=batch_selected at_ms=${batchSelectedAtMs.toString()} elapsed_ms=${Math.max(0, batchSelectedAtMs - workerStartedAtMs).toString()} selected_tx_count=${budgetedCommitSelection.plan.selectedTxCount.toString()} selected_tx_bytes=${budgetedCommitSelection.plan.selectedTxBytes.toString()} stop_reason=${budgetedCommitSelection.plan.stopReason}`,
    );
    const effectiveUserEventOnlyEndTime =
      schedulerAwareCommitSelection.userEventOnlyEndTime;
    // A source-owned block takes the latest end every cap admits. When the
    // floors exceed the caps, the resolved end stays at the cap and the build
    // refuses it exactly as it refused an over-cap end before.
    const historyCommitEndTimeFit =
      historyEndTime === undefined
        ? undefined
        : yield* Effect.gen(function* () {
            const lucid = yield* acquireCommitLucidOnce;
            const contracts = yield* MidgardContracts;
            const submitSlot = yield* lucid.submitSlotSnapshot().pipe(
              Effect.mapError(
                (cause) =>
                  new SDK.LucidError({
                    message:
                      "Failed to acquire the submit-slot snapshot for the history commit end",
                    cause,
                  }),
              ),
            );
            // The fence reads the live state queue, the first L1 read of this
            // candidate build; a lost submit response is reconciled first,
            // exactly as every submit attempt requires.
            yield* PendingBlockFinalizationsDB.assertNoUnreconciledSignedSubmission;
            const appendFenceEndTimeMs =
              yield* resolveCommitAppendFenceEndTimeCapLocal(
                lucid.api,
                {
                  stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
                  stateQueuePolicyId: contracts.stateQueue.policyId,
                },
                speculativeBuild?.base.blockEndTimeMs,
              );
            return resolveHistoryCommitEndTime({
              lucid: lucid.api,
              currentSlot: submitSlot.currentSlot,
              latestEndTime:
                latestEndTimeMsForSchedulerPlanning ??
                currentBlockStartTime.getTime(),
              nowMs: schedulerPlanningNowMs,
              minimumFutureBufferMs: HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
              eventEndTimeMs: Math.min(
                historyEndTime.getTime(),
                effectiveUserEventOnlyEndTime.getTime(),
              ),
              schedulerWindowEndTimeMs:
                schedulerAwareCommitSelection.blockEndTimeCapMs,
              appendFenceEndTimeMs,
            });
          });
    if (historyCommitEndTimeFit?.status === "exceeds_cap") {
      yield* Effect.logInfo(
        `🔹 History commit floors exceed its end-time caps (${historyCommitEndTimeFit.reason}).`,
      );
    }
    const blockEndTimeCapMs =
      historyCommitEndTimeFit === undefined
        ? schedulerAwareCommitSelection.blockEndTimeCapMs
        : historyCommitEndTimeFit.maximumEndTimeMs;
    const fixedHistoryEndTime =
      historyCommitEndTimeFit === undefined
        ? undefined
        : new Date(historyCommitEndTimeFit.resolvedEndTime - 1);
    if (
      shouldDeferCommitSubmission({
        localFinalizationPending: workerInput.data.localFinalizationPending,
        hasAvailableConfirmedBlock: hasAvailableLocalFinalizationBlock,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 Local finalization pending and no recoverable confirmed block is available yet; deferring new submission.",
      );
      return {
        type: "NothingToCommitOutput",
      } satisfies WorkerOutput;
    }

    const recoverableLocalFinalizationBlock =
      workerInput.data.localFinalizationPending &&
      hasAvailableLocalFinalizationBlock
        ? availableLocalFinalizationBlock
        : undefined;
    if (recoverableLocalFinalizationBlock !== undefined) {
      const recoverableConfirmedBlock = yield* deserializeStateQueueUTxO(
        recoverableLocalFinalizationBlock,
      );
      const lucid = yield* acquireCommitLucidOnce;
      return yield* recoverLocalFinalizationAgainstConfirmedBlock({
        latestBlock: recoverableConfirmedBlock,
        transactionsMpf,
        processedMempoolTxs: [],
        mempoolTxHashes: [],
        workerInput,
        sizeOfProcessedTxs: 0,
        consensusProfile: deploymentIdentity.consensusProfile,
      }).pipe(Effect.provideService(Lucid, lucid));
    }

    if (speculativeBuild === undefined) {
      const foreignEventResolution =
        yield* reconcileOverdueAwaitingEventsAgainstRetainedForeignTips();
      if (foreignEventResolution.type === "AwaitingForeignDa") {
        return {
          type: "AwaitingForeignDaOutput",
          foreignHeaderHash: foreignEventResolution.foreignHeaderHash,
          reason: `${foreignEventResolution.reason}:${foreignEventResolution.detail}`,
        } satisfies WorkerOutput;
      }
    }

    // Events after a source-owned block's fixed end are not work for it; an
    // event horizon far past that end must not drive empty commits.
    const pendingUserEventCounts = yield* pendingUserEventCountsUpTo(
      fixedHistoryEndTime ?? effectiveUserEventOnlyEndTime,
    );
    const pendingUserEventCount =
      pendingUserEventCounts.deposits +
      pendingUserEventCounts.forcedTransactions +
      pendingUserEventCounts.withdrawals;
    if (
      shouldSkipIdleCommitBehindUnmergedTail({
        localFinalizationPending: workerInput.data.localFinalizationPending,
        stateQueueHasUnmergedTail:
          workerInput.data.stateQueueHasUnmergedTail ?? false,
        mempoolTxCount: mempoolTxs.length,
        processedTxCount: processedPendingTxs.length,
        pendingUserEventCount,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 State queue has an unmerged tail and no pending tx/user-event work; waiting for merge before the next commit attempt.",
      );
      return {
        type: "NothingToCommitOutput",
      } satisfies WorkerOutput;
    }

    if (
      shouldShortCircuitIdleCommitAttempt({
        candidateTxCount: candidateSelection.candidateTxs.length,
        processedPendingTxCount: processedPendingTxs.length,
        pendingUserEventCount,
        localFinalizationPending: workerInput.data.localFinalizationPending,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 No pending tx/user-event work for block commitment; skipping commit base hydration.",
      );
      return {
        type: "NothingToCommitOutput",
      } satisfies WorkerOutput;
    }

    const baseHydrationStartedAtMs = Date.now();
    baseHydrationPasses += 1;
    const commitBase = yield* resolveCommitBaseLedgerEntries({
      availableConfirmedBlock,
      speculativeBase: speculativeBuild?.base,
      nativeMpfRoot: workerInput.nativeMpf.durableRoot,
      requireEntries: shouldHydrateCommitBaseEntries({
        payloadRootCheck: nodeConfig.MPF_PAYLOAD_ROOT_CHECK,
        recordCorpus: nodeConfig.MPF_RECORD_CORPUS,
        candidateTxCount: candidateSelection.candidateTxs.length,
        pendingForcedTransactionCount:
          pendingUserEventCounts.forcedTransactions,
        pendingWithdrawalCount: pendingUserEventCounts.withdrawals,
      }),
    });
    const initialLedgerEntries = yield* alignCommitMpfsToBase({
      nativeMpfRoot: workerInput.nativeMpf.durableRoot,
      transactionsMpf,
      base: commitBase,
    });
    const nativeMpfContext: NativeMpfBuildContext = {
      client: nativeMpfClient,
      handle: yield* Effect.tryPromise({
        try: () => nativeMpfClient.fork(commitBase.root),
        catch: (cause) =>
          new CommitWorkerInvariantError({
            message: `Architecture G fork failed: ${String(cause)}`,
          }),
      }),
      ownerBinarySha256: workerInput.nativeMpf.ownerBinarySha256,
    };
    if (nativeMpfState !== undefined) {
      nativeMpfState.context = nativeMpfContext;
    }
    yield* Effect.logInfo(
      `🔹 Commit base hydration phase completed duration_ms=${Math.max(
        0,
        Date.now() - baseHydrationStartedAtMs,
      ).toString()},source=${commitBase.source},base_entry_count=${initialLedgerEntries.length.toString()}`,
    );
    if (speculativeBuild !== undefined) {
      yield* reachPipelinedCommitCrashCheckpoint("speculative_mid_build");
    }
    const mpfProcessingStartedAtMs = Date.now();
    mpfProcessingPasses += 1;
    // Canonical V1 is the only consensus profile this node can carry: the
    // deployment manifest parser and the derived-contract path both reject
    // anything that is not exactly MIDGARD_CONSENSUS_PROFILE_V1, so forced
    // proof validation is unconditional and its slot mapping is required.
    // The mapping is plain node-selected data, which keeps candidate
    // construction provider-free.
    const proofValidationSlotConfig =
      workerInput.data.forcedValidationSlotConfig;
    if (proofValidationSlotConfig === undefined) {
      return yield* Effect.fail(
        new Error(
          "Canonical V1 commitment input is missing its node-selected slot configuration",
        ),
      );
    }
    const processed = yield* processMpfs(
      transactionsMpf,
      candidateSelection.candidateTxs,
      {
        fixedBlockEndTime: fixedHistoryEndTime,
        currentBlockStartTime: canBuildOnConfirmedBlock
          ? currentBlockStartTime
          : undefined,
        processedOnlyEndTime:
          candidateSelection.sourceTable === ProcessedMempoolDB.tableName
            ? candidateSelection.candidateTxs[0]?.[TxColumns.TIMESTAMPTZ]
            : undefined,
        depositVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? depositIngestionBarrierTime
          : undefined,
        withdrawalVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? withdrawalIngestionBarrierTime
          : undefined,
        txOrderVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? txOrderIngestionBarrierTime
          : undefined,
        depositOnlyEndTime: canBuildOnConfirmedBlock
          ? effectiveUserEventOnlyEndTime
          : undefined,
        initialLedgerEntries,
        consensusProfile: deploymentIdentity.consensusProfile,
        forcedValidation: {
          expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
          minFeeA: nodeConfig.MIN_FEE_A,
          minFeeB: nodeConfig.MIN_FEE_B,
          bucketConcurrency: nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
          slotForUnixTime: (unixTimeMs) =>
            BigInt(
              unixTimeToSlotForConfig(unixTimeMs, proofValidationSlotConfig),
            ),
        },
        selectedBaseUtxoRoot: commitBase.root,
        payloadRootCheck: nodeConfig.MPF_PAYLOAD_ROOT_CHECK,
        baseUtxoPayloadAggregate: commitBase.utxoPayloadAggregate,
        recordCorpusPath: nodeConfig.MPF_RECORD_CORPUS,
        excludedDepositEventIds:
          speculativeBuild === undefined
            ? undefined
            : new Set(speculativeBuild.excludedDepositEventIds),
        excludedForcedTransactionEventIds:
          speculativeBuild === undefined
            ? undefined
            : new Set(speculativeBuild.excludedForcedTransactionEventIds),
        excludedWithdrawalEventIds:
          speculativeBuild === undefined
            ? undefined
            : new Set(speculativeBuild.excludedWithdrawalEventIds),
        deferDatabaseWrites: speculativeBuild !== undefined,
        onMempoolLedgerReverted: notifyParent?.(MEMPOOL_LEDGER_REVERTED_NOTICE),
        nativeMpf: nativeMpfContext,
      },
    );
    const mpfProcessingFinishedAtMs = Date.now();
    yield* Effect.logInfo(
      `pipeline_trace phase=mpf_processing_finished at_ms=${mpfProcessingFinishedAtMs.toString()} duration_ms=${Math.max(0, mpfProcessingFinishedAtMs - mpfProcessingStartedAtMs).toString()}`,
    );

    const {
      utxoRoot,
      rawTxRoot,
      txRoot,
      transitionTraceRoot,
      eventToStepRoot,
      validationTracesRoot,
      transitionTraceMembers,
      eventToStepMembers,
      validationTraceMembers,
      transitionStepCount,
      validationTraceCount,
      utxoPayloadEntries,
      ledgerDelta,
      utxoPayloadAggregate,
      rejectedMempoolTxsCount,
      rejectedMempoolTxHashes,
      rejectionEntries,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
      includedForcedTransactionEntriesCount,
      includedForcedTransactionEntries,
      includedForcedTransactionEventIds,
      includedWithdrawalEntriesCount,
      includedWithdrawalEntries,
      includedWithdrawalEventIds,
      nativeMpfReplay,
      nativeMpfHandle,
    } = processed;

    const attachNativeMpfPromotion = (output: WorkerOutput): WorkerOutput => {
      if (
        nativeMpfHandle === undefined ||
        (output.type !== "SubmittedAwaitingConfirmationOutput" &&
          output.type !== "SubmittedAwaitingLocalFinalizationOutput" &&
          output.type !== "SuccessfulSubmissionOutput")
      ) {
        return output;
      }
      if (nativeMpfState !== undefined) nativeMpfState.preserve = true;
      return {
        ...output,
        nativeMpfPromotion: { handle: nativeMpfHandle },
      };
    };

    const processedMempoolTxs = processed.processedMempoolTxs;
    const mempoolTxHashes = processed.mempoolTxHashes;
    const rejectedMempoolTxIdSet = new Set(
      rejectedMempoolTxHashes.map((txId) => txId.toString("hex")),
    );
    const rejectedMempoolTxs = candidateSelection.candidateTxs.filter((entry) =>
      rejectedMempoolTxIdSet.has(entry[TxColumns.TX_ID].toString("hex")),
    );
    const sizeOfProcessedTxs = processed.sizeOfProcessedTxs;
    const mempoolTxSourceTable =
      candidateSelection.sourceTable === "processed_mempool"
        ? ProcessedMempoolDB.tableName
        : candidateSelection.sourceTable === "mempool"
          ? MempoolDB.tableName
          : "none";
    const recordSuccessfulBuildCalibration = (output: WorkerOutput) =>
      Effect.gen(function* () {
        if (
          calibration === undefined ||
          processedMempoolTxs.length === 0 ||
          !shouldPreserveCommitMpfRoots(output)
        ) {
          return;
        }
        const measuredBuildMs = Math.max(
          0,
          mpfProcessingFinishedAtMs - mpfProcessingStartedAtMs,
        );
        const nextEwma = updateCommitBuildEwma({
          previousMsPerTx: calibration.msPerTxEwma,
          measuredBuildMs,
          processedTxCount: processedMempoolTxs.length,
          alpha: nodeConfig.COMMIT_BUILD_EWMA_ALPHA,
        });
        const updated = yield* CommitBuildCalibrationDB.update(nextEwma);
        yield* Effect.logInfo(
          `commit_build_calibration measured_ms_per_tx=${(
            measuredBuildMs / processedMempoolTxs.length
          ).toString()} ewma_ms_per_tx=${updated.msPerTxEwma.toString()} sample_count=${updated.sampleCount.toString()}`,
        );
      });
    if (candidateSelection.sourceTable === "processed_mempool") {
      yield* Effect.logWarning(
        `🔹 Prioritizing ${processedPendingTxs.length.toString()} deferred processed tx(s) before newer mempool tx(s).`,
      );
    }

    if (rejectedMempoolTxsCount > 0) {
      yield* Effect.logWarning(
        `Rejected ${rejectedMempoolTxsCount} malformed tx(s) during commitment preprocessing.`,
      );
    }
    if (includedDepositEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment pre-state includes ${includedDepositEntriesCount} due deposit UTxO(s).`,
      );
    }
    if (includedWithdrawalEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment pre-state includes ${includedWithdrawalEntriesCount} due withdrawal event(s).`,
      );
    }
    if (includedForcedTransactionEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment source set includes ${includedForcedTransactionEntriesCount} due tx-order event(s).`,
      );
    }

    const mempoolTxsCount = processedMempoolTxs.length;
    const optEndTime = establishEndTimeFromTxRequests(processedMempoolTxs);
    let submitAvailableConfirmedBlock = availableConfirmedBlock;
    let submitWorkerInput = workerInput;
    let beforePendingJournalInsert:
      | ((
          blockEndTimeMs: number,
        ) => Effect.Effect<void, DatabaseError, Database>)
      | undefined;
    let afterPendingJournalPrepared: Effect.Effect<void> | undefined;
    let speculativeLedgerReverted = false;

    if (
      submitAvailableConfirmedBlock === "" &&
      speculativeBuild !== undefined
    ) {
      if (awaitSpeculativeInstruction === undefined) {
        return yield* Effect.fail(
          new CommitWorkerInvariantError({
            message: "Speculative commit build requires an instruction channel",
          }),
        );
      }
      const candidateEndTime =
        fixedHistoryEndTime ??
        (Option.isSome(optEndTime)
          ? optEndTime.value
          : effectiveUserEventOnlyEndTime);
      const candidateId = randomUUID();
      const [optDepositsRoot, optForcedTransactionsRoot, optWithdrawalsRoot] =
        yield* Effect.all(
          [
            resolveDepositsRoot(includedDepositEntries),
            resolveForcedTransactionsRoot(
              includedForcedTransactionEntries,
              deploymentIdentity.consensusProfile,
            ),
            resolveWithdrawalsRoot(includedWithdrawalEntries),
          ],
          { concurrency: "unbounded" },
        );
      const depositsRoot = Option.getOrElse(
        optDepositsRoot,
        () => SDK.EMPTY_MERKLE_TREE_ROOT,
      );
      const forcedTransactionsRoot = Option.getOrElse(
        optForcedTransactionsRoot,
        () => SDK.EMPTY_MERKLE_TREE_ROOT,
      );
      const withdrawalsRoot = Option.getOrElse(
        optWithdrawalsRoot,
        () => SDK.EMPTY_MERKLE_TREE_ROOT,
      );
      const candidate = {
        candidateId,
        baseHeaderHash: speculativeBuild.base.headerHash,
        endTimeMs: candidateEndTime.getTime(),
        builtAtMs: Date.now(),
        buildDurationMs: Math.max(0, Date.now() - workerStartedAtMs),
        invalidationKey: `${speculativeBuild.base.headerHash}:${candidateEndTime.getTime().toString()}:${minimumBarrierWatermarkMs(speculativeBuild.watermarks).toString()}`,
        watermarks: speculativeBuild.watermarks,
        expectedUserEventCounts: {
          deposits: includedDepositEventIds.length,
          forcedTransactions: includedForcedTransactionEventIds.length,
          withdrawals: includedWithdrawalEventIds.length,
        },
        expectedL2TransactionCount: processedMempoolTxs.length,
        roots: {
          utxos: utxoRoot,
          rawTransactions: rawTxRoot,
          transactions: txRoot,
          deposits: depositsRoot,
          forcedTransactions: forcedTransactionsRoot,
          withdrawals: withdrawalsRoot,
          transitionTrace: transitionTraceRoot,
          eventToStep: eventToStepRoot,
        },
      } satisfies SpeculativeCandidateReadyOutput["candidate"];
      yield* Effect.logInfo(
        `pipeline_trace phase=candidate_ready candidate_id=${candidateId} base_header_hash=${candidate.baseHeaderHash} build_duration_ms=${candidate.buildDurationMs.toString()} invalidation_key=${candidate.invalidationKey}`,
      );
      yield* reachPipelinedCommitCrashCheckpoint("candidate_ready_unconfirmed");
      speculativeReadyPassCounts = {
        candidateId,
        baseHydrationPasses,
        mpfProcessingPasses,
      };
      const instruction = yield* awaitSpeculativeInstruction(candidate);
      if (instruction.type === "InvalidateSpeculativeCandidate") {
        return {
          type: "SpeculativeCandidateInvalidatedOutput",
          candidateId,
          reason: instruction.reason,
        } satisfies SpeculativeCandidateInvalidatedOutput;
      }
      // Candidate construction remains provider-free. The parent acquires the
      // L1 control-plane permit before sending this submit instruction, so
      // provider-backed scheduler revalidation starts only after CandidateReady.
      const speculativeContracts = yield* MidgardContracts;
      const speculativeLucid = yield* acquireCommitLucidOnce;
      const resumedMpfContext = postWaitMpfContext?.();
      const confirmedBlock = yield* deserializeStateQueueUTxO(
        instruction.confirmedBlock,
      );
      const confirmedHeaderHash =
        confirmedBlock.datum.key === "Empty"
          ? undefined
          : yield* SDK.getHeaderFromStateQueueDatum(confirmedBlock.datum).pipe(
              Effect.flatMap(SDK.hashBlockHeader),
            );
      if (confirmedHeaderHash !== speculativeBuild.base.headerHash) {
        return {
          type: "SpeculativeCandidateInvalidatedOutput",
          candidateId,
          reason: "T2",
        } satisfies SpeculativeCandidateInvalidatedOutput;
      }
      if (instruction.localFinalizationBlock !== undefined) {
        if (activeMpfLeaseOwner === undefined) {
          return yield* Effect.fail(
            new CommitWorkerInvariantError({
              message:
                "Speculative local finalization requires an active MPF lease owner",
            }),
          );
        }
        const recoveryOutput =
          yield* recoverLocalFinalizationAgainstConfirmedBlock({
            latestBlock: yield* deserializeStateQueueUTxO(
              instruction.localFinalizationBlock,
            ),
            transactionsMpf:
              resumedMpfContext?.localFinalizationTransactionsMpf ??
              localFinalizationTransactionsMpf ??
              transactionsMpf,
            processedMempoolTxs: [],
            mempoolTxHashes: [],
            workerInput,
            sizeOfProcessedTxs: 0,
            consensusProfile: deploymentIdentity.consensusProfile,
            beforeTransactionsMpfReset: Effect.all(
              [
                StateQueueMutationLeasesDB.revalidate(
                  instruction.stateQueueLeaseToken,
                ),
                MpfEngineStateDB.revalidateLedgerStoreLease(
                  activeMpfLeaseOwner,
                ),
              ],
              { discard: true },
            ),
          }).pipe(Effect.provideService(Lucid, speculativeLucid));
        if (
          recoveryOutput.type !== "SuccessfulLocalFinalizationRecoveryOutput"
        ) {
          return recoveryOutput;
        }
        if (notifyParent !== undefined) {
          yield* notifyParent(recoveryOutput);
        }
      }
      const excludedUserEventIds = {
        depositEventIds: new Set(speculativeBuild.excludedDepositEventIds),
        forcedTransactionEventIds: new Set(
          speculativeBuild.excludedForcedTransactionEventIds,
        ),
        withdrawalEventIds: new Set(
          speculativeBuild.excludedWithdrawalEventIds,
        ),
      };
      const submitPendingUserEventCount = yield* pendingUserEventCountUpTo(
        candidateEndTime,
        excludedUserEventIds,
      );
      const expectedUserEventCount =
        candidate.expectedUserEventCounts.deposits +
        candidate.expectedUserEventCounts.forcedTransactions +
        candidate.expectedUserEventCounts.withdrawals;
      if (submitPendingUserEventCount !== expectedUserEventCount) {
        return {
          type: "SpeculativeCandidateInvalidatedOutput",
          candidateId,
          reason: "T3",
        } satisfies SpeculativeCandidateInvalidatedOutput;
      }
      yield* speculativeLucid.switchToOperatorsMainWallet;
      const submitSchedulerWindow =
        yield* resolveCurrentOperatorSchedulerWindow(
          speculativeLucid.api,
          speculativeContracts,
        );
      if (submitSchedulerWindow !== undefined) {
        const confirmedEndTimeMs = Number(
          (yield* getLatestBlockDatumEndTime(confirmedBlock.datum)).getTime(),
        );
        const submitFit = resolveCommitEndTimeFit({
          lucid: speculativeLucid.api,
          latestEndTime: confirmedEndTimeMs,
          candidateEndTime: candidateEndTime.getTime(),
          nowMs: Date.now(),
          minimumFutureBufferMs:
            workerInput.history === undefined
              ? COMMIT_MINIMUM_FUTURE_BUFFER_MS
              : HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
          maximumEndTimeMs: submitSchedulerWindow.endTimeMs,
        });
        if (submitFit.status === "exceeds_cap") {
          return {
            type: "SpeculativeCandidateInvalidatedOutput",
            candidateId,
            reason: "T4",
          } satisfies SpeculativeCandidateInvalidatedOutput;
        }
      }
      if (activeMpfLeaseOwner === undefined) {
        return yield* Effect.fail(
          new CommitWorkerInvariantError({
            message:
              "Speculative journal preparation requires an active MPF lease owner",
          }),
        );
      }
      // The speculative builder is read-only. Journal preparation invokes
      // this effect only after confirmation and inside the journal SQL
      // transaction, while both state-queue and MPF leases are held.
      beforePendingJournalInsert = (blockEndTimeMs) =>
        revalidateAndPersistSpeculativeCandidateSources({
          includedDepositEntries,
          includedForcedTransactionEntries,
          includedWithdrawalEntries,
          selectedMempoolTxs: processedMempoolTxs,
          rejectedMempoolTxs,
          mempoolTxSourceTable,
          rejectionEntries,
          ledgerRevert: processed.ledgerRevert,
          expectedEventRoots: {
            deposits: candidate.roots.deposits,
            forcedTransactions: candidate.roots.forcedTransactions,
            withdrawals: candidate.roots.withdrawals,
          },
          candidateEndTime: new Date(blockEndTimeMs),
          excludedUserEventIds,
          stateQueueLeaseToken: instruction.stateQueueLeaseToken,
          activeMpfLeaseOwner,
          consensusProfile: deploymentIdentity.consensusProfile,
        }).pipe(
          Effect.map((reverted) => {
            speculativeLedgerReverted = reverted;
          }),
        );
      // The revert commits with the journal, so the parent hears of it only
      // once the journal transaction has committed.
      afterPendingJournalPrepared = Effect.suspend(() =>
        speculativeLedgerReverted && notifyParent !== undefined
          ? notifyParent(MEMPOOL_LEDGER_REVERTED_NOTICE)
          : Effect.void,
      );
      submitAvailableConfirmedBlock = instruction.confirmedBlock;
      submitWorkerInput = {
        ...workerInput,
        data: {
          ...workerInput.data,
          availableConfirmedBlock: instruction.confirmedBlock,
          localFinalizationPending: false,
          stateQueueLeaseToken: instruction.stateQueueLeaseToken,
          baseSnapshotId: instruction.baseSnapshotId,
          stateQueueHasUnmergedTail: instruction.stateQueueHasUnmergedTail,
          speculativeBuild: undefined,
        },
      };
    }

    const submissionContracts = yield* MidgardContracts;
    const submissionLucid = yield* acquireCommitLucidOnce;

    if (submitAvailableConfirmedBlock === "") {
      // The tx confirmation worker has not yet confirmed a previously
      // submitted tx, so the root we have found can not be used yet.
      // However, it is stored on disk in our LevelDB mempool. Therefore,
      // the processed txs must be transferred to `ProcessedMempoolDB` from
      // `MempoolDB`.
      if (mempoolTxSourceTable === ProcessedMempoolDB.tableName) {
        yield* Effect.logInfo(
          "🔹 No confirmed block available and selected tx payload is already durable in ProcessedMempoolDB; preserving it for the next commit attempt.",
        );
        return {
          type: "SkippedSubmissionOutput",
          mempoolTxsCount: 0,
          sizeOfProcessedTxs: 0,
        } satisfies WorkerOutput;
      }
      const output = yield* deferProcessedCommitPayloadUntilConfirmation({
        processedMempoolTxs,
        mempoolTxHashes,
        mempoolTxsCount,
        sizeOfProcessedTxs,
      });
      yield* recordSuccessfulBuildCalibration(output);
      return output;
    } else {
      yield* Effect.logInfo(
        "🔹 Previous submitted block is now confirmed, deserializing...",
      );
      const latestBlock = yield* deserializeStateQueueUTxO(
        submitAvailableConfirmedBlock,
      );

      if (Option.isNone(optEndTime)) {
        // No transaction requests found (neither in `ProcessedMempoolDB`, nor
        // in `MempoolDB`). We check if there are any user events slated for
        // inclusion within `startTime` and current moment.
        yield* Effect.logInfo(
          "🔹 Checking for user events... (no tx requests in queue)",
        );
        const output = yield* submitDepositOnlyCommit({
          contracts: submissionContracts,
          consensusProfile: deploymentIdentity.consensusProfile,
          deploymentMarker,
          latestBlock,
          endTime: fixedHistoryEndTime ?? effectiveUserEventOnlyEndTime,
          includedDepositEntries,
          includedDepositEventIds,
          includedForcedTransactionEntries,
          includedForcedTransactionEventIds,
          includedWithdrawalEntries,
          includedWithdrawalEventIds,
          workerInput: submitWorkerInput,
          blockEndTimeCapMs,
          utxoRoot,
          txRoot,
          transitionTraceRoot,
          eventToStepRoot,
          validationTracesRoot,
          transitionTraceMembers,
          eventToStepMembers,
          validationTraceMembers,
          transitionStepCount,
          validationTraceCount,
          utxoPayloadEntries,
          ledgerDelta,
          utxoPayloadAggregate,
          selectedBaseUtxosRoot: commitBase.root,
          implicitGenesisEntries:
            commitBase.source === "genesis" ? initialLedgerEntries : [],
          beforePendingJournalInsert,
          afterPendingJournalPrepared,
          nativeMpfReplay,
        }).pipe(Effect.provideService(Lucid, submissionLucid));
        return attachNativeMpfPromotion(
          attachSpeculativeExecutionEvidence(output),
        );
      } else {
        // One or more transactions found in either `ProcessedMempoolDB` or
        // `MempoolDB`. Use the shared max-candidate timestamp rule as the upper
        // bound of the block we are about to submit.
        const endTime = fixedHistoryEndTime ?? optEndTime.value;

        yield* Effect.logInfo("🔹 Checking for user events...");
        const output = yield* submitTxBackedCommit({
          contracts: submissionContracts,
          consensusProfile: deploymentIdentity.consensusProfile,
          deploymentMarker,
          latestBlock,
          endTime,
          includedDepositEntries,
          includedDepositEventIds,
          includedForcedTransactionEntries,
          includedForcedTransactionEventIds,
          includedWithdrawalEntries,
          includedWithdrawalEventIds,
          utxoRoot,
          txRoot,
          transitionTraceRoot,
          eventToStepRoot,
          validationTracesRoot,
          transitionTraceMembers,
          eventToStepMembers,
          validationTraceMembers,
          transitionStepCount,
          validationTraceCount,
          utxoPayloadEntries,
          ledgerDelta,
          utxoPayloadAggregate,
          selectedBaseUtxosRoot: commitBase.root,
          implicitGenesisEntries:
            commitBase.source === "genesis" ? initialLedgerEntries : [],
          transactionsMpf:
            postWaitMpfContext?.().transactionsMpf ?? transactionsMpf,
          processedMempoolTxs,
          mempoolTxHashes,
          mempoolTxSourceTable,
          workerInput: submitWorkerInput,
          sizeOfProcessedTxs,
          blockEndTimeCapMs,
          beforePendingJournalInsert,
          afterPendingJournalPrepared,
          nativeMpfReplay,
        }).pipe(Effect.provideService(Lucid, submissionLucid));
        yield* recordSuccessfulBuildCalibration(output);
        return attachNativeMpfPromotion(
          attachSpeculativeExecutionEvidence(output),
        );
      }
    }
  });
